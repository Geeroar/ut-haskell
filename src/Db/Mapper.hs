{-# LANGUAGE OverloadedStrings #-}
module Db.Mapper where

import Control.Applicative
import Database.MongoDB
import Text.Read (readMaybe)
import Types

budgetToDocument :: Budget -> Document
budgetToDocument (Budget bid uid i d f ob cb) = (idFieldIfExists bid) ++
    [ "userId"          =: uid
    , "income"          =: (incomeToDocuments i)
    , "demands"         =: (demandsToDocuments d)
    , "fills"           =: (fillsToDocuments f)
    , "openingBalance"  =: ob
    , "closingBalance"  =: cb
    ]
    where idFieldIfExists Nothing = []
          idFieldIfExists (Just bid') = ["_id" =: bid']
          demandsToDocuments [] = []
          demandsToDocuments (x:xs) = demandToDocument x : demandsToDocuments xs
          fillsToDocuments [] = []
          fillsToDocuments (x:xs) = fillToDocument x : fillsToDocuments xs
          incomeToDocuments [] = []
          incomeToDocuments (x:xs) = incomeToDocument x : incomeToDocuments xs

documentToBudget :: Document -> Maybe Budget
documentToBudget d = Budget <$>
                        d !? "_id" <*>
                        at "userId" d <*>
                        (documentsToIncome <$> d !? "income") <*>
                        (documentsToDemands <$> d !? "demands") <*>
                        (documentsToFills <$> d !? "fills") <*>
                        at "openingBalance" d <*>
                        at "closingBalance" d
                where documentsToDemands []     = []
                      documentsToDemands (x:xs) = documentToDemand x : documentsToDemands xs
                      documentsToFills []       = []
                      documentsToFills (x:xs)   = documentToFill x : documentsToFills xs
                      documentsToIncome []      = []
                      documentsToIncome (x:xs)  = documentToIncome x : documentsToIncome xs

documentToDemand :: Document -> Demand
documentToDemand d = Demand (documentToPeriod $ at "period" d)
                            (at "envelope" d)
                            (at "amount" d)

documentToFill :: Document -> Fill
documentToFill d = Fill (at "date" d)
                        (at "envelope" d)
                        (at "amount" d)

documentToIncome :: Document -> Income
documentToIncome d = Income (at "date" d)
                            (at "amount" d)

documentToPeriod :: Document -> Period
documentToPeriod d = Period (at "start" d)
                            (at "end" d)

demandToDocument :: Demand -> Document
demandToDocument (Demand p e a) = [ "period" =: (periodToDocument p)
                                  , "envelope" =: e
                                  , "amount" =: a
                                  ]

fillToDocument :: Fill -> Document
fillToDocument (Fill d e a) = ["date" =: d, "envelope" =: e, "amount" =: a]

incomeToDocument :: Income -> Document
incomeToDocument (Income d a) = ["date" =: d, "amount" =: a]

maybeDocumentToBudget :: Maybe Document -> Maybe Budget
maybeDocumentToBudget doc = case doc of
                                Just d  -> documentToBudget d
                                Nothing -> Nothing

periodToDocument :: Period -> Document
periodToDocument (Period s e) = ["start" =: s, "end" =: e]

stringToObjectId :: String -> Maybe ObjectId
stringToObjectId s = readMaybe s
