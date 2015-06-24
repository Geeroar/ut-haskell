{-# LANGUAGE OverloadedStrings #-}
module Db.Mapper where

import Control.Applicative
import Database.MongoDB
import Text.Read (readMaybe)
import Types

budgetToDocument :: Budget -> Document
budgetToDocument (Budget bid uid i d f ds ob cb) = (idFieldIfExists bid) ++
    [ "userId"          =: uid
    , "income"          =: (incomeToDocuments i)
    , "demands"         =: (demandsToDocuments d)
    , "fills"           =: (fillsToDocuments f)
    , "demandSummaries" =: (summariesToDocuments ds)
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
          summariesToDocuments [] = []
          summariesToDocuments (x:xs) = summaryToDocument x : summariesToDocuments xs

documentToBudget :: Document -> Maybe Budget
documentToBudget d = Budget <$>
    d !? "_id" <*>
    at "userId" d <*>
    (documentsToIncome <$> d !? "income") <*>
    (documentsToDemands <$> d !? "demands") <*>
    (documentsToFills <$> d !? "fills") <*>
    (documentsToSummaries <$> d !? "demandSummaries") <*>
    at "openingBalance" d <*>
    at "closingBalance" d
    where documentsToDemands []         = []
          documentsToDemands (x:xs)     = documentToDemand x : documentsToDemands xs
          documentsToFills []           = []
          documentsToFills (x:xs)       = documentToFill x : documentsToFills xs
          documentsToIncome []          = []
          documentsToIncome (x:xs)      = documentToIncome x : documentsToIncome xs
          documentsToSummaries []       = []
          documentsToSummaries (x:xs)   = documentToSummary x : documentsToSummaries xs

documentToDemand :: Document -> Demand
documentToDemand d = Demand (documentToPeriod $ at "period" d)
                            (at "envelope" d)
                            (at "amount" d)

documentToFill :: Document -> Fill
documentToFill d = Fill (at "date" d)
                        (documentToDemand $ at "demand" d)
                        (at "amount" d)

documentToIncome :: Document -> Income
documentToIncome d = Income (at "date" d)
                            (at "amount" d)

documentToPeriod :: Document -> Period
documentToPeriod d = Period (at "start" d)
                            (at "end" d)

documentToSummary :: Document -> DemandSummary
documentToSummary d = DemandSummary (documentToDemand $ at "demand" d)
                                    (at "fillAmount" d)
                                    (read $ at "colour" d)

demandToDocument :: Demand -> Document
demandToDocument (Demand p e a) = [ "period" =: (periodToDocument p)
                                  , "envelope" =: e
                                  , "amount" =: a
                                  ]

fillToDocument :: Fill -> Document
fillToDocument (Fill d dem a) = [ "date" =: d
                                , "demand" =: (demandToDocument dem)
                                , "amount" =: a
                                ]

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

summaryToDocument :: DemandSummary -> Document
summaryToDocument (DemandSummary d a c) = [ "demand" =: (demandToDocument d)
                                          , "fillAmount" =: a
                                          , "colour" =: show c
                                          ]
