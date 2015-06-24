{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Database.MongoDB (ObjectId)
import Util


--
-- Envelope
--
type Envelope = String


--
-- Demand Colour
--
data DemandColour = Red | Orange | Yellow | Green deriving (Eq, Read, Show)

instance ToJSON DemandColour where
    toJSON c = toJSON $ show c

--
-- Period
--
data Period = Period
    { startDate :: UTCTime
    , endDate :: UTCTime
    } deriving Show

instance Eq Period where
    (Period s1 e1) == (Period s2 e2) = s1 == s2 && e1 == e2

instance Ord Period where
    compare (Period s1 e1) (Period s2 e2)
            | s1 == s2  = compare e1 e2
            | otherwise = compare s1 s2

instance FromJSON Period where
    parseJSON (Object p) = do 
                            s <- liftM parseBudgetTime (p .: "start")
                            e' <- p .:? "end"
                            e <- return (parseBudgetTime <$> e') .!= s
                            return $ Period s e
    parseJSON _          = mzero
        

instance ToJSON Period where
    toJSON (Period s e) = object ["start" .= s, "end" .= e]


--
-- Demand
--
data Demand = Demand
    { demandPeriod :: Period
    , demandEnvelope :: Envelope
    , demandAmount :: Int
    } deriving Show

instance Eq Demand where
    (Demand p1 e1 _) == (Demand p2 e2 _) = p1 == p2 && e1 == e2

instance Ord Demand where
    compare (Demand p1 _ _) (Demand p2 _ _)
            | p1 < p2   = LT
            | otherwise = GT

instance FromJSON Demand where
    parseJSON (Object d) = Demand <$>
                           d .: "period" <*>
                           d .: "envelope" <*>
                           d .: "amount"
    parseJSON _          = mzero

instance ToJSON Demand where
    toJSON (Demand p e a) = object ["period" .= p, "envelope" .= e, "amount" .= a]


--
-- Demand Summary
--
data DemandSummary = DemandSummary
    { demandSummaryDemand :: Demand
    , demandSummaryFillAmount :: Int
    , demandSummaryColour :: DemandColour
    } deriving (Eq, Show)

instance ToJSON DemandSummary where
    toJSON (DemandSummary d fa c) = object
        [ "demand" .= d
        , "fillAmount" .= fa
        , "colour" .= c
        ]


--
-- Fill
--
data Fill = Fill
    { fillDate :: UTCTime
    , fillDemand :: Demand
    , fillAmount :: Int
    } deriving (Eq, Show)

instance FromJSON Fill where
    parseJSON (Object f) = Fill <$>
                           liftM parseBudgetTime (f .: "date") <*>
                           f .: "envelope" <*>
                           f .: "amount"
    parseJSON _          = mzero

instance ToJSON Fill where
    toJSON (Fill d dem a) = object
        [ "date" .= d
        , "envelope" .= (demandEnvelope dem)
        , "demandPeriod" .= (demandPeriod dem)
        , "amount" .= a
        ]


--
-- Income
--
data Income = Income
    { incomeDate :: UTCTime
    , incomeAmount :: Int
    } deriving (Eq, Show)

instance Ord Income where
    compare (Income d1 _) (Income d2 _)
            | d1 < d2   = LT
            | otherwise = GT

instance FromJSON Income where
    parseJSON (Object i) = Income <$>
                           liftM parseBudgetTime (i .: "date") <*>
                           i .: "amount"
    parseJSON _          = mzero

instance ToJSON Income where
    toJSON (Income d a) = object ["date" .= d, "amount" .= a]


--
-- Budget
--
data Budget = Budget
    { budgetId :: Maybe ObjectId
    , budgetUserId :: String
    , budgetIncome :: [Income]
    , budgetDemands :: [Demand]
    , budgetFills :: [Fill]
    , budgetDemandSummaries :: [DemandSummary]
    , budgetOpeningBalance :: Int
    , budgetClosingBalance :: Int
    } deriving Show

instance ToJSON Budget where
    toJSON (Budget bid uid i d f s ob cb) = object
        [ "id"                  .= (show <$> bid)
        , "userId"              .= uid
        , "income"              .= i
        , "demands"             .= d
        , "fills"               .= f
        , "demandSummaries"     .= s
        , "openingBalance"      .= ob
        , "closingBalance"      .= cb
        ]


--
-- BudgetRequest
--
data BudgetRequest = BudgetRequest
    { income :: [Income]
    , demands :: [Demand]
    , openingBalance :: Int
    } deriving Show

instance FromJSON BudgetRequest where
    parseJSON (Object r) = BudgetRequest <$>
                           r .:? "income" .!= [] <*>
                           r .:? "demands" .!= [] <*>
                           r .:? "balance" .!= 0
    parseJSON _          = mzero

instance ToJSON BudgetRequest where
    toJSON (BudgetRequest i d b) = object ["income" .= i, "demands" .= d, "balance" .= b]
