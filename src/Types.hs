{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text (splitOn, unpack)


--
-- Envelope
--
type Envelope = String


--
-- Date
--
data Date = Date Int Int Int deriving Show

instance Eq Date where
    (Date y1 m1 d1) == (Date y2 m2 d2) = y1 == y2 && m1 == m2 && d1 == d2

instance Ord Date
    where compare (Date y1 m1 d1) (Date y2 m2 d2)
            | y1 == y2 && m1 == m2      = compare d1 d2
            | y1 == y2                  = compare m1 m2
            | otherwise                 = compare y1 y2

instance FromJSON Date where
    parseJSON (String s) = Date <$>
                           (parseJSON $ Number $ y) <*>
                           (parseJSON $ Number $ m) <*>
                           (parseJSON $ Number $ d)
                where (y:m:d:_) = map (read . unpack) $ splitOn "-" s
    parseJSON _          = mzero

instance ToJSON Date where
    toJSON (Date y m d) = toJSON $ (show y) ++ "-" ++ (show m) ++ "-" ++ (show d)


--
-- Period
--
data Period = Period
    { startDate :: Date
    , endDate :: Date
    } deriving Show

instance Eq Period where
    (Period s1 e1) == (Period s2 e2) = s1 == s2 && e1 == e2

instance Ord Period
    where compare (Period s1 e1) (Period s2 e2)
            | s1 == s2  = compare e1 e2
            | otherwise = compare s1 s2

instance FromJSON Period where
    parseJSON (Object p) = Period <$>
                           p .: "start" <*>
                           p .:? "end" .!= Date 0 0 0
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
    } deriving (Eq, Show)

instance Ord Demand
    where compare (Demand p1 _ _) (Demand p2 _ _)
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
-- Fill
--
data Fill = Fill
    { fillEnvelope :: Envelope
    , fillDate :: Date
    , fillAmount :: Int
    } deriving (Eq, Show)

instance FromJSON Fill where
    parseJSON (Object f) = Fill <$>
                           f .: "envelope" <*>
                           f .: "date" <*>
                           f .: "amount"
    parseJSON _          = mzero

instance ToJSON Fill where
    toJSON (Fill e d a) = object ["envelope" .= e, "date" .= d, "amount" .= a]


--
-- Income
--
data Income = Income
    { incomeDate :: Date
    , incomeAmount :: Int
    } deriving (Eq, Show)

instance Ord Income
    where compare (Income d1 _) (Income d2 _)
            | d1 < d2   = LT
            | otherwise = GT

instance FromJSON Income where
    parseJSON (Object i) = Income <$>
                           i .: "date" <*>
                           i .: "amount"
    parseJSON _          = mzero

instance ToJSON Income where
    toJSON (Income d a) = object ["date" .= d, "amount" .= a]


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


--
-- BudgetResponse
--
data BudgetResponse = BudgetResponse
    { fills :: [Fill]
    , closingBalance :: Int
    } deriving Show

instance FromJSON BudgetResponse where
    parseJSON (Object r) = BudgetResponse <$>
                           r .: "fills" <*>
                           r .: "balance"
    parseJSON _          = mzero

instance ToJSON BudgetResponse where
    toJSON (BudgetResponse f b) = object ["fills" .= f, "balance" .= b]
