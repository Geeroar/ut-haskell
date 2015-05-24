{-# LANGUAGE OverloadedStrings #-}
module Budget where

type Envelope = String

data Date = Date Int Int Int deriving Show

data Period = Period
    { startDate :: Date
    , endDate :: Date
    } deriving Show

data Demand = Demand
    { demandPeriod :: Period
    , demandEnvelope :: Envelope
    , demandAmount :: Int
    } deriving Show

data Fill = Fill
    { fillEnvelope :: Envelope
    , fillDate :: Date
    , fillAmount :: Int
    } deriving Show

data Income = Income
    { incomeDate :: Date
    , incomeAmount :: Int
    } deriving Show

data BudgetRequest = BudgetRequest
    { income :: [Income]
    , demands :: [Demand]
    , openingBalance :: Int
    } deriving Show

data BudgetResponse = BudgetResponse
    { fills :: [Fill]
    , closingBalance :: Int
    } deriving Show

fill :: Demand -> Int -> Fill
fill (Demand (Period s _) e _) a = Fill e s a

toFillAmount :: Demand -> Int -> Int
toFillAmount (Demand _ _ a) b = max 0 (min a b)

toFills :: [Income] -> [Demand] -> Int -> ([Fill], Int)
toFills _ [] b = ([], b)
toFills i (d:ds) b
            | a > 0     = (fill d a : fst x, snd x)
            | otherwise = (fst y, snd y)
            where a = toFillAmount d b
                  x = toFills i ds (b - a)
                  y = toFills i ds b

toBudgetResponse :: ([Fill], Int) -> BudgetResponse
toBudgetResponse (f, b) = BudgetResponse f b

toBudgetResponseBalance :: BudgetResponse -> Int
toBudgetResponseBalance (BudgetResponse _ b) = b

budget :: BudgetRequest -> BudgetResponse
budget (BudgetRequest i d b) = toBudgetResponse $ toFills i d b
