{-# LANGUAGE OverloadedStrings #-}
module Budget where

import Data.List (sort)

type Envelope = String

data Date = Date Int Int Int deriving Show

instance Eq Date where
    (Date y1 m1 d1) == (Date y2 m2 d2) = y1 == y2 && m1 == m2 && d1 == d2

instance Ord Date
    where compare (Date y1 m1 d1) (Date y2 m2 d2)
            | y1 == y2 && m1 == m2      = compare d1 d2
            | y1 == y2                  = compare m1 m2
            | otherwise                 = compare y1 y2

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

data Demand = Demand
    { demandPeriod :: Period
    , demandEnvelope :: Envelope
    , demandAmount :: Int
    } deriving (Eq, Show)

instance Ord Demand
    where compare (Demand p1 _ _) (Demand p2 _ _)
            | p1 < p2   = LT
            | otherwise = GT

data Fill = Fill
    { fillEnvelope :: Envelope
    , fillDate :: Date
    , fillAmount :: Int
    } deriving (Eq, Show)

data Income = Income
    { incomeDate :: Date
    , incomeAmount :: Int
    } deriving (Eq, Show)

instance Ord Income
    where compare (Income d1 _) (Income d2 _)
            | d1 < d2   = LT
            | otherwise = GT

data BudgetRequest = BudgetRequest
    { income :: [Income]
    , demands :: [Demand]
    , openingBalance :: Int
    } deriving Show

data BudgetResponse = BudgetResponse
    { fills :: [Fill]
    , closingBalance :: Int
    } deriving Show

toFills :: [Income] -> [Demand] -> Int -> Date -> ([Fill], Int)
toFills income demands balance currentDate = do
    case nextItem income demands of
        Nothing -> ([], balance)
        Just item -> do
            case item of
                Left i -> toFills (tail income) demands newBalance newDate
                    where newBalance        = balance + incomeAmount i
                          newDate           = incomeDate i
                Right d -> (newFill : fst otherFills, remainingBalance)
                    where amount            = max 0 $ min (demandAmount d) balance
                          remainingBalance  = balance - amount
                          newFill           = Fill (demandEnvelope d) currentDate amount
                          otherFills        = toFills income (tail demands)
                                                    remainingBalance currentDate

nextItem :: [Income] -> [Demand] -> Maybe (Either Income Demand)
nextItem [] [] = Nothing
nextItem [] (d:ds) = Just $ Right d
nextItem (i:is) [] = Just $ Left i
nextItem (i:is) (d:ds)
            | (incomeDate i) < (startDate $ demandPeriod d) = Just $ Left i
            | otherwise = Just $ Right d

toBudgetResponse :: ([Fill], Int) -> BudgetResponse
toBudgetResponse (f, b) = BudgetResponse f b

budget :: BudgetRequest -> BudgetResponse
budget (BudgetRequest i d b) = toBudgetResponse $ toFills (sort i) (sort d) b (Date 2015 1 1)
