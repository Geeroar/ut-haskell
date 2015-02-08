{-# LANGUAGE OverloadedStrings #-}
module Budget where

type Date = (Int, Int, Int)
type Envelope = String
type Period = (Date, Date)

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

demands :: [Demand]
demands =
    [ Demand ((2014, 1, 1), (2014, 1, 10)) "Car" 200
    , Demand ((2014, 1, 2), (2014, 1, 2)) "Bike" 100
    , Demand ((2014, 1, 2), (2014, 1, 4)) "Bike" 500
    , Demand ((2014, 1, 4), (2014, 1, 5)) "Car" 100
    ]

income :: [Income]
income =
    [ Income (2014, 1, 1) 1500
    , Income (2014, 1, 30) 1500
    ]

fill :: Demand -> Int -> Fill
fill (Demand (s, _) e _) a = Fill e s a

getFillAmount :: Demand -> Int -> Int
getFillAmount (Demand _ _ a) b = max 0 (min a b)

budget :: [Income] -> [Demand] -> Int -> [Fill]
budget _ [] _ = []
budget i (x:xs) b
            | a > 0     = fill x a : budget i xs (b - a)
            | otherwise = budget i xs b
            where a = getFillAmount x b

runBudget :: Int -> [Fill]
runBudget = budget income demands
