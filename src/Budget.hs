module Budget (budget) where

import Data.List (sort)
import Data.Time.Clock
import Types

toFills :: [Income] -> [Demand] -> Int -> UTCTime -> ([Fill], Int)
toFills [] _ 0 _ = ([], 0)
toFills [] [] bal _ = ([], bal)
toFills (i:is) [] bal curDate = toFills is [] (bal + incomeAmount i) curDate
toFills (i:is) dem 0 _ = toFills is laterDemands (incomeAmount i) (incomeDate i)
        where laterDemands = filter (\d -> (startDate $ demandPeriod d) > (incomeDate i)) dem
toFills inc (d:ds) bal curDate = (newFill : fst otherFills, snd otherFills)
        where newFill           = Fill curDate d amount
              otherFills        = toFills inc remainingDemands remainingBalance curDate
              amount            = max 0 $ min (demandAmount d) bal
              partialDemand     = Demand (demandPeriod d) (demandEnvelope d)
                                         (demandAmount d - amount)
              remainingBalance  = bal - amount
              remainingDemands  = if amount == (demandAmount d) then ds else partialDemand : ds

toSums :: [Demand] -> [Fill] -> [DemandSummary]
toSums [] _ = []
toSums (d:ds) f = newSummary : toSums ds f
        where newSummary        = DemandSummary d amount colour
              amount            = (sum . map fillAmount) matchingFills
              matchingFills     = filter (\x -> d == (fillDemand x)) f
              colour            | amount == 0                   = Red
                                | amount < (demandAmount d)     = Orange
                                | otherwise                     = Green

budget :: BudgetRequest -> String -> UTCTime -> Budget
budget (BudgetRequest i d b) uid begin = toBudget $ toFills (sort i) (sort d) b begin
        where toBudget (f, cb)  = Budget Nothing uid i d f (toSums (sort d) f) b cb
