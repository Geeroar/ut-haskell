module Budget (budget) where

import Data.List (sort)
import Types

toFills :: [Income] -> [Demand] -> Int -> Date -> ([Fill], Int)
toFills [] _ 0 _ = ([], 0)
toFills [] [] bal _ = ([], bal)
toFills (i:is) [] bal curDate = toFills is [] (bal + incomeAmount i) curDate
toFills (i:is) dem 0 _ = toFills is laterDemands (incomeAmount i) (incomeDate i)
        where laterDemands = filter (\d -> (startDate $ demandPeriod d) > (incomeDate i)) dem
toFills inc (d:ds) bal curDate = (newFill : fst otherFills, snd otherFills)
        where newFill           = Fill (demandEnvelope d) curDate amount
              otherFills        = toFills inc remainingDemands remainingBalance curDate
              amount            = max 0 $ min (demandAmount d) bal
              partialDemand     = Demand (demandPeriod d) (demandEnvelope d)
                                         (demandAmount d - amount)
              remainingBalance  = bal - amount
              remainingDemands  = if amount == (demandAmount d) then ds else partialDemand : ds

budget :: BudgetRequest -> BudgetResponse
budget (BudgetRequest i d b) = toBudgetResponse $ toFills (sort i) (sort d) b startDate
        where toBudgetResponse (f, b)   = BudgetResponse f b
              startDate                 = Date 2015 1 1 -- TODO: Remove hardcoded data
