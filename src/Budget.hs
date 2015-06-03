module Budget (budget) where

import Data.List (sort)
import Types

toFills :: [Income] -> [Demand] -> Int -> Date -> ([Fill], Int)
toFills inc dem bal curDate = do
    case nextItem inc dem of
        Nothing -> ([], bal)
        Just item -> do
            case item of
                Left i -> toFills (tail inc) dem newBalance newDate
                    where newBalance        = bal + incomeAmount i
                          newDate           = incomeDate i
                Right d -> (newFill : fst otherFills, remainingBalance)
                    where amount            = max 0 $ min (demandAmount d) bal
                          remainingBalance  = bal - amount
                          newFill           = Fill (demandEnvelope d) curDate amount
                          otherFills        = toFills inc (tail dem)
                                                    remainingBalance curDate

nextItem :: [Income] -> [Demand] -> Maybe (Either Income Demand)
nextItem [] [] = Nothing
nextItem [] (d:_) = Just $ Right d
nextItem (i:_) [] = Just $ Left i
nextItem (i:_) (d:_)
            | (incomeDate i) < (startDate $ demandPeriod d) = Just $ Left i
            | otherwise = Just $ Right d

toBudgetResponse :: ([Fill], Int) -> BudgetResponse
toBudgetResponse (f, b) = BudgetResponse f b

budget :: BudgetRequest -> BudgetResponse
budget (BudgetRequest i d b) = toBudgetResponse $ toFills (sort i) (sort d) b (Date 2015 1 1) -- TODO: Remove hardcoded date
