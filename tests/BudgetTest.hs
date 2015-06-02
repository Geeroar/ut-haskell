module BudgetTest ( tests ) where

import Budget
import Distribution.TestSuite

budgetRequestWithIncome :: TestInstance
budgetRequestWithIncome = buildTest result "income should contribute to balance"
  where result
         | closingBalance response /= 50 = Fail "closing balance should be 50"
         | length fillList /= 1 = Fail "there should be one fill"
         | fillDate firstFill /= Date 2015 1 2 = Fail "the fill date should be the income date"
         | fillEnvelope firstFill /= "Gas" = Fail "the fill should be for Gas"
         | fillAmount firstFill /= 50 = Fail "the fill amount should be 50"
         | otherwise = Pass
        firstFill = head fillList
        fillList = fills response
        response = budget request
        request = BudgetRequest
                    { income = [
                        Income (Date 2015 1 2) 100
                    ]
                    , demands = [
                        Demand (Period (Date 2015 1 5) (Date 2015 1 5)) "Gas" 50
                    ]
                    , openingBalance = 0
                    }

budgetRequestOneDemandWithBalance :: TestInstance
budgetRequestOneDemandWithBalance = buildTest result "demand should pull from balance"
  where result
         | closingBalance response /= 100 = Fail "closing balance should be 100"
         | length fillList /= 1 = Fail "there should be one fill"
         | fillEnvelope firstFill /= "Gas" = Fail "the fill should be for Gas"
         | otherwise = Pass
        firstFill = head fillList
        fillList = fills response
        response = budget request
        request = BudgetRequest
                    { income = []
                    , demands = [
                        Demand (Period (Date 2015 1 1) (Date 2015 1 1)) "Gas" 100
                    ]
                    , openingBalance = 200
                    }

emptyBudgetRequest :: TestInstance
emptyBudgetRequest = buildTest result "empty budget request should give empty response"
  where result
         | closingBalance response /= 0 = Fail "closing balance should be 0"
         | length fillList /= 0 = Fail "fills should be empty"
         | otherwise = Pass
        fillList = fills response
        response = budget request
        request = BudgetRequest { income = [], demands = [], openingBalance = 0 }


buildTest :: Result -> String -> TestInstance
buildTest result label = TestInstance
  { run = return $ Finished result
  , name = label
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ buildTest result label
  }

tests :: IO [Test]
tests = return $ map Test
            [ emptyBudgetRequest
            , budgetRequestOneDemandWithBalance
            , budgetRequestWithIncome
            ]
