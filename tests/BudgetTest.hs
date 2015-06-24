module BudgetTest (budgetTestInstances) where

import Budget (budget)
import Data.Time.Clock
import Distribution.TestSuite
import TestUtil
import Types
import Util

twoPartialFills :: TestInstance
twoPartialFills = buildTest result "demand should be filled twice"
  where result
         | budgetClosingBalance response /= 150 = Fail "closing balance should be 150"
         | length fillList /= 2 = Fail "there should be two fills"
         | fillEnvelope firstFill /= "Gas" = Fail "the first fill should be for Gas"
         | fillAmount firstFill /= 100 = Fail "the first fill amount should be 100"
         | fillEnvelope secondFill /= "Gas" = Fail "the second fill should be for Gas"
         | fillAmount secondFill /= 100 = Fail "the second fill amount should be 100"
         | otherwise = Pass
        firstFill = head fillList
        secondFill = head $ tail fillList
        fillEnvelope = (demandEnvelope . fillDemand)
        fillList = budgetFills response
        response = budget request "id" beginDate
        request = BudgetRequest
                    { income = [
                        Income (constructTime "2015" "01" "02") 100,
                        Income (constructTime "2015" "01" "04") 250
                    ]
                    , demands = [
                        Demand (Period (constructTime "2015" "01" "05")
                                       (constructTime "2015" "01" "05")) "Gas" 200
                    ]
                    , openingBalance = 0
                    }

multipleDemands :: TestInstance
multipleDemands = buildTest result "balance should reduce properly"
  where result
         | budgetClosingBalance response /= 50 = Fail "closing balance should be 50"
         | length fillList /= 2 = Fail "there should be two fills"
         | fillEnvelope firstFill /= "Gas" = Fail "the first fill should be for Gas"
         | fillAmount firstFill /= 50 = Fail "the first fill amount should be 50"
         | fillEnvelope secondFill /= "Phone" = Fail "the second fill should be for Phone"
         | fillAmount secondFill /= 100 = Fail "the second fill amount should be 100"
         | otherwise = Pass
        firstFill = head fillList
        secondFill = head $ tail fillList
        fillEnvelope = (demandEnvelope . fillDemand)
        fillList = budgetFills response
        response = budget request "id" beginDate
        request = BudgetRequest
                    { income = []
                    , demands = [
                        Demand (Period (constructTime "2015" "01" "05")
                                       (constructTime "2015" "01" "05")) "Gas" 50,
                        Demand (Period (constructTime "2015" "01" "07")
                                       (constructTime "2015" "01" "07")) "Phone" 100
                    ]
                    , openingBalance = 200
                    }

singleIncome :: TestInstance
singleIncome = buildTest result "income should contribute to balance"
  where result
         | budgetClosingBalance response /= 50 = Fail "closing balance should be 50"
         | length fillList /= 1 = Fail "there should be one fill"
         | fillDate firstFill /= (constructTime "2015" "01" "2") =
                                        Fail "the fill date should be the income date"
         | fillEnvelope firstFill /= "Gas" = Fail "the fill should be for Gas"
         | fillAmount firstFill /= 50 = Fail "the fill amount should be 50"
         | otherwise = Pass
        firstFill = head fillList
        fillEnvelope = (demandEnvelope . fillDemand)
        fillList = budgetFills response
        response = budget request "id" beginDate
        request = BudgetRequest
                    { income = [
                        Income (constructTime "2015" "01" "02") 100
                    ]
                    , demands = [
                        Demand (Period (constructTime "2015" "01" "05")
                                       (constructTime "2015" "01" "05")) "Gas" 50
                    ]
                    , openingBalance = 0
                    }

oneDemandWithBalance :: TestInstance
oneDemandWithBalance = buildTest result "demand should pull from balance"
  where result
         | budgetClosingBalance response /= 100 = Fail "closing balance should be 100"
         | length fillList /= 1 = Fail "there should be one fill"
         | fillEnvelope firstFill /= "Gas" = Fail "the fill should be for Gas"
         | otherwise = Pass
        firstFill = head fillList
        fillEnvelope = (demandEnvelope . fillDemand)
        fillList = budgetFills response
        response = budget request "id" beginDate
        request = BudgetRequest
                    { income = []
                    , demands = [
                        Demand (Period (constructTime "2015" "01" "01")
                                       (constructTime "2015" "01" "01")) "Gas" 100
                    ]
                    , openingBalance = 200
                    }

emptyBudgetRequest :: TestInstance
emptyBudgetRequest = buildTest result "empty budget request should give empty response"
  where result
         | budgetClosingBalance response /= 0 = Fail "closing balance should be 0"
         | length fillList /= 0 = Fail "fills should be empty"
         | otherwise = Pass
        fillList = budgetFills response
        response = budget request "id" beginDate
        request = BudgetRequest { income = [], demands = [], openingBalance = 0 }

beginDate :: UTCTime
beginDate = constructTime "2015" "01" "01"

budgetTestInstances :: [TestInstance]
budgetTestInstances =
        [ emptyBudgetRequest
        , oneDemandWithBalance
        , singleIncome
        , multipleDemands
        , twoPartialFills
        ]
