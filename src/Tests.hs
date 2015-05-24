{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Budget
import Test.HUnit


expectedBaseResponse = BudgetResponse [] 100
mockOpeningBalanceOnlyRequest = BudgetRequest [] [] 100

testBaseResponse = TestCase (assertEqual "" (toBudgetResponseBalance expectedBaseResponse) (toBudgetResponseBalance $ budget mockOpeningBalanceOnlyRequest))


--expected = BudgetResponse [Fill "" Date 1 1 1 400] 300 

--request = BudgetRequest [Income Date 1 1 1 1000] [Demand Period Date 1 1 1 Date 2 2 2 "" 3] 2000

--testSingleIncomeSingleDemand = TestCase (assertEqual "boggie" expected (budget request))

tests = TestList [TestLabel "Test valid request with single income and single demand" testBaseResponse]
	--TestLabel "Test valid request with single income and single demand" testSingleIncomeSingleDemand 
