module MainTest (tests) where

import BudgetTest
import Distribution.TestSuite
import TypesTest

tests :: IO [Test]
tests = return $ map Test $
            budgetTestInstances ++
            typesTestInstances
