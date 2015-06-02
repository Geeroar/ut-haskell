{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Budget
import Test.HUnit

test1 = TestCase (assertEqual "boggie" 1 1)

tests = TestList [ TestLabel "dud test" test1 ]
