module TypesTest (typesTestInstances) where

import Distribution.TestSuite
import Types
import TestUtil
import Util

demandsSamePeriodAndEnvelopeEqual :: TestInstance
demandsSamePeriodAndEnvelopeEqual = buildTest result "two demands should be equal"
    where result
            | d1 /= d2 = Fail "demands should be equal"
            | otherwise = Pass
          d1 = Demand (Period (constructTime "2015" "01" "05")
                      (constructTime "2015" "01" "05")) "Gas" 200
          d2 = Demand (Period (constructTime "2015" "01" "05")
                      (constructTime "2015" "01" "05")) "Gas" 700

demandsSamePeriodDifferentEnvelopeNotEqual :: TestInstance
demandsSamePeriodDifferentEnvelopeNotEqual = buildTest result "two demands should be different"
    where result
            | d1 == d2 = Fail "demands should be different"
            | otherwise = Pass
          d1 = Demand (Period (constructTime "2015" "01" "05")
                      (constructTime "2015" "01" "05")) "Gas" 200
          d2 = Demand (Period (constructTime "2015" "01" "05")
                      (constructTime "2015" "01" "05")) "Electricity" 200

demandsDifferentPeriodSameEnvelopeNotEqual :: TestInstance
demandsDifferentPeriodSameEnvelopeNotEqual = buildTest result "two demands should be different"
    where result
            | d1 == d2 = Fail "demands should be different"
            | otherwise = Pass
          d1 = Demand (Period (constructTime "2015" "01" "05")
                      (constructTime "2015" "01" "05")) "Gas" 200
          d2 = Demand (Period (constructTime "2015" "01" "05")
                      (constructTime "2015" "01" "06")) "Gas" 200

typesTestInstances :: [TestInstance]
typesTestInstances =
        [ demandsSamePeriodAndEnvelopeEqual
        , demandsSamePeriodDifferentEnvelopeNotEqual
        , demandsDifferentPeriodSameEnvelopeNotEqual
        ]
