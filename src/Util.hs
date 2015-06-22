module Util where

import Data.Time.Clock
import Data.Time.Format

constructTime :: String -> String -> String -> UTCTime
constructTime y m d = buildTime defaultTimeLocale [('Y', y), ('m', m), ('d', d)]

parseBudgetTime :: String -> UTCTime
parseBudgetTime s = parseTimeOrError True defaultTimeLocale "%F" s
