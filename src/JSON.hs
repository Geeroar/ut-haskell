{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Budget
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text hiding (map)

instance FromJSON Date where
    parseJSON (Array d)  = Date <$>
                           (parseJSON $ Number 2015) <*>
                           (parseJSON $ Number 2) <*>
                           (parseJSON $ Number 15)
    parseJSON (String s) = Date <$>
                           (parseJSON $ Number $ y) <*>
                           (parseJSON $ Number $ m) <*>
                           (parseJSON $ Number $ d)
                where (y:m:d:_) = map (read . unpack) $ splitOn "-" s
    parseJSON _          = mzero

instance ToJSON Date where
    toJSON (Date y m d) = toJSON $ (show y) ++ "-" ++ (show m) ++ "-" ++ (show d)

instance FromJSON Period where
    parseJSON (Object p) = Period <$>
                           p .: "start" <*>
                           p .:? "end" .!= Date 0 0 0
    parseJSON _          = mzero

instance ToJSON Period where
    toJSON (Period s e) = object ["start" .= s, "end" .= e]

instance FromJSON Demand where
    parseJSON (Object d) = Demand <$>
                           d .: "period" <*>
                           d .: "envelope" <*>
                           d .: "amount"
    parseJSON _          = mzero

instance ToJSON Demand where
    toJSON (Demand p e a) = object ["period" .= p, "envelope" .= e, "amount" .= a]

instance FromJSON Fill where
    parseJSON (Object f) = Fill <$>
                           f .: "envelope" <*>
                           f .: "date" <*>
                           f .: "amount"
    parseJSON _          = mzero

instance ToJSON Fill where
    toJSON (Fill e d a) = object ["envelope" .= e, "date" .= d, "amount" .= a]

instance FromJSON Income where
    parseJSON (Object i) = Income <$>
                           i .: "date" <*>
                           i .: "amount"
    parseJSON _          = mzero

instance ToJSON Income where
    toJSON (Income d a) = object ["date" .= d, "amount" .= a]

instance FromJSON BudgetRequest where
    parseJSON (Object r) = BudgetRequest <$>
                           r .:? "income" .!= [] <*>
                           r .:? "demands" .!= [] <*>
                           r .:? "balance" .!= 0
    parseJSON _          = mzero

instance ToJSON BudgetRequest where
    toJSON (BudgetRequest i d b) = object ["income" .= i, "demands" .= d, "balance" .= b]

instance FromJSON BudgetResponse where
    parseJSON (Object r) = BudgetResponse <$>
                           r .: "fills" <*>
                           r .: "balance"
    parseJSON _          = mzero

instance ToJSON BudgetResponse where
    toJSON (BudgetResponse f b) = object ["fills" .= f, "balance" .= b]
