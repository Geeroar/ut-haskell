{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Budget
import Control.Applicative
import Control.Monad
import Data.Aeson

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
