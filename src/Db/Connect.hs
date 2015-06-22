{-# LANGUAGE OverloadedStrings #-}
module Db.Connect where

import Control.Monad.Trans (MonadIO, liftIO)
import Database.MongoDB
import Data.Text (Text, pack)

dbName :: String
dbName = "unquantifiable-treasures-database"
mongoHost :: Host
--mongoHost = host "127.0.0.1"
mongoHost = Host "dogen.mongohq.com" $ PortNumber 10079
mongoPass :: Text
mongoPass = "youfoolsturnitoff"
mongoUser :: Text
mongoUser = "higgs"

runMongo :: MonadIO m => Action m a -> m a
runMongo action = do
    pipe <- liftIO $ connect mongoHost
    e <- access pipe master (pack dbName) (authoriseAction action)
    liftIO $ close pipe
    return e

authoriseAction :: MonadIO m => Action m a -> Action m a
authoriseAction action = do
    auth mongoUser mongoPass
    action
