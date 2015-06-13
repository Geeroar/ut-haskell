{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Database where
import Database.MongoDB    (Action, Document, Document, Field, Value, Val, access,
                            close, connect, delete, exclude, find, count,
                            host, insertMany, master, project, rest,
                            select, sort, aggregate, (=:), (!?), typed, value,
                            ObjectId, Label, valueAt, modify, look)

import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Text (pack)
import Control.Monad.Trans (liftIO)

runMongo dbName functionToRun = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master (pack dbName) functionToRun
  close pipe
