{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Db.Repository where

import Control.Monad.Trans (MonadIO)
import Database.MongoDB
import Db.Connect
import Db.Mapper
import Types

budgetCollection :: Collection
budgetCollection = "budget"

findLatestBudget :: MonadIO m => String -> m (Maybe Budget)
findLatestBudget uid = do
    doc <- runMongo $ findOne $ (select ["userId" =: uid] budgetCollection)
                                {sort = ["_id" =: sortDescending]}
    return $ maybeDocumentToBudget doc

findBudgetById :: MonadIO m => String -> m (Maybe Budget)
findBudgetById bid = do
    oid <- return $ stringToObjectId bid
    case oid of
        Just oid' -> do
            doc <- runMongo $ findOne $ select ["_id" =: oid'] budgetCollection
            return $ maybeDocumentToBudget doc
        Nothing   -> return $ Nothing

insertBudget :: MonadIO m => Budget -> m Value
insertBudget b = runMongo $ insert budgetCollection $ budgetToDocument b

sortDescending :: Int
sortDescending = -1
