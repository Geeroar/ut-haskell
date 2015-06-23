{-# LANGUAGE OverloadedStrings #-}
module Api.Budgets where

import Api.Error
import Api.Util
import Budget
import Data.Bson
import Db.Repository
import Snap.Core


createBudget :: String -> Snap ()
createBudget uid = do
    modifyResponse responseIntercept
    body <- readRequestBody 5243000
    request <- return $ decodeBudgetRequest body
    case request of
        Just b  -> do
            b' <- return $ budget b uid
            docId <- insertBudget b'
            doBudgetResponse $ Just $ attachBudgetId (typed docId) b'
        Nothing -> badRequest "Foooook"

getBudgetById :: String -> Snap ()
getBudgetById _ = do
    modifyResponse responseIntercept
    i <- extractParam "id"
    b <- findBudgetById i
    case b of
        Just _  -> doBudgetResponse b
        Nothing -> notFound $ "No budget found with id " ++ i

getLatestBudget :: String -> Snap ()
getLatestBudget uid = do
    modifyResponse responseIntercept
    b <- findLatestBudget uid
    case b of
        Just _  -> doBudgetResponse b
        Nothing -> notFound $ "No latest budget found for " ++ uid
