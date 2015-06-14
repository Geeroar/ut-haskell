{-# LANGUAGE OverloadedStrings #-}
module BudgetService where

import           Budget
import           Data.Aeson
import qualified Data.ByteString.Lazy as Lz
import           Snap.Core
import           Types


responseIntercept :: Response -> Response
responseIntercept = setContentType "application/json"

badRequest :: Response -> Response
badRequest = setResponseCode 400

budgetHandler :: Snap ()
budgetHandler = do
    body <- readRequestBody 1000
    modifyResponse responseIntercept
    doResponse $ decodeBudgetRequest body

decodeBudgetRequest :: Lz.ByteString -> Maybe BudgetRequest
decodeBudgetRequest = decode

doResponse :: Maybe BudgetRequest -> Snap ()
doResponse resp = case resp of
        Just r  -> writeLBS $ encode $ budget r
        Nothing -> do
            modifyResponse badRequest
            writeLBS "{\"error\": \"You can fuck off\"}"
