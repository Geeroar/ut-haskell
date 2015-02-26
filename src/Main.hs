{-# LANGUAGE OverloadedStrings #-}
module Main where

import Budget
import Control.Applicative
import Data.Aeson
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as Lz
import JSON
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("budget", writeBS "I said fuck off")
          , ("budget/json", budgetHandler)
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

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
doResponse r = do
    case r of
        Just r  -> writeLBS $ encode $ budget r
        Nothing -> do
            modifyResponse badRequest
            writeLBS "{\"error\": \"You can fuck off\"}"

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
