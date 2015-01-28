{-# LANGUAGE OverloadedStrings #-}
module Main where

import Budget
import Control.Applicative
import Data.ByteString.Char8
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("budget", writeBS "You can fuck off")
          , ("budget/:balance", budgetHandler)
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

budgetHandler :: Snap ()
budgetHandler = do
    param <- getParam "balance"
    case param of
        Just param -> writeBS $ pack $ show $ runBudget $ readIntParam param
        Nothing -> redirect "/budget"

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

readIntParam :: ByteString -> Int
readIntParam x = read $ unpack x
