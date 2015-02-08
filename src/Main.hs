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
          , ("budget", writeBS "You can fuck off")
          , ("budget/json", writeBS "{\"error\": \"You can fuck off\"}")
          , ("budget/:balance", budgetHandler)
          , ("budget/json/:balance", jsonHandler)
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

jsonHandler :: Snap ()
jsonHandler = do
    param <- getParam "balance"
    generalHandler param (Lz.toStrict . encode)

budgetHandler :: Snap ()
budgetHandler = do
    param <- getParam "balance"
    generalHandler param (pack . show)

generalHandler :: Maybe ByteString -> ([Fill] -> ByteString) -> Snap ()
generalHandler param textTransform = do
    case param of
        Just param -> writeBS $ textTransform $ runBudget $ readIntParam param
        Nothing -> redirect "/budget"

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

readIntParam :: ByteString -> Int
readIntParam x = read $ unpack x
