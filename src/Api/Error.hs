{-# LANGUAGE OverloadedStrings #-}
module Api.Error where

import Data.ByteString.Lazy.Char8 (pack)
import Snap.Core

badRequest :: String -> Snap ()
badRequest message = errorResponse 400 message

notFound :: String -> Snap ()
notFound message = errorResponse 404 message

unAuthorized :: String -> Snap ()
unAuthorized message = errorResponse 401 message

errorResponse :: Int -> String -> Snap ()
errorResponse code message = do
    modifyResponse $ setResponseCode code
    writeLBS $ pack $ "{\"error\": \"" ++ message ++ "\"}"
