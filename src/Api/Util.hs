{-# LANGUAGE OverloadedStrings #-}
module Api.Util where

import Api.Error
import Data.Aeson
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as Lz
import Data.Time.Clock
import Database.MongoDB
import Snap.Core
import Types

attachBudgetId :: ObjectId -> Budget -> Budget
attachBudgetId bid (Budget _ uid i d f ob cb) = Budget (Just bid) uid i d f ob cb

decodeBudgetRequest :: Lz.ByteString -> Maybe BudgetRequest
decodeBudgetRequest = decode

doBudgetResponse :: Maybe Budget -> Snap ()
doBudgetResponse b = case b of
    Just b' -> writeLBS $ encode b'
    Nothing -> badRequest "You can fuck off"

extractAuthorization :: Snap (Maybe String)
extractAuthorization = do
    req <- getRequest
    a <- return $ getHeader "Authorization" req
    case a of
        Just a'  -> return $ Just $ unpack a'
        Nothing -> return Nothing

extractParam :: ByteString -> Snap String
extractParam p = do
    i <- getParam p
    case i of
        Just i' -> return $ unpack i'
        Nothing -> do
            badRequest "Invalid parameter"
            return ""

responseIntercept :: Response -> Response
responseIntercept = setContentType "application/json"
