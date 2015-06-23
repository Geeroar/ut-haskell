{-# LANGUAGE OverloadedStrings #-}
module Api.Server where

import Api.Budgets
import Api.Error
import Api.Util
import Snap.Core
import Snap.Http.Server

main :: IO ()
main = serve =<< commandLineConfig emptyConfig

serve :: Config Snap a -> IO ()
serve config = httpServe config $ route
        [ ("budgets",           method POST $ ensureAuthorised createBudget)
        , ("budgets/:id",       method GET $ ensureAuthorised getBudgetById)
        , ("budgets/latest",    method GET $ ensureAuthorised getLatestBudget)
        ]

ensureAuthorised :: (String -> Snap ()) -> Snap ()
ensureAuthorised f = do
    auth <- extractAuthorization
    case auth of
        Just a  -> f a
        Nothing -> do
            modifyResponse responseIntercept
            unAuthorized "No auth credentials"
