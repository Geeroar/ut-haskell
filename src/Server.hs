{-# LANGUAGE OverloadedStrings #-}

module Server where

import           BudgetService
import           Snap.Core
import           Snap.Http.Server

main :: IO ()
main = serve defaultConfig

serve :: Config Snap a -> IO()
serve config = httpServe config $ route [
            ("budgets", budgetHandler)
          , ("echo/:echoparam", echoHandler)
          ]

echoHandler :: Snap ()
echoHandler = do
  param <- getParam "echoparam"
  maybe (writeBS "must specify echo/param in URL")
    writeBS param
