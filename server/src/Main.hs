{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.AddTransaction (addTransaction)
import Api.RoomSummary (getRoomSummary)
import Db.Connection (createConnection, DbConnection)
import System.Environment (getEnv)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import View.Main (mainTemplate)
import Web.Scotty (get, html, scotty, ScottyM)

renderTemplate :: String -> ScottyM ()
renderTemplate assetsPath =
  get "/" $
    html . renderHtml $ mainTemplate assetsPath

apiHandlers :: DbConnection -> ScottyM ()
apiHandlers db = do
  getRoomSummary db
  addTransaction db

main :: IO ()
main = do
  assetsPath <- getEnv "ASSETS_PATH"
  conn <- createConnection

  scotty 3000 $ do
    renderTemplate assetsPath
    apiHandlers conn
