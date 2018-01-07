{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.AddTransaction (addTransaction)
import Api.RoomSummary (getRoomSummary)
import Db.Connection
import System.Environment (getEnv)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import View.Main (mainTemplate)
import Web.Scotty (get, html, scotty, ScottyM)

renderTemplate :: String -> ScottyM ()
renderTemplate assetsPath =
  get "/" $
    html . renderHtml $ mainTemplate assetsPath

main :: IO ()
main = do
  conn <- connection
  assetsPath <- getEnv "ASSETS_PATH"

  scotty 3000 $ do
    renderTemplate assetsPath
    getRoomSummary conn
    addTransaction conn
