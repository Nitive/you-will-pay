{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.AddTransaction
import Api.RoomSummary
import Control.Monad.IO.Class (liftIO)
import Data.List (maximumBy, minimumBy)
import Db.Connection
import System.Environment (getEnv)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import View.Main
import Web.Scotty (get, html, scotty)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderTemplate assetsPath =
  get "/" $
    html . renderHtml $ mainTemplate assetsPath

main = do
  conn <- connection
  assetsPath <- getEnv "ASSETS_PATH"

  scotty 3000 $ do
    renderTemplate assetsPath
    getRoomSummary conn
    addTransaction conn
