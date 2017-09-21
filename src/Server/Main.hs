{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.AddTransaction
import Api.RoomSummary
import Control.Monad.IO.Class (liftIO)
import Data.List (maximumBy, minimumBy)
import Db.Connection
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import View.Main
import Web.Scotty (get, html, scotty)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderTemplate =
  get "/" $
    html . renderHtml $ mainTemplate

main = do
  conn <- connection

  scotty 3000 $ do
    renderTemplate
    getRoomSummary conn
    addTransaction conn
