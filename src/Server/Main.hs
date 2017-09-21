{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (maximumBy, minimumBy)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as S

import Api.RoomSummary
import Api.AddTransaction
import Db.Connection
import View.Main

renderTemplate =
  S.get "/" $
    S.html . renderHtml $ mainTemplate

main = do
  conn <- connection

  S.scotty 3000 $ do
    renderTemplate
    getRoomSummary conn
    addTransaction conn
