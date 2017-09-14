{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Aeson (Value (Null), (.=), object)
import Server.Api.Api (SummaryReport(..), User(..), Transaction(..))

template = do
  H.docTypeHtml $ do
    H.html ! A.lang "en" $ do
      H.head $ do
        H.meta ! A.charset "utf-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "ie=edge"
        H.title "App"
      H.body $ do
        H.script ! A.src "http://localhost:1337/app.js" $ ""

main = S.scotty 3000 $ do
  S.get "/" $ do
    S.html . renderHtml $ template

  S.get "/api/summary/" $ do
    S.json SummaryReport
      { payUserName = "Jon"
      , payDiff = 10
      , users = [User { name = "Jon", color = "#50e3c2" }]
      , history =
        [ Transaction
          { time = "Thu Sep 14 2017 10:19:46 GMT+0300 (MSK)"
          , userName = "Jon"
          , price = 100
          , description = "Дал Арье на карманные расходы"
          }
        ]
      }
