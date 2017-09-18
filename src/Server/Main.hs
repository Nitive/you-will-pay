{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad.IO.Class
import Data.Aeson (Value (Null), (.=), object)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as S

import Api.Api as Api
import Db.Connection
import Db.Selectors

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

main = do
  conn <- connection

  S.scotty 3000 $ do
    S.get "/" $ do
      S.html . renderHtml $ template

    S.get "/api/summary/" $ do
      users <- liftIO $ selectUsers conn

      S.json SummaryReport
        { payUserName = "Jon"
        , payDiff = 10
        , users = users
        , history =
          [ Transaction
            { time = "Thu Sep 14 2017 10:19:46 GMT+0300 (MSK)"
            , userId = 1
            , price = 100
            , description = "Дал Арье на карманные расходы"
            }
          ]
        }
