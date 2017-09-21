{-# LANGUAGE OverloadedStrings #-}

module View.Main
( mainTemplate
) where

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

mainTemplate :: Html
mainTemplate =
  docTypeHtml $
    html ! lang "en" $ do
      H.head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        meta ! httpEquiv "X-UA-Compatible" ! content "ie=edge"
        H.title "App"
      body $
        script ! src "http://localhost:1337/app.js" $ ""
