{-# LANGUAGE OverloadedStrings #-}

module View.Main
( mainTemplate
) where

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

mainTemplate :: String -> Html
mainTemplate assetsPath =
  docTypeHtml $
    html ! lang "en" $ do
      H.head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        meta ! httpEquiv "X-UA-Compatible" ! content "ie=edge"
        link ! href "https://fonts.googleapis.com/css?family=Lato:400,700&amp;subset=latin-ext" ! rel "stylesheet"
        H.title "App"
      body $
        script ! src jsEntry $ ""
  where
    jsEntry = stringValue $ assetsPath ++ "app.js"
