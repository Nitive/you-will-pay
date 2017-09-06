{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty as S
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

template = do
  docTypeHtml $ do
    H.html ! lang "en" $ do
      H.head $ do
        H.meta ! charset "utf-8"
        H.meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        H.meta ! httpEquiv "X-UA-Compatible" ! content "ie=edge"
        H.title "App"
      H.body $ do
        H.div "Loading..."

main = scotty 3000 $ do
  get "/" $ do
    S.html . renderHtml $ template