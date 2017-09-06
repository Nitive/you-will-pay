{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

template = do
  H.docTypeHtml $ do
    H.html ! A.lang "en" $ do
      H.head $ do
        H.meta ! A.charset "utf-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "ie=edge"
        H.title "App"
      H.body $ do
        H.div "Loading..."

main = S.scotty 3000 $ do
  S.get "/" $ do
    S.html . renderHtml $ template
