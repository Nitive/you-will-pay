{-# LANGUAGE OverloadedStrings #-}

module View.Main
( mainTemplate
) where

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

manifests assetsPath = do
  link ! rel "apple-touch-icon" ! sizes "180x180" ! href' "apple-touch-icon.png"
  link ! rel "icon" ! type_ "image/png" ! sizes "32x32" ! href' "favicon-32x32.png"
  link ! rel "icon" ! type_ "image/png" ! sizes "16x16" ! href' "favicon-16x16.png"
  link ! rel "manifest" ! href' "manifest.json"
  link ! rel "mask-icon" ! href' "safari-pinned-tab.svg" ! customAttribute "color" themeColor
  link ! rel "shortcut icon" ! href' "favicon.ico"
  meta ! name "apple-mobile-web-app-capable" ! content "yes"
  meta ! name "apple-mobile-web-app-status-bar-style" ! content "black"
  meta ! name "msapplication-config" ! content "browserconfig.xml"
  meta ! name "theme-color" ! content themeColor

  where
    themeColor = "#373737"
    href' = href . stringValue . (assetsPath ++)

mainTemplate :: String -> Html
mainTemplate assetsPath =
  docTypeHtml $
    html ! lang "en" $ do
      H.head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        meta ! httpEquiv "X-UA-Compatible" ! content "ie=edge"
        script ! src "//cdn.cian.site/frontend/fonts/l/fonts.min.js" ! H.dataAttribute "fonts-public-path" "//cdn.cian.site/frontend/fonts/" $ ""
        manifests assetsPath
        H.title "You Will Pay"
      body $
        script ! src jsEntry $ ""
  where
    jsEntry = stringValue $ assetsPath ++ "app.js"
