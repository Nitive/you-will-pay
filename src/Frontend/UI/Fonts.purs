module UI.Fonts where

import CSS (CSS, StyleM, fontFamily, fromString, key, sansSerif)
import Data.NonEmpty (singleton)
import Prelude (Unit, ($))

fontFamilyLato :: StyleM Unit
fontFamilyLato = fontFamily ["Lato", "Helvetica"] $ singleton sansSerif

webkitFontSmoothing :: String -> CSS
webkitFontSmoothing = key $ fromString "-webkit-font-smoothing"
