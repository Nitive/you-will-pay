module UI.Fonts where

import CSS (StyleM, fontFamily, sansSerif)
import Data.NonEmpty (singleton)
import Prelude (Unit, ($))

fontFamilyLato :: StyleM Unit
fontFamilyLato = fontFamily ["Lato", "Helvetica"] $ singleton sansSerif
