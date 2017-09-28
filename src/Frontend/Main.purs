module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Screens.Room (room)
import Types (AppEffects)

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI room unit body
