module Main where

import Control.Monad.Eff (Eff)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, unit)
import Screens.Room (room)
import Types (AppEffects)

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI room unit body
