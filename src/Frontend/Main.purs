module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Screens.Room (room)

main :: Eff (HalogenEffects (ajax :: AJAX)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI room unit body
