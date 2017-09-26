module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Screens.Room (room)

type AppEffects = HalogenEffects
  ( ajax :: AJAX
  , console :: CONSOLE
  , now :: NOW
  )

main :: Eff AppEffects Unit
main = runHalogenAff do
  body <- awaitBody
  runUI room unit body
