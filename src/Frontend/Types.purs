module Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Halogen.Aff (HalogenEffects)
import Network.HTTP.Affjax (AJAX)

type AppEffects eff = HalogenEffects
  ( ajax :: AJAX
  , console :: CONSOLE
  , now :: NOW
  | eff
  )

type ComponentEffects eff = Aff (AppEffects eff)
