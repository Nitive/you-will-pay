module Button where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Int

data Query a
  = Increment a

data Message = Incremented Int

myButton :: forall m. H.Component HH.HTML Query Unit Message m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 1

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = show state
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Increment)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Increment next -> do
      state <- H.get
      let nextState = state + 1
      H.put nextState
      H.raise $ Incremented nextState
      pure next
