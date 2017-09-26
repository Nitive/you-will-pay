module UI.Counter where

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

counter :: forall m. H.Component HH.HTML Query Unit Message m
counter =
  H.component
    { initialState: const 1
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Increment)
        ]
        [ HH.text label ]
      where
        label = show state

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Increment next -> do
        state <- H.get
        let nextState = state + 1
        H.put nextState
        H.raise $ Incremented nextState
        pure next
