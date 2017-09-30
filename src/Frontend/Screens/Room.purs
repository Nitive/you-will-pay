module Screens.Room where

import Prelude

import Api.GetSummary (GetSummaryResponse(..), getSummary)
import Components.AddTransactionForm as ATF
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Types (ComponentEffects)

data Query a
  = GetSummary a

type User =
  { name :: String
  , color :: String
  }

type Transaction =
  { user :: User
  , price :: Int
  }

type Summary =
  { payUser :: User
  , payDiff :: Int
  , users :: Array User
  , history :: Array Transaction
  }

type State =
  { loading :: Boolean
  , summary :: Maybe Summary
  }

data Slot = ATFSlot
derive instance eqATFSlot :: Eq Slot
derive instance ordATFSlot :: Ord Slot

responseToSummary :: AffjaxResponse GetSummaryResponse -> Maybe Summary
responseToSummary { status: StatusCode 200, response: (GetSummaryResponse result) } = Just
  { payUser
  , payDiff: result.payDiff
  , users: []
  , history: []
  }
  where
    payUser = { name: result.payUserName, color: "tomato" }
responseToSummary _ = Nothing

room :: forall eff. H.Component HH.HTML Query Unit Void (ComponentEffects eff)
room =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just $ H.action GetSummary
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { loading: true
    , summary: Nothing
    }

  render :: State -> H.ParentHTML Query ATF.Query Slot (ComponentEffects eff)
  render state =
    case state.summary of
      Just summary ->
        HH.div_
          [ HH.text $ summary.payUser.name <> " " <> (show summary.payDiff)
          , HH.h1_ [ HH.text "room" ]
          , HH.slot ATFSlot ATF.addTransactionForm unit (const Nothing)
          ]
      Nothing ->
        HH.text "Loading..."

  eval :: Query ~> H.ParentDSL State Query ATF.Query Slot Void (ComponentEffects eff)
  eval = case _ of
    GetSummary next -> do
      H.modify (_ { loading = true })
      res <- H.liftAff $ getSummary 1
      H.modify (_ { loading = false, summary = responseToSummary res })
      pure next
