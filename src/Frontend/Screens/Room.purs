module Screens.Room where

import Api.GetSummary (getSummary, GetSummaryResponse(..))
import Api.Response (SuccessResponse(..))
import Components.AddTransactionForm as ATF
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Prelude
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
      req <- H.liftAff $ getSummary 1
      let (SuccessResponse res) = req.response
      let (GetSummaryResponse result) = res.result

      let summary = { payUser: { name: result.payUserName, color: "tomato" }
                    , payDiff: result.payDiff
                    , users: []
                    , history: []
                    } :: Summary
      H.modify (_ { loading = false, summary = Just summary })
      pure next
