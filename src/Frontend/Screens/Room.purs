module Screens.Room where

import Prelude

import Api.AddTransaction (AddTransactionRequest(..), AddTransactionResponse(..), SuccessResponse(..), addTransaction)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Data.Maybe (Maybe(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax (AJAX)


type Summary =
  { transactionId :: Int
  }

type State =
  { loading :: Boolean
  , summary :: Maybe Summary
  }

data Query a
  = SubmitForm Event a

room :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX, dom :: DOM | eff))
room =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { loading: false, summary: Nothing }

    render :: State -> H.ComponentHTML Query
    render state =
      case state.summary of
        Just summary ->
          HH.text $ "yeah: " <> show summary.transactionId
        Nothing ->
          HH.form [ HE.onSubmit (HE.input SubmitForm) ]
            [ HH.text "form"
            , HH.input []
            , HH.button [] [ HH.text "test" ]
            ]

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX, dom :: DOM | eff))
    eval = case _ of
      SubmitForm event next -> do
        liftEff $ preventDefault event

        H.modify (_ { loading = true })
        let request = AddTransactionRequest {
              created: "2017-09-25 10:00:00+03"
            , userId: 1
            , price: 123
            , description: "some words"
            , roomId: 1
            }

        req <- H.liftAff $ addTransaction request
        let (SuccessResponse response) = (req.response :: SuccessResponse AddTransactionResponse)
        let (AddTransactionResponse result) = response.result
        let summary = { transactionId: result.transactionId } :: Summary
        H.modify (_ { loading = false, summary = Just summary })
        pure next

