module Screens.Room where

import Prelude

import Api.AddTransaction (AddTransactionRequest(..), AddTransactionResponse(..), SuccessResponse(..), addTransaction)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, now)
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Data.DateTime.Instant (toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
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

room :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX, dom :: DOM, console :: CONSOLE, now :: NOW | eff))
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

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX, dom :: DOM, console :: CONSOLE, now :: NOW | eff))
    eval = case _ of
      SubmitForm event next -> do
        created <- liftEff $ toDateTime <$> now
        liftEff $ preventDefault event

        H.modify (_ { loading = true })
        let request = AddTransactionRequest {
              created: either id id $ formatDateTime "YYYY-MM-DD hh:mm:ss+03" created
            , userId: 1
            , price: 123
            , description: "some words"
            , roomId: 1
            }

        res <- H.liftAff $ addTransaction request
        let (SuccessResponse response) = (res.response :: SuccessResponse AddTransactionResponse)
        let (AddTransactionResponse result) = response.result
        let summary = { transactionId: result.transactionId } :: Summary
        H.modify (_ { loading = false, summary = Just summary })
        pure next

