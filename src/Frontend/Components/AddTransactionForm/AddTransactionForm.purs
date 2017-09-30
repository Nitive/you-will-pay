module Components.AddTransactionForm where

import Api.AddTransaction (AddTransactionRequest(AddTransactionRequest), AddTransactionResponse(..), addTransaction)
import Control.Monad.Eff.Now (now)
import DOM.Event.Event (Event, preventDefault)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (type (~>), Unit, Void, bind, const, discard, id, pure, show, ($), (<$>), (<>))
import Types (ComponentEffects)


type Report =
  { transactionId :: Int
  }

data Status = Pending | Loaded

type State =
  { status :: Status
  , report :: Maybe Report
  , price :: String
  , description :: String
  }

data Query a
  = SubmitForm Event a
  | SetPrice String a
  | SetDescription String a

stateToRequest :: DateTime -> State -> Maybe AddTransactionRequest
stateToRequest created state = createRequest <$> price
    where
      createRequest price =
        AddTransactionRequest
          { created: either id id $ formatDateTime "YYYY-MM-DD hh:mm:ss+03" created
          , userId: 2
          , price
          , description: state.description
          , roomId: 1
          }
      price = fromString state.price

responseToReport :: AffjaxResponse AddTransactionResponse -> Maybe Report
responseToReport { status: StatusCode 200, response: (AddTransactionResponse result) } = Just result
responseToReport _ = Nothing

addTransactionForm :: forall eff. H.Component HH.HTML Query Unit Void (ComponentEffects eff)
addTransactionForm =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState =
      { status: Loaded
      , report: Nothing
      , price: ""
      , description: ""
      }

    render :: State -> H.ComponentHTML Query
    render state =
      case state.status of
        Loaded ->
          case state.report of
            Just report ->
              HH.text $ "transaction #" <> show report.transactionId <> " " <> "created"
            Nothing ->
              HH.form [ HE.onSubmit (HE.input SubmitForm) ]
                [ HH.label_
                  [ HH.text "Price: "
                  , HH.input
                    [ HP.value state.price
                    , HE.onValueInput (HE.input SetPrice)
                    ]
                  ]
                , HH.label_
                  [ HH.text "Description: "
                  , HH.input
                    [ HP.value state.description
                    , HE.onValueInput (HE.input SetDescription)
                    ]
                  ]
                , HH.button_ [ HH.text "Submit" ]
                ]
        Pending ->
          HH.text "Pending..."

    eval :: Query ~> H.ComponentDSL State Query Void (ComponentEffects eff)
    eval = case _ of
      SubmitForm event next -> do
        liftEff $ preventDefault event

        created <- liftEff $ toDateTime <$> now
        state <- H.get
        let request = stateToRequest created state
        case request of
          Just req -> do
            H.modify (_ { status = Pending })
            res <- H.liftAff $ addTransaction req
            H.modify (_ { status = Loaded, report = responseToReport res })
          Nothing -> do
            H.modify (_ { status = Loaded })
        pure next

      SetPrice price next -> do
        H.modify (_ { price = price })
        pure next

      SetDescription description next -> do
        H.modify (_ { description = description })
        pure next
