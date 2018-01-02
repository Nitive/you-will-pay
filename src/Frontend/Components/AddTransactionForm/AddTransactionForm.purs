module Components.AddTransactionForm
  ( module Components.AddTransactionForm
  , module Components.AddTransactionForm.Model
  ) where

import Components.AddTransactionForm.Model

import Api.AddTransaction (AddTransactionRequest(AddTransactionRequest), AddTransactionResponse(..), addTransaction)
import Components.AddTransactionForm.Template (addTransactionFormTemplate)
import Control.Monad.Eff.Now (now)
import DOM.Event.Event (preventDefault)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (floor, fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (type (~>), Unit, Void, bind, const, discard, id, pure, show, ($), (<$>), (<*>))
import Screens.Room.Model (User)
import Types (ComponentEffects)
import Utils.LocalStorage (LocalStorageContent(..), getLocalStorageContent, updateLocalStorageContent)
import Utils.Parsers (parseNumber)


updatePayUserIdInLocalStorage :: Maybe Int -> LocalStorageContent -> LocalStorageContent
updatePayUserIdInLocalStorage payUserId (LocalStorageContent content) = LocalStorageContent content { userId = payUserId }

stateToRequest :: DateTime -> State -> Maybe AddTransactionRequest
stateToRequest created state = createRequest <$> price <*> payUserId
    where
      createRequest price' userId =
        AddTransactionRequest
          { created: either id id $ formatDateTime "YYYY-MM-DD hh:mm:ss+03" created
          , userId
          , price: price'
          , description: state.description
          , roomId: 1
          }
      price = floor <$> parseNumber state.price
      payUserId = fromString state.payUserId

responseToReport :: AffjaxResponse AddTransactionResponse -> Maybe Report
responseToReport { status: StatusCode 200, response: (AddTransactionResponse result) } = Just result
responseToReport _ = Nothing

addTransactionForm :: forall eff. Int -> Array User -> H.Component HH.HTML Query Unit Void (ComponentEffects eff)
addTransactionForm payUserId users =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just $ H.action InitializeForm
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState =
      { status: Loaded
      , report: Nothing
      , price: ""
      , description: ""
      , payUserId: show payUserId
      }

    render = addTransactionFormTemplate users

    eval :: Query ~> H.ComponentDSL State Query Void (ComponentEffects eff)
    eval (SubmitForm event next) = do
      H.liftEff $ preventDefault event

      created <- H.liftEff $ toDateTime <$> now
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

    eval (SetPrice price next) = do
      H.modify (_ { price = price })
      pure next

    eval (SetDescription description next) = do
      H.modify (_ { description = description })
      pure next

    eval (SetPayUserId userId next) = do
      H.modify (_ { payUserId = userId })
      H.liftEff $ updateLocalStorageContent (updatePayUserIdInLocalStorage $ fromString userId)
      pure next

    eval (InitializeForm next) = do
      (LocalStorageContent content) <- H.liftEff getLocalStorageContent
      H.modify (\st -> st { payUserId = fromMaybe st.payUserId (show <$> content.userId) })
      pure next
