module Screens.Room where

import Api.GetSummary (GetSummaryResponse(..), getSummary)
import Components.AddTransactionForm as ATF
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($), (<$>), (<*>), (==))
import Screens.Room.Model (Query(GetSummary), State, Status(..), Summary)
import Screens.Room.Template (roomTemplate)
import Types (ComponentEffects)

responseToSummary :: AffjaxResponse GetSummaryResponse -> Maybe Summary
responseToSummary { status: StatusCode 200, response: (GetSummaryResponse result) } = summary
  where
    payUser = find (\x -> x.id == result.payUserId) result.users

    parseTransaction tr = parse <$> user
      where
        user = find (\x -> x.id == tr.userId) result.users
        parse u =
          { user: u
          , price: tr.price
          , description: tr.description
          }

    transactions = sequence $ parseTransaction <$> result.history

    parseSummary user history =
      { payUser: user
      , payDiff: result.payDiff
      , users: result.users
      , history: history
      }
    summary = parseSummary <$> payUser <*> transactions
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
    { status: Pending
    , summary: Nothing
    }

  -- render :: State -> H.ParentHTML Query ATF.Query Slot (ComponentEffects eff)
  render = roomTemplate

  eval :: Query ~> H.ParentDSL State Query ATF.Query _ Void (ComponentEffects eff)
  eval (GetSummary next) = do
    H.modify (_ { status = Pending })
    res <- H.liftAff $ getSummary 1
    H.modify (_ { status = Loaded, summary = responseToSummary res })
    pure next
