module Screens.Room where

import Api.GetSummary (GetSummaryResponse(..), getSummary)
import CSS (StyleM, color)
import Color.Scheme.HTML (red)
import Components.AddTransactionForm as ATF
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (class Eq, class Ord, type (~>), Unit, Void, bind, const, discard, pure, show, unit, ($), (<$>), (<*>), (<>), (==))
import Types (ComponentEffects)

data Query a
  = GetSummary a

type User =
  { name :: String
  , color :: String
  , id :: Int
  }

type Transaction =
  { user :: User
  , price :: Int
  , description :: String
  }

type Summary =
  { payUser :: User
  , payDiff :: Int
  , users :: Array User
  , history :: Array Transaction
  }

data Status = Pending | Loaded

type State =
  { status :: Status
  , summary :: Maybe Summary
  }

data Slot = ATFSlot
derive instance eqATFSlot :: Eq Slot
derive instance ordATFSlot :: Ord Slot

errorStyle :: StyleM Unit
errorStyle = do
  color red

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

  renderTransaction tr = HH.li_
    [ HH.text $ tr.user.name <> ": " <> (show tr.price) <> " (" <> tr.description <> ")"
    ]

  render :: State -> H.ParentHTML Query ATF.Query Slot (ComponentEffects eff)
  render state =
    case state.status of
      Loaded ->
        case state.summary of
          Just summary ->
            HH.div_
              [ HH.text $ summary.payUser.name <> " " <> (show summary.payDiff)
              , HH.h1_ [ HH.text "room #1" ]
              , HH.slot ATFSlot ATF.addTransactionForm unit (const Nothing)
              , HH.ul_ $ renderTransaction <$> summary.history
              ]
          Nothing ->
            HH.div [ style errorStyle ] [ HH.text "Error..." ]
      Pending ->
        HH.text "status..."

  eval :: Query ~> H.ParentDSL State Query ATF.Query Slot Void (ComponentEffects eff)
  eval (GetSummary next) = do
    H.modify (_ { status = Pending })
    res <- H.liftAff $ getSummary 1
    H.modify (_ { status = Loaded, summary = responseToSummary res })
    pure next
