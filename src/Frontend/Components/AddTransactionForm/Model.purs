module Components.AddTransactionForm.Model where

import DOM.Event.Types (Event)
import Data.Maybe (Maybe)

type Report =
  { transactionId :: Int
  }

data Status = Pending | Loaded

type State =
  { status :: Status
  , report :: Maybe Report
  , price :: String
  , description :: String
  , payUserId :: String
  }

data Query a
  = SubmitForm Event a
  | SetPrice String a
  | SetDescription String a
  | SetPayUserId String a
