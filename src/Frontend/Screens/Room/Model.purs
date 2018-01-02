module Screens.Room.Model where

import Data.Maybe (Maybe)

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

