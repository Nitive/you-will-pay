{-# LANGUAGE DeriveGeneric #-}

module Server.Api.Api
( SummaryReport(..)
, User(..)
, Transaction(..)
) where

import Data.Aeson
import GHC.Generics

data User = User
  { name :: String
  , color :: String
  , id :: Int
  } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

data Transaction = Transaction
  { time :: String
  , userId :: Int
  , price :: Int
  , description :: String
  } deriving (Generic, Show)

instance FromJSON Transaction
instance ToJSON Transaction

data SummaryReport = SummaryReport
  { payUserName :: String
  , payDiff :: Int
  , users :: [User]
  , history :: [Transaction]
  } deriving (Generic, Show)

instance FromJSON SummaryReport
instance ToJSON SummaryReport
