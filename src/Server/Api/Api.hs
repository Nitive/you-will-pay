{-# LANGUAGE DeriveGeneric #-}

module Api.Api
( SummaryReport(..)
, User(..)
, Transaction(..)
) where

import Data.Aeson
import GHC.Generics
import Database.PostgreSQL.Simple


data User = User
  { color :: String
  , id :: Int
  , name :: String
  } deriving (Generic, Show)

instance FromRow User
instance ToRow User

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
