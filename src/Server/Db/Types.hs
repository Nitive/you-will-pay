{-# LANGUAGE DeriveGeneric #-}

module Db.Types
( RoomSummaryReport(..)
, User(..)
, Transaction(..)
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime(..))
import Database.PostgreSQL.Simple
import GHC.Generics (Generic(..))


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
  { created :: UTCTime
  , userId :: Int
  , price :: Int
  , description :: String
  , roomId :: Int
  } deriving (Generic, Show)

instance FromRow Transaction
instance ToRow Transaction

instance FromJSON Transaction
instance ToJSON Transaction


data RoomSummaryReport = RoomSummaryReport
  { payUserName :: String
  , payDiff :: Int
  , users :: [User]
  , history :: [Transaction]
  } deriving (Generic, Show)

instance FromJSON RoomSummaryReport
instance ToJSON RoomSummaryReport
