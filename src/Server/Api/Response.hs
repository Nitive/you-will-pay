{-# LANGUAGE DeriveGeneric #-}

module Api.Response
( successResponse
, SuccessResponse(..)
) where

import Data.Aeson
import GHC.Generics


data SuccessResponse a = SuccessResponse
  { status :: String
  , result :: a
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (SuccessResponse a)
instance ToJSON a => ToJSON (SuccessResponse a)


successResponse :: ToJSON a => a -> SuccessResponse a
successResponse x = SuccessResponse { status = "ok", result = x }
