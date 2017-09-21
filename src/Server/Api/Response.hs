{-# LANGUAGE DeriveGeneric #-}

module Api.Response
( successResponse
) where

import Data.Aeson
import GHC.Generics


data SuccessResponse a = SuccessResponse
  { status :: String
  , response :: a
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (SuccessResponse a)
instance ToJSON a => ToJSON (SuccessResponse a)


successResponse :: ToJSON a => a -> SuccessResponse a
successResponse x = SuccessResponse { status = "ok", response = x }
