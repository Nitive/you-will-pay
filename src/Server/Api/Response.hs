{-# LANGUAGE DeriveGeneric #-}

module Api.Response
( successResponse
, SuccessResponse(..)
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic(..))


data SuccessResponse a = SuccessResponse
  { status :: String
  , result :: a
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (SuccessResponse a)


successResponse :: ToJSON a => a -> SuccessResponse a
successResponse x = SuccessResponse { status = "ok", result = x }
