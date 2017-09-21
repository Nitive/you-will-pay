{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.AddTransaction
( addTransaction
) where

import Control.Monad.IO.Class (liftIO)

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics

import Db.Types
import Db.Inserters
import Api.Response
import Debug.Trace

data AddTransactionResult = AddTransactionResult
  { transactionId :: Int } deriving (Generic, Show)

instance ToJSON AddTransactionResult

addTransaction conn =
  put "/api/add-transaction/" $ do
    transaction <- jsonData
    let t = transaction :: Transaction
    i <- liftIO $ insertTransaction t conn

    json $ successResponse $ AddTransactionResult { transactionId = i }
