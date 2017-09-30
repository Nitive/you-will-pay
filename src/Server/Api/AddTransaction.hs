{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.AddTransaction
( addTransaction
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Db.Inserters
import Db.Types
import GHC.Generics (Generic(..))
import Web.Scotty (put, jsonData, json)

data AddTransactionResult = AddTransactionResult
  { transactionId :: Int } deriving (Generic, Show)

instance ToJSON AddTransactionResult

addTransaction conn =
  put "/api/add-transaction" $ do
    transaction <- jsonData
    tranId <- liftIO $ insertTransaction (transaction :: Transaction) conn
    json $ AddTransactionResult { transactionId = tranId }
