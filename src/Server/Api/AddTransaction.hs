{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.AddTransaction
( addTransaction
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Db.Inserters
import GHC.Generics (Generic(..))
import Web.Scotty (put, jsonData, json, ScottyM)
import Database.PostgreSQL.Simple (Connection)

newtype AddTransactionResult = AddTransactionResult
  { transactionId :: Int } deriving (Generic, Show)

instance ToJSON AddTransactionResult

addTransaction :: Connection -> ScottyM ()
addTransaction conn =
  put "/api/add-transaction" $ do
    transaction <- jsonData
    tranId <- liftIO $ insertTransaction transaction conn
    json AddTransactionResult { transactionId = tranId }
