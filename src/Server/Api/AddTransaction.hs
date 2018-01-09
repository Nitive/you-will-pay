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
import Db.Connection (DbConnection, getConnection)

newtype AddTransactionResult = AddTransactionResult
  { transactionId :: Int } deriving (Generic, Show)

instance ToJSON AddTransactionResult

addTransaction :: DbConnection -> ScottyM ()
addTransaction db =
  put "/api/add-transaction" $ do
    conn <- liftIO $ getConnection db
    transaction <- jsonData
    tranId <- liftIO $ insertTransaction transaction conn
    json AddTransactionResult { transactionId = tranId }
