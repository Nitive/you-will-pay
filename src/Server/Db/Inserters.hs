{-# LANGUAGE OverloadedStrings #-}

module Db.Inserters
( insertTransaction
) where

import Database.PostgreSQL.Simple
import Db.Types
import Control.Monad.IO.Class
import GHC.Int

insertTransaction :: Transaction -> Connection -> IO Int
insertTransaction transaction conn = fromOnly . head <$> result
  where
    result :: IO [Only Int]
    result = query conn "INSERT INTO transactions (created, user_id, price, summary, room_id) VALUES (?, ?, ?, ?, ?) returning id" transaction
