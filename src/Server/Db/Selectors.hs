{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Selectors
( selectUsers
, selectTransactions
) where

import Database.PostgreSQL.Simple
import Db.Types
import GHC.Generics

selectUsers :: [Int] -> Connection -> IO [User]
selectUsers ids conn = query conn "select color, id, nickname from users where id in ?" $ Only $ In ids

selectTransactions :: Int -> Connection -> IO [Transaction]
selectTransactions room conn = query conn "select created, id, price, summary from transactions where room_id = ? limit 10" $ Only room
