{-# LANGUAGE OverloadedStrings #-}

module Db.Selectors
( selectAllUsers
, selectTransactions
) where

import Database.PostgreSQL.Simple
import Db.Types

selectAllUsers :: Connection -> IO [User]
selectAllUsers conn = query_ conn "select color, id, nickname from users"

selectTransactions :: Int -> Connection -> IO [Transaction]
selectTransactions room conn = query conn "select created, user_id, price, summary, room_id from transactions where room_id = ? order by created desc" $ Only room
