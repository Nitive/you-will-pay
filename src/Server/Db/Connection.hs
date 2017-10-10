{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Connection
( connection
) where

import Database.PostgreSQL.Simple

connection :: IO Connection
connection = connect $ ConnectInfo
  { connectHost = "localhost"
  , connectPort = 5432
  , connectUser = "ywp_user"
  , connectPassword = ""
  , connectDatabase = "ywp_db"
  }
