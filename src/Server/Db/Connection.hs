{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Connection
( connection
) where

import Database.PostgreSQL.Simple
import System.Environment (getEnv)

connection :: IO Connection
connection = do
  host <- getEnv "DB_HOST"

  connect $ ConnectInfo
    { connectHost = host
    , connectPort = 5432
    , connectUser = "ywp_user"
    , connectPassword = ""
    , connectDatabase = "ywp_db"
    }
