{-# LANGUAGE OverloadedStrings #-}

module Db.Connection
( connection
) where

import Database.PostgreSQL.Simple
import System.Environment (getEnv)

connection :: IO Connection
connection = do
  host <- getEnv "DB_HOST"
  port <- read <$> getEnv "DB_PORT"
  pass <- getEnv "DB_PASS"

  connect ConnectInfo
    { connectHost = host
    , connectPort = port
    , connectUser = "ywp_user"
    , connectPassword = pass
    , connectDatabase = "ywp_db"
    }
