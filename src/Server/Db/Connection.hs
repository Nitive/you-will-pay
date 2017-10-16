{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Connection
( connection
) where

import Control.Monad (liftM)
import Database.PostgreSQL.Simple
import System.Environment (getEnv)

connection :: IO Connection
connection = do
  host <- getEnv "DB_HOST"
  port <- liftM read $ getEnv "DB_PORT"
  pass <- getEnv "DB_PASS"

  connect $ ConnectInfo
    { connectHost = host
    , connectPort = port
    , connectUser = "ywp_user"
    , connectPassword = pass
    , connectDatabase = "ywp_db"
    }
