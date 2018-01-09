{-# LANGUAGE OverloadedStrings #-}

module Db.Connection
( createConnection
, getConnection
, DbConnection
) where

import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Data.Pool (createPool, withResource, Pool)

createConnection' :: IO Connection
createConnection' = do
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

type DbConnection = Pool Connection

createConnection :: IO DbConnection
createConnection = createPool createConnection' close 4 600 200

getConnection :: DbConnection -> IO Connection
getConnection pool = withResource pool pure
