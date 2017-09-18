{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Connection
( connection
) where

import Database.PostgreSQL.Simple

connection :: IO Connection
connection = connect defaultConnectInfo { connectDatabase = "you-will-pay" }
