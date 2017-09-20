{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Connection
( connection
) where

import Database.PostgreSQL.Simple

connection :: IO Connection
connection = connectPostgreSQL "dbname='you-will-pay'"
