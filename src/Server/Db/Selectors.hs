{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Selectors
( selectUsers
) where

import Database.PostgreSQL.Simple
import Db.Types
import GHC.Generics

selectUsers :: Connection -> IO [User]
selectUsers conn = (query_ conn "select color, id, nickname from users" :: IO [User])
