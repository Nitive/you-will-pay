{-# LANGUAGE OverloadedStrings #-}

module Api.RoomSummary
( getRoomSummary
) where

import Control.Monad.IO.Class (liftIO)
import Data.List (maximumBy, minimumBy)
import Db.Selectors
import Db.Types as T
import Web.Scotty (get, param, json, ScottyM)
import Db.Connection (DbConnection, getConnection)

getTransactionsAmount :: [Transaction] -> Int
getTransactionsAmount transactions = sum $ map price transactions

getUserTransactions :: [Transaction] -> User -> [Transaction]
getUserTransactions transactions user = filter (\transaction -> userId transaction == T.id user) transactions

getSummaryReport :: [Transaction] -> [User] -> RoomSummaryReport
getSummaryReport transactions users' =
  RoomSummaryReport
    { payUserId = payUserId'
    , payDiff = payDiff'
    , users = users'
    , history = take 10 transactions
    }
  where
    transactionsAmounts = map (getTransactionsAmount . getUserTransactions transactions) users'
    usersWithTransactionAmount = zip users' transactionsAmounts
    sortByTransactionAmount x y = compare (snd x) (snd y)
    payUser = minimumBy sortByTransactionAmount usersWithTransactionAmount
    mostValuableUser = maximumBy sortByTransactionAmount usersWithTransactionAmount
    payUserId' = T.id $ fst payUser
    payDiff' = snd mostValuableUser - snd payUser


getRoomSummary :: DbConnection -> ScottyM ()
getRoomSummary db =
  get "/api/room/:room/summary" $ do
    conn <- liftIO $ getConnection db
    roomIdParam <- param "room"
    let roomId' = read roomIdParam :: Int

    transactions <- liftIO $ selectTransactions roomId' conn
    users' <- liftIO $ selectAllUsers conn

    json $ getSummaryReport transactions users'
