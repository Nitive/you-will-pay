{-# LANGUAGE OverloadedStrings #-}

module Api.RoomSummary
( getRoomSummary
) where

import Api.Response
import Control.Monad.IO.Class (liftIO)
import Data.List (maximumBy, minimumBy)
import Db.Selectors
import Db.Types as T
import Web.Scotty (get, param, json)

getTransactionsAmount :: [Transaction] -> Int
getTransactionsAmount transactions = sum $ map price transactions

getUserTransactions :: [Transaction] -> User -> [Transaction]
getUserTransactions transactions user = filter (\transaction -> userId transaction == T.id user) transactions

getSummaryReport :: [Transaction] -> [User] -> RoomSummaryReport
getSummaryReport transactions users =
  RoomSummaryReport
    { payUserName = payUserName
    , payDiff = payDiff
    , users = users
    , history = transactions
    }
  where
    transactionsAmounts = map (getTransactionsAmount . getUserTransactions transactions) users
    usersWithTransactionAmount = zip users transactionsAmounts
    sortByTransactionAmount x y = compare (snd x) (snd y)
    payUser = minimumBy sortByTransactionAmount usersWithTransactionAmount
    mostValuableUser = maximumBy sortByTransactionAmount usersWithTransactionAmount
    payUserName = name $ fst payUser
    payDiff = snd mostValuableUser - snd payUser


getRoomSummary conn =
  get "/api/room-summary/:room" $ do
    roomIdParam <- param "room"
    let roomId = read roomIdParam :: Int

    transactions <- liftIO $ selectTransactions roomId conn
    let usersIds = map userId transactions
    users <- liftIO $ selectUsers usersIds conn

    json $ successResponse $ getSummaryReport transactions users
