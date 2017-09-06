{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty (html, param, get, scotty)
import Data.Monoid (mconcat)

template = mconcat
  [ "<!DOCTYPE html>"
  , "<html lang='en'>"
  ,   "<head>"
  ,     "<meta charset='UTF-8'>"
  ,     "<meta name='viewport' content='width=device-width, initial-scale=1.0'>"
  ,     "<meta http-equiv='X-UA-Compatible' content='ie=edge'>"
  ,     "<title>App</title>"
  ,   "</head>"
  ,   "<body>"
  ,     "<div id='app'>Loading...</div>"
  ,   "</body>"
  , "</html>"
  ]

main = scotty 3000 $ do
  get "/" $ do
    html template