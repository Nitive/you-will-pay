module Api.AddTransaction where

import Api.Response (gFromResponse, gResponseType)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Network.HTTP.Affjax (Affjax, put)
import Network.HTTP.Affjax.Request (class Requestable, toRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Prelude (($))

newtype AddTransactionRequest = AddTransactionRequest
  { created :: String
  , userId :: Int
  , price :: Int
  , description :: String
  , roomId :: Int
  }

instance encodeJsonAddTransactionRequest :: EncodeJson AddTransactionRequest where
  encodeJson (AddTransactionRequest req)
    =  "created" := req.created
    ~> "userId" := req.userId
    ~> "price" := req.price
    ~> "description" := req.description
    ~> "roomId" := req.roomId
    ~> jsonEmptyObject

instance requestableAddTransactionRequest :: Requestable AddTransactionRequest where
  toRequest req = toRequest $ encodeJson req

newtype AddTransactionResponse = AddTransactionResponse
  { transactionId :: Int
  }

instance respondableAddTransactionResponse :: Respondable AddTransactionResponse where
  responseType = gResponseType
  fromResponse = gFromResponse

addTransaction :: forall eff. AddTransactionRequest -> Affjax eff AddTransactionResponse
addTransaction = put "http://localhost:3000/api/add-transaction"
