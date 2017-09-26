module Api.AddTransaction where

import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, AffjaxResponse, put)
import Network.HTTP.Affjax.Request (class Requestable, toRequest)
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(JSONResponse))
import Prelude (pure, ($), (<<<))
import Unsafe.Coerce (unsafeCoerce)

newtype SuccessResponse t = SuccessResponse
  { status :: String
  , result :: t
  }

instance respondableSuccessResponse :: Respondable (SuccessResponse a) where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = pure <<< unsafeCoerce

newtype AddTransactionResponse = AddTransactionResponse
  { transactionId :: Int
  }

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

addTransaction :: forall eff. AddTransactionRequest -> Aff (ajax :: AJAX | eff) (AffjaxResponse (SuccessResponse AddTransactionResponse))
addTransaction = put "http://localhost:3000/api/add-transaction"
