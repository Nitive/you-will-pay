module Api.Response where

import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(JSONResponse))
import Prelude (pure, (<<<))
import Unsafe.Coerce (unsafeCoerce)

newtype SuccessResponse t = SuccessResponse
  { status :: String
  , result :: t
  }

instance respondableSuccessResponse :: Respondable (SuccessResponse a) where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = pure <<< unsafeCoerce
