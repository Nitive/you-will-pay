module Api.Response where

import Data.Foreign (F)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (ResponseContent, ResponseType(JSONResponse))
import Prelude (pure, (<<<))
import Unsafe.Coerce (unsafeCoerce)

gResponseType :: forall a. Tuple (Maybe MediaType) (ResponseType a)
gResponseType = Tuple (Just applicationJSON) JSONResponse

gFromResponse :: forall a. ResponseContent -> F a
gFromResponse = pure <<< unsafeCoerce
