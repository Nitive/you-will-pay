module Api.GetSummary where

import Api.Response (gFromResponse, gResponseType)
import Network.HTTP.Affjax (Affjax, get)
import Network.HTTP.Affjax.Response (class Respondable)
import Prelude (show, ($), (<>))

type User =
  { name :: String
  , color :: String
  , id :: Int
  }

type Transaction =
  { created :: String
  , userId :: Int
  , roomId :: Int
  , price :: Int
  , description :: String
  }

newtype GetSummaryResponse = GetSummaryResponse
  { payUserId :: Int
  , payDiff :: Int
  , users :: Array User
  , history :: Array Transaction
  }

instance respondableGetSummaryResponse :: Respondable GetSummaryResponse where
  responseType = gResponseType
  fromResponse = gFromResponse

getSummary :: forall eff. Int -> Affjax eff GetSummaryResponse
getSummary roomId = get $ "http://localhost:3000/api/room/" <> (show roomId) <> "/summary"
