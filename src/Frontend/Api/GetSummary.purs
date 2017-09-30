module Api.GetSummary where

import Api.Response (SuccessResponse)
import Network.HTTP.Affjax (Affjax, get)
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

type GetSummaryResponse =
  { payUserName :: String
  , payDiff :: Int
  , users :: Array User
  , history :: Array Transaction
  }

getSummary :: forall eff. Int -> Affjax eff (SuccessResponse GetSummaryResponse)
getSummary roomId = get $ "http://localhost:3000/api/room/" <> (show roomId) <> "/summary"
