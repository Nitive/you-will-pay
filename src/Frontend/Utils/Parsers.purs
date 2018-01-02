module Utils.Parsers where

import Data.Maybe (Maybe)
import Data.Number (fromString)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Prelude ((<<<))

parseNumber :: String -> Maybe Number
parseNumber = fromString <<< replaceAll (Pattern ",") (Replacement ".")
