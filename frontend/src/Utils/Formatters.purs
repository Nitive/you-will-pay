module Utils.Formatters where

import Data.Formatter.Number (Formatter(..), format)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Prelude ((<<<))

currencyFormatter :: Formatter
currencyFormatter = Formatter
  { comma: true
  , before: 0
  , after: 0
  , abbreviations: false
  , sign: false
  }

formatCurrency :: Number -> String
formatCurrency = replaceAll (Pattern ",") (Replacement " ") <<< format currencyFormatter
