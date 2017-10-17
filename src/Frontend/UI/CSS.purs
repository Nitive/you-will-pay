module UI.CSS where

import CSS (class Val, CSS, Size, StyleM, fromString, key, paddingLeft, paddingRight)
import CSS.Common (class None)
import Prelude (Unit, discard, ($))


borderWidth :: forall a. Size a -> CSS
borderWidth = key $ fromString "border-width"

data OutlineValue = None
instance valOutlineValue :: Val OutlineValue where
  value (None) = fromString "none"

instance noneOutlineValue :: None OutlineValue where
  none = None

outline :: OutlineValue -> CSS
outline = key $ fromString "outline"

paddingX :: forall a. Size a -> StyleM Unit
paddingX x = do
  paddingLeft x
  paddingRight x
