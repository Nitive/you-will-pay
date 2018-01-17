module UI.CSS where

import CSS (class Val, CSS, Size, StyleM, absolute, fromString, height, key, margin, padding, paddingLeft, paddingRight, position, px, width)
import CSS.Common (class None)
import CSS.Overflow (hidden, overflow)
import Prelude (Unit, discard, negate, show, ($), (<<<), (<>))


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

opacity :: Number -> CSS
opacity = key (fromString "opacity") <<< show

data RectValue = Rect Number Number Number Number
instance valRectValue :: Val RectValue where
  value (Rect a b c d) = fromString $ "rect(" <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> ")"

clip :: RectValue -> CSS
clip = key $ fromString "clip"

visuallyHidden :: StyleM Unit
visuallyHidden = do
  position absolute
  overflow hidden
  clip $ Rect 0.0 0.0 0.0 0.0
  height (px 1.0)
  width (px 1.0)
  margin (px $ -1.0) (px $ -1.0) (px $ -1.0) (px $ -1.0)
  padding (px 0.0) (px 0.0) (px 0.0) (px 0.0)
  borderWidth (px 0.0)
