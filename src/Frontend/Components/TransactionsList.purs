module Components.TransactionsList where

import CSS (Color, StyleM, absolute, backgroundColor, borderRadius, color, display, fontSize, fromHexString, height, inlineBlock, left, marginBottom, marginRight, paddingLeft, pct, position, px, relative, top, width)
import CSS.Common (none)
import CSS.ListStyle.Type (listStyleType)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Halogen.HTML (HTML, li, span, text, ul)
import Halogen.HTML.CSS (style)
import Prelude (Unit, discard, negate, ($), (<$>), (<>))
import Screens.Room.Model (Transaction)
import UI.Colors (warmGrey, white)
import Utils.Formatters (formatCurrency)

listStyle :: StyleM Unit
listStyle = do
  marginBottom $ px (-10.0)
  paddingLeft $ px 16.0
  listStyleType none
  fontSize $ px 13.0
  color warmGrey

itemStyle :: StyleM Unit
itemStyle = do
  marginBottom $ px 10.0
  position relative

circleStyle :: Color -> StyleM Unit
circleStyle userColor = do
  position absolute
  left $ px $ -16.0
  top $ px 4.0
  display inlineBlock
  height $ px 10.0
  width $ px 10.0
  marginRight $ px 5.0
  borderRadius (pct 100.0) (pct 100.0) (pct 100.0) (pct 100.0)
  backgroundColor userColor

transactionsList :: forall p i. Array Transaction -> HTML p i
transactionsList transactions = ul [ style listStyle ] $ renderTransaction <$> transactions
  where
    renderTransaction tr = li [ style itemStyle ]
      [ span [ style $ circleStyle circleColor ] []
      , text description
      ]
      where
        circleColor = fromMaybe white $ fromHexString tr.user.color
        description = tr.user.name <> ", " <> (formatCurrency $ toNumber tr.price) <> " ₽, «" <> tr.description <> "»"
