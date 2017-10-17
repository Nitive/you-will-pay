module Components.TransactionsList where

import CSS (Color, StyleM, absolute, backgroundColor, borderRadius, bottom, color, display, fontSize, fromHexString, height, inlineBlock, left, marginBottom, marginRight, paddingBottom, paddingLeft, pct, position, px, relative, top, width)
import CSS.Common (none)
import CSS.ListStyle.Type (listStyleType)
import Data.Array (length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Halogen.HTML (HTML, div, li, text, ul)
import Halogen.HTML.CSS (style)
import Prelude (Unit, discard, negate, ($), (-), (/=), (<>))
import Screens.Room.Model (Transaction)
import UI.Colors (warmGray, white)
import Utils.Formatters (formatCurrency)

listStyle :: StyleM Unit
listStyle = do
  marginBottom $ px (-10.0)
  paddingLeft $ px 16.0
  listStyleType none
  fontSize $ px 13.0
  color warmGray

itemStyle :: StyleM Unit
itemStyle = do
  paddingBottom $ px 10.0
  position relative

circleStyle :: Color -> StyleM Unit
circleStyle userColor = do
  position absolute
  left $ px $ -16.0
  top $ px 4.0
  display inlineBlock
  height $ px 11.0
  width $ px 11.0
  marginRight $ px 5.0
  borderRadius (pct 100.0) (pct 100.0) (pct 100.0) (pct 100.0)
  backgroundColor userColor

lineStyle :: StyleM Unit
lineStyle = do
  position absolute
  top $ px 4.0
  left $ px $ -11.0
  bottom $ px $ -4.0
  width $ px 1.0
  backgroundColor warmGray

transactionsList :: forall p i. Array Transaction -> HTML p i
transactionsList [] = text "No transactions yet"
transactionsList transactions =
    ul [ style listStyle ] $ mapWithIndex renderTransaction transactions

  where
    renderTransaction index tr = li [ style itemStyle ]
      (line <> [ div [ style $ circleStyle circleColor ] [], text description ])
      where
        withLine = index /= length transactions - 1
        line = if withLine then [ div [ style lineStyle ] [] ] else []
        circleColor = fromMaybe white $ fromHexString tr.user.color
        description = tr.user.name <> ", " <> (formatCurrency $ toNumber tr.price) <> " ₽, «" <> tr.description <> "»"
