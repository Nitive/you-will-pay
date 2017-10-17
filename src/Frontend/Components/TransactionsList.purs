module Components.TransactionsList where

import CSS (StyleM, color, fontSize, marginBottom, paddingLeft, px)
import CSS.Common (none)
import CSS.ListStyle.Type (listStyleType)
import Data.Int (toNumber)
import Halogen.HTML (HTML, li, text, ul)
import Halogen.HTML.CSS (style)
import Prelude (Unit, discard, negate, ($), (<$>), (<>))
import Screens.Room.Model (Transaction)
import UI.Colors (warmGrey)
import Utils.Formatters (formatCurrency)

listStyle :: StyleM Unit
listStyle = do
  marginBottom $ px (-10.0)
  paddingLeft $ px 0.0
  listStyleType none
  fontSize $ px 13.0
  color warmGrey

itemStyle :: StyleM Unit
itemStyle = do
  marginBottom $ px 10.0

transactionsList :: forall p i. Array Transaction -> HTML p i
transactionsList transactions = ul [ style listStyle ] $ renderTransaction <$> transactions
  where
    renderTransaction tr = li [ style itemStyle ] [ text description ]
      where
        description = tr.user.name <> ", " <> (formatCurrency $ toNumber tr.price) <> " ₽, «" <> tr.description <> "»"
