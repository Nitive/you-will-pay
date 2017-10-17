module Screens.Room.Template where

import CSS (StyleM, backgroundColor, color, fontSize, fromInt, margin, marginBottom, marginTop, maxWidth, paddingLeft, paddingRight, paddingTop, px)
import CSS.Common (auto)
import CSS.TextAlign (textAlign, center)
import Color.Scheme.HTML (red)
import Components.AddTransactionForm as ATF
import Components.Layout (layout)
import Components.TransactionsList (transactionsList)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Prelude (class Eq, class Ord, Unit, const, discard, show, unit, ($), (<>))
import Screens.Room.Model (Query, State, Status(..))
import Types (ComponentEffects)
import UI.Colors (warmGray)

data Slot = ATFSlot
derive instance eqATFSlot :: Eq Slot
derive instance ordATFSlot :: Ord Slot

screenStyle :: StyleM Unit
screenStyle = do
  maxWidth $ px 500.0
  margin (px 0.0) auto (px 0.0) auto

headerStyle :: StyleM Unit
headerStyle = do
  paddingRight $ px 5.0
  paddingLeft $ px 5.0
  backgroundColor $ fromInt 0x4990E2
  fontSize $ px 10.0

mainStyle :: StyleM Unit
mainStyle = do
  paddingTop $ px 20.0
  paddingRight $ px 35.0
  paddingLeft $ px 35.0

userPaysStyle :: StyleM Unit
userPaysStyle = do
  marginTop $ px 0.0
  marginBottom $ px 8.0
  textAlign center
  fontSize $ px 24.0

relativePriceStyle :: StyleM Unit
relativePriceStyle = do
  marginBottom $ px 40.0
  color warmGray
  textAlign center
  fontSize $ px 13.0

errorStyle :: StyleM Unit
errorStyle = do
  color red

roomTemplate :: forall eff. State -> H.ParentHTML Query ATF.Query Slot (ComponentEffects eff)
roomTemplate state = layout
  case state.status of
    Loaded ->
      case state.summary of
        Just summary ->
          HH.div [ style screenStyle ]
            [ HH.header [ style headerStyle ] [ HH.text "Matrix" ]
            , HH.div [ style mainStyle ]
              [ HH.p [ style userPaysStyle ]
                [ HH.text $ summary.payUser.name <> " pays" ],
                HH.p [ style relativePriceStyle ]
                  [ HH.text $ summary.payUser.name <> " spent " <> (show summary.payDiff) <> " RUB less" ]
              , HH.slot ATFSlot ATF.addTransactionForm unit (const Nothing)
              , transactionsList summary.history
            ]
        ]

        Nothing ->
          HH.div [ style errorStyle ] [ HH.text "Error..." ]

    Pending ->
      HH.text "Loading..."
