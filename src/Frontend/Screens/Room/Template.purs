module Screens.Room.Template where

import CSS (StyleM, backgroundColor, color, fontSize, margin, marginBottom, marginTop, maxWidth, paddingLeft, paddingRight, paddingTop, px, rgb, white)
import CSS.Common (auto)
import CSS.TextAlign (textAlign, center)
import Color.Scheme.HTML (red)
import Components.AddTransactionForm as ATF
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Prelude (class Eq, class Ord, Unit, const, discard, show, unit, ($), (<$>), (<>))
import Screens.Room.Model (Query, State, Status(..))
import Types (ComponentEffects)

data Slot = ATFSlot
derive instance eqATFSlot :: Eq Slot
derive instance ordATFSlot :: Ord Slot

screenStyle :: StyleM Unit
screenStyle = do
  maxWidth $ px 500.0
  margin (px 0.0) auto (px 0.0) auto
  color white
  backgroundColor $ rgb 0x37 0x37 0x37

headerStyle :: StyleM Unit
headerStyle = do
  paddingRight $ px 5.0
  paddingLeft $ px 5.0
  backgroundColor $ rgb 0x49 0x90 0xE2
  fontSize $ px 10.0

mainStyle :: StyleM Unit
mainStyle = do
  paddingTop $ px 20.0
  paddingRight $ px 30.0
  paddingLeft $ px 30.0

userPaysStyle :: StyleM Unit
userPaysStyle = do
  marginTop $ px 0.0
  marginBottom $ px 8.0
  textAlign center
  fontSize $ px 24.0

relativePriceStyle :: StyleM Unit
relativePriceStyle = do
  marginBottom $ px 40.0
  color $ rgb 0x9F 0x9F 0x9F
  textAlign center
  fontSize $ px 13.0

errorStyle :: StyleM Unit
errorStyle = do
  color red

roomTemplate :: forall eff. State -> H.ParentHTML Query ATF.Query Slot (ComponentEffects eff)
roomTemplate state =
  case state.status of
    Loaded ->
      case state.summary of
        Just summary ->
          HH.div [ style screenStyle ] [
            HH.header [ style headerStyle ] [ HH.text "Matrix" ]
          , HH.div [ style mainStyle ] [
              HH.p [ style userPaysStyle ] [
                HH.text $ summary.payUser.name <> " pays"
              ],
              HH.p [ style relativePriceStyle ] [
                HH.text $ summary.payUser.name <> " spent " <> (show summary.payDiff) <> " RUB less"
              ]
            , HH.slot ATFSlot ATF.addTransactionForm unit (const Nothing)
            , HH.ul_ $ renderTransaction <$> summary.history
          ]
        ]

        Nothing ->
          HH.div [ style errorStyle ] [ HH.text "Error..." ]

    Pending ->
      HH.text "Loading..."

  where
    renderTransaction tr = HH.li_
      [ HH.text $ tr.user.name <> ": " <> (show tr.price) <> " (" <> tr.description <> ")"
      ]
