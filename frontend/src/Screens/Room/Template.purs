module Screens.Room.Template where

import CSS (StyleM, alignItems, backgroundColor, block, color, display, flex, fontSize, fromInt, height, justifyContent, letterSpacing, lineHeight, margin, marginBottom, marginTop, maxWidth, paddingLeft, paddingRight, paddingTop, px, vh)
import CSS.Common (auto, center)
import CSS.TextAlign as TA
import Color.Scheme.HTML (red)
import Components.AddTransactionForm as ATF
import Components.Layout (layout)
import Components.TransactionsList (transactionsList)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Prelude (class Eq, class Ord, Unit, discard, show, unit, ($), (<>))
import Screens.Room.Model (State, Status(..))
import Types (ComponentEffects)
import UI.Colors (warmGray)

data Slot = ATFSlot
derive instance eqATFSlot :: Eq Slot
derive instance ordATFSlot :: Ord Slot

data Query a
  = GetSummary a
  | HandleForm ATF.Message a

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
  TA.textAlign TA.center
  fontSize $ px 24.0

relativePriceStyle :: StyleM Unit
relativePriceStyle = do
  marginBottom $ px 40.0
  color warmGray
  TA.textAlign TA.center
  fontSize $ px 13.0

errorStyle :: StyleM Unit
errorStyle = do
  color red

serverErrorContainer :: StyleM Unit
serverErrorContainer = do
  display flex
  justifyContent center
  alignItems center
  height $ vh 100.0

serverErrorH1 :: StyleM Unit
serverErrorH1 = do
  fontSize $ px 200.0
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
  lineHeight $ px 200.0

serverErrorTrolling :: StyleM Unit
serverErrorTrolling = do
  fontSize $ px 24.0
  letterSpacing $ px 1.6
  display block
  margin (px 10.0) (px 10.0) (px 10.0) (px 10.0)

roomTemplate :: forall eff. State -> H.ParentHTML Query ATF.Query Slot (ComponentEffects eff)
roomTemplate state = layout
  case state.status of
    Loaded ->
      case state.summary of
        Just summary ->
          let
            formProps =
              { payUserId: summary.payUser.id
              , users: summary.users
              }
          in
            HH.div [ style screenStyle ]
              [ HH.header [ style headerStyle ] [ HH.text "Matrix" ]
              , HH.div [ style mainStyle ]
                [ HH.p [ style userPaysStyle ]
                  [ HH.text $ summary.payUser.name <> " pays" ],
                  HH.p [ style relativePriceStyle ]
                    [ HH.text $ summary.payUser.name <> " spent " <> (show summary.payDiff) <> " RUB less" ]
                , HH.slot ATFSlot (ATF.addTransactionForm formProps) unit (HE.input HandleForm)
                , transactionsList summary.history
                ]
              ]

        Nothing ->
          HH.div [ style errorStyle ] [ HH.text "Error..." ]

    Pending ->
      HH.text "Loading..."

    Errored ->
      HH.div [ style serverErrorContainer ]
        [ HH.div_
            [ HH.h1 [ style serverErrorH1 ] [ HH.text "502" ]
            , HH.span [ style serverErrorTrolling ] [ HH.text "тебе должно быть стыдно" ]
            ]
        ]
