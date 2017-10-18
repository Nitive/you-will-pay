module Components.AddTransactionForm.Template where

import Components.AddTransactionForm.Model

import CSS (StyleM, backgroundColor, block, borderBottom, borderBox, boxSizing, color, display, displayNone, fontSize, marginBottom, marginTop, paddingBottom, px, solid, width)
import CSS.Common (none)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, discard, show, ($), (<>))
import UI.CSS (borderWidth, outline, paddingX)
import UI.Colors (paleGrey, transparent, white)


inputStyle :: StyleM Unit
inputStyle = do
  display block
  width $ px 260.0
  paddingX $ px 15.0
  color white
  backgroundColor transparent
  borderWidth $ px 0.0
  borderBottom solid (px 1.0) paleGrey
  boxSizing borderBox
  outline none

priceInputStyle :: StyleM Unit
priceInputStyle = do
  inputStyle
  marginTop $ px 25.0
  marginBottom $ px 20.0
  fontSize $ px 36.0

descriptionInputStyle :: StyleM Unit
descriptionInputStyle = do
  inputStyle
  marginBottom $ px 35.0
  paddingBottom $ px 3.0
  fontSize $ px 18.0

submitButtonStyle :: StyleM Unit
submitButtonStyle = do
  display displayNone

addTransactionFormTemplate :: State -> H.ComponentHTML Query
addTransactionFormTemplate state =
  case state.status of
    Loaded ->
      case state.report of
        Just report ->
          HH.text $ "transaction #" <> show report.transactionId <> " " <> "created"
        Nothing ->
          HH.form [ HE.onSubmit (HE.input SubmitForm) ]
            [ userSelect state.payUserId
            , price state.price
            , description state.description
            , submitButton
            ]
    Pending ->
      HH.text "Pending..."

  where
    userSelect payUserId = HH.label_
      [ HH.text "User ID: "
      , HH.input
        [ HP.value payUserId
        , HE.onValueInput (HE.input SetPayUserId)
        , HP.type_ HP.InputNumber
        ]
      ]

    price value = HH.input
      [ HP.value value
      , HE.onValueInput (HE.input SetPrice)
      , HP.type_ HP.InputNumber
      , HP.placeholder "Price"
      , style priceInputStyle
      ]

    description value = HH.input
      [ HP.value value
      , HE.onValueInput (HE.input SetDescription)
      , HP.placeholder "A few words about it"
      , style descriptionInputStyle
      ]

    submitButton = HH.button [ style submitButtonStyle ] []
