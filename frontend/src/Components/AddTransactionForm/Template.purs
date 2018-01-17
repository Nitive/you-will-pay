module Components.AddTransactionForm.Template where

import Components.AddTransactionForm.Model

import CSS (StyleM, backgroundColor, block, borderBottom, borderBox, borderRadius, boxSizing, color, display, fontSize, marginBottom, marginTop, paddingBottom, px, solid, width)
import CSS.Common (none)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (StepValue(..))
import Halogen.HTML.Properties as HP
import Prelude (Unit, discard, show, ($), (<$>), (<>), (==))
import Screens.Room.Model (User)
import UI.CSS (borderWidth, outline, paddingX, visuallyHidden)
import UI.Colors (paleGrey, transparent, white)


selectStyle :: StyleM Unit
selectStyle = do
  fontSize $ px 24.0
  color white
  backgroundColor transparent

inputStyle :: StyleM Unit
inputStyle = do
  display block
  width $ px 260.0
  paddingX $ px 15.0
  color white
  backgroundColor transparent
  borderWidth $ px 0.0
  borderBottom solid (px 1.0) paleGrey
  borderRadius (px 0.0) (px 0.0) (px 0.0) (px 0.0)
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
submitButtonStyle = visuallyHidden

getReport :: forall a b. Report -> HTML a b
getReport report = HH.text $ "transaction #" <> show report.transactionId <> " " <> "created"

getSpinner :: forall a b. Status -> Maybe (HTML a b)
getSpinner Pending = Just $ HH.text "Pending..."
getSpinner Loaded = Nothing

addTransactionFormTemplate :: Array User -> State -> H.ComponentHTML Query
addTransactionFormTemplate users state =
  HH.div_ $ catMaybes
    [ getReport <$> state.report
    , getSpinner state.status
    , Just $
        HH.form [ HP.action "", HE.onSubmit (HE.input SubmitForm) ]
          [ userSelect state.payUserId
          , price state.price
          , description state.description
          , submitButton
          ]
    ]

  where
    userSelect payUserId = HH.select [ HE.onValueChange (HE.input SetPayUserId), style selectStyle ] options
      where
        renderUserSelectOption user = HH.option
          [ HP.value $ show user.id, HP.selected $ show user.id == payUserId ]
          [ HH.text user.name ]
        options = renderUserSelectOption <$> users

    price value = HH.input
      [ HP.value value
      , HE.onValueInput (HE.input SetPrice)
      , HP.type_ HP.InputNumber
      , HP.step (Step 0.01)
      , HP.min 0.0
      , HP.placeholder "Price"
      , style priceInputStyle
      ]

    description value = HH.input
      [ HP.value value
      , HE.onValueInput (HE.input SetDescription)
      , HP.placeholder "A few words about it"
      , HP.required true
      , style descriptionInputStyle
      ]

    submitButton = HH.input [ HP.type_ HP.InputSubmit, HP.value "Save", style submitButtonStyle ]
