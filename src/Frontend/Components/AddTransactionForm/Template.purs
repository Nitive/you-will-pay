module Components.AddTransactionForm.Template where

import Components.AddTransactionForm.Model
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (show, ($), (<>))

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

    price value = HH.label_
      [ HH.text "Price: "
      , HH.input
        [ HP.value value
        , HE.onValueInput (HE.input SetPrice)
        , HP.type_ HP.InputNumber
        ]
      ]

    description value = HH.label_
      [ HH.text "Description: "
      , HH.input
        [ HP.value value
        , HE.onValueInput (HE.input SetDescription)
        ]
      ]

    submitButton = HH.button_ [ HH.text "Submit" ]
