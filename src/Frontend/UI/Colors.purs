module UI.Colors where

import Color (Color, fromInt, rgba)

darkGray :: Color
darkGray = fromInt 0x373737

paleGrey :: Color
paleGrey = fromInt 0xe4e6ea

dodgerBlue :: Color
dodgerBlue = fromInt 0x4e9dff

warmGrey :: Color
warmGrey = fromInt 0x9F9F9F

white :: Color
white = fromInt 0xfdfdfd

transparent :: Color
transparent = rgba 0xff 0xff 0xff 0.0
