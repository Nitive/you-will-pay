module Components.Layout where

import CSS (StyleM, backgroundColor, body, color, margin, px, (?))
import Halogen.HTML (HTML, div_, link)
import Halogen.HTML.CSS (stylesheet)
import Halogen.HTML.Properties (href, rel)
import Prelude (Unit, discard)
import UI.Colors (darkGray, white)
import UI.Fonts (fontFamilyLato)


bodyStyle :: StyleM Unit
bodyStyle =
  body ? do
    margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
    backgroundColor darkGray
    color white
    fontFamilyLato


layout :: forall a b. HTML a b -> HTML a b
layout content =
  div_
    [ stylesheet bodyStyle
    , link [ href "https://fonts.googleapis.com/css?family=Lato:400,700&amp;subset=latin-ext", rel "stylesheet" ]
    , content
    ]
