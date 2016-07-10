module Icon exposing (..)

import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type alias SvgId =
    String


type Msg
    = NoOp


view : SvgId -> Html Msg
view svgId =
    Html.div [ HtmlAttr.class "icon" ]
        [ Svg.svg []
            [ Svg.use [ SvgAttr.xlinkHref ("#icon-" ++ svgId) ] [] ]
        ]
