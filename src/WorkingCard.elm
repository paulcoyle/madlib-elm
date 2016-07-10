module WorkingCard exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Icon


type Msg
    = NoOp Icon.Msg


view : String -> String -> Html Msg
view id message =
    Html.div
        [ Attr.id id
        , Attr.class "card-contents"
        ]
        [ App.map NoOp (Icon.view "cog")
        , Html.p [ Attr.class "message" ] [ Html.text message ]
        ]
