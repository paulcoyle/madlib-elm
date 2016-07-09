module Editor exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onFocus, onBlur, onClick)
import String


type Msg
    = Change String
    | Focus
    | Blur
    | Make
    | Clear


type Event
    = EventNone
    | EventMake


type alias Model =
    { content : String
    , focused : Bool
    , enabled : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { content = ""
            , focused = False
            , enabled = True
            }
    in
        ( model, Cmd.none )


setContent : Model -> String -> Model
setContent model newContent =
    { model | content = newContent }


disable : Model -> Model
disable model =
    { model | enabled = False }


enable : Model -> Model
enable model =
    { model | enabled = True }


update : Msg -> Model -> ( Model, Cmd Msg, Event )
update message model =
    case message of
        Change newContent ->
            ( { model | content = newContent }, Cmd.none, EventNone )

        Focus ->
            ( { model | focused = True }, Cmd.none, EventNone )

        Blur ->
            ( { model | focused = False }, Cmd.none, EventNone )

        Make ->
            ( model, Cmd.none, EventMake )

        Clear ->
            ( { model | content = "" }, Cmd.none, EventNone )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.classList
            [ ( "editor", True )
            , ( "focused", model.focused )
            ]
        ]
        [ Html.textarea
            [ Attr.class "editor-input"
            , Attr.placeholder "Type in here to make a madlib..."
            , Attr.value model.content
            , Attr.disabled (not model.enabled)
            , onInput Change
            , onFocus Focus
            , onBlur Blur
            ]
            []
        , controlsView model
        ]


controlsView : Model -> Html Msg
controlsView model =
    let
        active =
            model.enabled && ((String.length model.content) > 0)
    in
        Html.div [ Attr.class "editor-controls" ]
            [ Html.button
                [ Attr.disabled (not active)
                , onClick Make
                ]
                [ Html.text "Make" ]
            , Html.button
                [ Attr.disabled (not active)
                , onClick Clear
                ]
                [ Html.text "Clear" ]
            ]
