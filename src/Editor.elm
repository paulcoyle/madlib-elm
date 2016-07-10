module Editor exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onFocus, onBlur, onClick)


type Msg
    = Change String
    | Focus
    | Blur
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

        Clear ->
            ( { model | content = "" }, Cmd.none, EventNone )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.classList
            [ ( "card-contents", True )
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
        ]
