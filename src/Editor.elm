port module Editor exposing (..)

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
    | TextareaHeight ( String, Int )


port checkTextareaHeight : String -> Cmd msg


port textareaHeight : (( String, Int ) -> msg) -> Sub msg


type Event
    = EventNone
    | EventMake


type alias Model =
    { id : String
    , height : Int
    , content : String
    , focused : Bool
    , enabled : Bool
    }


init : String -> ( Model, Cmd Msg )
init id =
    let
        model =
            { id = id
            , height = 0
            , content = ""
            , focused = False
            , enabled = True
            }
    in
        ( model, checkTextareaHeight id )


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
            ( { model | content = newContent }, checkTextareaHeight model.id, EventNone )

        Focus ->
            ( { model | focused = True }, Cmd.none, EventNone )

        Blur ->
            ( { model | focused = False }, Cmd.none, EventNone )

        Make ->
            ( model, Cmd.none, EventMake )

        Clear ->
            ( { model | content = "" }, checkTextareaHeight model.id, EventNone )

        TextareaHeight ( id, height ) ->
            if id == model.id then
                ( { model | height = height }, Cmd.none, EventNone )
            else
                ( model, Cmd.none, EventNone )


subscriptions : Model -> Sub Msg
subscriptions model =
    textareaHeight TextareaHeight


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.classList
            [ ( "editor", True )
            , ( "focused", model.focused )
            ]
        ]
        [ Html.textarea
            [ Attr.id model.id
            , Attr.class "editor-input"
            , Attr.placeholder "Type in here to make a madlib..."
            , Attr.value model.content
            , Attr.disabled (not model.enabled)
            , Attr.style [ ( "height", (toString model.height) ++ "px" ) ]
            , onInput Change
            , onFocus Focus
            , onBlur Blur
            ]
            []
        , controlsView model
        , Html.div [] [ Html.text (toString model.height) ]
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
