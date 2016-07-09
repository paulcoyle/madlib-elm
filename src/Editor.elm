module Editor exposing (..)

import Array exposing (Array)
import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events exposing (onWithOptions, onFocus, onBlur, keyCode)
import Json.Decode as Json exposing ((:=))
import Regex
import String


type Msg
    = Focus
    | Blur
    | Key String


type Event
    = EventNone


type alias Model =
    { content : String
    , caret : Int
    , focused : Bool
    }


init : ( Model, Cmd Msg )
init =
    initWithContent ""


initWithContent : String -> ( Model, Cmd Msg )
initWithContent content =
    let
        model =
            { content = content
            , caret = String.length content
            , focused = False
            }
    in
        ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg, Event )
update message model =
    case message of
        Focus ->
            ( { model | focused = True }, Cmd.none, EventNone )

        Blur ->
            ( { model | focused = False }, Cmd.none, EventNone )

        Key key ->
            if (isContentKey key) then
                ( (appendAtCaret model key), Cmd.none, EventNone )
            else
                case key of
                    "ArrowLeft" ->
                        ( (updateCaret model -1), Cmd.none, EventNone )

                    "ArrowRight" ->
                        ( (updateCaret model 1), Cmd.none, EventNone )

                    "Backspace" ->
                        ( (dropBackAtCaret model), Cmd.none, EventNone )

                    "Delete" ->
                        ( (dropForwardAtCaret model), Cmd.none, EventNone )

                    _ ->
                        ( model, Cmd.none, EventNone )


appendAtCaret : Model -> String -> Model
appendAtCaret model append =
    let
        left =
            String.slice 0 model.caret model.content

        right =
            String.slice model.caret (String.length model.content) model.content

        newContent =
            left ++ append ++ right

        newCaret =
            model.caret + (String.length append)
    in
        { model
            | content = newContent
            , caret = newCaret
        }


updateCaret : Model -> Int -> Model
updateCaret model delta =
    let
        newCaret =
            clamp 0 (String.length model.content) (model.caret + delta)
    in
        { model | caret = newCaret }


dropBackAtCaret : Model -> Model
dropBackAtCaret model =
    let
        newContent =
            dropCharAt model.content model.caret

        newModel =
            updateCaret model -1
    in
        { newModel | content = newContent }


dropForwardAtCaret : Model -> Model
dropForwardAtCaret model =
    let
        newContent =
            dropCharAt model.content (model.caret + 1)
    in
        { model | content = newContent }


dropCharAt : String -> Int -> String
dropCharAt content index =
    let
        length =
            String.length content

        leftBound =
            clamp 0 length (index - 1)

        rightBound =
            clamp 0 length index

        left =
            String.slice 0 leftBound content

        right =
            String.slice rightBound length content
    in
        left ++ right


isContentKey : String -> Bool
isContentKey key =
    let
        expr =
            Regex.caseInsensitive (Regex.regex "^[a-z,;\\.\\:\\s\\-\\!\\?]{1}$")
    in
        Regex.contains expr key


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.tabindex 0
          -- to allow focusing
        , Attr.classList
            [ ( "editor", True )
            , ( "focused", model.focused )
            ]
        , onFocus Focus
        , onBlur Blur
        , onKeyDown Key
        ]
        [ contentView model ]


contentView : Model -> Html Msg
contentView model =
    let
        hasCaret : Int -> Bool
        hasCaret charIndex =
            charIndex == (model.caret - 1)

        indexedChars : List ( Int, Char )
        indexedChars =
            Array.toIndexedList
                (String.foldl (\a b -> Array.push a b)
                    Array.empty
                    model.content
                )

        charToString : Char -> String
        charToString char =
            case char of
                ' ' ->
                    " "

                _ ->
                    String.fromChar char

        charToHtml : ( Int, Char ) -> Html Msg
        charToHtml ( index, char ) =
            Html.span
                [ Attr.classList
                    [ ( "has-caret", hasCaret index )
                    ]
                ]
                [ Html.text (charToString char) ]

        spanFold : ( Int, Char ) -> List (Html Msg) -> List (Html Msg)
        spanFold char spans =
            List.append spans [ (charToHtml char) ]

        charSpans : List (Html Msg)
        charSpans =
            List.foldl spanFold [] indexedChars
    in
        Html.div
            [ Attr.classList
                [ ( "editor-content", True )
                ]
            ]
            charSpans


onKeyDown : (String -> msg) -> Attribute msg
onKeyDown tagger =
    let
        options =
            { stopPropagation = False, preventDefault = True }

        charDecoder =
            "key" := Json.string
    in
        onWithOptions "keydown" options (Json.map tagger charDecoder)
