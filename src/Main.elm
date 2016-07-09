module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Editor


type Msg
    = Editor Editor.Msg


type alias Model =
    { editor : Editor.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( editor, editorCmd ) =
            Editor.init

        commands =
            Cmd.batch [ Cmd.map Editor editorCmd, Cmd.none ]
    in
        ( { editor = editor }, commands )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Editor editorMsg ->
            let
                ( editor, editorCmd, editorEvent ) =
                    Editor.update editorMsg model.editor

                ( newModel, commandsFromEvent ) =
                    updateByEditorEvent editorEvent { model | editor = editor }
            in
                ( newModel
                , Cmd.batch
                    [ Cmd.map Editor editorCmd
                    , commandsFromEvent
                    ]
                )


updateByEditorEvent : Editor.Event -> Model -> ( Model, Cmd Msg )
updateByEditorEvent event model =
    case event of
        Editor.EventMake ->
            ( { model | editor = (Editor.disable model.editor) }, Cmd.none )

        Editor.EventNone ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div [ Attr.id "app" ]
        [ App.map Editor (Editor.view model.editor)
        ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
