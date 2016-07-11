port module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Editor
import WorkingCard
import String
import Icon


type Msg
    = StageForward
    | StageBack
    | ReceiveParse ParseResponse
    | Editor Editor.Msg
      -- These Icon messages are ignored.
    | Icon Icon.Msg
    | WorkingCard WorkingCard.Msg


type Stage
    = StageTextEntry
    | StageWaitingForParse
    | StageConfigMadlib


type alias Model =
    { stage : Stage
    , editor : Editor.Model
    , lastParseId : Int
    , lastGoodParseId : Int
    , lastParse : List Parse
    }


type alias Parse =
    { kind : String, value : String }


type alias ParseResponse =
    ( Int, Bool, List Parse )


init : ( Model, Cmd Msg )
init =
    let
        ( editor, editorCmd ) =
            Editor.init

        commands =
            Cmd.batch [ Cmd.map Editor editorCmd, Cmd.none ]
    in
        ( { stage = StageTextEntry
          , editor = editor
          , lastParseId = 0
          , lastGoodParseId = -1
          , lastParse = []
          }
        , commands
        )


port parse : ( Int, String ) -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        StageForward ->
            if canStageForward model then
                stepStageForward model
            else
                ( model, Cmd.none )

        StageBack ->
            if canStageBack model then
                stepStageBack model
            else
                ( model, Cmd.none )

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

        ReceiveParse ( id, success, parseData ) ->
            let
                lastGood =
                    if success then
                        id
                    else
                        model.lastGoodParseId

                -- Add stage change here to auto step forward
            in
                ( { model
                    | lastGoodParseId = lastGood
                    , lastParse = parseData
                  }
                , Cmd.none
                )

        Icon _ ->
            ( model, Cmd.none )

        WorkingCard _ ->
            ( model, Cmd.none )


port parsed : (ParseResponse -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    parsed ReceiveParse


stageOrder : Stage -> Int
stageOrder stage =
    case stage of
        StageTextEntry ->
            0

        StageWaitingForParse ->
            1

        StageConfigMadlib ->
            2


stepStageForward : Model -> ( Model, Cmd Msg )
stepStageForward model =
    case model.stage of
        StageTextEntry ->
            let
                nextId =
                    model.lastParseId + 1
            in
                ( { model
                    | stage = StageWaitingForParse
                    , editor = Editor.disable model.editor
                    , lastParseId = nextId
                  }
                , parse ( nextId, model.editor.content )
                )

        StageWaitingForParse ->
            ( { model | stage = StageConfigMadlib }, Cmd.none )

        StageConfigMadlib ->
            ( model, Cmd.none )


canStageForward : Model -> Bool
canStageForward model =
    case model.stage of
        StageTextEntry ->
            (String.length model.editor.content) > 0

        StageWaitingForParse ->
            model.lastParseId == model.lastGoodParseId

        StageConfigMadlib ->
            -- placeholder
            False


stepStageBack : Model -> ( Model, Cmd Msg )
stepStageBack model =
    case model.stage of
        StageTextEntry ->
            ( model, Cmd.none )

        StageWaitingForParse ->
            ( { model
                | stage = StageTextEntry
                , editor = Editor.enable model.editor
              }
            , Cmd.none
            )

        StageConfigMadlib ->
            ( { model
                | stage = StageTextEntry
                , editor = Editor.enable model.editor
              }
            , Cmd.none
            )


canStageBack : Model -> Bool
canStageBack model =
    case model.stage of
        StageTextEntry ->
            False

        StageWaitingForParse ->
            True

        StageConfigMadlib ->
            True


updateByEditorEvent : Editor.Event -> Model -> ( Model, Cmd Msg )
updateByEditorEvent event model =
    case event of
        Editor.EventMake ->
            ( { model | editor = (Editor.disable model.editor) }, Cmd.none )

        Editor.EventNone ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ Attr.id "app" ]
        (List.append (cardViews model)
            [ Html.div []
                [ Html.text (toString model.stage)
                ]
            , stepForwardView model
            , stepBackView model
            ]
        )


cardViews : Model -> List (Html Msg)
cardViews model =
    [ (textEntryCardView model)
    , (waitingForParseCardView model)
    ]


textEntryCardView : Model -> Html Msg
textEntryCardView model =
    Html.div [ cardClassList model.stage StageTextEntry ]
        [ App.map Editor (Editor.view model.editor) ]


waitingForParseCardView : Model -> Html Msg
waitingForParseCardView model =
    let
        message =
            if model.lastParseId == model.lastGoodParseId then
                "Parsing complete!"
            else
                "Parsing your Madlib..."
    in
        Html.div [ cardClassList model.stage StageWaitingForParse ]
            [ App.map WorkingCard
                (WorkingCard.view "waiting-for-parse" message)
            ]


cardClassList : Stage -> Stage -> Html.Attribute Msg
cardClassList currentStage cardStage =
    let
        current =
            stageOrder currentStage

        card =
            stageOrder cardStage
    in
        Attr.classList
            [ ( "card", True )
            , ( "discarded", card < current )
            , ( "pending", card > current )
            ]


stepForwardView : Model -> Html Msg
stepForwardView model =
    Html.div
        [ Attr.id "step-forward"
        , Attr.classList
            [ ( "stepper", True )
            , ( "enabled", (canStageForward model) )
            ]
        , onClick StageForward
        ]
        [ App.map Icon (Icon.view "chevron-right") ]


stepBackView : Model -> Html Msg
stepBackView model =
    Html.div
        [ Attr.id "step-back"
        , Attr.classList
            [ ( "stepper", True )
            , ( "enabled", (canStageBack model) )
            ]
        , onClick StageBack
        ]
        [ App.map Icon (Icon.view "chevron-left") ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
