port module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Editor
import Parse exposing (Parse)
import Fragments
import WorkingCard
import ConfigCard
import GeneratorCard
import String
import Icon


type Msg
    = StageForward
    | StageBack
    | ReceiveParse ParseResponse
    | ReceiveCorpus Corpus
    | ReceiveSeed Int
    | Editor Editor.Msg
      -- These Icon messages are ignored.
    | Icon Icon.Msg
    | WorkingCard WorkingCard.Msg
    | ConfigCard ConfigCard.Msg
    | GeneratorCard GeneratorCard.Msg


type Stage
    = StageTextEntry
    | StageWaitingForParse
    | StageConfigure
    | StageGenerate


type alias Model =
    { stage : Stage
    , editor : Editor.Model
    , lastParseId : Int
    , lastGoodParseId : Int
    , lastParse : Maybe (List Parse)
    , configureCard : ConfigCard.Model
    , generatorCard : GeneratorCard.Model
    }


type alias ParseResponse =
    ( Int, Bool, List Parse )


type alias Corpus =
    ( String, List ( Int, List String ) )


init : ( Model, Cmd Msg )
init =
    let
        ( editor, editorCmd ) =
            Editor.init

        ( configureCard, configureCmd, _ ) =
            ConfigCard.init Nothing

        ( generatorCard, generatorCmd ) =
            GeneratorCard.init Nothing

        commands =
            Cmd.batch
                [ Cmd.map Editor editorCmd
                , Cmd.map ConfigCard configureCmd
                , Cmd.map GeneratorCard generatorCmd
                ]
    in
        ( { stage = StageTextEntry
          , editor = editor
          , lastParseId = 0
          , lastGoodParseId = -1
          , lastParse = Nothing
          , configureCard = configureCard
          , generatorCard = generatorCard
          }
        , commands
        )


port parse : ( Int, String ) -> Cmd msg


port positionControls : String -> Cmd msg


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

                ( newModel, commands ) =
                    updateByEditorEvent editorEvent { model | editor = editor }
            in
                ( newModel
                , Cmd.batch
                    [ Cmd.map Editor editorCmd
                    , commands
                    ]
                )

        ReceiveParse ( id, success, parseData ) ->
            let
                lastGood =
                    if success then
                        id
                    else
                        model.lastGoodParseId

                lastParse =
                    Just parseData

                ( configureCard, configureCmd, _ ) =
                    ConfigCard.init lastParse
            in
                ( { model
                    | stage = StageConfigure
                    , lastGoodParseId = lastGood
                    , lastParse = lastParse
                    , configureCard = configureCard
                  }
                , Cmd.map ConfigCard configureCmd
                )

        ReceiveCorpus ( kind, words ) ->
            let
                generatorCard =
                    case (Fragments.kindFromString kind) of
                        Just k ->
                            GeneratorCard.setCorpus k words model.generatorCard

                        Nothing ->
                            model.generatorCard
            in
                ( { model | generatorCard = generatorCard }, Cmd.none )

        ReceiveSeed seed ->
            let
                x =
                    Debug.log "seed" seed
            in
                ( { model
                    | generatorCard = GeneratorCard.setSeed seed model.generatorCard
                  }
                , Cmd.none
                )

        Icon _ ->
            ( model, Cmd.none )

        WorkingCard _ ->
            ( model, Cmd.none )

        ConfigCard configureMsg ->
            let
                ( configureCard, configureCmd, configureExtern ) =
                    ConfigCard.update configureMsg model.configureCard

                ( model', cmds' ) =
                    case configureExtern of
                        Nothing ->
                            ( model, Cmd.none )

                        Just (ConfigCard.PositionFrag id) ->
                            ( model, positionControls id )
            in
                ( { model' | configureCard = configureCard }
                , Cmd.batch
                    [ Cmd.map ConfigCard configureCmd
                    , cmds'
                    ]
                )

        GeneratorCard generatorMsg ->
            let
                ( generatorCard, generatorCmd ) =
                    GeneratorCard.update generatorMsg model.generatorCard
            in
                ( { model | generatorCard = generatorCard }
                , Cmd.map GeneratorCard generatorCmd
                )


port parsed : (ParseResponse -> msg) -> Sub msg


port corpus : (Corpus -> msg) -> Sub msg


port seed : (Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ parsed ReceiveParse
        , corpus ReceiveCorpus
        , seed ReceiveSeed
        ]


stageOrder : Stage -> Int
stageOrder stage =
    case stage of
        StageTextEntry ->
            0

        StageWaitingForParse ->
            1

        StageConfigure ->
            2

        StageGenerate ->
            3


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
            ( { model | stage = StageConfigure }, Cmd.none )

        StageConfigure ->
            ( { model
                | stage = StageGenerate
                , generatorCard =
                    GeneratorCard.setFragments
                        model.configureCard.fragments
                        model.generatorCard
              }
            , Cmd.none
            )

        StageGenerate ->
            ( model, Cmd.none )


canStageForward : Model -> Bool
canStageForward model =
    case model.stage of
        StageTextEntry ->
            (String.length model.editor.content) > 0

        StageWaitingForParse ->
            model.lastParseId == model.lastGoodParseId

        StageConfigure ->
            (ConfigCard.numConfigured model.configureCard) > 0

        StageGenerate ->
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

        StageConfigure ->
            ( { model
                | stage = StageTextEntry
                , editor = Editor.enable model.editor
              }
            , Cmd.none
            )

        StageGenerate ->
            ( { model | stage = StageConfigure }, Cmd.none )


canStageBack : Model -> Bool
canStageBack model =
    case model.stage of
        StageTextEntry ->
            False

        StageWaitingForParse ->
            True

        StageConfigure ->
            True

        StageGenerate ->
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
        (List.concat
            [ (stagesView model)
            , (cardViews model)
            , [ stepForwardView model
              , stepBackView model
              ]
            ]
        )


cardViews : Model -> List (Html Msg)
cardViews model =
    [ (textEntryCardView model)
    , (waitingForParseCardView model)
    , (configureCardView model)
    , (generatorCardView model)
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


configureCardView : Model -> Html Msg
configureCardView model =
    Html.div [ cardClassList model.stage StageConfigure ]
        [ App.map ConfigCard
            (ConfigCard.view model.configureCard)
        ]


generatorCardView : Model -> Html Msg
generatorCardView model =
    Html.div [ cardClassList model.stage StageGenerate ]
        [ App.map GeneratorCard
            (GeneratorCard.view model.generatorCard)
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


type alias StagesMapData =
    { passedCurrent : Bool
    , stages : List ( String, String )
    }


stagesView : Model -> List (Html Msg)
stagesView model =
    let
        mapData : StagesMapData
        mapData =
            StagesMapData False []

        orderedStages : List Stage
        orderedStages =
            List.sortBy stageOrder
                [ StageTextEntry, StageWaitingForParse, StageConfigure, StageGenerate ]

        mapper : Stage -> StagesMapData -> StagesMapData
        mapper stage data =
            let
                label =
                    stageAsString stage

                class =
                    if data.passedCurrent then
                        "unreached"
                    else
                        "reached"
            in
                if stage == model.stage then
                    { data
                        | passedCurrent = True
                        , stages = List.append data.stages [ ( "current", label ) ]
                    }
                else
                    { data
                        | stages = List.append data.stages [ ( class, label ) ]
                    }

        displayData =
            List.foldl mapper mapData orderedStages

        displayMarkup stage =
            Html.div
                [ Attr.class ("stage-map-item " ++ (fst stage)) ]
                [ Html.text (snd stage) ]
    in
        [ Html.div
            [ Attr.id "stage-map" ]
            (List.map
                displayMarkup
                displayData.stages
            )
        ]


stageAsString : Stage -> String
stageAsString stage =
    case stage of
        StageTextEntry ->
            "Write"

        StageWaitingForParse ->
            "Parse"

        StageConfigure ->
            "Configure"

        StageGenerate ->
            "Sensible Chuckles"


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
