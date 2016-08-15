module GeneratorCard exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Madlib
import Fragments
    exposing
        ( Fragment(..)
        , FragKind(..)
        )


type Msg
    = Generate


type alias FragmentList =
    List ( Int, Fragment )


type alias CorpusByLength =
    List ( Int, List String )


type alias Model =
    { fragments : Maybe FragmentList
    , adjWords : Maybe CorpusByLength
    , nounWords : Maybe CorpusByLength
    , verbWords : Maybe CorpusByLength
    , seed : Random.Seed
    , madlib : Maybe String
    }


init : Maybe FragmentList -> ( Model, Cmd Msg )
init fragments =
    ( { fragments = fragments
      , adjWords = Nothing
      , nounWords = Nothing
      , verbWords = Nothing
      , seed = Random.initialSeed 0
      , madlib = Nothing
      }
    , Cmd.none
    )


setFragments : FragmentList -> Model -> Model
setFragments fragments model =
    { model | fragments = Just fragments }


setCorpus : FragKind -> CorpusByLength -> Model -> Model
setCorpus fragKind corpus model =
    case fragKind of
        Adj ->
            { model | adjWords = Just corpus }

        Noun ->
            { model | nounWords = Just corpus }

        Verb ->
            { model | verbWords = Just corpus }


setSeed : Int -> Model -> Model
setSeed seed model =
    { model | seed = Random.initialSeed seed }


clearMadlib : Model -> Model
clearMadlib model =
    { model | madlib = Nothing }


canGenerateMadlib : Model -> Bool
canGenerateMadlib model =
    (model.adjWords /= Nothing)
        && (model.nounWords /= Nothing)
        && (model.verbWords /= Nothing)


generateMadlib : Model -> Model
generateMadlib model =
    let
        corpii =
            ( Maybe.withDefault [] model.adjWords
            , Maybe.withDefault [] model.nounWords
            , Maybe.withDefault [] model.verbWords
            )

        fragments =
            case model.fragments of
                Just frags ->
                    frags

                Nothing ->
                    []

        ( seed, madlib ) =
            Madlib.generate model.seed corpii fragments
    in
        { model
            | seed = seed
            , madlib = Just madlib
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( generateMadlib model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.id "generate-card"
        , Attr.class "card-contents"
        ]
        [ Html.div [ Attr.class "card-instructions" ]
            [ controlsView model ]
        , Html.div [ Attr.class "card-body" ]
            [ madlibView model ]
        ]


controlsView : Model -> Html Msg
controlsView model =
    let
        content =
            if (canGenerateMadlib model) then
                Html.div
                    [ Attr.class "generate-button"
                    , Events.onClick Generate
                    ]
                    [ Html.text "Generate a Madlib" ]
            else
                Html.div []
                    [ Html.text "Waiting for corpus data..." ]
    in
        Html.div [ Attr.id "generate-controls" ]
            [ content ]


madlibView : Model -> Html Msg
madlibView model =
    let
        madlib =
            Maybe.withDefault "" model.madlib
    in
        Html.div
            [ Attr.id "generate-madlib"
            , Attr.class "big-text"
            ]
            [ Html.div []
                [ Html.text madlib ]
            ]
