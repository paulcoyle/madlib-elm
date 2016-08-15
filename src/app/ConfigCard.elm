module ConfigCard exposing (..)

import List
import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events as Events
import Parse exposing (Parse)
import Icon
import Fragments
    exposing
        ( Fragment(..)
        , FragKind(..)
        , startConfiguring
        , configure
        , stopConfiguring
        , unconfigure
        , valueLength
        )


type Msg
    = StartConfiguring Int
    | SetConfiguration Int Int
    | StopConfiguring Int
    | Unconfigure Int
    | NoOp Icon.Msg


type ExternMsg
    = PositionFrag String


type alias FragmentList =
    List ( Int, Fragment )


type alias Model =
    { parseData : Maybe (List Parse)
    , fragments : FragmentList
    , activeConfig : Maybe Int
    }


init : Maybe (List Parse) -> ( Model, Cmd Msg, Maybe ExternMsg )
init parseData =
    let
        parseDataToFragment : Parse -> Fragment
        parseDataToFragment parse =
            case parse.kind of
                "NOUN" ->
                    Configurable Noun parse.value

                "VERB" ->
                    Configurable Verb parse.value

                "ADJ" ->
                    Configurable Adj parse.value

                _ ->
                    Plain parse.value

        parseDataToFragments : Maybe (List Parse) -> FragmentList
        parseDataToFragments parseData =
            case parseData of
                Just data ->
                    List.indexedMap (,) <|
                        List.map parseDataToFragment data

                Nothing ->
                    []
    in
        ( { parseData = parseData
          , fragments = parseDataToFragments parseData
          , activeConfig = Nothing
          }
        , Cmd.none
        , Nothing
        )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe ExternMsg )
update msg model =
    case msg of
        StartConfiguring index ->
            let
                deconfigured =
                    List.map (\( i, f ) -> ( i, stopConfiguring f )) model.fragments

                configured =
                    startConfiguringIndexed index deconfigured

                fragment =
                    getFragByIndex index configured

                activeConfig =
                    case fragment of
                        Just ( i, _ ) ->
                            Just i

                        Nothing ->
                            Nothing
            in
                ( { model
                    | fragments = configured
                    , activeConfig = activeConfig
                  }
                , Cmd.none
                , Just (PositionFrag ("frag-" ++ (toString index)))
                )

        SetConfiguration index num ->
            let
                fragment =
                    getFragByIndex index model.fragments

                num' =
                    case fragment of
                        Just ( _, f ) ->
                            max num (valueLength f)

                        Nothing ->
                            0
            in
                ( { model
                    | fragments = configureIndexed num' index model.fragments
                  }
                , Cmd.none
                , Nothing
                )

        StopConfiguring index ->
            ( { model
                | fragments = stopConfiguringIndexed index model.fragments
                , activeConfig = Nothing
              }
            , Cmd.none
            , Nothing
            )

        Unconfigure index ->
            ( { model
                | fragments = unconfigureIndexed index model.fragments
                , activeConfig = Nothing
              }
            , Cmd.none
            , Nothing
            )

        NoOp _ ->
            ( model, Cmd.none, Nothing )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.id "configure-card"
        , Attr.class "card-contents"
        ]
        [ Html.div
            [ Attr.class "card-instructions" ]
            [ Html.text "Configure at least one of the words below, then continue forward to generate." ]
        , Html.div
            [ Attr.class "card-body" ]
            [ Html.div [ Attr.class "fragments big-text" ]
                (List.map fragmentView model.fragments)
            , fragControlsView model
            ]
        ]


numConfigured : Model -> Int
numConfigured model =
    Fragments.numConfigured <|
        List.map (\( _, frag ) -> frag) model.fragments


fragmentView : ( Int, Fragment ) -> Html Msg
fragmentView ( index, fragment ) =
    let
        fragmentClass : FragKind -> String
        fragmentClass kind =
            case kind of
                Noun ->
                    "frag frag-noun"

                Verb ->
                    "frag frag-verb"

                Adj ->
                    "frag frag-adj"

        id =
            "frag-" ++ (toString index)
    in
        case fragment of
            Plain value ->
                Html.span
                    [ Attr.id id
                    , Attr.class "frag frag-plain"
                    ]
                    [ Html.text value ]

            Configurable kind value ->
                Html.span
                    [ Attr.id id
                    , Attr.class (fragmentClass kind)
                    , Events.onClick (StartConfiguring index)
                    ]
                    [ Html.text value
                    , Html.span
                        [ Attr.class "frag-kind-label" ]
                        [ Html.text (Fragments.kindToString kind) ]
                    ]

            Configuring kind value num ->
                Html.span
                    [ Attr.id id
                    , Attr.class ((fragmentClass kind) ++ " frag-controlled")
                    ]
                    [ Html.text value
                    , Html.span
                        [ Attr.class "frag-kind-label" ]
                        [ Html.text (Fragments.kindToString kind) ]
                    ]

            Configured kind value num ->
                Html.span
                    [ Attr.id id
                    , Attr.class ((fragmentClass kind) ++ " frag-configured")
                    , Events.onClick (StartConfiguring index)
                    ]
                    [ Html.text value
                    , Html.span
                        [ Attr.class "frag-kind-label" ]
                        [ Html.text (Fragments.kindToString kind) ]
                    ]


fragControlsView : Model -> Html Msg
fragControlsView model =
    let
        frag =
            case model.activeConfig of
                Just i ->
                    getFragByIndex i model.fragments

                Nothing ->
                    Nothing
    in
        case frag of
            Just ( index, Configuring kind value num ) ->
                Html.div
                    [ Attr.id "frag-controls"
                    , Attr.class "active"
                    ]
                    [ Html.p
                        [ Attr.class "frag-config-label" ]
                        [ Html.text "Levenshtein Distance" ]
                    , Html.div
                        [ Attr.class "frag-config-stepper" ]
                        [ Html.div
                            [ Attr.class "decrement"
                            , Events.onClick (SetConfiguration index (num - 1))
                            ]
                            [ App.map NoOp (Icon.view "minus") ]
                        , Html.div
                            [ Attr.class "value" ]
                            [ Html.text (toString num) ]
                        , Html.div
                            [ Attr.class "increment"
                            , Events.onClick (SetConfiguration index (num + 1))
                            ]
                            [ App.map NoOp (Icon.view "plus") ]
                        ]
                    , Html.div
                        [ Attr.class "frag-config-cancel"
                        , Events.onClick (Unconfigure index)
                        ]
                        [ Html.text "Don't Use" ]
                    , Html.div
                        [ Attr.class "frag-config-set"
                        , Events.onClick (StopConfiguring index)
                        ]
                        [ Html.text "Done" ]
                    ]

            _ ->
                Html.div
                    [ Attr.id "frag-controls" ]
                    []


startConfiguringIndexed : Int -> FragmentList -> FragmentList
startConfiguringIndexed index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            startConfiguring frag
    in
        modifyIndexed index setMap list


configureIndexed : Int -> Int -> FragmentList -> FragmentList
configureIndexed configValue index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            configure configValue frag
    in
        modifyIndexed index setMap list


stopConfiguringIndexed : Int -> FragmentList -> FragmentList
stopConfiguringIndexed index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            stopConfiguring frag
    in
        modifyIndexed index setMap list


unconfigureIndexed : Int -> FragmentList -> FragmentList
unconfigureIndexed index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            unconfigure frag
    in
        modifyIndexed index setMap list


getFragByIndex : Int -> FragmentList -> Maybe ( Int, Fragment )
getFragByIndex index list =
    List.head <|
        List.filter (\( i, f ) -> i == index) <|
            list


modifyIndexed : Int -> (( Int, a ) -> a) -> List ( Int, a ) -> List ( Int, a )
modifyIndexed index modify list =
    let
        -- https://github.com/elm-lang/elm-compiler/blob/0.17.0/hints/type-annotations.md#annotation-vs-internal-annotation
        mapper ( i, value ) =
            if i == index then
                ( i, modify ( i, value ) )
            else
                ( i, value )
    in
        List.map mapper list
