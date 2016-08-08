module ConfigCard exposing (..)

import List
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Parse exposing (Parse)
import String


type Msg
    = StartConfiguring Int
    | SetConfiguration Int Int
    | StopConfiguring Int
    | Unconfigure Int


type FragKind
    = Noun
    | Verb
    | Adj


type Fragment
    = Plain String
    | Configurable FragKind String
    | Configuring FragKind String Int
    | Configured FragKind String Int


type alias FragmentList =
    List ( Int, Fragment )


type alias Model =
    { parseData : Maybe (List Parse)
    , fragments : FragmentList
    }


init : Maybe (List Parse) -> ( Model, Cmd Msg )
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
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartConfiguring index ->
            ( { model
                | fragments = startConfiguringIndexed index model.fragments
              }
            , Cmd.none
            )

        SetConfiguration index num ->
            ( { model
                | fragments = configureIndexed index num model.fragments
              }
            , Cmd.none
            )

        StopConfiguring index ->
            ( { model
                | fragments = stopConfiguringIndexed index model.fragments
              }
            , Cmd.none
            )

        Unconfigure index ->
            ( { model
                | fragments = unconfigureIndexed index model.fragments
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.id "configure-card"
        , Attr.class "card-contents"
        ]
        [ Html.div
            [ Attr.class "card-instructions"
            ]
            [ Html.text "Configure at least one of the words below, then continue forward to generate."
            ]
        , Html.div
            [ Attr.class "card-body"
            ]
            [ Html.div [ Attr.class "fragments big-text" ]
                (List.map fragmentView model.fragments)
            ]
        ]


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
    in
        case fragment of
            Plain value ->
                Html.span
                    [ Attr.class "frag frag-plain" ]
                    [ Html.text value ]

            Configurable kind value ->
                Html.span
                    [ Attr.class (fragmentClass kind)
                    , Events.onClick (StartConfiguring index)
                    ]
                    [ Html.text value ]

            Configuring kind value num ->
                Html.span
                    [ Attr.class ((fragmentClass kind) ++ " frag-controlled") ]
                    [ Html.div
                        [ Attr.class "frag-controls" ]
                        [ Html.span
                            [ Attr.class "frag-lev-num" ]
                            [ Html.text (toString num) ]
                        , Html.span
                            [ Attr.class "frag-config-cancel"
                            , Events.onClick (Unconfigure index)
                            ]
                            [ Html.text "Don't Use" ]
                        , Html.span
                            [ Attr.class "frag-config-set"
                            , Events.onClick (StopConfiguring index)
                            ]
                            [ Html.text "Done" ]
                        ]
                    , Html.div
                        [ Attr.class "frag-word" ]
                        [ Html.text value ]
                    ]

            Configured kind value num ->
                Html.span
                    [ Attr.class (fragmentClass kind) ]
                    [ Html.span
                        [ Attr.class "frag-reconfigure"
                        , Events.onClick (StartConfiguring index)
                        ]
                        [ Html.text "Change" ]
                    , Html.span
                        [ Attr.class "frag-lev-num" ]
                        [ Html.text (toString num) ]
                    , Html.text value
                    ]


startConfiguringIndexed : Int -> FragmentList -> FragmentList
startConfiguringIndexed index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            startConfiguring frag
    in
        modifyIndexed index setMap list


startConfiguring : Fragment -> Fragment
startConfiguring frag =
    case frag of
        Plain _ ->
            frag

        Configurable kind value ->
            Configuring kind value (String.length value)

        Configuring kind value num ->
            Configuring kind value num

        Configured kind value num ->
            Configuring kind value num


configureIndexed : Int -> Int -> FragmentList -> FragmentList
configureIndexed configValue index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            configure configValue frag
    in
        modifyIndexed index setMap list


configure : Int -> Fragment -> Fragment
configure configValue frag =
    case frag of
        Plain _ ->
            frag

        Configurable kind value ->
            Configured kind value configValue

        Configuring kind value _ ->
            Configuring kind value configValue

        Configured kind value _ ->
            Configured kind value configValue


stopConfiguringIndexed : Int -> FragmentList -> FragmentList
stopConfiguringIndexed index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            stopConfiguring frag
    in
        modifyIndexed index setMap list


stopConfiguring : Fragment -> Fragment
stopConfiguring frag =
    case frag of
        Plain _ ->
            frag

        Configurable _ _ ->
            frag

        Configuring kind value num ->
            Configured kind value num

        Configured _ _ _ ->
            frag


unconfigureIndexed : Int -> FragmentList -> FragmentList
unconfigureIndexed index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            unconfigure frag
    in
        modifyIndexed index setMap list


unconfigure : Fragment -> Fragment
unconfigure frag =
    case frag of
        Plain _ ->
            frag

        Configurable _ _ ->
            frag

        Configuring kind value _ ->
            Configurable kind value

        Configured kind value _ ->
            Configurable kind value


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
