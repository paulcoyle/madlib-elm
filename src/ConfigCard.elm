module ConfigCard exposing (..)

import List
import Html exposing (Html)
import Html.Attributes as Attr
import Parse exposing (Parse)


type Msg
    = NoOp


type FragKind
    = Noun
    | Verb
    | Adj


type Fragment
    = Plain String
    | Configurable FragKind String
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
                    List.indexedMap (,)
                        <| List.map parseDataToFragment data

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
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        x =
            Debug.log "zz" model.fragments
    in
        Html.div
            [ Attr.id "configure-card"
            , Attr.class "card-contents"
            ]
            [ Html.div [ Attr.class "fragments" ]
                (List.map fragmentView
                    <| List.map snd model.fragments
                )
            ]


fragmentView : Fragment -> Html Msg
fragmentView fragment =
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

        ( content, className ) =
            case fragment of
                Plain value ->
                    ( value, "frag frag-plain" )

                Configurable kind value ->
                    ( value, fragmentClass kind )

                Configured kind value _ ->
                    ( value, fragmentClass kind )
    in
        Html.span [ Attr.class className ] [ Html.text content ]


configure : Int -> Int -> FragmentList -> FragmentList
configure configValue index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            case frag of
                Plain _ ->
                    frag

                Configurable kind value ->
                    Configured kind value configValue

                Configured kind value _ ->
                    Configured kind value configValue
    in
        modifyIndexed index setMap list


unconfigure : Int -> FragmentList -> FragmentList
unconfigure index list =
    let
        setMap : ( Int, Fragment ) -> Fragment
        setMap ( i, frag ) =
            case frag of
                Plain _ ->
                    frag

                Configurable _ _ ->
                    frag

                Configured kind value _ ->
                    Configurable kind value
    in
        modifyIndexed index setMap list


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
