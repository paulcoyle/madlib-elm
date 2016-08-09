module Fragments exposing (..)

import String


type FragKind
    = Noun
    | Verb
    | Adj


type Fragment
    = Plain String
    | Configurable FragKind String
    | Configuring FragKind String Int
    | Configured FragKind String Int


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


configure : Int -> Fragment -> Fragment
configure levNumber frag =
    case frag of
        Plain _ ->
            frag

        Configurable kind value ->
            Configured kind value levNumber

        Configuring kind value _ ->
            Configuring kind value levNumber

        Configured kind value _ ->
            Configured kind value levNumber


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


numConfigured : List Fragment -> Int
numConfigured frags =
    let
        mapper frag =
            case frag of
                Configured _ _ _ ->
                    1

                _ ->
                    0
    in
        List.sum <| List.map mapper frags


valueLength : Fragment -> Int
valueLength frag =
    let
        value =
            case frag of
                Plain val ->
                    val

                Configurable _ val ->
                    val

                Configuring _ val _ ->
                    val

                Configured _ val _ ->
                    val
    in
        String.length value
