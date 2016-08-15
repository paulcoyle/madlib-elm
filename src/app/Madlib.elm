module Madlib exposing (..)

import Random
import String
import Lev
import Fragments
    exposing
        ( Fragment(..)
        , FragKind(..)
        )


type alias FragmentList =
    List ( Int, Fragment )


type alias CorpusByLength =
    List ( Int, List String )


type alias CorpusSet =
    ( CorpusByLength, CorpusByLength, CorpusByLength )


type alias FragFold =
    ( Random.Seed, List String )


generate : Random.Seed -> CorpusSet -> FragmentList -> ( Random.Seed, String )
generate seed corpii fragmentList =
    let
        fragments =
            List.map (\( _, frag ) -> frag) fragmentList

        accumulator =
            ( seed, [] )

        folder =
            foldFragment corpii

        ( seed', words ) =
            List.foldl folder accumulator fragments
    in
        ( seed', glueWords words )


foldFragment : CorpusSet -> Fragment -> FragFold -> FragFold
foldFragment corpii frag ( seed, words ) =
    case frag of
        Configured kind word length ->
            let
                corpus =
                    corpusForFragKind corpii kind

                relevantCorpus =
                    List.filter (\( len, _ ) -> len >= length) corpus

                mergedCorpus =
                    List.foldl (\( _, words ) acc -> List.append acc words) [] relevantCorpus

                ( seed', word' ) =
                    randomLevWord seed mergedCorpus length word
            in
                ( seed', List.append words [ word' ] )

        Plain word ->
            ( seed, List.append words [ word ] )

        Configurable _ word ->
            ( seed, List.append words [ word ] )

        Configuring _ word _ ->
            ( seed, List.append words [ word ] )


corpusForFragKind : CorpusSet -> FragKind -> CorpusByLength
corpusForFragKind ( adj, noun, verb ) kind =
    case kind of
        Adj ->
            adj

        Noun ->
            noun

        Verb ->
            verb


randomLevWord : Random.Seed -> List String -> Int -> String -> ( Random.Seed, String )
randomLevWord seed corpus targetDistance word =
    let
        ( seed', randomWord ) =
            randomFromList seed corpus

        unsafeWord =
            Maybe.withDefault word randomWord

        distance =
            Lev.distance word unsafeWord
    in
        if distance >= targetDistance then
            let
                x =
                    Debug.log "distance" (String.join " " [ word, "->", unsafeWord, " = ", toString distance ])
            in
                ( seed', unsafeWord )
        else
            randomLevWord seed' corpus targetDistance word


randomFromList : Random.Seed -> List a -> ( Random.Seed, Maybe a )
randomFromList seed list =
    let
        indexGen =
            Random.int 0 ((List.length list) - 1)

        ( index, seed' ) =
            Random.step indexGen seed

        item =
            List.head <| List.drop index list
    in
        ( seed', item )


glueWords : List String -> String
glueWords fragments =
    String.join " " fragments
