module Lev exposing (distance)

import Array exposing (Array)
import String


distance : String -> String -> Int
distance left right =
    case ( String.length left, String.length right ) of
        ( 0, 0 ) ->
            0

        ( 0, r ) ->
            r

        ( l, 0 ) ->
            l

        ( l, r ) ->
            calculateDistanced l r left right


type alias OuterAccum =
    ( Int, Array Int, Array Int )


type alias InnerAccum =
    ( Char, Int, Int, Array Int, Array Int )


calculateDistanced : Int -> Int -> String -> String -> Int
calculateDistanced leftLen rightLen left right =
    let
        v0 =
            Array.initialize (rightLen + 1) identity

        outerFold : Char -> OuterAccum -> OuterAccum
        outerFold char ( i, v0, v1 ) =
            let
                w1 =
                    Array.set 0 (i + 1) v0

                ( _, _, _, _, z1 ) =
                    String.foldl innerFold ( char, i, 0, v0, w1 ) right
            in
                ( (i + 1), z1, z1 )

        innerFold : Char -> InnerAccum -> InnerAccum
        innerFold char ( rChar, i, j, v0, v1 ) =
            let
                z1 =
                    Array.set (j + 1) (cost char ( rChar, i, j, v0, v1 )) v1
            in
                ( rChar, i, j + 1, v0, z1 )

        cost : Char -> InnerAccum -> Int
        cost lChar ( rChar, i, j, v0, v1 ) =
            let
                getWithDefault : Int -> Array Int -> Int
                getWithDefault index arr =
                    Maybe.withDefault -1 (Array.get index arr)

                adjust =
                    if lChar == rChar then
                        0
                    else
                        1

                min =
                    List.minimum
                        [ (getWithDefault j v1) + 1
                        , (getWithDefault (j + 1) v0) + 1
                        , (getWithDefault j v0) + adjust
                        ]
            in
                case min of
                    Just m ->
                        m

                    Nothing ->
                        -1

        ( _, _, z1 ) =
            String.foldl outerFold ( 0, v0, v0 ) left
    in
        case (Array.get rightLen z1) of
            Just distance ->
                distance

            Nothing ->
                -1
