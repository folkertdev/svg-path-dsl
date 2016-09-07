module Svg.Path.Optimize exposing (optimize)

{-| Helpers for optimizing SVG paths.

Currently unpubished, but should at some point be able
to compress svg paths.

Two consequtive moves can be joined into one

    M 20,20 M40,40 == M20,20 40,40

#Optimize
@docs optimize
-}

import Svg.Path.Internal as Internal exposing (..)
import List.Extra as List


join2 : Segment -> Segment -> List Segment
join2 seg1 seg2 =
    case ( seg1, seg2 ) of
        ( MoveAbsolute p1, MoveAbsolute p2 ) ->
            [ MoveAbsoluteMany [ p1, p2 ] ]

        _ ->
            [ seg1, seg2 ]


{-|
-}
optimize : List Segment -> List Segment
optimize =
    let
        unfolder segments =
            case segments of
                [] ->
                    Nothing

                [ x ] ->
                    Just ( Just x, [] )

                x :: y :: rest ->
                    case ( x, y ) of
                        ( MoveAbsolute p1, MoveAbsolute p2 ) ->
                            Just ( Nothing, MoveAbsoluteMany [ p1, p2 ] :: rest )

                        ( MoveAbsoluteMany ps, MoveAbsolute p ) ->
                            Just ( Nothing, MoveAbsoluteMany (ps ++ [ p ]) :: rest )

                        _ ->
                            Just ( Just x, y :: rest )

        folder elem accum =
            case elem of
                Nothing ->
                    accum

                Just v ->
                    v :: accum
    in
        -- fuse with hylo
        List.unfoldr unfolder >> List.foldr folder []



-- refold folder unfolder []


refold : (b -> c -> c) -> (a -> Maybe ( b, a )) -> c -> a -> c
refold reducer expander base seed =
    let
        -- go : c -> a -> c
        go current seed =
            case expander seed of
                Nothing ->
                    current

                Just ( next, newSeed ) ->
                    go (reducer next current) newSeed
    in
        go base seed
