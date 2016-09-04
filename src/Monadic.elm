module Monadic exposing (..)

import Internal exposing (..)
import Svg.Attributes
import String


type Diagram appendable
    = Diagram (DrawState -> ( appendable, DrawState ))


fromFunction : (DrawState -> ( appendable, DrawState )) -> Diagram appendable
fromFunction =
    Diagram


finalValue : DrawState -> Diagram appendable -> appendable
finalValue initial (Diagram f) =
    fst (f initial)


concatenate : Diagram appendable -> Diagram appendable -> Diagram appendable
concatenate (Diagram dia1) (Diagram dia2) =
    Diagram
        <| \state ->
            let
                ( first, newState ) =
                    dia1 state

                ( secon, newerState ) =
                    dia2 newState
            in
                ( first ++ secon, newerState )


asAttribute =
    Svg.Attributes.d << fst


moveAbs : Point -> Diagram (List Segment) -> Diagram (List Segment)
moveAbs ( x, y ) =
    concatenate
        <| fromFunction
            (\state ->
                ( [ MoveTo Absolute [ ( x, y ) ] ]
                , { state | current = ( x, y ) }
                )
            )


segmentsToString : Diagram (List Segment) -> Diagram String
segmentsToString (Diagram f) =
    Diagram
        (\state ->
            let
                ( value, newState ) =
                    f state
            in
                ( List.map segmentToString value |> List.reverse |> String.concat, newState )
        )


fromSegmentAndUpdate : ( Segment, DrawState -> DrawState ) -> Diagram (List Segment)
fromSegmentAndUpdate ( segment, modify ) =
    Diagram
        (\state ->
            ( [ segment ]
            , modify state
            )
        )


modifyCurrent : (( Float, Float ) -> ( Float, Float )) -> DrawState -> DrawState
modifyCurrent f state =
    { state | current = f state.current }


moveRel : Point -> Diagram (List Segment) -> Diagram (List Segment)
moveRel ( x, y ) =
    fromSegmentAndUpdate
        ( MoveTo Relative [ ( x, y ) ]
        , modifyCurrent (addTuple ( x, y ))
        )
        |> concatenate


verticalAbs : Float -> Diagram (List Segment) -> Diagram (List Segment)
verticalAbs y =
    fromSegmentAndUpdate
        ( Vertical Absolute y
        , modifyCurrent (\( h, v ) -> ( h, y ))
        )
        |> concatenate


verticalRel : Float -> Diagram (List Segment) -> Diagram (List Segment)
verticalRel dy =
    fromSegmentAndUpdate
        ( Vertical Relative dy
        , modifyCurrent (\( h, v ) -> ( h, v + dy ))
        )
        |> concatenate


horizontalAbs : Float -> Diagram (List Segment) -> Diagram (List Segment)
horizontalAbs x =
    fromSegmentAndUpdate
        ( Horizontal Absolute x
        , modifyCurrent (\( h, v ) -> ( x, v ))
        )
        |> concatenate


horizontalRel : Float -> Diagram (List Segment) -> Diagram (List Segment)
horizontalRel dx =
    fromSegmentAndUpdate
        ( Horizontal Relative dx
        , modifyCurrent (\( h, v ) -> ( h + dx, v ))
        )
        |> concatenate


close : Diagram (List Segment) -> Diagram (List Segment)
close =
    fromFunction (\state -> ( [], state ))
        |> concatenate


type alias DrawState =
    { initial : Point
    , current : Point
    , empty : Bool
    }


emptyDrawState =
    { initial = ( 0, 0 ), current = ( 0, 0 ), empty = False }


emptyDiagram =
    ( "", emptyDrawState )


{-| andThen ( very much like state but joins the values with (++) instead of only keeping the most recent one)
-}
andThen : (DrawState -> ( appendable, DrawState )) -> ( appendable, DrawState ) -> ( appendable, DrawState )
andThen f ( accum, state ) =
    let
        ( new, newState ) =
            f state
    in
        -- should optimize here, such that two adjacent M's are joined
        ( accum ++ new, newState )


segmentToString : Segment -> String
segmentToString segment =
    Internal.formatSegment segment


mapFst f ( a, b ) =
    ( f a, b )


addFst v =
    mapFst ((+) v)


addSnd v =
    mapSnd ((+) v)


mapSnd f ( a, b ) =
    ( a, f b )


mapBoth f ( a, b ) =
    ( f a, f b )


addTuple ( a, b ) ( c, d ) =
    ( a + c, b + d )
