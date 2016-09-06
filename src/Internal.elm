module Internal exposing (..)

import String
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| A sequence of segments. New elements are added at the front, so the internal
representation is reversed.
-}
type alias Path =
    List Segment


type alias Point =
    ( Float, Float )


type CurveContinuation
    = QuadAbsolute Point
    | QuadRelative Point
    | CubiAbsolute Point Point
    | CubiRelative Point Point


formatCurveContinuation continuation =
    case continuation of
        QuadAbsolute goal ->
            "T" ++ formatPoint goal

        QuadRelative goal ->
            "t" ++ formatPoint goal

        CubiAbsolute control goal ->
            "S" ++ formatPoints [ control, goal ]

        CubiRelative control goal ->
            "s" ++ formatPoints [ control, goal ]


type Segment
    = ClosePath
      -- move (*many versions for optimization)
    | MoveAbsolute Point
    | MoveAbsoluteMany (List Point)
    | MoveRelative Point
    | MoveRelativeMany (List Point)
      -- line
    | LineAbsolute Point
    | LineAbsoluteMany (List Point)
    | LineRelative Point
    | LineRelativeMany (List Point)
      -- one-directional line
    | VerticalAbsolute Float
    | VerticalRelative Float
    | HorizontalAbsolute Float
    | HorizontalRelative Float
      -- curves
    | QuadraticAbsolute Point Point
    | QuadraticRelative Point Point
    | QuadraticAbsoluteMany Point Point (List CurveContinuation)
    | QuadraticRelativeMany Point Point (List CurveContinuation)
    | CubicAbsolute Point Point Point
    | CubicRelative Point Point Point
    | CubicAbsoluteMany Point Point Point (List CurveContinuation)
    | CubicRelativeMany Point Point Point (List CurveContinuation)
      -- arcs
    | ArcTo Point Float ( ArcFlag, Direction ) Point


formatPoints =
    -- List.foldl (\elem accum -> accum ++ formatPoint elem) ""
    -- optimize this again, beware of trailing/preceding spaces
    List.map formatPoint
        >> String.join " "


concatMapString : (a -> String) -> List a -> String
concatMapString f =
    List.foldl (\e accum -> accum ++ (f e)) ""


formatSegment segment =
    case segment of
        ClosePath ->
            "Z"

        MoveAbsolute point ->
            "M" ++ formatPoint point

        MoveAbsoluteMany points ->
            "M" ++ formatPoints points

        MoveRelative point ->
            "m" ++ formatPoint point

        MoveRelativeMany points ->
            "m" ++ formatPoints points

        LineAbsolute point ->
            "L" ++ formatPoint point

        LineAbsoluteMany points ->
            "L" ++ formatPoints points

        LineRelative point ->
            "l" ++ formatPoint point

        LineRelativeMany points ->
            "l" ++ formatPoints points

        VerticalAbsolute y ->
            "V" ++ toString y

        VerticalRelative dy ->
            "v" ++ toString dy

        HorizontalAbsolute x ->
            "H" ++ toString x

        HorizontalRelative dx ->
            "h" ++ toString dx

        QuadraticAbsolute ( cx, cy ) ( x, y ) ->
            "Q" ++ formatPoint ( cx, cy ) ++ " " ++ formatPoint ( x, y )

        QuadraticRelative ( dcx, dcy ) ( dx, dy ) ->
            "q" ++ formatPoint ( dcx, dcy ) ++ " " ++ formatPoint ( dx, dy )

        CubicAbsolute c1 c2 point ->
            -- note that cubic == quadratic iff c1 == c2. Maybe optimize that?
            "C" ++ String.join " " (List.map formatPoint [ c1, c2, point ])

        CubicRelative dc1 dc2 dpoint ->
            "c" ++ String.join " " (List.map formatPoint [ dc1, dc2, dpoint ])

        QuadraticAbsoluteMany control goal continuations ->
            formatSegment (QuadraticAbsolute control goal) ++ concatMapString formatCurveContinuation continuations

        QuadraticRelativeMany dcontrol dgoal continuations ->
            formatSegment (QuadraticRelative dcontrol dgoal) ++ concatMapString formatCurveContinuation continuations

        CubicAbsoluteMany c1 c2 goal continuations ->
            formatSegment (CubicAbsolute c1 c2 goal) ++ concatMapString formatCurveContinuation continuations

        CubicRelativeMany dc1 dc2 dgoal continuations ->
            formatSegment (CubicRelative dc1 dc2 dgoal) ++ concatMapString formatCurveContinuation continuations

        ArcTo radii xAxisRotate ( arcFlag, sweepFlag ) goal ->
            let
                arc : String
                arc =
                    case arcFlag of
                        Smallest ->
                            "0"

                        Largest ->
                            "1"

                sweep : String
                sweep =
                    case sweepFlag of
                        AntiClockwise ->
                            "0"

                        Clockwise ->
                            "1"
            in
                String.join " " [ "A" ++ formatPoint radii, toString xAxisRotate, arc ++ "," ++ sweep, formatPoint goal ]


{-|
smallest (0) or largest (1) arc drawn
-}
type ArcFlag
    = Smallest
    | Largest


{-|
Also called 'sweepflag'. Clockwise (1) or anti-clockwise (0) direction
-}
type Direction
    = AntiClockwise
    | Clockwise


points =
    [ ( 2, 2 ), ( 4, 4 ), ( 5, 5 ), ( 50, 50 ) ]


concat : Segment -> Segment -> List Segment
concat instrA instrB =
    [ instrA, instrB ]


formatPoint : Point -> String
formatPoint ( x, y ) =
    toString x ++ "," ++ toString y
