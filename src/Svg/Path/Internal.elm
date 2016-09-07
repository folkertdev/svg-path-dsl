module Svg.Path.Internal exposing (..)

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


{-| gives a string representation of a float with at most `i` decimal places.
-}
fixed : Int -> Float -> String
fixed i n =
    let
        pow =
            10 ^ i

        nInt =
            round (n * pow)
    in
        toString (toFloat (nInt) / pow)


roundToAtMost : Maybe Int -> Float -> String
roundToAtMost doRound =
    case doRound of
        Nothing ->
            toString

        Just n ->
            fixed n


formatPoint : Maybe Int -> Point -> String
formatPoint dp ( x, y ) =
    roundToAtMost dp x ++ "," ++ roundToAtMost dp y


letterAndFloat : Maybe Int -> String -> Float -> String
letterAndFloat dp letter num =
    letter ++ roundToAtMost dp num


letterAndPoint : Maybe Int -> String -> Point -> String
letterAndPoint dp letter point =
    letter ++ formatPoint dp point


letterAndPoints : Maybe Int -> String -> List Point -> String
letterAndPoints dp letter points =
    letter ++ String.join " " (List.map (formatPoint dp) points)


letterAndPoint2 : Maybe Int -> String -> Point -> Point -> String
letterAndPoint2 dp letter p1 p2 =
    letter ++ formatPoint dp p1 ++ " " ++ formatPoint dp p2


letterAndPoint3 : Maybe Int -> String -> Point -> Point -> Point -> String
letterAndPoint3 dp letter p1 p2 p3 =
    letter ++ formatPoint dp p1 ++ " " ++ formatPoint dp p2 ++ " " ++ formatPoint dp p3


formatCurveContinuation : Maybe Int -> CurveContinuation -> String
formatCurveContinuation dp continuation =
    case continuation of
        QuadAbsolute goal ->
            letterAndPoint dp "T" goal

        QuadRelative goal ->
            letterAndPoint dp "t" goal

        CubiAbsolute control goal ->
            letterAndPoint2 dp "S" control goal

        CubiRelative control goal ->
            letterAndPoint2 dp "s" control goal


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
    | ArcBy Point Float ( ArcFlag, Direction ) Point


concatMapString : (a -> String) -> List a -> String
concatMapString f =
    List.foldl (\e accum -> accum ++ (f e)) ""


formatSegment : Maybe Int -> Segment -> String
formatSegment dp segment =
    case segment of
        ClosePath ->
            "Z"

        MoveAbsolute point ->
            letterAndPoint dp "M" point

        MoveAbsoluteMany points ->
            letterAndPoints dp "M" points

        MoveRelative point ->
            letterAndPoint dp "m" point

        MoveRelativeMany points ->
            letterAndPoints dp "m" points

        LineAbsolute point ->
            letterAndPoint dp "L" point

        LineAbsoluteMany points ->
            letterAndPoints dp "L" points

        LineRelative point ->
            letterAndPoint dp "l" point

        LineRelativeMany points ->
            letterAndPoints dp "l" points

        VerticalAbsolute y ->
            letterAndFloat dp "V" y

        VerticalRelative dy ->
            letterAndFloat dp "v" dy

        HorizontalAbsolute x ->
            letterAndFloat dp "H" x

        HorizontalRelative dx ->
            letterAndFloat dp "h" dx

        QuadraticAbsolute control goal ->
            letterAndPoint2 dp "Q" control goal

        QuadraticRelative control goal ->
            letterAndPoint2 dp "q" control goal

        CubicAbsolute c1 c2 point ->
            -- note that cubic == quadratic iff c1 == c2. Maybe optimize that?
            letterAndPoint3 dp "C" c1 c2 point

        CubicRelative dc1 dc2 dpoint ->
            letterAndPoint3 dp "c" dc1 dc2 dpoint

        QuadraticAbsoluteMany control goal continuations ->
            formatSegment dp (QuadraticAbsolute control goal) ++ concatMapString (formatCurveContinuation dp) continuations

        QuadraticRelativeMany dcontrol dgoal continuations ->
            formatSegment dp (QuadraticRelative dcontrol dgoal) ++ concatMapString (formatCurveContinuation dp) continuations

        CubicAbsoluteMany c1 c2 goal continuations ->
            formatSegment dp (CubicAbsolute c1 c2 goal) ++ concatMapString (formatCurveContinuation dp) continuations

        CubicRelativeMany dc1 dc2 dgoal continuations ->
            formatSegment dp (CubicRelative dc1 dc2 dgoal) ++ concatMapString (formatCurveContinuation dp) continuations

        ArcTo ( rx, ry ) xAxisRotate ( arcFlag, sweepFlag ) ( x, y ) ->
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
                "A" ++ formatPoint dp ( rx, ry ) ++ " " ++ roundToAtMost dp xAxisRotate ++ " " ++ arc ++ "," ++ sweep ++ " " ++ formatPoint dp ( x, y )

        ArcBy ( rx, ry ) xAxisRotate ( arcFlag, sweepFlag ) ( x, y ) ->
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
                "a" ++ formatPoint dp ( rx, ry ) ++ " " ++ roundToAtMost dp xAxisRotate ++ " " ++ arc ++ "," ++ sweep ++ " " ++ formatPoint dp ( x, y )


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
