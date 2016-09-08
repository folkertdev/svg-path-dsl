module Svg.Path.Internal exposing (..)

import String
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Point =
    ( Float, Float )


{-| the T, t, S and s commands
-}
type CurveContinuation
    = QuadAbsolute Point
    | QuadRelative Point
    | CubiAbsolute Point Point
    | CubiRelative Point Point


{-| SVG instructions
-}
type Instruction
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


formatInstruction : Maybe Int -> Instruction -> String
formatInstruction dp segment =
    let
        formatPoint =
            formatPointWithPrecision dp

        doRound =
            roundToAtMost dp

        letterAndFloat : String -> Float -> String
        letterAndFloat letter num =
            letter ++ doRound num

        letterAndPoint : String -> Point -> String
        letterAndPoint letter point =
            letter ++ formatPoint point

        letterAndPoints : String -> List Point -> String
        letterAndPoints letter points =
            letter ++ String.join " " (List.map formatPoint points)

        letterAndPoint2 : String -> Point -> Point -> String
        letterAndPoint2 letter p1 p2 =
            letter ++ formatPoint p1 ++ " " ++ formatPoint p2

        letterAndPoint3 : String -> Point -> Point -> Point -> String
        letterAndPoint3 letter p1 p2 p3 =
            letter ++ formatPoint p1 ++ " " ++ formatPoint p2 ++ " " ++ formatPoint p3

        formatCurveContinuation : CurveContinuation -> String
        formatCurveContinuation continuation =
            case continuation of
                QuadAbsolute goal ->
                    letterAndPoint "T" goal

                QuadRelative goal ->
                    letterAndPoint "t" goal

                CubiAbsolute control goal ->
                    letterAndPoint2 "S" control goal

                CubiRelative control goal ->
                    letterAndPoint2 "s" control goal
    in
        case segment of
            ClosePath ->
                "Z"

            MoveAbsolute point ->
                letterAndPoint "M" point

            MoveAbsoluteMany points ->
                letterAndPoints "M" points

            MoveRelative point ->
                letterAndPoint "m" point

            MoveRelativeMany points ->
                letterAndPoints "m" points

            LineAbsolute point ->
                letterAndPoint "L" point

            LineAbsoluteMany points ->
                letterAndPoints "L" points

            LineRelative point ->
                letterAndPoint "l" point

            LineRelativeMany points ->
                letterAndPoints "l" points

            VerticalAbsolute y ->
                letterAndFloat "V" y

            VerticalRelative dy ->
                letterAndFloat "v" dy

            HorizontalAbsolute x ->
                letterAndFloat "H" x

            HorizontalRelative dx ->
                letterAndFloat "h" dx

            QuadraticAbsolute control goal ->
                letterAndPoint2 "Q" control goal

            QuadraticRelative control goal ->
                letterAndPoint2 "q" control goal

            CubicAbsolute c1 c2 point ->
                -- note that cubic == quadratic iff c1 == c2. Maybe optimize that?
                letterAndPoint3 "C" c1 c2 point

            CubicRelative dc1 dc2 dpoint ->
                letterAndPoint3 "c" dc1 dc2 dpoint

            QuadraticAbsoluteMany control goal continuations ->
                formatInstruction dp (QuadraticAbsolute control goal) ++ concatMapString formatCurveContinuation continuations

            QuadraticRelativeMany dcontrol dgoal continuations ->
                formatInstruction dp (QuadraticRelative dcontrol dgoal) ++ concatMapString formatCurveContinuation continuations

            CubicAbsoluteMany c1 c2 goal continuations ->
                formatInstruction dp (CubicAbsolute c1 c2 goal) ++ concatMapString formatCurveContinuation continuations

            CubicRelativeMany dc1 dc2 dgoal continuations ->
                formatInstruction dp (CubicRelative dc1 dc2 dgoal) ++ concatMapString formatCurveContinuation continuations

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
                    "A" ++ formatPoint ( rx, ry ) ++ " " ++ doRound xAxisRotate ++ " " ++ arc ++ "," ++ sweep ++ " " ++ formatPoint ( x, y )

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
                    "a" ++ formatPoint ( rx, ry ) ++ " " ++ doRound xAxisRotate ++ " " ++ arc ++ "," ++ sweep ++ " " ++ formatPoint ( x, y )


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


formatPointWithPrecision : Maybe Int -> Point -> String
formatPointWithPrecision dp ( x, y ) =
    roundToAtMost dp x ++ "," ++ roundToAtMost dp y
