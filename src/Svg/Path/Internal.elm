module Svg.Path.Internal exposing (Point, CurveContinuation(..), Instruction(..), Direction(..), ArcFlag(..), formatInstruction)

import String
import Html
import Svg exposing (..)
import Svg.Attributes


type alias Point =
    ( Float, Float )


point x y =
    ( x, y )


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


translate : Point -> Instruction -> Instruction
translate ( dx, dy ) instruction =
    map (\( x, y ) -> ( x + dx, y + dy )) instruction


mapPointsCurveCont f curveCont =
    case curveCont of
        QuadAbsolute p ->
            QuadAbsolute (f p)

        QuadRelative p ->
            QuadRelative (f p)

        CubiAbsolute c p ->
            CubiAbsolute (f c) (f p)

        CubiRelative c p ->
            CubiRelative (f c) (f p)


addPoint ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            last xs


type alias DrawState =
    { start : Point, current : Point, instructions : List Instruction }


fromReversed { start, current, instructions } =
    DrawState start current (List.reverse instructions)


{-| A draw state where the instructions are reversed
-}
type alias ReversedDrawState =
    { start : Point, current : Point, instructions : List Instruction }


run : List Instruction -> ReversedDrawState -> ReversedDrawState
run list state =
    let
        helper elem accum =
            let
                newState =
                    propagate elem accum
            in
                { newState | instructions = elem :: newState.instructions }
    in
        List.foldl helper state list


propagate : Instruction -> { a | start : Point, current : Point } -> { a | start : Point, current : Point }
propagate instruction ({ start, current } as state) =
    let
        ( cx, cy ) =
            current

        resultContinuation continuation state =
            case continuation of
                QuadAbsolute p ->
                    { state | current = p }

                QuadRelative p ->
                    { state | current = addPoint state.current p }

                CubiAbsolute _ p ->
                    { state | current = p }

                CubiRelative _ p ->
                    { state | current = addPoint state.current p }
    in
        case instruction of
            MoveAbsolute p ->
                { state | current = p }

            MoveRelative p ->
                { state | current = addPoint p current }

            MoveAbsoluteMany ps ->
                case last ps of
                    Nothing ->
                        state

                    Just p ->
                        { state | current = p }

            MoveRelativeMany ps ->
                case last ps of
                    Nothing ->
                        state

                    Just p ->
                        { state | current = addPoint current p }

            LineAbsolute p ->
                { state | current = p }

            LineRelative p ->
                { state | current = addPoint current p }

            VerticalAbsolute y ->
                { state | current = point cx y }

            VerticalRelative y ->
                { state | current = point cx (cy + y) }

            HorizontalAbsolute x ->
                { state | current = point x cy }

            HorizontalRelative x ->
                { state | current = point (cx + x) cy }

            LineAbsoluteMany ps ->
                case last ps of
                    Nothing ->
                        state

                    Just p ->
                        { state | current = p }

            LineRelativeMany ps ->
                List.foldl (\rel newState -> { newState | current = addPoint rel newState.current }) state ps

            ArcTo radii xAngleRotation flags p ->
                { state | current = p }

            ArcBy radii xAngleRotation flags p ->
                { state | current = addPoint current p }

            QuadraticAbsolute control p ->
                { state | current = p }

            QuadraticRelative control p ->
                { state | current = addPoint current p }

            CubicAbsolute c1 c2 p ->
                { state | current = p }

            CubicRelative c1 c2 p ->
                { state | current = addPoint current p }

            QuadraticAbsoluteMany control goal continuations ->
                List.foldl resultContinuation { state | current = goal } continuations

            QuadraticRelativeMany control goal continuations ->
                List.foldl resultContinuation { state | current = addPoint current goal } continuations

            CubicAbsoluteMany c1 c2 goal continuations ->
                List.foldl resultContinuation { state | current = goal } continuations

            CubicRelativeMany c1 c2 goal continuations ->
                List.foldl resultContinuation { state | current = addPoint current goal } continuations

            ClosePath ->
                { state | current = start }


{-| Applies a function to all wrapped coordinates.
-}
map f instruction =
    let
        fX =
            (\x -> fst (f ( x, 0 )))

        fY =
            (\y -> snd (f ( 0, y )))
    in
        case instruction of
            MoveAbsolute p ->
                MoveAbsolute (f p)

            MoveRelative p ->
                MoveRelative (f p)

            MoveAbsoluteMany p ->
                MoveAbsoluteMany (List.map f p)

            MoveRelativeMany p ->
                MoveRelativeMany (List.map f p)

            LineAbsolute p ->
                LineAbsolute (f p)

            LineRelative p ->
                LineRelative (f p)

            VerticalAbsolute y ->
                VerticalAbsolute (fY y)

            VerticalRelative y ->
                VerticalRelative (fY y)

            HorizontalAbsolute x ->
                HorizontalAbsolute (fX x)

            HorizontalRelative x ->
                HorizontalRelative (fX x)

            LineAbsoluteMany p ->
                LineAbsoluteMany (List.map f p)

            LineRelativeMany p ->
                LineRelativeMany (List.map f p)

            ArcTo radii xAngleRotation flags p ->
                ArcTo radii xAngleRotation flags (f p)

            ArcBy radii xAngleRotation flags p ->
                ArcBy radii xAngleRotation flags (f p)

            QuadraticAbsolute control goal ->
                QuadraticAbsolute (f control) (f goal)

            QuadraticRelative control goal ->
                QuadraticRelative (f control) (f goal)

            CubicAbsolute c1 c2 point ->
                CubicAbsolute (f c1) (f c2) (f point)

            CubicRelative c1 c2 point ->
                CubicRelative (f c1) (f c2) (f point)

            QuadraticAbsoluteMany control goal continuations ->
                QuadraticAbsoluteMany (f control) (f goal) (List.map (mapPointsCurveCont f) continuations)

            QuadraticRelativeMany control goal continuations ->
                QuadraticRelativeMany (f control) (f goal) (List.map (mapPointsCurveCont f) continuations)

            CubicAbsoluteMany c1 c2 goal continuations ->
                CubicAbsoluteMany (f c1) (f c2) (f goal) (List.map (mapPointsCurveCont f) continuations)

            CubicRelativeMany c1 c2 goal continuations ->
                CubicRelativeMany (f c1) (f c2) (f goal) (List.map (mapPointsCurveCont f) continuations)

            ClosePath ->
                ClosePath
