module Svg.Path.Instruction exposing (..)

import Svg.Path.Point as Point exposing (Point)
import String


type Instruction
    = ClosePath
    | Absolute (DrawInstruction Point)
    | Relative (DrawInstruction Point)


type DrawInstruction p
    = Move p
    | Line p
    | Vertical Float
    | Horizontal Float
    | Arc ( Float, Float ) Float ( ArcFlag, Direction ) p
    | Quadratic p p
    | Cubic p p p
    | Many (ManyInstruction p)


type ManyInstruction p
    = LineMany (List p)
    | QuadraticMany p p (List (CurveContinuation p))
    | CubicMany p p p (List (CurveContinuation p))


{-| the T, t, S and s commands
-}
type CurveContinuation p
    = QuadContAbsolute p
    | QuadContRelative p
    | CubiContAbsolute p p
    | CubiContRelative p p


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


instructionToString : Maybe Int -> Instruction -> String
instructionToString dp instruction =
    let
        formatPoint =
            formatPointWithPrecision dp

        doRound =
            roundToAtMost dp

        concatMapString : (a -> String) -> List a -> String
        concatMapString f =
            List.foldl (\e accum -> accum ++ (f e)) ""

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

        formatCurveContinuation : CurveContinuation Point -> String
        formatCurveContinuation continuation =
            case continuation of
                QuadContAbsolute goal ->
                    letterAndPoint "T" goal

                QuadContRelative goal ->
                    letterAndPoint "t" goal

                CubiContAbsolute control goal ->
                    letterAndPoint2 "S" control goal

                CubiContRelative control goal ->
                    letterAndPoint2 "s" control goal
    in
        case instruction of
            ClosePath ->
                "Z"

            Absolute (Move point) ->
                letterAndPoint "M" point

            Relative (Move point) ->
                letterAndPoint "m" point

            Absolute (Line point) ->
                letterAndPoint "L" point

            Absolute (Many (LineMany points)) ->
                case points of
                    [] ->
                        ""

                    _ ->
                        letterAndPoints "L" points

            Relative (Line point) ->
                letterAndPoint "l" point

            Relative (Many (LineMany points)) ->
                case points of
                    [] ->
                        ""

                    _ ->
                        letterAndPoints "l" points

            Absolute (Vertical y) ->
                letterAndFloat "V" y

            Relative (Vertical dy) ->
                letterAndFloat "v" dy

            Absolute (Horizontal x) ->
                letterAndFloat "H" x

            Relative (Horizontal dx) ->
                letterAndFloat "h" dx

            Absolute (Quadratic control goal) ->
                letterAndPoint2 "Q" control goal

            Relative (Quadratic control goal) ->
                letterAndPoint2 "q" control goal

            Absolute (Cubic c1 c2 point) ->
                -- note that cubic == quadratic iff c1 == c2. Maybe optimize that?
                letterAndPoint3 "C" c1 c2 point

            Relative (Cubic dc1 dc2 dpoint) ->
                letterAndPoint3 "c" dc1 dc2 dpoint

            Absolute (Many (QuadraticMany control goal continuations)) ->
                instructionToString dp (Absolute <| Quadratic control goal) ++ concatMapString formatCurveContinuation continuations

            Relative (Many (QuadraticMany control goal continuations)) ->
                instructionToString dp (Relative <| Quadratic control goal) ++ concatMapString formatCurveContinuation continuations

            Absolute (Many (CubicMany c1 c2 goal continuations)) ->
                instructionToString dp (Absolute <| Cubic c1 c2 goal) ++ concatMapString formatCurveContinuation continuations

            Relative (Many (CubicMany c1 c2 goal continuations)) ->
                instructionToString dp (Relative <| Cubic c1 c2 goal) ++ concatMapString formatCurveContinuation continuations

            Absolute (Arc ( rx, ry ) xAxisRotate ( arcFlag, sweepFlag ) ( x, y )) ->
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

            Relative (Arc ( rx, ry ) xAxisRotate ( arcFlag, sweepFlag ) ( x, y )) ->
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
    law: mapAbsolute id = id
-}
mapAbsolute : (Point -> Point) -> Instruction -> Instruction
mapAbsolute f instruction =
    case instruction of
        ClosePath ->
            ClosePath

        Relative draw ->
            case draw of
                Many many ->
                    case many of
                        QuadraticMany c p continuations ->
                            List.map (mapAbsoluteCurveCont f) continuations
                                |> QuadraticMany c p
                                |> Many
                                |> Relative

                        CubicMany c1 c2 p continuations ->
                            List.map (mapAbsoluteCurveCont f) continuations
                                |> CubicMany c1 c2 p
                                |> Many
                                |> Relative

                        _ ->
                            Many many
                                |> Relative

                _ ->
                    instruction

        Absolute draw ->
            Absolute <|
                case draw of
                    Move p ->
                        Move (f p)

                    Line p ->
                        Line (f p)

                    Arc radii xAngleRotate flags p ->
                        Arc radii xAngleRotate flags (f p)

                    Quadratic c p ->
                        Quadratic (f c) (f p)

                    Cubic c1 c2 p ->
                        Cubic (f c1) (f c2) (f p)

                    Horizontal x ->
                        Horizontal <| Tuple.first (f ( x, 0 ))

                    Vertical y ->
                        Vertical <| Tuple.second (f ( 0, y ))

                    Many many ->
                        case many of
                            LineMany ps ->
                                LineMany (List.map f ps)
                                    |> Many

                            QuadraticMany c p continuations ->
                                List.map (mapAbsoluteCurveCont f) continuations
                                    |> QuadraticMany c p
                                    |> Many

                            CubicMany c1 c2 p continuations ->
                                List.map (mapAbsoluteCurveCont f) continuations
                                    |> CubicMany c1 c2 p
                                    |> Many


mapAbsoluteCurveCont : (Point -> Point) -> CurveContinuation Point -> CurveContinuation Point
mapAbsoluteCurveCont f curveCont =
    case curveCont of
        QuadContAbsolute p ->
            QuadContAbsolute (f p)

        CubiContAbsolute c p ->
            CubiContAbsolute (f c) (f p)

        _ ->
            curveCont


{-| gives a string representation of a float with at most `i` decimal places.
-}
fixed : Int -> Float -> String
fixed i n =
    let
        pow =
            toFloat (10 ^ i)

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


{-| Modifies a state by simulating the drawing of a single instruction. Updates the start and current  position and
a normalized direction vector
-}
propagate : Instruction -> { a | start : Point, current : Point, direction : Point } -> { a | start : Point, current : Point, direction : Point }
propagate instruction ({ start, current } as state) =
    let
        ( cx, cy ) =
            current

        resultContinuation continuation state =
            case continuation of
                QuadContAbsolute p ->
                    { state | current = p }

                QuadContRelative p ->
                    { state | current = Point.add state.current p }

                CubiContAbsolute _ p ->
                    { state | current = p }

                CubiContRelative _ p ->
                    { state | current = Point.add state.current p }
    in
        case instruction of
            Absolute (Move p) ->
                { state | current = p, direction = Point.direction p state.current }

            Relative (Move p) ->
                { state | current = Point.add p current }

            Absolute (Line p) ->
                { state | current = p, direction = Point.direction current p }

            Relative (Line p) ->
                { state | current = Point.add current p }

            Absolute (Vertical y) ->
                { state | current = Point.point cx y, direction = Point.direction current (Point.point cx y) }

            Relative (Vertical y) ->
                { state | current = Point.point cx (cy + y) }

            Absolute (Horizontal x) ->
                { state | current = Point.point x cy, direction = Point.direction current (Point.point x cy) }

            Relative (Horizontal x) ->
                { state | current = Point.point (cx + x) cy }

            Absolute (Many (LineMany ps)) ->
                case List.drop (List.length ps - 2) ps of
                    [ q, p ] ->
                        { state | current = p, direction = Point.direction q p }

                    [ p ] ->
                        { state | current = p, direction = Point.direction state.current p }

                    _ ->
                        state

            Relative (Many (LineMany ps)) ->
                let
                    newState =
                        List.foldl (\rel newState -> { newState | current = Point.add rel newState.current }) state ps
                in
                    case List.drop (List.length ps - 2) ps of
                        [ q, p ] ->
                            { newState | direction = Point.direction q p }

                        [ p ] ->
                            { newState | direction = Point.normalize p }

                        _ ->
                            newState

            Absolute (Arc radii xAngleRotation flags p) ->
                { state | current = p, direction = Point.direction state.current p }

            Relative (Arc radii xAngleRotation flags p) ->
                let
                    new =
                        Point.add current p
                in
                    { state | current = new, direction = Point.direction current new }

            Absolute (Quadratic control p) ->
                { state | current = p, direction = Point.direction state.current p }

            Relative (Quadratic control p) ->
                let
                    new =
                        Point.add current p
                in
                    { state | current = new, direction = Point.direction current new }

            Absolute (Cubic c1 c2 p) ->
                { state | current = p, direction = Point.direction current p }

            Relative (Cubic c1 c2 p) ->
                let
                    new =
                        Point.add current p
                in
                    { state | current = new, direction = Point.direction current new }

            Absolute (Many (QuadraticMany control goal continuations)) ->
                let
                    newState =
                        List.foldl resultContinuation { state | current = goal } continuations
                in
                    { newState | direction = Point.direction current newState.current }

            Relative (Many (QuadraticMany control goal continuations)) ->
                let
                    newState =
                        List.foldl resultContinuation { state | current = Point.add current goal } continuations
                in
                    { newState | direction = Point.direction current newState.current }

            Absolute (Many (CubicMany c1 c2 goal continuations)) ->
                let
                    newState =
                        List.foldl resultContinuation { state | current = goal } continuations
                in
                    { newState | direction = Point.direction current newState.current }

            Relative (Many (CubicMany c1 c2 goal continuations)) ->
                let
                    newState =
                        List.foldl resultContinuation { state | current = Point.add current goal } continuations
                in
                    { newState | direction = Point.direction current newState.current }

            ClosePath ->
                { state | current = start, direction = Point.direction current start }
