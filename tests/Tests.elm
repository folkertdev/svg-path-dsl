module Tests exposing (..)

import Test exposing (..)
import Test.Runner.Html exposing (run)
import Expect
import Fuzz exposing (float, tuple)
import Svg.Path exposing (..)
import Svg.Path.Internal exposing (..)


pointGenerator =
    tuple ( float, float )


instructionToString =
    formatInstruction Nothing


subpathToString =
    (\x -> [ x ]) >> pathToString


all : Test
all =
    describe "The SVG DSL Test Suite"
        [ describe "instruction to string"
            [ test "move absolute" <| \() -> MoveAbsolute ( 42, 0 ) |> instructionToString |> Expect.equal "M42,0"
            , test "move relative" <| \() -> MoveRelative ( 42, 0 ) |> instructionToString |> Expect.equal "m42,0"
              -- vertical and horizontal
            , test "vertical absolute" <| \() -> VerticalAbsolute 42 |> instructionToString |> Expect.equal "V42"
            , test "vertical relative" <| \() -> VerticalRelative 42 |> instructionToString |> Expect.equal "v42"
            , test "horizontal absolute" <| \() -> HorizontalAbsolute 42 |> instructionToString |> Expect.equal "H42"
            , test "horizontal relative" <| \() -> HorizontalRelative 42 |> instructionToString |> Expect.equal "h42"
              -- close
            , test "toStart" <| \() -> ClosePath |> instructionToString |> Expect.equal "Z"
            , curves
            , arcs
            , lines
            ]
        , test "precision 0" <| \() -> LineAbsolute ( Basics.pi, 0 ) |> formatInstruction (Just 0) |> Expect.equal "L3,0"
        , test "precision 1" <| \() -> LineAbsolute ( Basics.pi, 0 ) |> formatInstruction (Just 1) |> Expect.equal "L3.1,0"
        , test "precision 2" <| \() -> LineAbsolute ( Basics.pi, 0 ) |> formatInstruction (Just 2) |> Expect.equal "L3.14,0"
        ]


lines =
    describe "lines"
        -- line
        [ test "line absolute" <| \() -> LineAbsolute ( 42, 0 ) |> instructionToString |> Expect.equal "L42,0"
        , test "line relative" <| \() -> LineRelative ( 42, 0 ) |> instructionToString |> Expect.equal "l42,0"
          -- line many
        , test "line absolute many with []" <| \() -> LineAbsoluteMany [] |> instructionToString |> Expect.equal ""
        , test "line relative many with []" <| \() -> LineRelativeMany [] |> instructionToString |> Expect.equal ""
        , test "line absolute many with multiple" <| \() -> LineAbsoluteMany [ ( 0, 0 ), ( 42, 42 ) ] |> instructionToString |> Expect.equal "L0,0 42,42"
        , test "line relative many with multiple" <| \() -> LineRelativeMany [ ( 0, 0 ), ( 42, 42 ) ] |> instructionToString |> Expect.equal "l0,0 42,42"
        , fuzz (tuple ( float, float )) "LineAbsoluteMany with [x] == LineAbsolute x"
            <| (\point ->
                    LineAbsoluteMany [ point ]
                        |> instructionToString
                        |> Expect.equal (instructionToString (LineAbsolute point))
               )
        , fuzz (tuple ( float, float )) "LineRelativeMany with [x] == LineAbsolute x"
            <| (\point ->
                    LineRelativeMany [ point ]
                        |> instructionToString
                        |> Expect.equal (instructionToString (LineRelative point))
               )
        ]


arcs =
    describe "arcs"
        [ test "smallest arc" <| \() -> ArcTo ( 0, 0 ) 0 ( smallestArc, clockwise ) ( 0, 0 ) |> instructionToString |> Expect.equal "A0,0 0 0,1 0,0"
        , test "anti-clockwise" <| \() -> ArcTo ( 0, 0 ) 0 ( largestArc, antiClockwise ) ( 0, 0 ) |> instructionToString |> Expect.equal "A0,0 0 1,0 0,0"
        , test "arc relative" <| \() -> ArcBy ( 0, 0 ) 0 ( largestArc, antiClockwise ) ( 0, 0 ) |> instructionToString |> Expect.equal "a0,0 0 1,0 0,0"
        ]


curves =
    describe "curves"
        [ test "quadratic absolute" <| \() -> QuadraticAbsolute ( 41, 0 ) ( 42, 0 ) |> instructionToString |> Expect.equal "Q41,0 42,0"
        , test "quadratic relative" <| \() -> QuadraticRelative ( 41, 0 ) ( 42, 0 ) |> instructionToString |> Expect.equal "q41,0 42,0"
        , test "cubic absolute" <| \() -> CubicAbsolute ( 40, 1 ) ( 41, 0 ) ( 42, 0 ) |> instructionToString |> Expect.equal "C40,1 41,0 42,0"
        , test "cubic relative" <| \() -> CubicRelative ( 40, 1 ) ( 41, 0 ) ( 42, 0 ) |> instructionToString |> Expect.equal "c40,1 41,0 42,0"
          -- many
        , test "quadratic absolute many with [] " <| \() -> QuadraticAbsoluteMany ( 41, 0 ) ( 42, 0 ) [] |> instructionToString |> Expect.equal "Q41,0 42,0"
        , test "quadratic relative many with []" <| \() -> QuadraticRelativeMany ( 41, 0 ) ( 42, 0 ) [] |> instructionToString |> Expect.equal "q41,0 42,0"
        , test "cubic absolute many with []" <| \() -> CubicAbsoluteMany ( 40, 1 ) ( 41, 0 ) ( 42, 0 ) [] |> instructionToString |> Expect.equal "C40,1 41,0 42,0"
        , test "cubic relative many with []" <| \() -> CubicRelativeMany ( 40, 1 ) ( 41, 0 ) ( 42, 0 ) [] |> instructionToString |> Expect.equal "c40,1 41,0 42,0"
        , test "quadratic absolute many with [x] "
            <| \() -> QuadraticAbsoluteMany ( 41, 0 ) ( 42, 0 ) [ QuadAbsolute ( 40, 5 ) ] |> instructionToString |> Expect.equal "Q41,0 42,0T40,5"
        , test "quadratic relative many with []"
            <| \() -> QuadraticRelativeMany ( 41, 0 ) ( 42, 0 ) [ QuadRelative ( 40, 5 ) ] |> instructionToString |> Expect.equal "q41,0 42,0t40,5"
        , test "cubic absolute many with []"
            <| \() -> CubicAbsoluteMany ( 40, 1 ) ( 41, 0 ) ( 42, 0 ) [ CubiAbsolute ( 40, 5 ) ( 39, 4 ) ] |> instructionToString |> Expect.equal "C40,1 41,0 42,0S40,5 39,4"
        , test "cubic relative many with []"
            <| \() -> CubicRelativeMany ( 40, 1 ) ( 41, 0 ) ( 42, 0 ) [ CubiRelative ( 40, 5 ) ( 39, 4 ) ] |> instructionToString |> Expect.equal "c40,1 41,0 42,0s40,5 39,4"
        ]


main =
    run all
