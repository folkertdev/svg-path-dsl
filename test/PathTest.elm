module PathTest exposing (..)

import Test exposing (..)
import Test.Runner.Html
import Expect
import Fuzz exposing (..)
import Svg.Path exposing (..)
import Svg.Path.Instruction exposing (..)
import String


point =
    tuple ( float, float )


formatPoint =
    formatPointWithPrecision Nothing


origin =
    ( 42, 79 )


conts =
    [ cubicContinueTo origin origin
    , cubicContinueBy origin origin
    , quadraticContinueTo origin
    , quadraticContinueBy origin
    ]


allInstructions =
    [ Absolute (Move origin)
    , Relative (Move origin)
    , lineTo origin
    , lineBy origin
    , lineToMany [ origin, origin ]
    , lineByMany [ origin, origin ]
    , verticalTo 42
    , verticalBy 42
    , horizontalTo 42
    , horizontalBy 42
    , arcTo origin 20 ( largestArc, clockwise ) origin
    , arcBy origin 20 ( largestArc, clockwise ) origin
    , quadraticTo origin origin
    , quadraticBy origin origin
    , cubicTo origin origin origin
    , cubicBy origin origin origin
    , quadraticToMany origin origin conts
    , quadraticByMany origin origin conts
    , cubicToMany origin origin origin conts
    , cubicByMany origin origin origin conts
    , toStart
    ]


choice : Bool -> a -> a -> a
choice b x y =
    if b then
        x
    else
        y


all : Test
all =
    describe "The Path module"
        [ describe "Lines"
            [ fuzz (tuple ( float, float )) "lineTo works on trivial input" <|
                \( a, b ) ->
                    lineTo ( a, b )
                        |> instructionToString Nothing
                        |> Expect.equal ("L" ++ toString a ++ "," ++ toString b)
            , fuzz (tuple ( float, float )) "lineBy works on trivial input" <|
                \( a, b ) ->
                    lineBy ( a, b )
                        |> instructionToString Nothing
                        |> Expect.equal ("l" ++ toString a ++ "," ++ toString b)
            ]
        , describe "Arc"
            [ test "arc works on trivial input" <|
                \() ->
                    arcTo ( 0, 0 ) 0 ( Largest, Clockwise ) ( 0, 0 )
                        |> instructionToString Nothing
                        |> Expect.equal "A0,0 0 1,1 0,0"
            , fuzz (tuple5 ( point, float, bool, bool, point )) "arc works on all input" <|
                \( radius, xanglerot, largeArcFlag, sweepFlag, goal ) ->
                    let
                        result =
                            arcTo radius
                                xanglerot
                                ( (choice largeArcFlag Largest Smallest)
                                , (choice sweepFlag Clockwise AntiClockwise)
                                )
                                goal

                        flags =
                            (choice largeArcFlag "1" "0") ++ "," ++ (choice sweepFlag "1" "0")

                        expected =
                            String.join " " [ "A" ++ formatPoint radius, toString xanglerot, flags, formatPoint goal ]
                    in
                        Expect.equal expected (instructionToString Nothing result)
            ]
        , optimizer
        , instruction
        ]


optimizer =
    describe "The Optimize module" []


instruction =
    describe "The Instruction module"
        [ describe "mapAbsolute id == id" (List.map (\inst -> test (toString inst) <| \() -> Expect.equal inst (mapAbsolute identity inst)) allInstructions)
        ]


main =
    Test.Runner.Html.run all
