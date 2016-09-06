module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Path exposing (..)
import Internal exposing (..)
import String


myPath =
    [ moveTo ( 50, 50 )
    , arc ( 30, 30 ) 0 ( Smallest, Clockwise ) ( 35, 20 )
    , lineTo ( 100, 100 )
    , moveTo ( 110, 110 )
    , lineTo ( 100, 0 )
    ]


quadCurve =
    [ moveTo ( 10, 40 )
    , quadraticToMany ( 52.5, 100 ) ( 95, 40 ) <|
        List.map quadraticContinueTo
            [ ( 180, 40 )
            , ( 265, 40 )
            ]
    ]


cubeAbsolute =
    [ moveTo ( 20, 20 )
    , lineTo ( 40, 20 )
    , lineTo ( 40, 40 )
    , lineTo ( 20, 40 )
    , lineTo ( 20, 20 )
    ]


cubeRelative =
    [ moveTo ( 20, 20 )
    , lineBy ( 20, 0 )
    , lineBy ( 0, 20 )
    , lineBy ( -20, 0 )
    , lineBy ( 0, -20 )
    ]


myPath2 =
    myPath
        ++ triangle ( 300, 0 ) ( 35, 35 )


multipleCloses =
    String.join " "
        [ "M50,50 50,100 100,100"
        , "Z"
        , "M100,100 100,200 200,200"
        , "Z"
        , "M200,200 200,400 400,400"
        , "Z"
        ]


arcExample =
    String.join " "
        [ "M50, 50"
        , "A 1,100 0 0,1 100,200"
          --, "Z"
        ]


rectangle : Point -> Point -> List Segment
rectangle ( x, y ) ( dx, dy ) =
    [ moveTo ( x, y )
    , verticalBy dy
    , horizontalBy -dx
    , close
    ]


triangle : Point -> Point -> List Segment
triangle ( x, y ) ( dx, dy ) =
    [ moveTo ( x, y )
    , moveTo ( x + dx, y )
    , moveTo ( x, y + dy )
    , close
    ]


result =
    --triangle ( 20, 20 ) ( 20, 20 )
    cubeRelative
        |> List.map Internal.formatSegment
        |> String.join " "


showVerbose =
    result
        |> Html.text


showImage =
    Svg.svg
        [ width "1024px", height "600px" ]
        [ Svg.path [ fill "red", stroke "#000", strokeWidth "1.5", strokeLinecap "round", strokeLinejoin "round", d result ] [] ]


main =
    showImage
