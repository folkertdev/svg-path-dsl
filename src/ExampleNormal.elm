module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Path exposing (..)
import Internal exposing (..)
import String


myPath =
    Path.path
        |> moveToAbs ( 50, 50 )
        |> arcTo ( 30, 30 ) 0 ( Smallest, Clockwise ) ( 35, 20 )
        |> lineToAbs ( 100, 100 )
        |> moveToAbs ( 110, 110 )
        |> lineToAbs ( 100, 0 )


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


main =
    Svg.svg [ width "400", height "400", viewBox "0 0 400 400" ]
        [ Svg.path
            [ color "red"
            , stroke "black"
            , strokeWidth "1"
            , fill "red"
              --, draw myPath2
            , Svg.Attributes.d arcExample
            ]
            []
        ]
