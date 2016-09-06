module Path exposing (..)

{-|

* make table with svg -> this api conversion
-}

import Internal exposing (..)
import String
import Html
import Svg
import Svg.Attributes exposing (..)
import List.Extra as List


{-| A location in 2D space
-}
type alias Point =
    ( Float, Float )


{-| Move the cursor to a point in 2D space. No line is drawn between the
current and the new location.
-}
moveTo : Point -> Segment
moveTo ( x, y ) =
    MoveAbsolute ( x, y )


{-| Move the cursor relative to its current position. No line
is drawn from the current to the new location.
-}
moveBy : Point -> Segment
moveBy ( dx, dy ) =
    MoveRelative ( dx, dy )


{-| Draw a line from the current cursor position to a point in 2D space.
-}
lineTo : Point -> Segment
lineTo ( x, y ) =
    LineAbsolute ( x, y )


{-| Draw a line from the current cursor position to a position relative
to the current position.
-}
lineBy : Point -> Segment
lineBy ( dx, dy ) =
    LineRelative ( dx, dy )


{-|
-}
verticalTo : Float -> Segment
verticalTo y =
    VerticalAbsolute y


{-|
-}
verticalBy : Float -> Segment
verticalBy dy =
    VerticalRelative dy


{-|
-}
horizontalTo : Float -> Segment
horizontalTo x =
    HorizontalAbsolute x


{-|
-}
horizontalBy : Float -> Segment
horizontalBy dx =
    HorizontalRelative dx



-- curve


{-|
-}
quadraticTo : Point -> Point -> Segment
quadraticTo control point =
    QuadraticAbsolute control point


{-|
-}
quadraticBy dcontrol dpoint =
    QuadraticRelative dcontrol dpoint


{-|
-}
quadraticContTo : Point -> Segment
quadraticContTo point =
    QuadraticNextAbsolute point


{-|
-}
quadraticContBy : Point -> Segment
quadraticContBy dpoint =
    QuadraticNextRelative dpoint


{-|
-}
cubicTo : Point -> Point -> Point -> Segment
cubicTo control1 control2 point =
    CubicAbsolute control1 control2 point


{-|
-}
cubicBy : Point -> Point -> Point -> Segment
cubicBy dcontrol1 dcontrol2 dpoint =
    CubicRelative dcontrol1 dcontrol2 dpoint


{-|
-}
cubicContTo : Point -> Point -> Segment
cubicContTo control point =
    CubicNextAbsolute control point


{-|
-}
cubicContBy : Point -> Point -> Segment
cubicContBy dcontrol dpoint =
    CubicNextRelative dcontrol dpoint



-- arc


{-|
-}
arc : Point -> ( ArcFlag, Direction ) -> Point -> Segment
arc radius ( largeArcFlag, sweepFlag ) point =
    ArcTo radius 0 ( largeArcFlag, sweepFlag ) point


{-|
-}
close : Segment
close =
    ClosePath


optimize : List Segment -> List Segment
optimize =
    let
        unfolder segments =
            case segments of
                [] ->
                    Nothing

                [ x ] ->
                    Just ( Just x, [] )

                x :: y :: rest ->
                    case ( x, y ) of
                        ( MoveAbsolute p1, MoveAbsolute p2 ) ->
                            Just ( Nothing, MoveAbsoluteMany [ p1, p2 ] :: rest )

                        ( MoveAbsoluteMany ps, MoveAbsolute p ) ->
                            Just ( Nothing, MoveAbsoluteMany (ps ++ [ p ]) :: rest )

                        _ ->
                            Just ( Just x, y :: rest )

        folder elem accum =
            case elem of
                Nothing ->
                    accum

                Just v ->
                    v :: accum
    in
        -- fuse with hylo
        List.unfoldr unfolder >> List.foldr folder []



-- refold folder unfolder []


refold : (b -> c -> c) -> (a -> Maybe ( b, a )) -> c -> a -> c
refold reducer expander base seed =
    let
        -- go : c -> a -> c
        go current seed =
            case expander seed of
                Nothing ->
                    current

                Just ( next, newSeed ) ->
                    go (reducer next current) newSeed
    in
        go base seed


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
    triangle ( 20, 20 ) ( 20, 20 )
        |> List.map Internal.formatSegment
        |> String.join " "


showVerbose =
    result
        |> Html.text


showImage =
    Svg.svg
        [ width "1024px", height "600px" ]
        [ Svg.path [ fill "none", stroke "#000", strokeWidth "1.5", strokeLinecap "round", strokeLinejoin "round", d result ] [] ]


main =
    showImage



{-
   val = [ M 417.19 124.65, l 5.9 -2.53, M 444.92 137.48, l -0.06 -5.29, M 354.89 151.4, l -4.7 2.02, M 431.79 131.4, l 5.9 -2.53, M 364.78 160.18, l 4.7 -2.02
       , M 261.64 191.45, l 4.7 -2.02, M 493.24 154.58, l 0.06 5.29, M 589.75 204.5, l 5.9 -2.53, M 380.23 167.33, l -0.05 -4.22, M 661.07 232.23, l -5.9 2.53
       , M 357.67 91.85, l 541.38 250.51, l -0.16 -12.96, M 289.47 121.13, l 541.38 250.51, l -0.16 -12.96, M 208.03 227.49, l 5.9 -2.53, M 199.34 218.2
       , l -5.9 2.53, M 682.91 247.6, l -0.06 -5.29, M 276.24 198.2, l 4.7 -2.02, M 675.67 238.99, l -5.9 2.53, M 592.87 261.52, l -4.7 2.02
       , M 522.75 233.27, l 4.7 -2.02, M 336.49 221.89, l 0.05 4.22, M 434.2 271.3, l 4.7 -2.02, M 280.29 255.66, l -5.9 2.53, M 267.13 254.84
       , l -0.06 -5.29, M 731.22 264.7, l 0.06 5.29, M 133.92 187.93, L 675.3 438.44, l -0.16 -12.96, M 200.92 159.16, l 541.38 250.51, l -0.16 -12.96
       , M 607.47 268.28, l -4.7 2.02, M 499.62 301.57, l 4.7 -2.02, M 518.92 306.3, l -4.7 2.02, M 157.98 279.13, l 0.35 28.91, l 124.19 57.46
       , M 366 300.58, l 5.9 -2.53, M 289.31 108.18, l 541.38 250.51, l 47.86 -20.55, M 833.64 312.08, l -5.9 2.53, M 446.02 337.61, l 5.9 -2.53
       , M 431.42 330.85, l 5.9 -2.53, M 878.71 351.09, l -0.16 -12.96, L 337.17 87.63, l -47.86 20.55, l 0.16 12.96, l -4.7 2.02, M 574.48 332.01
       , l 0.05 4.22, M 811.71 379.86, l -0.15 -12.88, l 0 -0.07, L 270.17 116.4, l -69.4 29.8, l 0.16 12.96, l -4.7 2.02, M 609.88 408.17
       , l -5.9 2.53, M 200.77 146.2, l 541.38 250.51, l 69.32 -29.77, M 765.43 341.37, l -4.7 2.02, M 512.37 368.31, l 5.9 -2.53, M 920.75 346.06
       , l 0.87 71.85, M 54.26 244.02, l 55.49 25.68, l 14.44 -6.2, M 54.97 302.91, L 576.5 544.23, l 20.46 0.07, l 37.33 -16.03, M 109.6 256.74
       , l 0.87 71.85, l 47.86 -20.55, M 505.12 364.96, l -0.06 -5.29, M 357.52 78.89, L 898.9 329.4, l 59.58 -25.59, M 840.66 380.45, l 80.95 37.46
       , l 38.04 -16.29, L 958.37 295, L 436.84 53.68, l -20.45 -0.07, l -58.87 25.28, l 0.16 12.96, l -5.9 2.53, M 855.11 374.25, l 66.35 30.7
       , l 38.04 -16.33, M 133.76 174.98, l 541.38 250.51, l 47.86 -20.55, M 723.16 417.89, l -0.16 -12.96, L 181.62 154.42, l -47.86 20.55, l 0.16 12.96
       , l -5.9 2.53, M 676.89 379.4, l -4.7 2.02, M 283.03 408.44, l -0.87 -71.85, M 347.58 366.86, l 0.87 71.85, l 47.86 -20.55, M 395.96 389.24
       , l 0.35 28.91, l 124.19 57.46, M 282.32 349.55, l 65.42 30.27, l 14.44 -6.2, M 53.95 218.11, l 521.53 241.32, l 20.46 0.07, l 58.87 -25.28, M 54.1 231.06
       , l 521.53 241.32, l 20.46 0.07, l 362.54 -155.69, M 520.14 446.71, l 0.87 71.85, M 654.96 447.18, l -0.16 -12.96, L 113.42 183.71, l -59.59 25.63
       , l 1.3 106.52, l 521.53 241.32, l 20.46 0.07, l 37.33 -16.03, l -0.87 -71.85, M 743.33 494.47, l -0.87 -71.85, M 662.38 457.01, l 80.95 37.46
       , l 69.4 -29.8, l -0.87 -71.85, M 676.82 450.81, l 66.36 30.7, l 69.4 -29.8, M 520.3 459.66, l 55.49 25.68, l 20.45 0.07, l 362.54 -155.69
       ]
-}
