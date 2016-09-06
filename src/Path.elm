module Path
    exposing
        ( close
          -- move
        , moveTo
        , moveBy
        , moveToMany
        , moveByMany
          -- line
        , lineTo
        , lineBy
        , verticalTo
        , verticalBy
        , horizontalTo
        , horizontalBy
          -- arc
        , arc
          -- curve
        , cubicBy
        , cubicTo
        , quadraticBy
        , quadraticTo
        , cubicToMany
        , cubicByMany
        , quadraticToMany
        , quadraticByMany
          -- continuations
        , quadraticContinueTo
        , quadraticContinueBy
        , cubicContinueTo
        , cubicContinueBy
        , segmentToString
        , segmentsToString
        , segmentsToAttribute
        )

{-| A Domain-specific language for SVG paths.

The naming convention used in this package is that `*To` is the absolute version (capital in path syntax) and `*By` is the
relative version (lowercase in path syntax).

To learn what these commands do exactly and visually, [MDN has an excellent tutorial](https://developer.mozilla.org/en/docs/Web/SVG/Tutorial/Paths).

#Conversion
@docs segmentToString, segmentsToString, segmentsToAttribute

#Move
@docs moveTo, moveBy

The many variants take a list of points to move to. For generating many moves, this function is more efficient and
the resulting `d` attribute string is shorter.

@docs moveToMany, moveByMany

#Line
@docs lineTo, lineBy, verticalTo, verticalBy, horizontalTo, horizontalBy

#Arc

@docs arc

#Close
@docs close

#Quadratic Curve
Quadratic curves are defined by a control point and a goal point.
@docs quadraticTo, quadraticBy, quadraticToMany, quadraticByMany

#Cubic Curve
Cubic curves are defined by two control points and a goal point.
@docs cubicTo, cubicBy, cubicToMany, cubicByMany

#Curve Continuations
Special syntax for writing longer curves. These functions take one fewer control point than the
complete constructors for quadratic and cubic. The missing control point is
inferred from the current control point.

**note: ** adding continuations after non-curve instructions is not illegal in svg, but this library makes it impossible.
A continuation after a non-curve instruction should be replaced by `lineTo/lineBy`.
@docs quadraticContinueTo, quadraticContinueBy, cubicContinueTo, cubicContinueBy
-}

import Internal exposing (..)
import String
import Html
import Svg
import Svg.Attributes exposing (..)
import List.Extra as List


{-| Convert a segment to a string

    segmentToString (moveTo ( 20, 20 )) == "M20,20"
    segmentToString close == "Z"
-}
segmentToString : Segment -> String
segmentToString =
    Internal.formatSegment


{-| Convert a list of segments to string

    segmentsToString
        [ moveTo ( 20, 20 )
        , lineTo ( 20, 40 )
        , close
        ]
            == "M20,20 L20,40 Z"
-}
segmentsToString : List Segment -> String
segmentsToString =
    String.join " " << List.map Internal.formatSegment


{-| Helper to convert a list of segments to an SVG attribute
-}
segmentsToAttribute : List Segment -> Svg.Attribute msg
segmentsToAttribute =
    Svg.Attributes.d << segmentsToString


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


{-| Equivalent of moveTo that takes a list of points and moves
to them in order.
-}
moveToMany : List Point -> Segment
moveToMany =
    MoveAbsoluteMany


{-| Move the cursor relative to its current position. No line
is drawn from the current to the new location.
-}
moveBy : Point -> Segment
moveBy ( dx, dy ) =
    MoveRelative ( dx, dy )


{-| Equivalent of moveBy that takes a list of (dx, dy) pairs.
-}
moveByMany : List Point -> Segment
moveByMany =
    MoveRelativeMany


{-| Draw a line from the current cursor position to a point in 2D space.

    cubeAbsolute =
        [ moveTo ( 20, 20 )
        , lineTo ( 40, 20 )
        , lineTo ( 40, 40 )
        , lineTo ( 20, 40 )
        , lineTo ( 20, 20 )
        ]
-}
lineTo : Point -> Segment
lineTo ( x, y ) =
    LineAbsolute ( x, y )


{-| Draw a line from the current cursor position to a position relative
to the current position.

    cubeRelative =
        [ moveTo ( 20, 20 )
        , lineBy ( 20, 0 )
        , lineBy ( 0, 20 )
        , lineBy ( -20, 0 )
        , lineBy ( 0, -20 )
        ]
-}
lineBy : Point -> Segment
lineBy ( dx, dy ) =
    LineRelative ( dx, dy )


{-| Draw a straight line from the current cursor position to the given y coordinate.
-}
verticalTo : Float -> Segment
verticalTo y =
    VerticalAbsolute y


{-| Draw a straight vertical line from the current cursor position of the given length.
-}
verticalBy : Float -> Segment
verticalBy dy =
    VerticalRelative dy


{-| Draw a straight line from the current cursor position to the given x coordinate.
-}
horizontalTo : Float -> Segment
horizontalTo x =
    HorizontalAbsolute x


{-| Draw a straight horizontal line from the current cursor position of the given length.
-}
horizontalBy : Float -> Segment
horizontalBy dx =
    HorizontalRelative dx



-- curve


{-| Draw a quadratic curve from the current cursor position to the
-}
quadraticTo : Point -> Point -> Segment
quadraticTo control point =
    QuadraticAbsolute control point


{-|
-}
quadraticBy : Point -> Point -> Segment
quadraticBy dcontrol dpoint =
    QuadraticRelative dcontrol dpoint


{-|
-}
quadraticToMany : Point -> Point -> List CurveContinuation -> Segment
quadraticToMany =
    QuadraticAbsoluteMany


{-|
-}
quadraticByMany : Point -> Point -> List CurveContinuation -> Segment
quadraticByMany =
    QuadraticRelativeMany


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
cubicToMany : Point -> Point -> Point -> List CurveContinuation -> Segment
cubicToMany =
    CubicAbsoluteMany


{-|
-}
cubicByMany : Point -> Point -> Point -> List CurveContinuation -> Segment
cubicByMany =
    CubicRelativeMany


{-| Extend a curve by a cubic point
-}
cubicContinueTo : Point -> Point -> CurveContinuation
cubicContinueTo =
    CubiAbsolute


{-|
-}
cubicContinueBy : Point -> Point -> CurveContinuation
cubicContinueBy =
    CubiRelative


{-| Extend a curve by a quadratic point

    [ moveTo ( 10, 40 )
    , quadraticToMany ( 52.5, 100 ) ( 95, 40 ) <|
        List.map quadraticContinueTo
            [ ( 180, 40 )
            , ( 265, 40 )
            ]
    ]
    -- produces "M10,40  Q52.5,100  95,40 T180,40 T265,40"

Creates

<svg style="margin-left: 90px;" width="275" height="100px"><path fill="red" stroke="#000" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"
    d="M10,40  Q52.5,100  95,40 T180,40 T265,40 "></path></svg>
-}
quadraticContinueTo : Point -> CurveContinuation
quadraticContinueTo =
    QuadAbsolute


{-|
-}
quadraticContinueBy : Point -> CurveContinuation
quadraticContinueBy =
    QuadRelative



-- arc


{-| Arcs are segments of ellipses. The arc command describes an ellips - the first argument
is the radii (x and y direction), the final argument the center point - and what segment
of the ellipse to draw.

For a visual interactive demo, see [http://codepen.io/lingtalfi/pen/yaLWJG](http://codepen.io/lingtalfi/pen/yaLWJG).
-}
arc : Point -> Float -> ( ArcFlag, Direction ) -> Point -> Segment
arc radius xstartangle ( largeArcFlag, sweepFlag ) point =
    ArcTo radius xstartangle ( largeArcFlag, sweepFlag ) point


{-| Draws a line from the current position to the first point of the path.
-}
close : Segment
close =
    ClosePath



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
