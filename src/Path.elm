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
        , lineToMany
        , lineByMany
        , verticalTo
        , verticalBy
        , horizontalTo
        , horizontalBy
          -- arc
        , arcTo
        , arcBy
        , smallestArc
        , largestArc
        , clockwise
        , antiClockwise
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

The path syntax is a minimal syntax to describe SVG paths in 2D space. It looks a bit like assembly and is hard to
read. That is the problem this module solves. Here are some resources to learn more about
 [`<path>` elements](https://developer.mozilla.org/en/docs/Web/SVG/Tutorial/Paths)
 and the [`d` attribute syntax](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d).

**Conventions**

* To distinguish between absolute (capital letters, like `M20,20`) and relative (lowercase letters, like `m20,20`) instructions,
this module uses a `To` suffix for absolute commands and a `By` suffix for relative commands.

* When a function takes multiple `Point` arguments, the final point is always `(x, y)` for absolute or `(dx, dy)` for relative.
Other `Point` arguments have function-specific uses. Curves use them for control points, arcs for describing and ellipse's radii.

* Functions with a `Many` suffix take a list of arguments and will try to merge them into one instruction. For example
    `[ moveTo (20, 20), moveTo (40, 40) ]` produces `M20,20 M40,40`, but `[ moveToMany [ (20, 20), (40, 40) ] ]` will produce
    `M20,20 40,40`. The generated path will look the same in both cases.

#Conversion

Conversion from segments to string and attribute. All conversion functions take a `Maybe Int` argument that specifies
the maximum number of decimals that a number in the output will have. `Nothing` will just use `toString` for the conversion
from float to string.

@docs segmentToString, segmentsToString, segmentsToAttribute

#Move
@docs moveTo, moveBy
@docs moveToMany, moveByMany

#Line
@docs lineTo, lineBy, lineToMany, lineByMany
@docs verticalTo, verticalBy, horizontalTo, horizontalBy

#Arc
Arcs are segments of ellipses. The arc command describes an ellips and what segment of that elips to draw.

The first argument is an (rx, ry) pair - the radii of the elips. The second argument `xAxisRotate` rotates
the elips around its center by a number of degrees. The final argument is the center of the ellips.

The remaining argument is a pair of flags that select the part of the ellips to draw.
For a visual interactive demo, see [http://codepen.io/lingtalfi/pen/yaLWJG](http://codepen.io/lingtalfi/pen/yaLWJG).

@docs arcTo, arcBy, largestArc, smallestArc, clockwise, antiClockwise

#Close
@docs close

#Quadratic Curve
Quadratic curves are defined by a control point and a goal point.
An interactive demo is available [here](http://blogs.sitepointstatic.com/examples/tech/svg-curves/quadratic-curve.html).
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


{-| Convert a segment to a string.

    segmentToString (Just 2) (moveTo ( 20.001, 20 ))
        == "M20,20"
    segmentToString Nothing close == "Z"
-}
segmentToString : Maybe Int -> Segment -> String
segmentToString =
    Internal.formatSegment


{-| Convert a list of segments to string.

    segmentsToString Nothing
        [ moveTo ( 20, 20 )
        , lineTo ( 20, 40 )
        , close
        ]
            == "M20,20 L20,40 Z"
-}
segmentsToString : Maybe Int -> List Segment -> String
segmentsToString maxNumOfDecimals =
    String.join " " << List.map (Internal.formatSegment maxNumOfDecimals)


{-| Helper to convert a list of segments directly to an SVG attribute.
-}
segmentsToAttribute : Maybe Int -> List Segment -> Svg.Attribute msg
segmentsToAttribute maxNumOfDecimals =
    Svg.Attributes.d << segmentsToString maxNumOfDecimals


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


{-| Equivalent of moveTo that takes a list of points. It holds that:

    [ moveTo (20, 20), moveTo (40, 40) ]
        == [ moveToMany [ (20, 20), (40, 40) ] ]

For a large number of points this function is more convenient,
faster and the resulting string will be shorter.
-}
moveToMany : List Point -> Segment
moveToMany =
    MoveAbsoluteMany


{-| Move the cursor relative to its current position. No line
is drawn from the current to the new location.
-}
moveBy : Point -> Segment
moveBy =
    MoveRelative


{-| Equivalent of moveBy that takes a list of (dx, dy) pairs. It holds that:

    [ moveBy (20, 20), moveBy (40, 40) ]
        == [ moveByMany [ (20, 20), (40, 40) ] ]

For a large number of points this function is more convenient,
faster and the resulting string will be shorter.
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
lineTo =
    LineAbsolute


{-|
-}
lineToMany : List Point -> Segment
lineToMany =
    LineAbsoluteMany


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
lineBy =
    LineRelative


{-|
-}
lineByMany : List Point -> Segment
lineByMany =
    LineRelativeMany


{-| Draw a straight line from the current cursor position to the given y coordinate.
-}
verticalTo : Float -> Segment
verticalTo =
    VerticalAbsolute


{-| Draw a straight vertical line from the current cursor position of the given length.
-}
verticalBy : Float -> Segment
verticalBy =
    VerticalRelative


{-| Draw a straight line from the current cursor position to the given x coordinate.
-}
horizontalTo : Float -> Segment
horizontalTo =
    HorizontalAbsolute


{-| Draw a straight horizontal line from the current cursor position of the given length.
-}
horizontalBy : Float -> Segment
horizontalBy =
    HorizontalRelative


{-| -}
arcTo : Point -> Float -> ( ArcFlag, Direction ) -> Point -> Segment
arcTo radius xstartangle ( largeArcFlag, sweepFlag ) point =
    ArcTo radius xstartangle ( largeArcFlag, sweepFlag ) point


{-| -}
arcBy : Point -> Float -> ( ArcFlag, Direction ) -> Point -> Segment
arcBy radius xstartangle ( largeArcFlag, direction ) point =
    ArcBy radius xstartangle ( largeArcFlag, direction ) point


{-| Move from A to B in the clockwise direction
<svg width="100%" height="200px" id="svgcontext">

    <path id="arc2" d="M100 100 A 47 66 49 1 1 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc3" d="M100 100 A 47 66 49 1 0 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc4" d="M100 100 A 47 66 49 0 0 200 100" fill="none" stroke="green" stroke-width="2"></path>

    <path id="arc" d="M100 100 A 47 66 49 0 1 200 100" fill="none" stroke="red" stroke-width="4"></path>

    <line id="line0" x1="0" y1="100" x2="100" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line" x1="100" y1="100" x2="200" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line2" x1="200" y1="100" x2="1027" y2="100" fill="none" stroke="black" stroke-width="2"></line>

    <circle id="circle1" cx="100" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>

    <circle id="circle2" cx="200" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>


</svg>
-}
clockwise : Direction
clockwise =
    Clockwise


{-| Move from A to B in the anti-clockwise direction.

<svg width="100%" height="200px" id="svgcontext">

    <path id="arc2" d="M100 100 A 47 66 49 1 1 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc3" d="M100 100 A 47 66 49 1 0 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc4" d="M100 100 A 47 66 49 0 1 200 100" fill="none" stroke="green" stroke-width="2"></path>

    <path id="arc" d="M100 100 A 47 66 49 0 0 200 100" fill="none" stroke="red" stroke-width="4"></path>

    <line id="line0" x1="0" y1="100" x2="100" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line" x1="100" y1="100" x2="200" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line2" x1="200" y1="100" x2="1027" y2="100" fill="none" stroke="black" stroke-width="2"></line>

    <circle id="circle1" cx="100" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>

    <circle id="circle2" cx="200" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>


</svg>
-}
antiClockwise : Direction
antiClockwise =
    AntiClockwise


{-| Pick the arg with the greatest length.
<svg width="100%" height="200px" id="svgcontext">

    <path id="arc2" d="M100 100 A 47 66 49 1 1 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc3" d="M100 100 A 47 66 49 0 1 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc4" d="M100 100 A 47 66 49 0 0 200 100" fill="none" stroke="green" stroke-width="2"></path>

    <path id="arc" d="M100 100 A 47 66 49 1 0 200 100" fill="none" stroke="red" stroke-width="4"></path>

    <line id="line0" x1="0" y1="100" x2="100" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line" x1="100" y1="100" x2="200" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line2" x1="200" y1="100" x2="1920" y2="100" fill="none" stroke="black" stroke-width="2"></line>

    <circle id="circle1" cx="100" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>

    <circle id="circle2" cx="200" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>


</svg>
-}
largestArc : ArcFlag
largestArc =
    Largest


{-| Pick the arc with the smallest length.

<svg width="100%" height="200px" id="svgcontext">

    <path id="arc2" d="M100 100 A 47 66 49 1 1 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc3" d="M100 100 A 47 66 49 1 0 200 100" fill="none" stroke="green" stroke-width="2"></path>
    <path id="arc4" d="M100 100 A 47 66 49 0 1 200 100" fill="none" stroke="green" stroke-width="2"></path>

    <path id="arc" d="M100 100 A 47 66 49 0 0 200 100" fill="none" stroke="red" stroke-width="4"></path>

    <line id="line0" x1="0" y1="100" x2="100" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line" x1="100" y1="100" x2="200" y2="100" fill="none" stroke="black" stroke-width="2"></line>
    <line id="line2" x1="200" y1="100" x2="1027" y2="100" fill="none" stroke="black" stroke-width="2"></line>

    <circle id="circle1" cx="100" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>

    <circle id="circle2" cx="200" cy="100" r="5" fill="red" stroke="red" stroke-width="2"></circle>


</svg>
-}
smallestArc : ArcFlag
smallestArc =
    Smallest


{-| Draws a line from the current position to the first point of the path.

`[ moveTo (100, 100), lineTo (200, 100), lineTo (150, 50) ]`
<svg width="100%" height="100px" id="svgcontext">
    <path id="arc2" d="M100 100 L200 100 L150 50" fill="red" stroke="black" stroke-width="5"></path>
</svg>

`[ moveTo (100, 100), lineTo (200, 100), lineTo (150, 50), close ]`
<svg width="100%" height="100px" id="svgcontext">
    <path id="arc2" d="M100 100 L200 100 L150 50 Z" fill="red" stroke="black" stroke-width="5"></path>
</svg>

-}
close : Segment
close =
    ClosePath



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
