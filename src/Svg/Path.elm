module Svg.Path
    exposing
        ( Path
        , Subpath
        , subpath
        , Instruction
        , Point
        , StartingPoint
        , CloseOption
        , closed
        , open
          -- conversion
        , pathToString
        , pathToStringWithPrecision
        , pathToAttribute
          -- close (Z)
        , close
          -- move
        , startAt
        , moveBy
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
          -- types
        , ArcFlag
        , Direction
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

#Path
@docs Path, Subpath, subpath, Instruction, Point

#Converting
@docs pathToString, pathToStringWithPrecision, pathToAttribute

#Starting a subpath
@docs StartingPoint, startAt, moveBy

#Closing a subpath
@docs CloseOption, closed, open

#Line
@docs lineTo, lineBy, lineToMany, lineByMany
@docs verticalTo, verticalBy, horizontalTo, horizontalBy

#Arc
Arcs are segments of ellipses. The arc command describes an ellips and what segment of that elips to draw.

The first argument is an (rx, ry) pair - the radii of the elips. The second argument `xAxisRotate` rotates
the elips around its center by a number of degrees. The final argument is the center of the ellips.

The remaining argument is a pair of flags that select the part of the ellips to draw.
For a visual interactive demo, see [http://codepen.io/lingtalfi/pen/yaLWJG](http://codepen.io/lingtalfi/pen/yaLWJG).

@docs arcTo, arcBy
##arc size
@docs ArcFlag, largestArc, smallestArc
##direction
@docs Direction, clockwise, antiClockwise

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

import Svg.Path.Internal as Internal exposing (..)
import String
import Html
import Svg
import Svg.Attributes exposing (..)
import List.Extra as List


{-| A path is a list of subpaths.
-}
type alias Path =
    List Subpath


{-| Convert a path into a string. Ready to use as argument to `Svg.Attributes.d`.
-}
pathToString : Path -> String
pathToString path =
    instructionsToString Nothing (List.foldr subPathToInstructions [] path)


{-| Convert a path into a string. Ready to use as argument to `Svg.Attributes.d`.
-}
pathToAttribute : Path -> Svg.Attribute msg
pathToAttribute path =
    instructionsToString Nothing (List.foldr subPathToInstructions [] path)
        |> Svg.Attributes.d


subPathToInstructions : Subpath -> List Instruction -> List Instruction
subPathToInstructions (Subpath (StartingPoint start) (CloseOption closePath) segments) accum =
    if closePath then
        (start :: segments) ++ (close :: accum)
    else
        (start :: segments) ++ accum


{-| Convert a path into a string. Ready to use as argument to `Svg.Attributes.d`.
The first argument gives the maximum number of decimal places any number in the output will have.
-}
pathToStringWithPrecision : Int -> Path -> String
pathToStringWithPrecision decimalPlaces path =
    instructionsToString (Just decimalPlaces) (List.foldr subPathToInstructions [] path)


{-| A subpath is a list of svg instructions with a starting point and a closing option.
-}
type Subpath
    = Subpath StartingPoint CloseOption (List Instruction)


{-| Construct a subpath from a starting point (`startAt (x, y)` or `moveBy (dx, dy)`), a closing option (`closed` or `open`)
and a list of intructions.
-}
subpath : StartingPoint -> CloseOption -> List Instruction -> Subpath
subpath =
    Subpath


{-| Starting point of a subpath.
-}
type StartingPoint
    = StartingPoint Instruction


{-| Start a subpath at the absolute coordinates `(x, y)`.
-}
startAt : Point -> StartingPoint
startAt =
    StartingPoint << MoveAbsolute


{-| Start a subpath at the location given by
moving the current location by `(dx, dy)`.
-}
moveBy : Point -> StartingPoint
moveBy =
    StartingPoint << MoveRelative


{-| Close the subpath or not.
-}
type CloseOption
    = CloseOption Bool


{-| Create a closed subpath. After the final
instruction, a line will be drawn from the current
point to the starting point.
-}
closed : CloseOption
closed =
    CloseOption True


{-| Create an open path.
-}
open : CloseOption
open =
    CloseOption False


{-| What direction to pick. Also called "sweep flag".
-}
type alias Direction =
    Internal.Direction


{-| What arc to pick.
-}
type alias ArcFlag =
    Internal.ArcFlag


{-| A single SVG instruction
-}
type alias Instruction =
    Internal.Instruction


{-| Convert an instruction to a string.

    segmentToString (Just 2) (moveTo ( 20.001, 20 ))
        == "M20,20"
    segmentToString Nothing close == "Z"
-}
instructionToString : Maybe Int -> Instruction -> String
instructionToString =
    Internal.formatInstruction


{-| Convert a list of segments to string.

    segmentsToString Nothing
        [ moveTo ( 20, 20 )
        , lineTo ( 20, 40 )
        , close
        ]
            == "M20,20 L20,40 Z"
-}
instructionsToString : Maybe Int -> List Instruction -> String
instructionsToString maxNumOfDecimals =
    String.join " " << List.map (Internal.formatInstruction maxNumOfDecimals)


{-| Helper to convert a list of segments directly to an SVG attribute.
-}
instructionsToAttribute : Maybe Int -> List Instruction -> Svg.Attribute msg
instructionsToAttribute maxNumOfDecimals =
    Svg.Attributes.d << instructionsToString maxNumOfDecimals


{-| A 2-tuple of Floats. Used to store a `(x, y)` absolute or `(dx, dy)` relative coordinate.
-}
type alias Point =
    ( Float, Float )


{-| Draw a line from the current cursor position to a point in 2D space.

The code below draws a cube using absolute coordinates

    cubeAbsolute =
        subpath (startAt (0, 0)) open <|
            [ lineTo ( 40, 20 )
            , lineTo ( 40, 40 )
            , lineTo ( 20, 40 )
            , lineTo ( 20, 20 )
            ]
-}
lineTo : Point -> Instruction
lineTo =
    LineAbsolute


{-| Join many `lineTo`s into one instruction.

    [ lineTo (20, 20), lineTo (40, 40)  ]
    [ lineToMany [ (20, 20), (40, 40) ] ]
    -- will respectively produce
    "L20,20 L40,40"
    "L20,20  40,40"

Using `lineToMany` is convenient, more efficient and produces
shorter SVG.
-}
lineToMany : List Point -> Instruction
lineToMany =
    LineAbsoluteMany


{-| Draw a line from the current cursor position to a position relative
to the current position.

The code below draws a cube using absolute coordinates

    cubeRelative =
        subpath (startAt (0, 0)) open <|
            [ lineBy ( 20, 0 )
            , lineBy ( 0, 20 )
            , lineBy ( -20, 0 )
            , lineBy ( 0, -20 )
            ]
-}
lineBy : Point -> Instruction
lineBy =
    LineRelative


{-| Relative version of `lineToMany`.
-}
lineByMany : List Point -> Instruction
lineByMany =
    LineRelativeMany


{-| Draw a straight line from the current cursor position to the given y coordinate.
-}
verticalTo : Float -> Instruction
verticalTo =
    VerticalAbsolute


{-| Draw a straight vertical line from the current cursor position of the given length.
-}
verticalBy : Float -> Instruction
verticalBy =
    VerticalRelative


{-| Draw a straight line from the current cursor position to the given x coordinate.
-}
horizontalTo : Float -> Instruction
horizontalTo =
    HorizontalAbsolute


{-| Draw a straight horizontal line from the current cursor position of the given length.
-}
horizontalBy : Float -> Instruction
horizontalBy =
    HorizontalRelative


{-|
    subpath (startAt ( 100, 100 )) closed <|
        [ arcTo ( 50, 70 ) 0
                ( largestArc, clockwise ) ( 200, 100 )
        ]


Produces `M100,100 A50,70 0 1,1 200,100 Z` which displays as
<svg width="100%" height="100px" id="svgcontext">
    <path           d="M100,100 A50,70 0 1,1 200,100 Z" fill="none" stroke="black" stroke-width="2"></path>
</svg>

-}
arcTo : Point -> Float -> ( ArcFlag, Direction ) -> Point -> Instruction
arcTo radius xstartangle ( largeArcFlag, sweepFlag ) point =
    ArcTo radius xstartangle ( largeArcFlag, sweepFlag ) point


{-| Relative version of `arcTo`.
-}
arcBy : Point -> Float -> ( ArcFlag, Direction ) -> Point -> Instruction
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


{-| Pick the arc with the greatest length.
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
close : Instruction
close =
    ClosePath



-- curve


{-| Draw a quadratic curve from the current cursor position to the
-}
quadraticTo : Point -> Point -> Instruction
quadraticTo control point =
    QuadraticAbsolute control point


{-|
-}
quadraticBy : Point -> Point -> Instruction
quadraticBy dcontrol dpoint =
    QuadraticRelative dcontrol dpoint


{-|
-}
quadraticToMany : Point -> Point -> List CurveContinuation -> Instruction
quadraticToMany =
    QuadraticAbsoluteMany


{-|
-}
quadraticByMany : Point -> Point -> List CurveContinuation -> Instruction
quadraticByMany =
    QuadraticRelativeMany


{-|
-}
cubicTo : Point -> Point -> Point -> Instruction
cubicTo control1 control2 point =
    CubicAbsolute control1 control2 point


{-|
-}
cubicBy : Point -> Point -> Point -> Instruction
cubicBy dcontrol1 dcontrol2 dpoint =
    CubicRelative dcontrol1 dcontrol2 dpoint


{-|
-}
cubicToMany : Point -> Point -> Point -> List CurveContinuation -> Instruction
cubicToMany =
    CubicAbsoluteMany


{-|
-}
cubicByMany : Point -> Point -> Point -> List CurveContinuation -> Instruction
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
