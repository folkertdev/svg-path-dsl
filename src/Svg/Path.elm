module Svg.Path
    exposing
        ( Path
        , Subpath
        , subpath
        , emptySubpath
        , Instruction
        , Point
        , StartingPoint
        , CloseOption
        , CurveContinuation
        , closed
        , open
          -- conversion
        , pathToString
        , pathToStringWithPrecision
        , pathToAttribute
          -- close (Z)
        , toStart
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

{-| A domain-specific language for SVG paths.

The svg coordinate system has its origin in the upper-left corner with the positive x-axis pointing to the right and
the positive y-axis pointing down. A much more in-depth explanation of the svg coordinate system and how to manipulate it
can be found [here](https://sarasoueidan.com/blog/svg-coordinate-systems/).

The path syntax has absolute and relative instructions.
* **Absolute instructions have a `*To` suffix** and move to absolute coordinates, so
`lineTo (20, 20)` draws a line from the current position to the location `(20, 20)`.
* **Relative instructions have a `*By` suffix** and move the current position, so `lineBy (20, 20)` draws a line from the current position `(cy, cy)` to
the location `(cx + 20, cy + 20)`.


For the curious, here are some resources to learn more about
 [`<path>` elements](https://developer.mozilla.org/en/docs/Web/SVG/Tutorial/Paths)
 and the [`d` attribute syntax](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d).


#Path
@docs Path, Subpath, subpath, emptySubpath, Instruction, Point

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
Arcs are segments of ellipses. The arc command describes an ellipse and what segment of that ellipse to draw.

The ellipse is described by the first, second and final argument.
The first argument is an (rx, ry) pair - the horizontal and vertical radii of the ellipse. The second argument `xAxisRotate` rotates
the ellipse around its center by a number of degrees. The final argument is the center of the ellips.

The remaining argument is a pair of flags that select the part of the ellipse to draw.
For an interactive visual demo, see [http://codepen.io/lingtalfi/pen/yaLWJG](http://codepen.io/lingtalfi/pen/yaLWJG).


@docs arcTo, arcBy
##arc size
@docs ArcFlag, largestArc, smallestArc
##direction
@docs Direction, clockwise, antiClockwise

#Close
@docs toStart

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
@docs CurveContinuation
@docs quadraticContinueTo, quadraticContinueBy, cubicContinueTo, cubicContinueBy
-}

import Svg.Path.Internal as Internal
    exposing
        ( Instruction(..)
        , CurveContinuation(..)
        , Direction(..)
        , ArcFlag(..)
        , CloseOption(..)
        , StartingPoint(..)
        , Subpath
        , subpathToInstructions
        )
import String
import Svg
import Svg.Attributes


{-| A path is a list of subpaths.
-}
type alias Path =
    List Subpath


{-| Starting point of a subpath.
-}
type alias StartingPoint =
    Internal.StartingPoint


{-| Close the subpath or not.
-}
type alias CloseOption =
    Internal.CloseOption


{-| Convert a path into a string. Ready to use as argument to `Svg.Attributes.d`.

    Svg.path
        [ d (pathToString myPath)
        , stroke "black"
        , fill "none"
        ]
        []
-}
pathToString : Path -> String
pathToString path =
    instructionsToString Nothing (List.foldr subpathToInstructions [] path)


{-| Convert a path into an svg attribute. Use it like

    Svg.path
        [ pathToAttribute myPath
        , stroke "black"
        , fill "none"
        ]
        []
-}
pathToAttribute : Path -> Svg.Attribute msg
pathToAttribute path =
    instructionsToString Nothing (List.foldr subpathToInstructions [] path)
        |> Svg.Attributes.d


{-| Convert a path into a string. Ready to use as argument to `Svg.Attributes.d`.
The first argument gives the maximum number of decimal places any number in the output will have.
-}
pathToStringWithPrecision : Int -> Path -> String
pathToStringWithPrecision decimalPlaces path =
    instructionsToString (Just decimalPlaces) (List.foldr subpathToInstructions [] path)


{-| A subpath is a list of svg instructions with a starting point and a closing option.
-}
type alias Subpath =
    Internal.Subpath


{-| Construct a subpath from a starting point (`startAt (x, y)` or `moveBy (dx, dy)`), a closing option (`closed` or `open`)
and a list of instructions.
-}
subpath : StartingPoint -> CloseOption -> List Instruction -> Subpath
subpath =
    Internal.subpath


{-| An empty subpath
-}
emptySubpath : Subpath
emptySubpath =
    subpath (moveBy ( 0, 0 )) open []


{-| Start a subpath at the absolute coordinates `(x, y)`.
-}
startAt : Point -> StartingPoint
startAt =
    StartingPoint << MoveAbsolute


{-| Start a subpath at the location given by
moving the current location by `(dx, dy)`.

when the current cursor position is `(10, 10)`, the subpath

    subpath (moveBy (5, 10)) open [ ... ]

will start at the position `(15, 20)`.
-}
moveBy : Point -> StartingPoint
moveBy =
    StartingPoint << MoveRelative


{-| Create a closed subpath. After the final
instruction, a line will be drawn from the current
point to the starting point of the subpath.

    subpath (startAt ( 0, 0 )) closed <|
        [ lineTo ( 100, 0 )
        , lineTo ( 150, 100 )
        , lineTo ( 50, 100 )
        ]

yields

<svg width="100%" height="120px" id="svgcontext">
    <g transform="translate(10, 10)">
        <path d="M0,0 L100,0 L150,100 L50,100 Z" fill="red" stroke="black" stroke-width="5"></path>
    </g>
</svg>
-}
closed : CloseOption
closed =
    Internal.CloseOption True


{-| Create an open path.

    subpath (startAt ( 0, 0 )) open <|
        [ lineTo ( 100, 0 )
        , lineTo ( 150, 100 )
        , lineTo ( 50, 100 )
        ]

yields

<svg width="100%" height="120px" id="svgcontext">
    <g transform="translate(10, 10)">
        <path d="M0,0 L100,0 L150,100 L50,100" fill="red" stroke="black" stroke-width="5"></path>
    </g>
</svg>
-}
open : CloseOption
open =
    Internal.CloseOption False


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

    segmentToString (Just 2) (lineTo ( 20.001, 20 ))
        == "L20,20"
    segmentToString Nothing close == "Z"
-}
instructionToString : Maybe Int -> Instruction -> String
instructionToString =
    Internal.formatInstruction


{-| Convert a list of segments to string.

    segmentsToString Nothing
        [ lineTo ( 20, 40 )
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

Using `lineToMany` is convenient, more efficient, and produces
shorter SVG.
-}
lineToMany : List Point -> Instruction
lineToMany =
    LineAbsoluteMany


{-| Draw a line from the current cursor position to a position relative
to the current position.

The code below draws a cube using relative coordinates

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
    [arcTo (50, 70) 0 (largestArc, clockwise) (200, 100)]
        |> subpath (startAt ( 100, 100 )) open

Produces `M100,100 A50,70 0 1,1 200,100` which displays as
<svg width="100%" height="120px" id="svgcontext">
    <g transform="translate(10, 10)">
        <path d="M100,100 A50,70 0 1,1 200,100" fill="none" stroke="black" stroke-width="2"></path>
    </g>
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


{-| Draws a line from the current position to the starting point of the subpath.

    subpath (startAt (0, 0)) open <|
        [ lineTo (100, 0)
        , lineTo (150, 100)
        , lineTo (50, 100)
        ]

Yields

<svg width="100%" height="120px" id="svgcontext">
    <g transform="translate(10, 10)">
        <path d="M0,0 L100,0 L150,100 L50,100" fill="red" stroke="black" stroke-width="5"></path>
    </g>
</svg>

    subpath (startAt (0, 0)) open <|
        [ lineTo (100, 0)
        , lineTo (150, 100)
        , toStart
        , lineTo (50, 100)
        ]

Yields

<svg width="100%" height="120px" id="svgcontext">
    <g transform="translate(10, 10)">
        <path d="M0,0 L100,0 L150,100 Z L50,100" fill="red" stroke="black" stroke-width="5"></path>
    </g>
</svg>

After drawing a line to the second point, toStart draws a line to the initial point of the subpath and
then continues drawing.
-}
toStart : Instruction
toStart =
    ClosePath



-- curve


{-| Draws a curve between the current and the goal point. The position
of the control point determines the path of the curve.

    subpath (startAt (100, 0)) open
        <| [ quadraticTo (0, 0) (0, 100) ]
Yields
<svg width="120px" height="120px" id="svgcontext">
    <g transform="translate(10, 10)">
        <path d="M100,0 Q0,0 0,100" fill="none" stroke="black" stroke-width="5"></path>
    </g>
</svg>
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


{-| Extension of a curve by one point
-}
type alias CurveContinuation =
    Internal.CurveContinuation


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


    [ ( 180, 40 ) , ( 265, 40 ) ]
        |> List.map quadraticContinueTo
        |> quadraticToMany ( 52.5, 100 ) ( 95, 40 )
        |> (\x -> [ x ]) -- make list of instructions
        |> subpath (startAt (10, 40)) open


Produces `"M10,40  Q52.5,100  95,40 T180,40 T265,40"` and displays as

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
