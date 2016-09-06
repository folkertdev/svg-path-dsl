module Internal exposing (..)

import String
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| A sequence of segments. New elements are added at the front, so the internal
representation is reversed.
-}
type alias Path =
    List Segment


type alias Point =
    ( Float, Float )


type Segment
    = ClosePath
      -- move (*many versions for optimization)
    | MoveAbsolute Point
    | MoveAbsoluteMany (List Point)
    | MoveRelative Point
    | MoveRelativeMany (List Point)
      -- line
    | LineAbsolute Point
    | LineAbsoluteMany (List Point)
    | LineRelative Point
    | LineRelativeMany (List Point)
      -- one-directional line
    | VerticalAbsolute Float
    | VerticalRelative Float
    | HorizontalAbsolute Float
    | HorizontalRelative Float
      -- curves
    | QuadraticAbsolute Point Point
    | QuadraticRelative Point Point
    | QuadraticNextAbsolute Point
    | QuadraticNextRelative Point
    | CubicAbsolute Point Point Point
    | CubicRelative Point Point Point
    | CubicNextAbsolute Point Point
    | CubicNextRelative Point Point
      -- arcs
    | ArcTo Point Float ( ArcFlag, Direction ) Point


formatPoints =
    -- List.foldl (\elem accum -> accum ++ formatPoint elem) ""
    -- optimize this again, beware of trailing/preceding spaces
    List.map formatPoint
        >> String.join " "


formatSegment segment =
    case segment of
        ClosePath ->
            "Z"

        MoveAbsolute point ->
            "M" ++ formatPoint point

        MoveAbsoluteMany points ->
            "M" ++ formatPoints points

        MoveRelative point ->
            "m" ++ formatPoint point

        MoveRelativeMany points ->
            "m" ++ formatPoints points

        LineAbsolute point ->
            "L" ++ formatPoint point

        LineAbsoluteMany points ->
            "L" ++ formatPoints points

        LineRelative point ->
            "l" ++ formatPoint point

        LineRelativeMany points ->
            "l" ++ formatPoints points

        VerticalAbsolute y ->
            "V" ++ toString y

        VerticalRelative dy ->
            "v" ++ toString dy

        HorizontalAbsolute x ->
            "H" ++ toString x

        HorizontalRelative dx ->
            "h" ++ toString dx

        QuadraticAbsolute ( cx, cy ) ( x, y ) ->
            "Q" ++ formatPoint ( cx, cy ) ++ " " ++ formatPoint ( x, y )

        QuadraticRelative ( dcx, dcy ) ( dx, dy ) ->
            "q" ++ formatPoint ( dcx, dcy ) ++ " " ++ formatPoint ( dx, dy )

        CubicAbsolute c1 c2 point ->
            -- note that cubic == quadratic iff c1 == c2. Maybe optimize that?
            "C" ++ String.join " " (List.map formatPoint [ c1, c2, point ])

        CubicRelative dc1 dc2 dpoint ->
            "c" ++ String.join " " (List.map formatPoint [ dc1, dc2, dpoint ])

        QuadraticNextAbsolute point ->
            "T" ++ formatPoint point

        QuadraticNextRelative dpoint ->
            "t" ++ formatPoint dpoint

        CubicNextAbsolute c point ->
            "S" ++ formatPoints [ c, point ]

        CubicNextRelative dc dpoint ->
            "s" ++ formatPoints [ dc, dpoint ]

        ArcTo ( rx, ry ) xAxisRotate ( arcFlag, sweepFlag ) ( x, y ) ->
            let
                arc : Int
                arc =
                    case arcFlag of
                        Smallest ->
                            0

                        Largest ->
                            1

                sweep : Int
                sweep =
                    case sweepFlag of
                        AntiClockwise ->
                            0

                        Clockwise ->
                            1
            in
                String.concat [ "A", formatPoint ( rx, ry ), toString xAxisRotate, " " ++ toString arc ++ "," ++ toString sweep ++ " ", formatPoint ( x, y ) ]


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


points =
    [ ( 2, 2 ), ( 4, 4 ), ( 5, 5 ), ( 50, 50 ) ]


concat : Segment -> Segment -> List Segment
concat instrA instrB =
    [ instrA, instrB ]


formatPoint : Point -> String
formatPoint ( x, y ) =
    toString x ++ "," ++ toString y ++ " "
