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


type Mode
    = Relative
    | Absolute


type Segment
    = ClosePath
      -- moveTo and lineTo often take multiple points
      -- express as a list to optimize
    | MoveTo Mode (List Point)
    | LineTo Mode (List Point)
    | Vertical Mode Float
    | Horizontal Mode Float
    | ArcTo Point Float ( ArcFlag, SweepFlag ) Point


joinPoints =
    -- List.foldl (\elem accum -> accum ++ formatPoint elem) ""
    -- optimize this again, beware of trailing/preceding spaces
    List.map formatPoint
        >> String.join " "


formatSegment segment =
    case segment of
        ClosePath ->
            "Z"

        MoveTo Absolute points ->
            "M" ++ joinPoints points

        MoveTo Relative points ->
            "m" ++ joinPoints points

        LineTo Absolute points ->
            "L" ++ joinPoints points

        LineTo Relative points ->
            "l" ++ joinPoints points

        Vertical Absolute y ->
            "V" ++ toString y

        Vertical Relative dy ->
            "v" ++ toString dy

        Horizontal Absolute x ->
            "H" ++ toString x

        Horizontal Relative dx ->
            "h" ++ toString dx

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
clockwise (1) or anti-clockwise (0) direction
-}
type SweepFlag
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


draw : Path -> Svg.Attribute msg
draw segments =
    segments
        |> Debug.log "segments"
        |> List.reverse
        |> List.map formatSegment
        |> String.join " "
        |> Svg.Attributes.d


{-| Paramorphism that moves from left to the right.
The `List a` argument in the per-element function is
a list of items that have not yet been visited.
-}
paral : (a -> List a -> b -> b) -> b -> List a -> b
paral func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            paral func (func x xs acc) xs


{-| joins two segments of the same kind into one
-}
optimize : List String -> List String
optimize =
    identity
