module Monadic exposing (..)

import Internal exposing (..)
import Svg.Attributes


{-| An example using this module's api
-}
myPath =
    emptyDiagram
        |> move ( 100, 100 )
        |> move ( 200, 200 )
        |> asAttribute


asAttribute =
    Svg.Attributes.d << fst


move : Point -> ( String, DrawState ) -> ( String, DrawState )
move ( x, y ) =
    interpretSegment (MoveTo Absolute [ ( x, y ) ])
        |> andThen


type alias DrawState =
    { initial : Point
    , current : Point
    , empty : Bool
    }


emptyDrawState =
    { initial = ( 0, 0 ), current = ( 0, 0 ), empty = False }


emptyDiagram =
    ( "", emptyDrawState )


{-| fused fold to concat all elements + get the final element of a list.
-}
joinedAndLast current ( accumJoin, previous ) =
    ( accumJoin ++ " " ++ formatPoint current
    , Just current
    )


{-| andThen ( very much like state but joins the values with (++) instead of only keeping the most recent one)
-}
andThen : (DrawState -> ( appendable, DrawState )) -> ( appendable, DrawState ) -> ( appendable, DrawState )
andThen f ( accum, state ) =
    let
        ( new, newState ) =
            f state
    in
        ( accum ++ new, newState )



-- andThen << interpretSegment (MoveTo Absolute [ ( x, y ) ])


interpretMany : List Segment -> DrawState -> ( String, DrawState )
interpretMany segments state =
    List.foldl (andThen << interpretSegment) ( "", state ) segments


{-| Convert a segment to a string and update the current state
(mostly setting the current cursor point)
interpretSegment : Segment -> DrawState -> ( String, DrawState )
-}
interpretSegment segment state =
    case segment of
        MoveTo Absolute points ->
            case List.foldl joinedAndLast ( "", Nothing ) points of
                ( joined, Just last ) ->
                    ( "M" ++ joined
                    , { state | current = last }
                    )

                _ ->
                    ( "", state )

        MoveTo Relative points ->
            case List.foldl joinedAndLast ( "", Nothing ) points of
                ( joined, Just last ) ->
                    ( "m" ++ joined
                    , { state | current = last }
                    )

                _ ->
                    ( "", state )

        _ ->
            ( "", state )


{-| Implementation of https://github.com/gampleman/elm-visualization/blob/master/src/Visualization/Path.elm#L259
with this api.

The main advantage here is that this is a function while the original implementation uses a type.
Usig a function means that this means that users of the library can create
custom shapes (like cube, triangle, or custom arcs / curves), instead of just the library author.
-}
arcCustom : Point -> Float -> ( Float, Float ) -> SweepFlag -> DrawState -> Path
arcCustom ( x, y ) radius ( a0, a1 ) direction state =
    let
        r =
            abs radius

        ( dx, dy ) =
            ( r * cos a0
            , r * sin a0
            )

        ( x0', y0' ) =
            ( x + dx
            , y + dy
            )

        da =
            case direction of
                AntiClockwise ->
                    a0 - a1

                Clockwise ->
                    a1 - a0

        ( x1, y1 ) =
            state.current
    in
        if r == 0 then
            -- Is this arc empty? We’re done.
            if state.empty then
                [ MoveTo Absolute [ ( x0', y0' ) ] ]
            else if abs (x1 - x0') > epsilon || abs (y1 - y0') > epsilon then
                [ LineTo Absolute [ ( x0', y0' ) ] ]
            else
                []
        else if da > (tau - epsilon) then
            -- Is this a complete circle? Draw two arcs to complete the circle.
            [ ArcTo ( r, r ) 0 ( Largest, direction ) ( x - dx, y - dy )
            , ArcTo ( r, r ) 0 ( Largest, direction ) ( x0', y0' )
            ]
        else
            let
                da' =
                    if da < 0 then
                        (mod da tau) + tau
                    else
                        da

                largestFlag =
                    if da' >= pi then
                        Largest
                    else
                        Smallest

                point =
                    ( x + r * cos a1, y + r * sin a1 )
            in
                -- Otherwise, draw an arc!
                [ ArcTo ( r, r ) 0 ( largestFlag, direction ) point ]


epsilon =
    1.0e-6


tau =
    2 * pi


mod : Float -> Float -> Float
mod a b =
    let
        frac =
            a / b
    in
        (frac - toFloat (truncate frac)) * b
