module Svg.Geometry exposing (..)

import Svg.Path exposing (..)


{-| Type-safe shapes based on SVG paths
-}
line : Point -> Point -> Subpath
line from to =
    subpath (startAt from) open [ lineTo to ]


{-| The ‘polyline’ element defines a set of connected straight line segments. Typically, ‘polyline’ elements define open shapes.
-}
polyline : List Point -> Subpath
polyline points =
    case points of
        [] ->
            emptySubpath

        x :: xs ->
            subpath (startAt x) open (List.map lineTo xs)


{-| The ‘polygon’ element defines a closed shape consisting of a set of connected straight line segments.
-}
polygon : List Point -> Subpath
polygon points =
    case points of
        [] ->
            emptySubpath

        x :: xs ->
            subpath (startAt x) closed (List.map lineTo xs)


circle : Float -> Point -> Subpath
circle radius point =
    ellipse ( radius, radius ) point


ellipse : ( Float, Float ) -> Point -> Subpath
ellipse ( rx, ry ) ( x, y ) =
    subpath (startAt ( x - rx, y )) open
        <| [ arcTo ( rx, ry ) 0 ( largestArc, clockwise ) ( x + rx, y )
           , arcTo ( rx, ry ) 0 ( largestArc, clockwise ) ( x - rx, y )
           ]


square : Float -> Point -> Subpath
square length ( x, y ) =
    rect length length Nothing Nothing ( x, y )


{-| Point is the lower-left corner
-}
rect : Float -> Float -> Maybe Float -> Maybe Float -> Point -> Subpath
rect width height mrx mry ( x, y ) =
    let
        ( rx, ry ) =
            let
                ( vx, vy ) =
                    case ( mrx, mry ) of
                        ( Nothing, Nothing ) ->
                            ( 0, 0 )

                        ( Just v, Nothing ) ->
                            ( v, v )

                        ( Nothing, Just v ) ->
                            ( v, v )

                        ( Just v1, Just v2 ) ->
                            ( v1, v2 )
            in
                ( min vx (0.5 * width)
                , min vx (0.5 * height)
                )

        rectArcTo goal =
            arcTo ( rx, ry ) 0 ( smallestArc, clockwise ) goal
    in
        subpath (startAt ( x + rx, y )) closed
            <| [ horizontalTo (x + width - rx)
               , rectArcTo ( x + width, y + ry )
               , verticalTo (y + height - ry)
               , rectArcTo ( x + width - rx, y + height )
               , horizontalTo (x + rx)
               , rectArcTo ( x, y + height - ry )
               , verticalTo (y + ry)
               , rectArcTo ( x + ry, y )
               ]


triangle : Float -> Float -> Point -> Subpath
triangle base height center =
    Debug.crash " "


ngon : Int -> Float -> Float -> Point -> Subpath
ngon sides radius rotation ( x, y ) =
    let
        part =
            Basics.pi * 2 / toFloat sides

        makeTri i =
            ( x + radius * sin (part * i + rotation)
            , y + radius * cos (part * i + rotation)
            )

        points =
            List.map (makeTri << toFloat) [0..sides - 1]
    in
        polygon points


type Diagram
    = Diagram (Point -> ( Point, Path ))


line' : Point -> Diagram
line' to =
    Diagram
        (\from ->
            ( to, [ subpath (startAt from) open [ lineTo to ] ] )
        )


sameOrigin : Diagram -> Diagram -> Diagram
sameOrigin (Diagram f) (Diagram g) =
    Diagram
        (\start ->
            let
                ( newStart, accum1 ) =
                    f start

                ( newerStart, accum2 ) =
                    g start
            in
                ( newerStart, accum1 ++ accum2 )
        )


compose : Diagram -> Diagram -> Diagram
compose (Diagram f) (Diagram g) =
    Diagram
        (\start ->
            let
                ( newStart, accum1 ) =
                    f start

                ( newerStart, accum2 ) =
                    g newStart
            in
                ( newerStart, accum1 ++ accum2 )
        )


runDiagram : Point -> Diagram -> Path
runDiagram start (Diagram transform) =
    transform start
        |> snd
