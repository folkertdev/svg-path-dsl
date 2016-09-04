module Main exposing (..)

import Monadic exposing (..)
import Internal exposing (..)
import Svg
import Svg.Attributes exposing (..)


empty : Diagram (List Segment)
empty =
    fromFunction (\state -> ( [], state ))


myPath =
    empty
        |> moveAbs ( 100, 100 )
        |> moveAbs ( 200, 200 )
        |> verticalAbs 2


diagram : Diagram String
diagram =
    fromFunction
        (\state ->
            ( "m0,0 20,10 20,20", { state | current = ( fst state.current + 20, snd state.current + 20 ) } )
        )


dia =
    diagram
        |> concatenate diagram
        |> concatenate diagram


mozilla =
    empty
        |> moveAbs ( 37, 17 )
        |> verticalRel 15
        |> horizontalAbs 14
        |> verticalAbs 17
        |> horizontalAbs 37
        |> close
        |> moveAbs ( 50, 0 )
        |> horizontalAbs 0
        |> verticalRel 50
        |> horizontalRel 50
        |> verticalAbs 0
        |> close
        |> segmentsToString
        |> finalValue emptyDrawState


differentOrigin =
    { current = ( 128, 128 ), initial = ( 128, 128 ), empty = True }


pathWithDifferentOrigin : String
pathWithDifferentOrigin =
    empty
        |> moveAbs ( 100, 100 )
        |> moveAbs ( 200, 200 )
        |> verticalAbs 2
        |> moveAbs ( 0, 2 )
        |> segmentsToString
        |> finalValue differentOrigin


main =
    Svg.svg [ width "400", height "400", viewBox "0 0 400 400" ]
        [ Svg.path
            [ color "red"
            , stroke "black"
            , strokeWidth "1"
            , fill "red"
              --, draw myPath2
            , d mozilla
            ]
            []
        ]



{-
   arcCustom : Point -> Float -> ( Float, Float ) -> SweepFlag -> Diagram (List Segment) -> Diagram (List Segment)
   arcCustom ( x, y ) radius angles direction (Diagram f) =
       Debug.crash "WIP"
-}


{-| Implementation of https://github.com/gampleman/elm-visualization/blob/master/src/Visualization/Path.elm#L259
with this api.

The main advantage here is that this is a function while the original implementation uses a type.
Usig a function means that this means that users of the library can create
custom shapes (like cube, triangle, or custom arcs / curves), instead of just the library author.
-}
arcCustomHelper : Point -> Float -> ( Float, Float ) -> SweepFlag -> DrawState -> List Segment
arcCustomHelper ( x, y ) radius ( a0, a1 ) direction state =
    let
        r =
            abs radius

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
