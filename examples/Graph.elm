module Graph exposing (main)

import Svg
import Svg.Attributes exposing (..)
import Svg.Path exposing (..)
import Svg.Path.Instruction as Instruction
import Svg.Path.Subpath as Subpath exposing (Subpath(..), CloseOption(..), StartingPoint(..))
import Svg.Path.Point as Point exposing (Point)


triangle width height =
    subpath (startAt ( 0, 0 )) closed [ lineTo ( 0, height / 2 ), lineTo ( width, 0 ), lineTo ( 0, -(height / 2) ) ]


line ( x, y ) =
    subpath (startAt ( 0, 0 )) open [ lineTo (Point.rotate (Point.angleWithX ( x, y )) ( Point.length ( x, y ), 0 )) ]


arrow ( x, y ) width height =
    line ( x - height, y )
        --|> andThen (triangle width height)
        |>
            andThen (line ( x - height, y ))


longLine p =
    line p
        |> andThen (line p)
        |> andThen (line p)
        |> andThen (line p)


emptyDrawState =
    { start = ( 0, 0 ), current = ( 0, 0 ), direction = ( 0, 0 ) }


andThen =
    flip compose


compose path1 path2 =
    let
        finalState =
            List.foldl Instruction.propagate emptyDrawState (Subpath.subpathToInstructions path1 [])
    in
        join path1 (Subpath.mapAbsolute (Point.rotate (Point.angleWithX finalState.direction) >> Point.add finalState.current) path2)


join (Subpath start1 (CloseOption close1) instructions1) (Subpath (StartingPoint start2) (CloseOption close2) instructions2) =
    Subpath.subpath start1 (CloseOption close2) <|
        instructions1
            ++ (if close1 then
                    [ toStart ]
                else
                    []
               )
            ++ [ start2 ]
            ++ instructions2


result =
    -- [ arrow ( 100, 100 ) 20 20 ]
    [ longLine ( 100, 100 ) ]


main =
    Svg.svg [ Svg.Attributes.width "100%", Svg.Attributes.height "500px" ]
        [ Svg.g [ transform "translate(70,70)" ]
            [ Svg.path [ d (pathToStringWithPrecision 2 result), fill "none", stroke "black" ] []
            ]
        ]
