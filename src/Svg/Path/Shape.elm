module Svg.Path.Shape exposing (..)

import Svg
import Svg.Path.Point as Point exposing (Point)
import Svg.Path.Subpath as Subpath exposing (Subpath, subpath)
import Svg.Path exposing (startAt, lineTo, closed, open)


type alias Shape msg =
    { subpaths : List Subpath
    , origin : Point
    , direction : Point
    , finalPoint : Maybe Point
    , attributes : List (Svg.Attribute msg)
    }


line : Point -> Shape msg
line ( x, y ) =
    { subpaths = [ subpath (startAt ( 0, 0 )) open [ lineTo ( x, y ) ] ]
    , origin = ( 0, 0 )
    , direction = Point.normalize ( x, y )
    , finalPoint = Nothing
    , attributes = []
    }


{-| a /\ triangle
-}
triangle : Float -> Float -> Shape msg
triangle width height =
    { subpaths = [ subpath (startAt ( 0, 0 )) closed [ lineTo ( -(width / 2), 0 ), lineTo ( 0, height ), lineTo ( width / 2, 0 ) ] ]
    , origin = ( 0, 0 )
    , direction = ( 0, 1 )
    , finalPoint = Just ( 0, height )
    , attributes = []
    }


concat : Shape msg -> Shape msg -> Shape msg
concat shape1 shape2 =
    { shape2 | subpaths = shape1.subpaths ++ shape2.subpaths }
