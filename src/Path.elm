module Path exposing (..)

import Internal exposing (..)


path : Path
path =
    []


cons : Segment -> Path -> Path
cons segment path =
    case path of
        [] ->
            [ segment ]

        x :: xs ->
            -- TODO implement all optimization cases
            case ( x, segment ) of
                ( MoveTo Absolute old, MoveTo Absolute new ) ->
                    MoveTo Absolute (old ++ new) :: xs

                _ ->
                    segment :: x :: xs


moveTo : Mode -> Point -> Path -> Path
moveTo mode point =
    MoveTo mode [ point ]
        |> cons


close : Path -> Path
close =
    cons ClosePath


lineTo : Mode -> Point -> Path -> Path
lineTo mode point =
    LineTo mode [ point ]
        |> cons


vertical : Mode -> Float -> Path -> Path
vertical mode amount =
    Vertical mode amount
        |> cons


horizontal : Mode -> Float -> Path -> Path
horizontal mode amount =
    Horizontal mode amount
        |> cons


lineToAbs =
    lineTo Absolute


moveToAbs : Point -> Path -> Path
moveToAbs =
    moveTo Absolute


moveToRel : Point -> Path -> Path
moveToRel =
    moveTo Relative


verticalRel : Float -> Path -> Path
verticalRel =
    vertical Relative


horizontalRel : Float -> Path -> Path
horizontalRel =
    horizontal Relative


arcTo : Point -> Float -> ( ArcFlag, SweepFlag ) -> Point -> Path -> Path
arcTo ( rx, ry ) xAxisRotate ( arcFlag, sweepFlag ) ( x, y ) =
    ArcTo ( rx, ry ) xAxisRotate ( arcFlag, sweepFlag ) ( x, y )
        |> cons



{-
   arc : Point -> Point -> Float -> Path
   arc ( x1, y1 ) ( x2, y2 ) radius =
       let
           -- TODO: Figure out how this actually works and write a lot of comments/refactor.
           -- Currently this is a straight port from D3.
           r =
               abs radius

           ( x0', y0 ) =
               ( x1, y1 )

           x21 =
               x2' - x1'

           y21 =
               y2' - y1'

           x01 =
               x0' - x1'

           y01 =
               y0' - y1'

           l01_2 =
               x01 ^ 2 + y01 ^ 2

           x20 =
               x2' - x0'

           y20 =
               y2' - y0'

           l21_2 =
               x21 ^ 2 + y21 ^ 2

           l20_2 =
               x20 ^ 2 + y20 ^ 2

           l21 =
               sqrt l21_2

           l01 =
               sqrt l01_2

           l =
               r * tan ((pi - acos ((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2)

           t01 =
               l / l01

           t21 =
               l / l21

           str' =
               if abs (t01 - 1) > epsilon then
                   append "L" [ x1' + t01 * x01, y1' + t01 * y01 ] str
               else
                   str
       in
           if empty then
               ( append "M" [ x1', y1' ] str, x0, y0, x1', y1', False )
           else if l01_2 < epsilon then
               ( str, x0, y0, x1, y1, empty )
               -- do nothing
           else if not (abs (y01 * x21 - y21 * x01) > epsilon) || r == 0 then
               ( append "L" [ x1', y1' ] str, x0, y0, x1', y1', False )
           else
               ( append "A"
                   [ r
                   , r
                   , 0
                   , 0
                   , (if y01 * x20 > x01 * y20 then
                       1
                      else
                       0
                     )
                   , x1' + t21 * x21
                   , y1' + t21 * y21
                   ]
                   str'
               , x0
               , y0
               , x1' + t21 * x21
               , y1' + t21 * y21
               , False
               )
-}


rectangle : Point -> Point -> Path
rectangle ( x, y ) ( dx, dy ) =
    path
        |> moveToAbs ( x, y )
        |> verticalRel dy
        |> horizontalRel -dx
        |> close


triangle : Point -> Point -> Path
triangle ( x, y ) ( dx, dy ) =
    path
        |> moveToAbs ( x, y )
        |> moveToAbs ( x + dx, y )
        |> moveToAbs ( x, y + dy )
        |> close
