module Svg.Path.Point exposing (..)


type alias Point =
    ( Float, Float )


add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


negate ( x, y ) =
    ( -x, -y )


sub a b =
    add a (negate b)


point x y =
    ( x, y )


mul ( x, y ) n =
    ( n * x, n * y )


div p n =
    if n == 0 then
        point 0 0
    else
        mul p (1 / n)


length ( x, y ) =
    sqrt (x * x + y * y)


normalize v =
    div v (length v)


direction x y =
    let
        _ =
            Debug.log "x, y" ( x, y )
    in
        normalize (sub x y)
            |> Debug.log "the direction"


rotate angle ( x, y ) =
    let
        ( cs, sn ) =
            ( cos angle, sin angle )
    in
        ( x * cs - y * sn
        , x * sn + y * cs
        )


angleWithX ( x, y ) =
    let
        _ =
            Debug.log "x, y" ( x, y )

        partial =
            if x == 0 then
                0
            else
                atan <| Debug.log "y / x" (y / x)
    in
        if partial < 0 then
            pi + partial
        else
            partial
