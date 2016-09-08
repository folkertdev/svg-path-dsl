module Arc exposing (..)

{-| Interactive demo of the `arc` instruction

port of http://codepen.io/lingtalfi/pen/yaLWJG
-}

import String
import Svg exposing (Svg)
import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg.Attributes exposing (color, stroke, cx, cy, r, x1, x2, y1, y2, strokeWidth, fill)
import Svg.Path exposing (..)
import Json.Decode as Json
import Mouse exposing (Position)


main =
    Html.App.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Point =
    ( Float, Float )


type alias Model =
    { radius : Point
    , rotation : Float
    , largeArcFlag : ArcFlag
    , sweepFlag : Direction
    , locationA : Point
    , locationB : Point
    , drag : Maybe Drag
    }


model =
    { radius = ( 100, 100 )
    , rotation = 0
    , largeArcFlag = smallestArc
    , sweepFlag = antiClockwise
    , locationA = ( 100, 250 )
    , locationB = ( 200, 250 )
    , drag = Nothing
    }


type alias Drag =
    { node : Node, lstart : Point, start : Position, current : Position }


type Msg
    = Setter (Model -> Model)
    | DragStart Node Position
    | DragAt Position
    | DragEnd Position


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


type Node
    = A
    | B


update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper msg model =
    case msg of
        Setter f ->
            f model

        DragStart n xy ->
            case n of
                A ->
                    { model | drag = Just (Drag n model.locationA xy xy) }

                B ->
                    { model | drag = Just (Drag n model.locationB xy xy) }

        DragAt pos ->
            case model.drag of
                Just { node, lstart, start, current } ->
                    case node of
                        A ->
                            { model
                                | locationA = add lstart (subtract pos start)
                                , drag = Just <| Drag node lstart start pos
                            }

                        B ->
                            { model
                                | locationB = add lstart (subtract pos start)
                                , drag = Just <| Drag node lstart start pos
                            }

                Nothing ->
                    model

        DragEnd _ ->
            { model | drag = Nothing }


add : Point -> Position -> Point
add ( x, y ) delta =
    ( x + toFloat delta.x, y + toFloat delta.y )


subtract : Position -> Position -> Position
subtract a b =
    { x = a.x - b.x, y = a.y - b.y }


line : Point -> Point -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
line ( a, b ) ( c, d ) attributes children =
    Svg.line (x1 (toString a) :: x2 (toString c) :: y1 (toString b) :: y2 (toString d) :: attributes) children


circle : Point -> Float -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
circle ( x, y ) radius attributes children =
    Svg.circle (cx (toString x) :: cy (toString y) :: r (toString radius) :: attributes) children


view : Model -> Html Msg
view model =
    let
        arcColor arc sweep =
            if model.largeArcFlag == arc && model.sweepFlag == sweep then
                "red"
            else
                "green"

        makeArcPath start radius rotation ( arc, sweep ) goal =
            Svg.path
                [ Svg.Path.pathToAttribute
                    [ subpath (moveBy start) open <|
                        [ arcTo radius rotation ( arc, sweep ) goal ]
                    ]
                , stroke (arcColor arc sweep)
                , Svg.Attributes.id "arc"
                , strokeWidth "4"
                , fill "none"
                ]
                []

        variations =
            [ ( largestArc, clockwise )
            , ( smallestArc, clockwise )
            , ( largestArc, antiClockwise )
            , ( smallestArc, antiClockwise )
            ]

        arcs { locationA, radius, rotation, locationB } =
            List.map (\( arc, direction ) -> makeArcPath locationA radius rotation ( arc, direction ) locationB) variations

        ( lineEnd1, lineEnd2 ) =
            let
                mul n ( a, b ) =
                    ( n * a, n * b )

                unit =
                    ( fst model.locationA - fst model.locationB
                    , snd model.locationA - snd model.locationB
                    )

                addP ( a, b ) ( c, d ) =
                    ( a + c, b + d )

                subP ( a, b ) ( c, d ) =
                    ( a - c, b - d )
            in
                ( addP model.locationB (mul 5 unit)
                , subP model.locationA (mul 5 unit)
                )

        diagram =
            Svg.svg [ Svg.Attributes.width "100%", Svg.Attributes.height "500px" ] <|
                arcs model
                    ++ [ line model.locationA model.locationB [ fill "none", stroke "black", strokeWidth "2" ] []
                       , line model.locationA lineEnd1 [ fill "none", stroke "black", strokeWidth "2" ] []
                       , line model.locationB lineEnd2 [ fill "none", stroke "black", strokeWidth "2" ] []
                       , circle model.locationA 5 [ fill "red", stroke "red", strokeWidth "2", onMouseDown A ] []
                       , circle model.locationB 5 [ fill "red", stroke "red", strokeWidth "2", onMouseDown B ] []
                       ]

        field label element =
            div [] [ text label, element ]

        controls =
            Html.div []
                [ field "Radius x: " <| slider 0 100 250 (\v model -> { model | radius = ( v, snd model.radius ) })
                , field "Radius y: " <| slider 0 100 250 (\v model -> { model | radius = ( fst model.radius, v ) })
                , field "Rotation: " <| slider 0 360 0 (\v model -> { model | rotation = v })
                , field "largeArcFlag: " <| checkbox (\v model -> { model | largeArcFlag = choice v largestArc smallestArc })
                , field "sweepFlag:    " <| checkbox (\v model -> { model | sweepFlag = choice v clockwise antiClockwise })
                , text ("model: " ++ toString model)
                , text <|
                    pathToString <|
                        [ subpath (startAt ( 100, 100 )) closed <|
                            [ arcTo ( 50, 70 ) 0 ( largestArc, clockwise ) ( 200, 100 ) ]
                        ]
                ]
    in
        Html.div []
            [ controls, diagram ]


{-| Inline if-statement. Will evaluate all arguments no matter what
-}
choice b x y =
    if b then
        x
    else
        y


checkbox : (Bool -> Model -> Model) -> Html Msg
checkbox setter =
    input [ type' "checkbox", onCheck (Setter << setter) ] []


slider : Int -> Int -> Int -> (Float -> Model -> Model) -> Html Msg
slider lower upper default setter =
    input
        [ type' "range"
        , Html.Attributes.min (toString lower)
        , Html.Attributes.max (toString upper)
        , defaultValue (toString default)
        , onInput (unsafeParseFloat >> setter >> Setter)
        ]
        []


{-| We can get away with this hack because we decode
values from a `type="range"` input field. It's guaranteed to succeed.
-}
unsafeParseFloat : String -> Float
unsafeParseFloat v =
    case String.toFloat v of
        Ok e ->
            e

        Err e ->
            Debug.crash <| "type=\"range\" field returned a non-number, decoding gives: " ++ e


onMouseDown : Node -> Attribute Msg
onMouseDown node =
    on "mousedown" (Json.map (DragStart node) Mouse.position)
