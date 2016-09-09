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
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


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
    , locationA : Vec2
    , locationB : Vec2
    , drag : Maybe Drag
    }


model =
    { radius = ( 120, 120 )
    , rotation = 0
    , largeArcFlag = smallestArc
    , sweepFlag = antiClockwise
    , locationA = vec2 100 250
    , locationB = vec2 200 250
    , drag = Nothing
    }


type alias Drag =
    { node : Node, lstart : Vec2, start : Vec2, current : Vec2 }


type Msg
    = Setter (Model -> Model)
    | DragStart Node Vec2
    | DragAt Vec2
    | DragEnd Vec2


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves (DragAt << fromIntRecord), Mouse.ups (DragEnd << fromIntRecord) ]


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
                                | locationA = Vec2.add lstart (Vec2.sub pos start)
                                , drag = Just <| Drag node lstart start pos
                            }

                        B ->
                            { model
                                | locationB = Vec2.add lstart (Vec2.sub pos start)
                                , drag = Just <| Drag node lstart start pos
                            }

                Nothing ->
                    model

        DragEnd _ ->
            { model | drag = Nothing }


line : Vec2 -> Vec2 -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
line from to attributes children =
    let
        ( ( a, b ), ( c, d ) ) =
            ( Vec2.toTuple from, Vec2.toTuple to )
    in
        Svg.line (x1 (toString a) :: x2 (toString c) :: y1 (toString b) :: y2 (toString d) :: attributes) children


circle : Vec2 -> Float -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
circle center radius attributes children =
    let
        ( x, y ) =
            Vec2.toTuple center
    in
        Svg.circle (cx (toString x) :: cy (toString y) :: r (toString radius) :: attributes) children


openEnding =
    open


view : Model -> Html Msg
view model =
    let
        arcColor arc sweep =
            if model.largeArcFlag == arc && model.sweepFlag == sweep then
                "red"
            else
                "green"

        makeArcPath start radius rotation ( arc, sweep ) goal =
            let
                singleton x =
                    [ x ]

                data =
                    [ arcTo radius rotation ( arc, sweep ) goal ]
                        |> subpath (startAt start) openEnding
                        |> singleton
                        |> Svg.Path.pathToString
            in
                Svg.path
                    [ Svg.Attributes.d data
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
            List.map (\( arc, direction ) -> makeArcPath (Vec2.toTuple locationA) radius rotation ( arc, direction ) (Vec2.toTuple locationB)) variations

        unit =
            Vec2.direction model.locationA model.locationB
                |> Vec2.scale 500

        ( lineEnd1, lineEnd2 ) =
            ( Vec2.sub model.locationB unit
            , Vec2.add model.locationA unit
            )

        diagram =
            Svg.svg [ Svg.Attributes.width "100%", Svg.Attributes.height "500px" ]
                <| arcs model
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
                [ field "Radius x: " <| slider 0 100 400 (\v model -> { model | radius = ( v, snd model.radius ) })
                , field "Radius y: " <| slider 0 100 400 (\v model -> { model | radius = ( fst model.radius, v ) })
                , field "Rotation: " <| slider 0 360 0 (\v model -> { model | rotation = v })
                , field "largeArcFlag: " <| checkbox (\v model -> { model | largeArcFlag = choice v largestArc smallestArc })
                , field "sweepFlag:    " <| checkbox (\v model -> { model | sweepFlag = choice v clockwise antiClockwise })
                , text ("model: " ++ toString model)
                , br [] []
                , [ arcTo ( 50, 70 ) 0 ( largestArc, clockwise ) ( 200, 100 ) ]
                    |> subpath (startAt ( 200, 200 )) closed
                    |> (\x -> [ x ])
                    |> pathToString
                    |> text
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
    on "mousedown" (Json.map (DragStart node << fromIntRecord) Mouse.position)


fromIntRecord : { a | x : Int, y : Int } -> Vec2
fromIntRecord { x, y } =
    vec2 (toFloat x) (toFloat y)
