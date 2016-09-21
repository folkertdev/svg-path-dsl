module Svg.Path.Subpath
    exposing
        ( mapAbsolute
        , Subpath(..)
        , CloseOption(..)
        , StartingPoint(..)
        , subpathToInstructions
        , subpath
        )

import Svg.Path.Instruction as Instruction exposing (..)
import Svg.Path.Point as Point exposing (Point)


type Subpath
    = Subpath StartingPoint CloseOption (List Instruction)


subpath =
    Subpath


{-| Close the subpath or not.
-}
type CloseOption
    = CloseOption Bool


{-| Starting point of a subpath.
-}
type StartingPoint
    = StartingPoint Instruction



{-
   g : Subpath -> DrawState -> ( DrawState, Subpath )
   g subpath state =
       let
           translated =
               mapSubpath (addPoint state.current) subpath

           simulate =
               List.foldl propagate
       in
           ( simulate state (subpathToInstructions translated []), translated )


   concatenate : List Subpath -> DrawState -> ( DrawState, List Subpath )
   concatenate subpaths state =
       let
           ( finalState, newSubpaths ) =
               List.foldl helper ( state, [] ) subpaths

           helper subpath ( currentState, accum ) =
               let
                   ( stateAfter, newSubpath ) =
                       g subpath currentState
               in
                   ( stateAfter, newSubpath :: accum )
       in
           ( finalState, List.reverse newSubpaths )
-}


mapAbsolute : (Point -> Point) -> Subpath -> Subpath
mapAbsolute f (Subpath (StartingPoint start) closeOpt instructions) =
    Subpath (StartingPoint (Instruction.mapAbsolute f start)) closeOpt (List.map (Instruction.mapAbsolute f) instructions)


subpathToInstructions : Subpath -> List Instruction -> List Instruction
subpathToInstructions (Subpath (StartingPoint start) (CloseOption closePath) segments) accum =
    if closePath then
        (start :: segments) ++ (ClosePath :: accum)
    else
        (start :: segments) ++ accum
