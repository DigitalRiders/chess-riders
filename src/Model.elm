module Model exposing (..)

import Array exposing (Array)


type Tile
    = Pawn Color
    | Knight Color
    | Bishop Color
    | Rook Color
    | King Color
    | Queen Color
    | Empty


type Color
    = White
    | Black


type alias Location =
    ( Int, Int )


type Selection
    = Selection Location Tile


selection loc tile =
    Selection loc tile
