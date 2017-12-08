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


type Selection
    = Selection ( Int, Int ) Tile


selection loc tile =
    Selection loc tile


getSelectionLocation (Selection loc _) =
    loc


getSelectionPiece (Selection _ piece) =
    piece
