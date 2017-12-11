module Model exposing (..)

import Array exposing (Array)


type Piece
    = Pawn Color
    | Knight Color
    | Bishop Color
    | Rook Color
    | King Color
    | Queen Color


type alias Location =
    ( Int, Int )


type Color
    = White
    | Black


type alias Board =
    Array (Array (Maybe Piece))


type Selection
    = Selection ( Int, Int ) Piece


selection loc piece =
    Selection loc piece


getSelectionLocation (Selection loc _) =
    loc


getSelectionPiece (Selection _ piece) =
    piece
