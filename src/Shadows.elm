module Shadows exposing (getShadows)

import Array exposing (Array)
import Maybe
import Model exposing (Color(..), Piece(..), Board, Location, Selection(Selection))
import Board
import Shadows.Pawn exposing (pawn)


getShadows : Selection -> Board -> List Location
getShadows (Selection loc piece) board =
    case piece of
        Pawn color ->
            pawn color loc board

        _ ->
            []
