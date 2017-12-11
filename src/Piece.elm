module Piece
    exposing
        ( revealPositions
        , isFriendly
        , move
        , toIcon
        )

import Array
import Model exposing (Color(..), Piece(..), Board, Location)
import Board


toIcon : Piece -> String
toIcon piece =
    case piece of
        Rook White ->
            "♖"

        Rook Black ->
            "♜"

        Knight White ->
            "♘"

        Knight Black ->
            "♞"

        Bishop White ->
            "♗"

        Bishop Black ->
            "♝"

        Queen White ->
            "♕"

        Queen Black ->
            "♛"

        King White ->
            "♔"

        King Black ->
            "♚"

        Pawn White ->
            "♙"

        Pawn Black ->
            "♟"


revealPositions : Color -> Board -> List ( Location, Piece )
revealPositions color board =
    List.indexedMap
        (\i row ->
            List.indexedMap
                (\j maybePiece ->
                    Maybe.andThen
                        (\piece ->
                            if isFriendly piece color then
                                Just ( ( i, j ), piece )
                            else
                                Nothing
                        )
                        maybePiece
                )
                (Array.toList row)
        )
        (Array.toList board)
        |> List.concatMap (\row -> List.filterMap identity row)


isFriendly : Piece -> Color -> Bool
isFriendly piece friendlyColor =
    let
        eq color =
            color == friendlyColor
    in
        case piece of
            Pawn color ->
                eq color

            Rook color ->
                eq color

            Knight color ->
                eq color

            Bishop color ->
                eq color

            Queen color ->
                eq color

            King color ->
                eq color


move : Location -> Location -> Board -> Board
move currentLoc newLoc board =
    let
        piece =
            Board.get currentLoc board

        isCurrentLoc loc =
            currentLoc == loc

        isNewLoc loc =
            newLoc == loc
    in
        Board.mapWithLocation
            (\loc maybePiece ->
                if isCurrentLoc loc then
                    Nothing
                else if isNewLoc loc then
                    piece
                else
                    maybePiece
            )
            board


getFromLoc : Location -> Board -> Maybe Piece
getFromLoc loc board =
    Array.get (Board.row loc) board
        |> Maybe.andThen (Array.get (Board.col loc))
        |> Maybe.withDefault Nothing
