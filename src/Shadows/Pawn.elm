module Shadows.Pawn exposing (..)

import Board
import Model exposing (Color(Black, White), Piece(..), Board, Location)
import Shadows.Helper exposing (black, white)
import Piece exposing (isFriendly)


pawn : Color -> Location -> Board -> List Location
pawn color loc board =
    let
        direction =
            case color of
                Black ->
                    black

                White ->
                    white

        simple =
            ( Board.row loc + 1 * direction, Board.col loc )

        extraStart =
            ( Board.row loc + 2 * direction, Board.col loc )

        murder =
            [ ( Board.row loc + 1 * direction, Board.col loc - 1 ), ( Board.row loc + 1 * direction, Board.col loc + 1 ) ]

        simpleMove moves =
            case Board.get simple board of
                Nothing ->
                    (simple :: moves)
                        |> startingExtraMove

                Just tile ->
                    moves

        startingExtraMove moves =
            if (color == White && Board.row loc == 6) || (color == Black && Board.row loc == 1) then
                case Board.get extraStart board of
                    Nothing ->
                        extraStart :: moves

                    _ ->
                        moves
            else
                moves

        murderMove moves =
            List.foldl
                (\murderLocation m ->
                    case Board.get murderLocation board of
                        Nothing ->
                            m

                        Just piece ->
                            if isFriendly piece color then
                                moves
                            else
                                murderLocation :: m
                )
                moves
                murder
    in
        simpleMove []
            |> murderMove
