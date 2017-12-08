module Shadows exposing (getShadows)

import Array exposing (Array)
import Maybe
import Model exposing (Color(..), Tile(..), Selection(Selection))
import Board exposing (Board, Location)


black : Int
black =
    1


white : Int
white =
    -1


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
                    moves

                Just tile ->
                    case tile of
                        Empty ->
                            (simple :: moves)
                                |> startingExtraMove

                        _ ->
                            moves

        startingExtraMove moves =
            if (color == White && Board.row loc == 6) || (color == Black && Board.row loc == 1) then
                case Maybe.withDefault Empty <| Board.get extraStart board of
                    Empty ->
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

                        Just tile ->
                            case tile of
                                Empty ->
                                    m

                                piece ->
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


getShadows : Selection -> Board -> List Location
getShadows (Selection loc piece) board =
    case piece of
        Pawn color ->
            pawn color loc board

        _ ->
            []


isFriendly : Tile -> Color -> Bool
isFriendly piece friendlyColor =
    let
        eq color =
            color == friendlyColor
    in
        case piece of
            Empty ->
                False

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
