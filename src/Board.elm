module Board
    exposing
        ( initialModel
        , fromList
        , get
        , set
        , row
        , col
        , map
        , mapWithLocation
        )

import Array exposing (Array)
import Model exposing (Piece(..), Color(..), Board, Location)


initialModel : Board
initialModel =
    [ [ Just (Rook Black), Just (Knight Black), Just (Bishop Black), Just (Queen Black), Just (King Black), Just (Bishop Black), Just (Knight Black), Just (Rook Black) ]
    , [ Just (Pawn Black), Just (Pawn Black), Just (Pawn Black), Just (Pawn Black), Just (Pawn Black), Just (Pawn Black), Just (Pawn Black), Just (Pawn Black) ]
    , List.repeat 8 Nothing
    , List.repeat 8 Nothing
    , List.repeat 8 Nothing
    , List.repeat 8 Nothing
    , [ Just (Pawn White), Just (Pawn White), Just (Pawn White), Just (Pawn White), Just (Pawn White), Just (Pawn White), Just (Pawn White), Just (Pawn White) ]
    , [ Just (Rook White), Just (Knight White), Just (Bishop White), Just (Queen White), Just (King White), Just (Bishop White), Just (Knight White), Just (Rook White) ]
    ]
        |> fromList


fromList : List (List (Maybe Piece)) -> Board
fromList list =
    Array.map (\row -> (Array.fromList row)) (Array.fromList list)


row : ( a, b ) -> a
row =
    Tuple.first


col : ( a, b ) -> b
col =
    Tuple.second


update : Location -> (Piece -> Maybe Piece) -> Board -> Board
update location f board =
    get location board
        |> Maybe.map
            (\maybeCurrent ->
                Array.get (row location) board
                    |> Maybe.map
                        (\oldRow ->
                            Array.set (col location) (f maybeCurrent) oldRow
                                |> (\newRow -> Array.set (row location) newRow board)
                        )
                    |> Maybe.withDefault board
            )
        |> Maybe.withDefault board


set : Location -> Maybe Piece -> Board -> Board
set location value m =
    update location (always value) m


get : Location -> Board -> Maybe Piece
get location m =
    Array.get (row location) m
        |> Maybe.andThen (Array.get (col location))
        |> Maybe.withDefault Nothing


map : (Maybe Piece -> Maybe Piece) -> Board -> Board
map fn board =
    Array.map (\row -> Array.map (\maybePiece -> fn maybePiece) row) board


mapWithLocation : (Location -> Maybe Piece -> Maybe Piece) -> Board -> Board
mapWithLocation fn board =
    Array.indexedMap (\i row -> Array.indexedMap (\j maybePiece -> fn ( i, j ) maybePiece) row) board
