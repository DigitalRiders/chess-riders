module Board exposing (Board, Location, initialModel, get, set, row, col, movePiece)

import Array exposing (Array)
import Model exposing (Tile(..), Color(..))


type alias Board =
    Array (Array Tile)


type alias Location =
    ( Int, Int )


initialModel : Array (Array Tile)
initialModel =
    [ [ Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black ] |> Array.fromList
    , [ Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black ] |> Array.fromList
    , List.repeat 8 Empty |> Array.fromList
    , List.repeat 8 Empty |> Array.fromList
    , List.repeat 8 Empty |> Array.fromList
    , List.repeat 8 Empty |> Array.fromList
    , [ Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White ] |> Array.fromList
    , [ Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White ] |> Array.fromList
    ]
        |> Array.fromList


getPieceFromLoc : Location -> Board -> Maybe Tile
getPieceFromLoc loc board =
    Array.get (row loc) board
        |> Maybe.andThen (Array.get (col loc))


row : ( a, b ) -> a
row =
    Tuple.first


col : ( a, b ) -> b
col =
    Tuple.second


update : Location -> (Tile -> Tile) -> Board -> Board
update location f board =
    get location board
        |> Maybe.map
            (\current ->
                Array.get (row location) board
                    |> Maybe.map
                        (\oldRow ->
                            Array.set (col location) (f current) oldRow
                                |> (\newRow -> Array.set (row location) newRow board)
                        )
                    |> Maybe.withDefault board
            )
        |> Maybe.withDefault board


set : Location -> Tile -> Board -> Board
set location value m =
    update location (always value) m


get : Location -> Board -> Maybe Tile
get location m =
    Array.get (row location) m |> Maybe.andThen (Array.get (col location))


map : (Tile -> Tile) -> Board -> Board
map fn board =
    Array.map (\row -> Array.map (\col -> fn col) row) board


mapWithLocation : (Location -> Tile -> Tile) -> Board -> Board
mapWithLocation fn board =
    Array.indexedMap (\i row -> Array.indexedMap (\j col -> fn ( i, j ) col) row) board


movePiece : Location -> Location -> Board -> Board
movePiece currentLoc newLoc board =
    let
        piece =
            Maybe.withDefault Empty <| getPieceFromLoc currentLoc board
    in
        mapWithLocation
            (\loc tile ->
                if currentLoc == loc then
                    Empty
                else if newLoc == loc then
                    piece
                else
                    tile
            )
            board
