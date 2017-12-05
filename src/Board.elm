module Board exposing (Board, initialModel, get, set, row, col)

import Array exposing (Array)
import Model exposing (Tile(..), Color(..))


type alias Board =
    Array (Array Tile)


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


getPieceFromLoc loc board =
    Array.get (row loc) board
        |> Maybe.andThen (Array.get (col loc))


row : ( a, b ) -> a
row =
    Tuple.first


col : ( a, b ) -> b
col =
    Tuple.second


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


set location value m =
    update location (always value) m


get location m =
    Array.get (row location) m |> Maybe.andThen (Array.get (col location))
