module PieceSpec exposing (..)

import Board
import Expect
import Model exposing (Color(..), Piece(..))
import Test exposing (..)
import Piece exposing (..)


all =
    describe "PieceSpec"
        [ test "revealPosition" <|
            \() ->
                revealPositions White Board.initialModel
                    |> Expect.equal
                        [ ( ( 6, 0 ), Pawn White )
                        , ( ( 6, 1 ), Pawn White )
                        , ( ( 6, 2 ), Pawn White )
                        , ( ( 6, 3 ), Pawn White )
                        , ( ( 6, 4 ), Pawn White )
                        , ( ( 6, 5 ), Pawn White )
                        , ( ( 6, 6 ), Pawn White )
                        , ( ( 6, 7 ), Pawn White )
                        , ( ( 7, 0 ), Rook White )
                        , ( ( 7, 1 ), Knight White )
                        , ( ( 7, 2 ), Bishop White )
                        , ( ( 7, 3 ), Queen White )
                        , ( ( 7, 4 ), King White )
                        , ( ( 7, 5 ), Bishop White )
                        , ( ( 7, 6 ), Knight White )
                        , ( ( 7, 7 ), Rook White )
                        ]
        , test "isFriendly" <|
            \() ->
                isFriendly (Rook White) Black
                    |> Expect.equal False
        , test "move" <|
            \() ->
                move ( 1, 0 )
                    ( 0, 0 )
                    (Board.fromList
                        [ [ Nothing, Nothing ]
                        , [ Just (Rook White), Nothing ]
                        ]
                    )
                    |> Expect.equal
                        (Board.fromList
                            [ [ Just (Rook White), Nothing ]
                            , [ Nothing, Nothing ]
                            ]
                        )
        ]
