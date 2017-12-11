module MainSpec exposing (..)

import Expect
import Test exposing (..)
import Main


all =
    describe "MainSpec"
        [ test "revealPosition" <|
            \() ->
                True |> Expect.true "cant fail"
        ]
