module Tests exposing (..)

import Test exposing (..)
import MainSpec
import PieceSpec


all =
    [ MainSpec.all, PieceSpec.all ]
        |> describe "Chess"
