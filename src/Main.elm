module Main exposing (main)

import Array exposing (Array)
import Board exposing (Board)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Tile(..), Color(..), Location, Selection, selection)
import Shadows


-- MODEL


type alias Model =
    { board : Board
    , selected : Maybe Selection
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { board = Board.initialModel
    , selected = Nothing
    }



-- UPDATE


type Msg
    = SetSelection (Maybe Selection)
    | RemoveSelection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSelection maybeSelection ->
            { model | selected = maybeSelection } ! []

        RemoveSelection ->
            { model | selected = Nothing } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW


size : Int
size =
    80


sideSize : Int
sideSize =
    50


gutterWidth : Int
gutterWidth =
    3


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "50%" )
            , ( "left", "50%" )
            , ( "transform", "translate(-50%, -50%)" )
            ]
        ]
        [ viewBoard model
        ]


piecesRepresentation piece =
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

        Empty ->
            ""


viewBoard : Model -> Html Msg
viewBoard model =
    let
        setSelection loc piece =
            if piece == Empty then
                Nothing
            else
                Just <| selection loc piece

        shadows =
            case model.selected of
                Nothing ->
                    []

                Just selection ->
                    Shadows.getShadows selection model.board
    in
        div []
            [ div
                [ style
                    [ ( "width", numberToPx (size * 8 + sideSize * 2) )
                    , ( "height", numberToPx (size * 8 + sideSize * 2) )
                    , ( "background", "#864534" )
                    , ( "position", "absolute" )
                    , ( "top", numberToPx (-1 * sideSize + gutterWidth) )
                    , ( "left", numberToPx (-1 * sideSize + gutterWidth) )
                    , ( "border-radius", "6px" )
                    ]
                ]
                []
            , div
                [ style
                    [ ( "border", numberToPx gutterWidth ++ " solid #f5ecab" )
                    , ( "display", "inline-block" )
                    , ( "border-radius", "3px" )
                    , ( "z-index", "100" )
                    , ( "position", "relative" )
                    ]
                ]
                (List.indexedMap
                    (\i side1 ->
                        div [ style [ ( "line-height", "0" ) ] ]
                            (List.indexedMap
                                (\j piece ->
                                    div
                                        [ onClick (SetSelection <| setSelection ( i, j ) piece)
                                        , style
                                            [ ( "background"
                                              , if (i + j) % 2 == 0 then
                                                    "#f5ecab"
                                                else
                                                    "#864534"
                                              )
                                            , ( "width", numberToPx size )
                                            , ( "height", numberToPx size )
                                            , ( "display", "inline-block" )
                                            , ( "position", "relative" )
                                            , ( "padding", "5px" )
                                            , ( "box-sizing", "border-box" )
                                            ]
                                        ]
                                        [ div
                                            [ style
                                                [ ( "position", "absolute" )
                                                , ( "top", "50%" )
                                                , ( "left", "50%" )
                                                , ( "transform", "translate(-50%, -50%)" )
                                                , ( "font-size", "45px" )
                                                , ( "cursor", "pointer" )
                                                ]
                                            ]
                                            [ text <| piecesRepresentation piece ]
                                        , if List.member ( i, j ) shadows then
                                            div
                                                [ style
                                                    [ ( "position", "absolute" )
                                                    , ( "top", "50%" )
                                                    , ( "left", "50%" )
                                                    , ( "transform", "translate(-50%, -50%)" )
                                                    , ( "font-size", "45px" )
                                                    , ( "cursor", "pointer" )
                                                    ]
                                                ]
                                                [ text "s" ]
                                          else
                                            text ""
                                        ]
                                )
                                (Array.toList side1)
                            )
                    )
                    (Array.toList model.board)
                )
            ]


numberToPx num =
    (toString num) ++ "px"


colorFromPiece piece =
    case piece of
        Pawn color ->
            toString color

        Rook color ->
            toString color

        Knight color ->
            toString color

        Bishop color ->
            toString color

        King color ->
            toString color

        Queen color ->
            toString color

        Empty ->
            ""



-- INIT


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
