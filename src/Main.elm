module Main exposing (main)

import Array exposing (Array)
import Board
import Html exposing (..)
import Html.Attributes exposing (classList, style)
import Html.Events exposing (onClick)
import Model exposing (Piece(..), Board, Location, Color(..), Selection, selection)
import Shadows
import Piece


-- MODEL


type alias Model =
    { board : Board
    , selected : Maybe Selection
    , turn : Color
    , kingUnderAttack : Maybe Color
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { board = Board.initialModel
    , selected = Nothing
    , turn = White
    , kingUnderAttack = Nothing
    }



-- UPDATE


type Msg
    = SetSelection (Maybe Selection)
    | RemoveSelection
    | GoToTile Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    checkKingIntegrity model
        |> doUpdate msg


doUpdate msg model =
    case msg of
        SetSelection maybeSelection ->
            { model | selected = maybeSelection } ! []

        RemoveSelection ->
            { model | selected = Nothing } ! []

        GoToTile newLocation ->
            let
                newBoard =
                    case model.selected of
                        Nothing ->
                            model.board

                        Just selected ->
                            Piece.move (Model.getSelectionLocation selected) newLocation model.board
            in
                { model | board = newBoard, selected = Nothing, turn = flipColor model.turn } ! []


checkKingIntegrity model =
    model



-- create a function to list all opposite pieces
--  go through all opposite pieces
-- each one check shadow and make sure none is equal to king's position
-- if any has king's position then compromised


flipColor : Color -> Color
flipColor color =
    case color of
        White ->
            Black

        Black ->
            White



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW


size : Float
size =
    8


sideSize : Float
sideSize =
    7


gutterWidth : Float
gutterWidth =
    0.5


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


viewBoard : Model -> Html Msg
viewBoard model =
    let
        setSelection loc piece =
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
                    [ ( "width", numberToVh (size * 8 + sideSize * 2 + gutterWidth * 2) )
                    , ( "height", numberToVh (size * 8 + sideSize * 2 + gutterWidth * 2) )
                    , ( "background", "#864534" )
                    , ( "position", "absolute" )
                    , ( "top", numberToVh (-1 * (sideSize + gutterWidth)) )
                    , ( "left", numberToVh (-1 * (sideSize + gutterWidth)) )
                    , ( "border-radius", "6px" )
                    ]
                ]
                []
            , div
                [ style
                    [ ( "border", numberToVh gutterWidth ++ " solid #f5ecab" )
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
                                (\j maybePiece ->
                                    div
                                        [ onClick
                                            (if List.member ( i, j ) shadows then
                                                GoToTile ( i, j )
                                             else
                                                case maybePiece of
                                                    Nothing ->
                                                        (SetSelection Nothing)

                                                    Just piece ->
                                                        if model.turn == colorFromPiece piece then
                                                            (SetSelection <| setSelection ( i, j ) piece)
                                                        else
                                                            (SetSelection Nothing)
                                            )
                                        , style
                                            [ ( "background"
                                              , if (i + j) % 2 == 0 then
                                                    "#f5ecab"
                                                else
                                                    "#864534"
                                              )
                                            , ( "width", numberToVh size )
                                            , ( "height", numberToVh size )
                                            , ( "display", "inline-block" )
                                            , ( "position", "relative" )
                                            , ( "padding", "5px" )
                                            , ( "box-sizing", "border-box" )
                                            ]
                                        , classList [ ( "tile__shadow", List.member ( i, j ) shadows ) ]
                                        ]
                                        [ div
                                            [ style
                                                [ ( "position", "absolute" )
                                                , ( "top", "50%" )
                                                , ( "left", "50%" )
                                                , ( "transform", "translate(-50%, -50%)" )
                                                , ( "font-size", "10vh" )
                                                , ( "cursor", "pointer" )
                                                ]
                                            ]
                                            [ text <|
                                                case maybePiece of
                                                    Nothing ->
                                                        ""

                                                    Just piece ->
                                                        Piece.toIcon piece
                                            ]
                                        ]
                                )
                                (Array.toList side1)
                            )
                    )
                    (Array.toList model.board)
                )
            ]


numberToVh num =
    (toString num) ++ "vh"


numberToPx : Int -> String
numberToPx num =
    (toString num) ++ "px"


colorFromPiece : Piece -> Color
colorFromPiece piece =
    case piece of
        Pawn color ->
            color

        Rook color ->
            color

        Knight color ->
            color

        Bishop color ->
            color

        King color ->
            color

        Queen color ->
            color



-- INIT


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
