module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import MapView exposing (mapView)
import Debug exposing (log)
import Data exposing (..)
import Game exposing (..)
import ExampleGame as ExampleGame


main : Program Never Game Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Game, Cmd Msg )
init =
    ( ExampleGame.game, Cmd.none )


view : Game -> Html Msg
view game =
    div []
        [ h1 [] [ text "Core catcher" ]
        , MapView.mapView game
        ]


wsUrl : String
wsUrl =
    "ws://localhost:3000"


subscriptions : Game -> Sub Msg
subscriptions _ =
    --WebSocket.listen wsUrl Receive
    Sub.none


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case log "msg" msg of
        Clicked n ->
            log "new game" (movePlayerInGame game 1 n) ! []
