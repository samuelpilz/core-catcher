module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import MapView exposing (mapView)
import Debug exposing (log)
import Game exposing (..)
import ExampleGame as ExampleGame
import Protocol exposing (..)
import Data exposing (..)
import Json.Encode exposing (encode)


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
    WebSocket.listen wsUrl Received


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case log "msg" msg of
        Clicked n ->
            (movePlayerInGame game 1 n)
                ! [ WebSocket.send wsUrl <| log "send" <| jsonActionOfNode n ]

        Received s ->
            game ! []


jsonActionOfNode : Int -> String
jsonActionOfNode n =
    encode 0 <|
        jsonEncAction <|
            { player = { playerId = 1 }
            , transport = { transportName = "" }
            , node = { nodeId = n }
            }
