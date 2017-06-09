module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import MapView exposing (mapView)
import Debug exposing (log)
import Example.ExampleNetwork as Example
import Example.ExampleGameView as Example
import Example.ExampleGameViewDisplay as Example
import Protocol exposing (..)
import GameViewDisplay exposing (..)
import Data exposing (..)
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import AllDict exposing (..)


main : Program Never ClientState Msg
main =
    log2 "start state"
        initialState
        -- TODO: remove
        program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( ClientState, Cmd Msg )
init =
    ( initialState, Cmd.none )


view : ClientState -> Html Msg
view state =
    div []
        [ h1 [] [ text "Core catcher" ]
        , MapView.mapView network displayInfo state
        ]


wsUrl : String
wsUrl =
    "ws://localhost:3000"


subscriptions : ClientState -> Sub Msg
subscriptions _ =
    WebSocket.listen wsUrl Received


update : Msg -> ClientState -> ( ClientState, Cmd Msg )
update msg state =
    case log "msg" msg of
        Clicked n ->
            --log "moved state" (movePlayerInRogueView state { playerId = 1 } n)
            state
                ! [ WebSocket.send wsUrl << log "send" <| jsonActionOfNode n ]

        Received s ->
            case decodeString jsonDecRogueGameView s of
                Ok newState ->
                    newState ! []
                Err err ->
                    log2 "error" err state ! []
            



-- random dev helper functions and type defs


type alias ClientState =
    RogueGameView


initialState : ClientState
initialState =
    Example.rogueGameView

network : Network
network =
    Example.network

displayInfo : GameViewDisplayInfo
displayInfo =
    Example.displayInfo


jsonActionOfNode : Node -> String
jsonActionOfNode n =
    encode 0
        << jsonEncAction
    <|
        { player = { playerId = 1 }
        , transport = { transportName = "" }
        , node = n
        }


cons : a -> b -> a
cons a b =
    a


log2 : String -> a -> b -> b
log2 s a b =
    cons b (log s a)


movePlayerInCatcherView : CatcherGameView -> Player -> Node -> CatcherGameView
movePlayerInCatcherView game player newNode =
    { game
        | catcherPlayerPositions =
            movePlayerInPlayerPositions game.catcherPlayerPositions player newNode
    }


movePlayerInRogueView : RogueGameView -> Player -> Node -> RogueGameView
movePlayerInRogueView game player newNode =
    { game
        | roguePlayerPositions =
            movePlayerInPlayerPositions game.roguePlayerPositions player newNode
    }


movePlayerInPlayerPositions : PlayerPositions -> Player -> Node -> PlayerPositions
movePlayerInPlayerPositions { playerPositions_ } { playerId } newNode =
    { playerPositions_ =
        List.map
            (\( p, n ) ->
                if p.playerId == playerId then
                    ( p, newNode )
                else
                    ( p, n )
            )
            playerPositions_
    }
