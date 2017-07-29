module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import View.MapView exposing (mapView)
import View.GameViewDisplay exposing (..)
import View.EnergyView exposing (..)
import Debug exposing (log)
import Example.ExampleGameViewDisplay as Example
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import ClientState exposing (..)
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import AllDict exposing (..)


main : Program Flags ClientState Msg
main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( ClientState, Cmd Msg )
init flags =
    initialState flags ! []


view : ClientState -> Html Msg
view state =
    div []
        [ h1 [] [ text <| "Core catcher (Player " ++ toString state.player.playerId ++ ")" ]
        , mapView state.network displayInfo state
        , energyView state.network displayInfo state
        ]


wsUrl : String -> String
wsUrl server =
    "ws://" ++ server ++ ":8000"


subscriptions : ClientState -> Sub Msg
subscriptions state =
    WebSocket.listen (wsUrl state.server) receivedStringToMsg



--|


receivedStringToMsg : String -> Msg
receivedStringToMsg s =
    case decodeString jsonDecMessageForClient (log "received" s) of
        Ok msg ->
            MsgFromServer msg

        Err err ->
            log2 "error" err None



-- TODO: popup for that?
-- TODO: handle json error?


update : Msg -> ClientState -> ( ClientState, Cmd Msg )
update msg state =
    case log "msg" msg of
        Clicked n ->
            { state | gameError = Nothing }
                ! [ WebSocket.send (wsUrl state.server)
                        << log "send"
                    <|
                        jsonActionOfNode state n
                  ]

        MsgFromServer msg ->
            case msg of
                GameView_ gameView ->
                    { state | gameView = gameView } ! []

                InitialInfoForClient_ initInfo ->
                    { state
                        | gameView = initInfo.initialGameView
                        , player = initInfo.initialPlayer
                        , network = initInfo.networkForGame
                    }
                        ! []

                GameError_ err ->
                    { state | gameError = Just err } ! []

                GameOverView_ gameOver ->
                    { state | gameOver = True } ! []

        SelectEnergy energy ->
            { state | selectedEnergy = energy } ! []

        None ->
            state ! []



-- random dev helper functions


initialState : Flags -> ClientState
initialState flags =
    { gameView = RogueView emptyRogueView
    , network = emptyNetwork
    , player = { playerId = 0 }
    , selectedEnergy = Orange
    , server = flags.server
    , gameError = Nothing
    , gameOver = False
    }


displayInfo : GameViewDisplayInfo
displayInfo =
    Example.displayInfo


jsonActionOfNode : ClientState -> Node -> String
jsonActionOfNode state n =
    encode 0
        << jsonEncAction
    <|
        { actionPlayer = state.player
        , actionEnergy = state.selectedEnergy
        , actionNode = n
        }


cons : a -> b -> a
cons a b =
    a


log2 : String -> a -> b -> b
log2 s a b =
    cons b (log s a)
