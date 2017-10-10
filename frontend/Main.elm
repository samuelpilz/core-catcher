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
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import AllDict exposing (..)
import EveryDict
import AllDict exposing (AllDict)
import Navigation exposing (..)
import AnimationFrame exposing (diffs)
import Experimental.Main as Experimental
import Experimental.ClientState as Experimental
import Experimental.Update as Experimental
import Experimental.View as Experimental


main : Program Never Experimental.ClientModel Experimental.Msg
main =
    Navigation.program (const Experimental.None)
        { init = Experimental.init
        , update = Experimental.update
        , view = Experimental.view
        , subscriptions =
            \state ->
                Sub.batch
                    [ WebSocket.listen (wsUrl state.server) receivedStringToMsg2
                    , case state.state of
                        Experimental.InGame_ (Experimental.GameActive_ _ gameActive) _ ->
                            if (AllDict.toList gameActive.networkModel.activeAnimations == []) then
                                Sub.none
                            else
                                diffs Experimental.Tick
                        Experimental.LoggedIn_ (Experimental.GameOver_ gameOver) _ ->
                            if (AllDict.toList gameOver.networkModel.activeAnimations == []) then
                                Sub.none
                            else
                                diffs Experimental.Tick

                        _ ->
                            Sub.none
                    ]
        }



--init : Location -> ( ClientState, Cmd Msg )
--init location =
--    initialState location ! []
--
--
--
---- enable this instead of "! []" for automiatic login
----        ! [ send location.hostname <|
----                Login_
----                    { loginPlayer =
----                        { playerName = "Alice" }
----                    }
----          ]
--
--
--preGameView : PreGame -> Html Msg
--preGameView state =
--    div []
--        [ h1 [] [ text "Core catcher" ]
--        , Html.form [ onSubmit (DoLogin { loginPlayer = { playerName = state.playerNameField } }) ]
--            [ input [ placeholder "Username", onInput PlayerNameChange, autofocus True ] []
--            , button [ type_ "submit" ] [ text "Login" ]
--            , p []
--                [ text "Players in Game"
--                , ol []
--                    [ li [] [ text "Alice" ]
--                    , li [] [ text "Bob" ]
--                    , li [] [ text "Charlie" ]
--                    ]
--                ]
--            ]
--        ]
--


wsUrl : String -> String
wsUrl server =
    "ws://" ++ server ++ ":7999"



--subscriptions : ClientState -> Sub Msg
--subscriptions state =
--    Sub.batch
--        [ WebSocket.listen (wsUrl <| getServer state) receivedStringToMsg
--        , case state of
--            PreGame_ _ ->
--                Sub.none
--
--            GameState_ state ->
--                if (AllDict.toList state.activeAnimations == []) then
--                    Sub.none
--                else
--                    diffs Tick
--        ]
--
--


receivedStringToMsg2 : String -> Experimental.Msg
receivedStringToMsg2 s =
    case decodeString jsonDecMessageForClient s of
        Ok msg ->
            Experimental.MsgFromServer msg

        Err err ->
            -- TODO: popup for that?
            -- how to handle json error?
            log2 "error" err Experimental.None



--
--receivedStringToMsg : String -> Msg
--receivedStringToMsg s =
--    case decodeString jsonDecMessageForClient s of
--        Ok msg ->
--            MsgFromServer msg
--
--        Err err ->
--            -- TODO: popup for that?
--            -- how to handle json error?
--            log2 "error" err None
--
--
--update : Msg -> ClientState -> ( ClientState, Cmd Msg )
--update msg state =
--    case ( log "msg" msg, state ) of
--        ( Movement n, GameState_ state ) ->
--            GameState_ { state | gameError = Nothing }
--                ! [ sendProtocolMsgMay state.server <| msgForActionOfNode state n
--                  ]
--
--        ( MsgFromServer (GameView_ gameView), GameState_ state ) ->
--            GameState_
--                { state
--                    | playerPositions = playerPositions gameView
--                    , playerEnergies = playerEnergies gameView
--                    , rogueHistory = rogueHistory gameView
--                    , nextPlayer = Just <| nextPlayer gameView
--                    , activeAnimations = updateActiveAnimations state <| playerPositions gameView
--                }
--                ! []
--
--        --        ( MsgFromServer (GameError_ err), GameState_ state ) ->
--        --            GameState_ { state | gameError = Just err } ! []
--        ( MsgFromServer (GameOverView_ gameOver), GameState_ state ) ->
--            GameState_
--                { state
--                    | gameOver = True
--                    , playerPositions = gameOver.gameOverViewPlayerPositions
--                    , playerEnergies = gameOver.gameOverViewPlayerEnergies
--                    , nextPlayer = Nothing
--                    , activeAnimations = updateActiveAnimations state gameOver.gameOverViewPlayerPositions
--
--                    --, rogueHistory = gameOver.gameOverViewRogueHistory
--                    -- TODO: openRougeHistory
--                }
--                ! []
--
--        ( MsgFromServer ServerHello, GameState_ state ) ->
--            -- reconnect upon ServerHello
--            GameState_ state
--                ! [ sendProtocolMsg state.server <|
--                        Login_
--                            { loginPlayer =
--                                { playerName = state.player.playerName }
--                            }
--                  ]
--
--        -- login
--        ( MsgFromServer (InitialInfoGameActive_ initInfo), PreGame_ preGame ) ->
--            let
--                emptyState =
--                    emptyGameState preGame.server initInfo.startingPlayer
--            in
--                GameState_
--                    { emptyState
--                        | network = initInfo.networkForGame
--                        , players = initInfo.allPlayers
--                        , energies = initInfo.allEnergies
--                        , playerPositions = playerPositions initInfo.initialGameView
--                        , playerEnergies = playerEnergies <| initInfo.initialGameView
--                        , rogueHistory = rogueHistory <| initInfo.initialGameView
--                        , nextPlayer = Just <| nextPlayer initInfo.initialGameView
--                    }
--                    ! []
--
--        -- reconnect
--        ( MsgFromServer (InitialInfoGameActive_ initInfo), GameState_ state ) ->
--            let
--                emptyState =
--                    emptyGameState state.server initInfo.startingPlayer
--            in
--                GameState_
--                    { emptyState
--                        | network = initInfo.networkForGame
--                        , players = initInfo.allPlayers
--                        , energies = initInfo.allEnergies
--                        , playerPositions = playerPositions initInfo.initialGameView
--                        , playerEnergies = playerEnergies <| initInfo.initialGameView
--                        , rogueHistory = rogueHistory <| initInfo.initialGameView
--                        , nextPlayer = Just <| nextPlayer initInfo.initialGameView
--                    }
--                    ! []
--
--        -- other message from the server that is not recognized
--        ( MsgFromServer _, _ ) ->
--            -- TODO: error for that
--            state ! []
--
--        ( Tick dt, GameState_ state ) ->
--            GameState_
--                { state
--                    | animationTime = dt + state.animationTime
--                    , activeAnimations =
--                        -- filter done animations
--                        AllDict.filter
--                            (\_ a ->
--                                a.startTime
--                                    + state.displayInfo.movementAnimationDuration
--                                    > state.animationTime
--                            )
--                            state.activeAnimations
--                }
--                ! []
--
--        ( SelectEnergy energy, GameState_ state ) ->
--            GameState_ { state | selectedEnergy = Just energy } ! []
--
--        ( None, state ) ->
--            state ! []
--
--        ( PlayerNameChange newPlayerName, PreGame_ state ) ->
--            PreGame_ { state | playerNameField = newPlayerName } ! []
--
--        ( DoLogin login, PreGame_ state ) ->
--            PreGame_ state
--                ! [ sendProtocolMsg state.server <|
--                        Login_
--                            { loginPlayer =
--                                { playerName = state.playerNameField }
--                            }
--                  ]
--
--        -- messages that have not been recogniced but defined in ClientState.elm
--        ( _, state ) ->
--            -- TODO: error for that
--            state ! []
--
--
--initialState : Location -> ClientState
--initialState location =
--    PreGame_ <| emptyPreGame location.hostname
--
--
--sendProtocolMsg : String -> MessageForServer -> Cmd a
--sendProtocolMsg server msg =
--    WebSocket.send (wsUrl server)
--        << encode 0
--        << jsonEncMessageForServer
--    <|
--        log "ws-send" msg
--
--
--sendProtocolMsgMay : String -> Maybe MessageForServer -> Cmd a
--sendProtocolMsgMay server msgMay =
--    case msgMay of
--        Nothing ->
--            Cmd.none
--
--        Just msg ->
--            sendProtocolMsg server msg
--
--
--msgForActionOfNode : GameState -> Node -> Maybe MessageForServer
--msgForActionOfNode state n =
--    Maybe.map
--        (\energy ->
--            Action_
--                { actionPlayer = state.player
--                , actionEnergy = energy
--                , actionNode = n
--                }
--        )
--        state.selectedEnergy
--
--


const : a -> b -> a
const a b =
    a


log2 : String -> a -> b -> b
log2 s a b =
    const b (log s a)
