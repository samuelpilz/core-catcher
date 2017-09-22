module Experimental.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import View.MapView exposing (mapView)
import Debug exposing (log)
import Example.ExampleGameViewDisplay as Example
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import Experimental.ClientState exposing (..)
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import AllDict exposing (..)
import EveryDict
import AllDict exposing (AllDict)
import Navigation exposing (..)
import AnimationFrame exposing (diffs)
import ProtocolEmpty exposing (..)


main : Program Never ClientModel Msg
main =
    Navigation.program (const None)
        { init = init
        , update = update
        , view = view
        , subscriptions = const Sub.none
        }


init : Location -> ( ClientModel, Cmd Msg )
init location =
    { state = Landing_ { playerNameField = "" }, server = location.hostname } ! []


view : ClientModel -> Html Msg
view state =
    div []
        [ p [] [ text <| toString state ]
        , p []
            [ button [ onClick <| MsgFromServer <| ServerHello ] [ text "Server Hello" ]
            , button [ onClick <| ConnectionLost ] [ text "Connection Lost" ]
            ]
        , p []
            [ button [ onClick <| DoLogin ] [ text "Login" ]
            , button [ onClick <| MsgFromServer <| LoginSuccess_ emptyLoginSuccess ] [ text "Login Sucess" ]
            , button [ onClick <| MsgFromServer <| LoginFail_ emptyLoginFail ] [ text "Login Fail" ]
            ]
        , p []
            [ button [ onClick <| ToHome ] [ text "To Home" ]
            , button [ onClick <| DoGameConnect ] [ text "Game Connect" ]
            , button [ onClick <| DoJoinGame ] [ text "Join Game " ]
            , button [ onClick <| DoOpenNewGame ] [ text "Open New Game" ]
            , button [ onClick <| DoCreateGame ] [ text "Create Game" ]
            , button [ onClick <| MsgFromServer <| PreGameLobby_ ] [ text "PreGame Lobby" ]
            , button [ onClick <| DoStartGame ] [ text "Start Game" ]
            , button [ onClick <| MsgFromServer <| InitialInfoForGame_ emptyInitialInfo ] [ text "Initial Game Info" ]
            ]
        , p []
            [ button [ onClick <| GameAction_ <| Movement emptyNode ] [ text "Action" ]
            , button [ onClick <| MsgFromServer <| GameView_ emptyGameView ] [ text "Game View" ]
            , button [ onClick <| MsgFromServer <| GameOverView_ emptyGameOverView ] [ text "Game Over" ]
            , button [ onClick <| DoGameReconnect ] [ text "Game Reconnect" ]
            ]
        ]


update : Msg -> ClientModel -> ( ClientModel, Cmd Msg )
update msg state =
    case state.state of
        Landing_ landing ->
            updateLanding msg state landing

        LandingConnected_ landing ->
            updateLandingConnected msg state landing

        LoginPending_ loginPending ->
            updateLoginPending msg state loginPending

        LoggedIn_ loggedInState loggedIn ->
            updateLoggedIn msg state ( loggedInState, loggedIn )

        InGame_ inGameState inGame ->
            updateInGame msg state ( inGameState, inGame )

        _ ->
            state ! []


updateLanding : Msg -> ClientModel -> Landing -> ( ClientModel, Cmd Msg )
updateLanding msg state landing =
    case msg of
        MsgFromServer ServerHello ->
            { state | state = LandingConnected_ { playerNameField = landing.playerNameField } } ! []

        _ ->
            state ! []


updateLandingConnected : Msg -> ClientModel -> LandingConnected -> ( ClientModel, Cmd Msg )
updateLandingConnected msg state landingConnected =
    case msg of
        DoLogin ->
            { state
                | state =
                    LoginPending_
                        { player =
                            { playerName = landingConnected.playerNameField }
                        }
            }
                ! []

        ConnectionLost ->
            { state | state = Landing_ { playerNameField = landingConnected.playerNameField } } ! []

        _ ->
            state ! []


updateLoginPending : Msg -> ClientModel -> LoginPending -> ( ClientModel, Cmd Msg )
updateLoginPending msg state loginPending =
    case msg of
        MsgFromServer (LoginSuccess_ login) ->
            { state | state = LoggedIn_ PlayerHome { player = login.loginSuccessPlayer } }
                ! []

        MsgFromServer (LoginFail_ _) ->
            { state | state = LoginFailed_ } ! []

        ConnectionLost ->
            { state | state = Landing_ { playerNameField = "" } } ! []

        _ ->
            state ! []


updateLoggedIn : Msg -> ClientModel -> ( LoggedInState, LoggedIn ) -> ( ClientModel, Cmd Msg )
updateLoggedIn msg state ( loggedInState, loggedIn ) =
    case ( msg, loggedInState ) of
        ( ConnectionLost, _ ) ->
            { state | state = LoggedIn_ Disconnected loggedIn } ! []

        ( MsgFromServer ServerHello, _ ) ->
            { state | state = LoggedIn_ ServerReconnected loggedIn } ! []

        ( MsgFromServer (LoginSuccess_ loginSuccess), ServerReconnected ) ->
            { state | state = LoggedIn_ PlayerHome loggedIn } ! []

        ( ToHome, Disconnected ) ->
            state ! []

        ( ToHome, ServerReconnected ) ->
            state ! []

        ( ToHome, _ ) ->
            { state | state = LoggedIn_ PlayerHome loggedIn } ! []

        ( DoOpenNewGame, PlayerHome ) ->
            { state | state = LoggedIn_ NewGame loggedIn } ! []

        ( DoJoinGame, PlayerHome ) ->
            { state | state = LoggedIn_ JoinGamePending loggedIn } ! []

        ( DoCreateGame, NewGame ) ->
            { state | state = LoggedIn_ NewGamePending loggedIn } ! []

        ( MsgFromServer PreGameLobby_, NewGamePending ) ->
            { state | state = LoggedIn_ PreGameLobby loggedIn } ! []

        ( MsgFromServer PreGameLobby_, JoinGamePending ) ->
            { state | state = LoggedIn_ PreGameLobby loggedIn } ! []

        ( DoStartGame, PreGameLobby ) ->
            { state | state = LoggedIn_ GameStartPending loggedIn } ! []

        ( MsgFromServer (InitialInfoForGame_ initInfo), PreGameLobby ) ->
            { state | state = InGame_ (GameActive_ YourTurn) { player = loggedIn.player } } ! []

        ( MsgFromServer (InitialInfoForGame_ initInfo), GameStartPending ) ->
            { state | state = InGame_ (GameActive_ YourTurn) { player = loggedIn.player } } ! []

        ( DoGameConnect, PlayerHome ) ->
            { state | state = LoggedIn_ GameConnectPending loggedIn } ! []

        ( MsgFromServer (InitialInfoForGame_ initInfo), GameConnectPending ) ->
            { state | state = InGame_ (GameActive_ YourTurn) { player = loggedIn.player } } ! []

        ( _, _ ) ->
            state ! []


updateInGame : Msg -> ClientModel -> ( InGameState, InGame ) -> ( ClientModel, Cmd Msg )
updateInGame msg state ( inGameState, inGame ) =
    case ( msg, inGameState ) of
        ( ConnectionLost, GameOver ) ->
            state ! []

        ( ConnectionLost, _ ) ->
            { state | state = InGame_ GameDisconnected inGame } ! []

        ( GameAction_ _, GameActive_ YourTurn ) ->
            { state | state = InGame_ (GameActive_ ActionPending) inGame } ! []

        ( MsgFromServer (GameView_ _), GameActive_ _ ) ->
            { state | state = InGame_ (GameActive_ OthersTurn) inGame } ! []

        ( MsgFromServer (GameOverView_ _), GameActive_ _ ) ->
            { state | state = InGame_ GameOver inGame } ! []

        ( ToHome, GameOver ) ->
            { state | state = LoggedIn_ PlayerHome { player = inGame.player } } ! []

        ( MsgFromServer ServerHello, GameActive_ _ ) ->
            { state | state = InGame_ GameServerReconnected inGame } ! []

        ( MsgFromServer ServerHello, GameDisconnected ) ->
            { state | state = InGame_ GameServerReconnected inGame } ! []

        ( DoGameReconnect, GameServerReconnected ) ->
            { state | state = InGame_ GameReconnectPending inGame } ! []

        ( MsgFromServer (LoginFail_ _), GameReconnectPending ) ->
            { state | state = LoginFailed_ } ! []

        ( MsgFromServer (InitialInfoForGame_ initInfo), GameReconnectPending ) ->
            { state | state = InGame_ (GameActive_ YourTurn) inGame } ! []

        ( _, _ ) ->
            -- TODO: handle internal error
            state ! []



{-
   Implementation of The Elm Architecture for larger state-machines...

   To-define:
   * view
   * update
   * subscriptions

   approaches for modularity:
   * define updates together, view together, model together (horizontal splitting)
   * defined states together (vertical splitting)

   Notes:
   * functions are well composable
   * in fancy table: model, view & config defined together...
   * state-machine is covariant -> errors for violated covariance.
   * horizontal splitting already done and is natural for growing projects.
   * vertical splitting requires design-breakpoints

   -- TODO: remove

-}


const : a -> b -> a
const a b =
    a


log2 : String -> a -> b -> b
log2 s a b =
    const b (log s a)
