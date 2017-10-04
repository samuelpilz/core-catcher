module Experimental.Update exposing (..)

import Protocol exposing (..)
import Experimental.ClientState exposing (..)
import AllDict exposing (..)
import EveryDict
import ProtocolEmpty exposing (..)
import Debug exposing (log)
import Json.Encode exposing (encode)
import WebSocket


const : a -> b -> a
const a b =
    a


log2 : String -> a -> b -> b
log2 s a b =
    const b (log s a)


update : Msg -> ClientModel -> ( ClientModel, Cmd Msg )
update msg state =
    case log2 "msg" msg state.state of
        LandingArea_ landingAreaState landingArea ->
            updateLandingArea msg state ( landingAreaState, landingArea )

        LoggedIn_ loggedInState loggedIn ->
            updateLoggedIn msg state ( loggedInState, loggedIn )

        InGame_ inGameState inGame ->
            updateInGame msg state ( inGameState, inGame )

        Disconnected disconnect ->
            case msg of
                ToLandingPage ->
                    { state | state = LandingArea_ Landing { playerNameField = "" } } ! []

                MsgFromServer ServerHello ->
                    { state | state = Reconnected disconnect } ! []

                _ ->
                    state ! []

        Reconnected reconnect ->
            case msg of
                MsgFromServer (PlayerHome_ _) ->
                    { state | state = LoggedIn_ InPlayerHome reconnect.loggedIn } ! []

                MsgFromServer (LoginFail_ _) ->
                    { state | state = LandingArea_ LoginFailed { playerNameField = "" } } ! []

                ConnectionLost ->
                    { state | state = Disconnected reconnect } ! []

                _ ->
                    state ! []


updateLandingArea : Msg -> ClientModel -> ( LandingAreaState, LandingArea ) -> ( ClientModel, Cmd Msg )
updateLandingArea msg state ( landingAreaState, landingArea ) =
    case ( msg, landingAreaState ) of
        ( MsgFromServer ServerHello, Landing ) ->
            { state | state = LandingArea_ LandingConnected landingArea } ! []

        ( MsgFromServer ServerHello, LoginPending_ { pendingPlayer } ) ->
            -- resend the login message
            state ! [ sendProtocolMsg <| Login_ { loginPlayer = pendingPlayer } ]

        ( MsgFromServer ServerHello, _ ) ->
            -- explicitly do nothing
            state ! []

        ( ConnectionLost, _ ) ->
            { state | state = LandingArea_ Landing landingArea } ! []

        ( DoLogin, LandingConnected ) ->
            { state | state = LandingArea_ (LoginPending_ { pendingPlayer = emptyPlayer }) landingArea }
                ! [ sendProtocolMsg <| Login_ emptyLogin ]

        ( _, LoginPending_ loginPending ) ->
            updateLoginPending msg state landingArea loginPending

        ( ToLandingPage, LoginFailed ) ->
            { state | state = LandingArea_ LandingConnected landingArea } ! []

        ( _, _ ) ->
            state ! []


updateLoginPending : Msg -> ClientModel -> LandingArea -> LoginPending -> ( ClientModel, Cmd Msg )
updateLoginPending msg state landingArea loginPending =
    case msg of
        MsgFromServer (PlayerHome_ playerHome) ->
            { state | state = LoggedIn_ InPlayerHome { player = playerHome.playerHomePlayer } }
                ! []

        MsgFromServer (LoginFail_ _) ->
            { state | state = LandingArea_ LoginFailed landingArea } ! []

        _ ->
            state ! []


updateLoggedIn : Msg -> ClientModel -> ( LoggedInState, LoggedIn ) -> ( ClientModel, Cmd Msg )
updateLoggedIn msg state ( loggedInState, loggedIn ) =
    case ( msg, loggedInState ) of
        ( ConnectionLost, _ ) ->
            { state | state = Disconnected { loggedIn = loggedIn } } ! []

        ( MsgFromServer ServerHello, _ ) ->
            { state | state = Reconnected { loggedIn = loggedIn } }
                -- resend the login message
                ! [ sendProtocolMsg <| Login_ { loginPlayer = loggedIn.player } ]

        ( ToHome, _ ) ->
            { state | state = LoggedIn_ InPlayerHome loggedIn } ! []

        ( _, InPlayerHome ) ->
            updatePlayerHome msg state ( loggedInState, loggedIn )

        ( DoCreateGame, NewGame ) ->
            { state | state = LoggedIn_ NewGamePending loggedIn }
                ! [sendProtocolMsg <| CreateNewGame_ emptyCreateNewGame]

        ( MsgFromServer (GameLobbyView_ lobby), NewGamePending ) ->
            { state | state = LoggedIn_ InPreGameLobby loggedIn } ! []

        ( MsgFromServer (GameLobbyView_ lobby), JoinGamePending ) ->
            { state | state = LoggedIn_ InPreGameLobby loggedIn } ! []

        ( MsgFromServer (InitialInfoGameActive_ initInfo), InPreGameLobby ) ->
            { state | state = InGame_ (GameActive_ YourTurn) { player = loggedIn.player } } ! []

        ( _, GameConnectPending ) ->
            updateGameConnectPending msg state ( loggedInState, loggedIn )

        ( _, _ ) ->
            state ! []


updateGameConnectPending : Msg -> ClientModel -> ( LoggedInState, LoggedIn ) -> ( ClientModel, Cmd Msg )
updateGameConnectPending msg state ( loggedInState, loggedIn ) =
    case msg of
        MsgFromServer (InitialInfoGameActive_ initInfo) ->
            { state | state = InGame_ (GameActive_ YourTurn) { player = loggedIn.player } } ! []

        MsgFromServer (GameOverView_ gameOver) ->
            { state | state = LoggedIn_ GameOver { player = loggedIn.player } } ! []

        _ ->
            state ! []


updatePlayerHome : Msg -> ClientModel -> ( LoggedInState, LoggedIn ) -> ( ClientModel, Cmd Msg )
updatePlayerHome msg state ( loggedInState, loggedIn ) =
    case msg of
        DoOpenNewGame ->
            { state | state = LoggedIn_ NewGame loggedIn } ! []

        DoJoinGame ->
            { state | state = LoggedIn_ JoinGamePending loggedIn } ! []

        DoGameConnect ->
            { state | state = LoggedIn_ GameConnectPending loggedIn } ! []

        _ ->
            state ! []


updateInGame : Msg -> ClientModel -> ( InGameState, InGame ) -> ( ClientModel, Cmd Msg )
updateInGame msg state ( inGameState, inGame ) =
    case ( msg, inGameState ) of
        ( ConnectionLost, _ ) ->
            { state | state = InGame_ GameDisconnected inGame } ! []

        ( GameAction_ _, GameActive_ YourTurn ) ->
            { state | state = InGame_ (GameActive_ ActionPending) inGame } ! []

        ( MsgFromServer (GameView_ _), GameActive_ _ ) ->
            { state | state = InGame_ (GameActive_ OthersTurn) inGame } ! []

        ( MsgFromServer (GameOverView_ _), GameActive_ _ ) ->
            { state | state = LoggedIn_ GameOver { player = inGame.player } } ! []

        ( MsgFromServer ServerHello, GameActive_ _ ) ->
            { state | state = InGame_ GameServerReconnected inGame } ! []

        ( MsgFromServer ServerHello, GameDisconnected ) ->
            { state | state = InGame_ GameServerReconnected inGame } ! []

        ( MsgFromServer (LoginFail_ _), GameServerReconnected ) ->
            { state | state = LandingArea_ LoginFailed { playerNameField = "" } } ! []

        ( MsgFromServer (InitialInfoGameActive_ initInfo), GameServerReconnected ) ->
            { state | state = InGame_ (GameActive_ YourTurn) inGame } ! []

        ( MsgFromServer (GameOverView_ _), GameServerReconnected ) ->
            { state | state = LoggedIn_ GameOver { player = inGame.player } } ! []

        ( _, _ ) ->
            -- TODO: handle internal error
            state ! []



-- TODO: extract these functions


wsUrl : String -> String
wsUrl server =
    "ws://" ++ server ++ ":7999"


sendProtocolMsg : MessageForServer -> Cmd a
sendProtocolMsg msg =
    WebSocket.send (wsUrl "localhost")
        << encode 0
        << jsonEncMessageForServer
    <|
        log "ws-send" msg


sendProtocolMsgMay : Maybe MessageForServer -> Cmd a
sendProtocolMsgMay msgMay =
    case msgMay of
        Nothing ->
            Cmd.none

        Just msg ->
            sendProtocolMsg msg
