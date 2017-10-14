module Experimental.Update exposing (..)

import Protocol exposing (..)
import Experimental.ClientState exposing (..)
import AllDict exposing (..)
import EveryDict
import ProtocolEmpty exposing (..)
import Debug exposing (log)
import Json.Encode exposing (encode)
import WebSocket
import Example.ExampleGameViewDisplay as Example
import ProtocolUtils exposing (..)
import View.PlayerAnimation exposing (..)


-- TODO: import view in update??


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
                MsgFromServer (PlayerHome_ playerHome) ->
                    { state
                        | state =
                            LoggedIn_
                                InPlayerHome
                                reconnect.loggedIn
                    }
                        ! []

                MsgFromServer (LoginFail_ _) ->
                    { state | state = LandingArea_ LoginFailed { playerNameField = "" } } ! []

                ConnectionLost ->
                    { state | state = Disconnected reconnect } ! []

                _ ->
                    state ! []


updateLandingArea : Msg -> ClientModel -> ( LandingAreaState, LandingArea ) -> ( ClientModel, Cmd Msg )
updateLandingArea msg state ( landingAreaState, landingArea ) =
    case ( msg, landingAreaState ) of
        ( ToLandingPage, _ ) ->
            { state | state = LandingArea_ LandingConnected landingArea } ! []

        ( MsgFromServer ServerHello, Landing ) ->
            { state | state = LandingArea_ LandingConnected landingArea } ! []

        ( MsgFromServer ServerHello, LoginPending_ { pendingPlayer } ) ->
            -- resend the login message
            state ! [ sendProtocolMsg state.server <| Login_ { loginPlayer = pendingPlayer } ]

        ( MsgFromServer ServerHello, _ ) ->
            -- explicitly do nothing
            state ! []

        ( ConnectionLost, _ ) ->
            { state | state = LandingArea_ Landing landingArea } ! []

        ( PlayerNameChange newPlayerName, _ ) ->
            { state
                | state =
                    LandingArea_
                        landingAreaState
                        { landingArea | playerNameField = newPlayerName }
            }
                ! []

        ( DoLogin, LandingConnected ) ->
            { state
                | state =
                    LandingArea_
                        (LoginPending_
                            { pendingPlayer =
                                { playerName = landingArea.playerNameField }
                            }
                        )
                        landingArea
            }
                ! [ sendProtocolMsg state.server <|
                        Login_
                            { loginPlayer =
                                { playerName = landingArea.playerNameField }
                            }
                  ]

        ( _, LoginPending_ loginPending ) ->
            updateLoginPending msg state landingArea loginPending

        ( _, _ ) ->
            state ! []


updateLoginPending : Msg -> ClientModel -> LandingArea -> LoginPending -> ( ClientModel, Cmd Msg )
updateLoginPending msg state landingArea loginPending =
    case msg of
        MsgFromServer (PlayerHome_ playerHome) ->
            { state
                | state =
                    LoggedIn_
                        InPlayerHome
                        { player = playerHome.playerHomePlayer
                        , playerHomeContent =
                            { activeLobbies = Just <| playerHome.activeLobbies
                            , activeGames = Just <| playerHome.activeGames
                            }
                        }
            }
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
                ! [ sendProtocolMsg state.server <| Login_ { loginPlayer = loggedIn.player } ]

        ( ToHome, _ ) ->
            { state | state = LoggedIn_ InPlayerHome loggedIn } ! []

        ( DoLogout, _ ) ->
            { state | state = LandingArea_ LoggedOut { playerNameField = "" } }
                ! [ sendProtocolMsg state.server Logout ]

        ( _, InPlayerHome ) ->
            updatePlayerHome msg state ( loggedInState, loggedIn )

        ( DoCreateGame, NewGame ) ->
            { state | state = LoggedIn_ NewGamePending loggedIn }
                ! [ sendProtocolMsg state.server <|
                        -- TODO: make field for game name
                        CreateNewGame_ { createGameName = loggedIn.player.playerName }
                  ]

        ( MsgFromServer (GameLobbyView_ lobby), NewGamePending ) ->
            { state
                | state =
                    InGame_
                        (InPreGameLobby_
                            { players = lobby.gameLobbyViewPlayers
                            }
                        )
                        { player = loggedIn.player
                        , gameName = lobby.gameLobbyViewGameName
                        }
            }
                ! []

        ( MsgFromServer (GameLobbyView_ lobby), JoinGamePending ) ->
            { state
                | state =
                    InGame_
                        (InPreGameLobby_
                            { players = lobby.gameLobbyViewPlayers
                            }
                        )
                        { player = loggedIn.player
                        , gameName = lobby.gameLobbyViewGameName
                        }
            }
                ! []

        ( Tick dt, GameOver_ gameOver ) ->
            let
                networkModel =
                    gameOver.networkModel
            in
                { state
                    | state =
                        LoggedIn_
                            (GameOver_
                                { gameOver
                                    | networkModel =
                                        { networkModel
                                            | animationTime = dt + networkModel.animationTime
                                            , activeAnimations =
                                                -- filter done animations
                                                AllDict.filter
                                                    (\_ a ->
                                                        a.startTime
                                                            + networkModel.displayInfo.movementAnimationDuration
                                                            > networkModel.animationTime
                                                    )
                                                    networkModel.activeAnimations
                                        }
                                }
                            )
                            loggedIn
                }
                    ! []

        ( _, GameConnectPending ) ->
            updateGameConnectPending msg state ( loggedInState, loggedIn )

        ( _, _ ) ->
            state ! []


updateGameConnectPending : Msg -> ClientModel -> ( LoggedInState, LoggedIn ) -> ( ClientModel, Cmd Msg )
updateGameConnectPending msg state ( loggedInState, loggedIn ) =
    case msg of
        MsgFromServer (InitialInfoGameActive_ initInfo) ->
            { state
                | state =
                    InGame_
                        (GameActive_ YourTurn
                            { networkModel =
                                networkModelFromInitInfo
                                    Example.displayInfo
                                    loggedIn.player
                                    initInfo
                            }
                        )
                        { player = loggedIn.player
                        , gameName = initInfo.initialInfoGameName
                        }
            }
                ! []

        MsgFromServer (GameOverView_ gameOverView) ->
            { state
                | state =
                    LoggedIn_
                        (GameOver_
                            { networkModel =
                                networkModelFromGameOverView
                                    Example.displayInfo
                                    loggedIn.player
                                    gameOverView
                            }
                        )
                        loggedIn
            }
                ! []

        _ ->
            state ! []


updatePlayerHome : Msg -> ClientModel -> ( LoggedInState, LoggedIn ) -> ( ClientModel, Cmd Msg )
updatePlayerHome msg state ( loggedInState, loggedIn ) =
    case msg of
        DoOpenNewGame ->
            { state | state = LoggedIn_ NewGame loggedIn } ! []

        DoJoinGame gameId ->
            { state | state = LoggedIn_ JoinGamePending loggedIn }
                ! [ sendProtocolMsg state.server <| JoinGame_ { joinGameId = gameId } ]

        DoGameConnect ->
            { state | state = LoggedIn_ GameConnectPending loggedIn } ! []

        DoPlayerHomeRefresh ->
            state ! [ sendProtocolMsg state.server PlayerHomeRefresh ]

        MsgFromServer (PlayerHome_ playerHome) ->
            { state
                | state =
                    LoggedIn_
                        InPlayerHome
                        { loggedIn
                            | playerHomeContent =
                                { activeLobbies = Just <| playerHome.activeLobbies
                                , activeGames = Just <| playerHome.activeGames
                                }
                        }
            }
                ! []

        _ ->
            state ! []


updateInGame : Msg -> ClientModel -> ( InGameState, InGame ) -> ( ClientModel, Cmd Msg )
updateInGame msg state ( inGameState, inGame ) =
    case ( msg, inGameState ) of
        ( MsgFromServer (GameLobbyView_ lobbyView), InPreGameLobby_ lobby ) ->
            { state
                | state =
                    InGame_
                        (InPreGameLobby_
                            { lobby
                                | players = lobbyView.gameLobbyViewPlayers
                            }
                        )
                        inGame
            }
                ! []

        ( ConnectionLost, _ ) ->
            { state | state = InGame_ GameDisconnected inGame } ! []

        ( MsgFromServer (InitialInfoGameActive_ initInfo), InPreGameLobby_ _ ) ->
            { state
                | state =
                    InGame_
                        (GameActive_ YourTurn
                            { networkModel =
                                networkModelFromInitInfo
                                    Example.displayInfo
                                    inGame.player
                                    initInfo
                            }
                        )
                        inGame
            }
                ! []

        ( DoStartGame, InPreGameLobby_ _ ) ->
            state ! [ sendProtocolMsg state.server StartGame ]

        -- TODO: helper functions that allow for more actions?
        ( GameAction_ (Movement targetNode), GameActive_ YourTurn gameActive ) ->
            case gameActive.networkModel.selectedEnergy of
                Nothing ->
                    state ! []

                Just energy ->
                    { state | state = InGame_ (GameActive_ ActionPending gameActive) inGame }
                        ! [ sendProtocolMsg state.server <|
                                Action_
                                    { actionPlayer = inGame.player
                                    , actionEnergy = energy
                                    , actionNode = targetNode
                                    }
                          ]

        ( SelectEnergy energy, GameActive_ gameActiveState gameActive ) ->
            let
                networkModel =
                    gameActive.networkModel
            in
                { state
                    | state =
                        InGame_
                            (GameActive_ gameActiveState
                                { gameActive
                                    | networkModel =
                                        { networkModel | selectedEnergy = Just energy }
                                }
                            )
                            inGame
                }
                    ! []

        ( MsgFromServer (GameView_ gameView), GameActive_ _ gameActive ) ->
            let
                networkModel =
                    gameActive.networkModel

                newActiveState =
                    if nextPlayer gameView == inGame.player then
                        YourTurn
                    else
                        OthersTurn
            in
                { state
                    | state =
                        InGame_
                            (GameActive_ newActiveState
                                { gameActive
                                    | networkModel =
                                        { networkModel
                                            | playerPositions = playerPositions gameView
                                            , playerEnergies = playerEnergies gameView
                                            , rogueHistory = rogueHistory gameView
                                            , nextPlayer = Just <| nextPlayer gameView
                                            , activeAnimations =
                                                updateActiveAnimations networkModel <|
                                                    playerPositions gameView
                                        }
                                }
                            )
                            inGame
                }
                    ! []

        ( Tick dt, GameActive_ gameActiveState gameActive ) ->
            let
                networkModel =
                    gameActive.networkModel
            in
                { state
                    | state =
                        InGame_
                            (GameActive_ gameActiveState
                                { gameActive
                                    | networkModel =
                                        { networkModel
                                            | animationTime = dt + networkModel.animationTime
                                            , activeAnimations =
                                                -- filter done animations
                                                AllDict.filter
                                                    (\_ a ->
                                                        a.startTime
                                                            + networkModel.displayInfo.movementAnimationDuration
                                                            > networkModel.animationTime
                                                    )
                                                    networkModel.activeAnimations
                                        }
                                }
                            )
                            inGame
                }
                    ! []

        ( MsgFromServer (ServerError_ (GameError_ err)), GameActive_ ActionPending gameActive ) ->
            { state | state = InGame_ (GameActive_ YourTurn gameActive) inGame } ! []

        ( MsgFromServer (GameOverView_ gameOverView), GameActive_ _ gameActive ) ->
            let
                networkModel =
                    gameActive.networkModel

                newNetworkModel =
                    { networkModel
                        | playerPositions = gameOverView.gameOverViewPlayerPositions
                        , playerEnergies = gameOverView.gameOverViewPlayerEnergies
                        , rogueHistory = OpenHistory gameOverView.gameOverViewRogueHistory
                        , nextPlayer = Nothing
                        , selectedEnergy = Nothing
                        , activeAnimations =
                            updateActiveAnimations
                                networkModel
                                gameOverView.gameOverViewPlayerPositions
                    }
            in
                { state
                    | state =
                        LoggedIn_ (GameOver_ { networkModel = gameActive.networkModel })
                            { player = inGame.player
                            , playerHomeContent = emptyPlayerHomeContent
                            }
                }
                    ! []

        ( MsgFromServer ServerHello, GameActive_ _ _ ) ->
            { state | state = InGame_ GameServerReconnected inGame } ! []

        ( MsgFromServer ServerHello, GameDisconnected ) ->
            { state | state = InGame_ GameServerReconnected inGame } ! []

        ( MsgFromServer (LoginFail_ _), GameServerReconnected ) ->
            { state | state = LandingArea_ LoginFailed { playerNameField = "" } } ! []

        ( MsgFromServer (InitialInfoGameActive_ initInfo), GameServerReconnected ) ->
            { state
                | state =
                    InGame_
                        (GameActive_ YourTurn
                            { networkModel =
                                networkModelFromInitInfo
                                    Example.displayInfo
                                    inGame.player
                                    initInfo
                            }
                        )
                        inGame
            }
                ! []

        -- TODO: fix this. upper: copied code; lower: old code. Missing: networkModel in reconnected...
        --        ( MsgFromServer (GameOverView_ _), GameServerReconnected ) ->
        --            { state
        --                | state =
        --                    LoggedIn_ (GameOver_ { networkModel = gameActive.networkModel })
        --                        { player = inGame.player
        --                        , playerHomeContent = emptyPlayerHomeContent
        --                        }
        --            }
        --                ! []
        --        ( MsgFromServer (GameOverView_ _), GameServerReconnected ) ->
        --            { state
        --                | state =
        --                    LoggedIn_
        --                        (GameOver_
        --                            { player = inGame.player
        --                            , playerHomeContent = emptyPlayerHomeContent
        --                            }
        --                        )
        --            }
        --                ! []
        ( _, _ ) ->
            state ! []


emptyPlayerHomeContent : PlayerHomeContent
emptyPlayerHomeContent =
    { activeLobbies = Nothing
    , activeGames = Nothing
    }



-- TODO: extract these functions


wsUrl : String -> String
wsUrl server =
    "ws://" ++ server ++ ":7999"


sendProtocolMsg : String -> MessageForServer -> Cmd a
sendProtocolMsg server msg =
    WebSocket.send (wsUrl server)
        << encode 0
        << jsonEncMessageForServer
    <|
        log "ws-send" msg



--sendProtocolMsgMay : Maybe MessageForServer -> Cmd a
--sendProtocolMsgMay msgMay =
--    case msgMay of
--        Nothing ->
--            Cmd.none
--
--        Just msg ->
--            sendProtocolMsg msg


const : a -> b -> a
const a b =
    a


log2 : String -> a -> b -> b
log2 s a b =
    const b (log s a)
