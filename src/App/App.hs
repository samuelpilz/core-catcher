{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module App.App (handleClientMsg, handleMsgStm) where

import           App.ConnectionMgnt
import           App.GameMgnt
import           App.State
import           ClassyPrelude
import qualified GameNg             as Game
import           GameState
import           Network.Protocol

{- TODO: features
* message for leaving a game??
* validation playerId matches connection-origin
* draw state diagrams for app / serverConnection
-}

handleClientMsg
    :: IsConnection conn
    => TVar (ServerState conn)
    -> ConnectionId
    -> MessageForServer
    -> IO ()
handleClientMsg stateVar cId msg = do
    putStrLn $ tshow msg
    toSend <- atomically $ handleMsgStm stateVar cId msg
    ServerState{serverStateConnections} <- readTVarIO stateVar
    mapM_ (uncurry sendSendableMsg) .
        mapMaybe
            (\(cIdToSend, m) -> do -- maybe monad
                conn <- findConnectionById cIdToSend serverStateConnections
                return (conn, m)
            ) $
        toSend

-- TODO: tests
handleMsgStm
    :: IsConnection conn
    => TVar (ServerState conn)
    -> ConnectionId
    -> MessageForServer
    -> STM [(ConnectionId, MessageForClient)]
handleMsgStm serverStateVar cId msg = do
    serverState <- readTVar serverStateVar
    case findConnectionById cId serverState of
        Nothing -> return [] -- TODO: fail
        Just (_,connState) ->
            case msg of
                Action_ action ->
                    case connectionInGame connState of
                        Nothing ->
                            case connectionLoggedInPlayer connState of
                                Nothing ->
                                    return [(cId, ServerError_ NotLoggedIn)]
                                Just player ->
                                    return [(cId, ServerError_ $ NotInGame player)]
                        Just gameId -> do
                            updateResult <- updateGame serverStateVar gameId action
                            case updateResult of
                                Right (Right newGameState) ->
                                    return $ distributeGameViewsForGame serverState newGameState
                                Right (Left gameError) ->
                                    return [(cId, GameError_ gameError)]
                                Left serverError ->
                                    return [(cId, ServerError_ serverError)]

                Login_ Login{ loginPlayer } -> do
                    modifyClientState serverStateVar cId (setConnectionLoggedInPlayer loginPlayer)
                    modifyTVar serverStateVar $ insertPlayer cId loginPlayer
                    return
                        [ (cId
                          , PlayerHome_ PlayerHome
                                { playerHomePlayer = loginPlayer
                                , activeGames = []
                                , activeLobbies = []
                                }
                          )
                          -- TODO: populate playerHome & general type for game-state-preview??
                        ]

                CreateNewGame_ CreateNewGame{createGameName} -> do
                    let gameLobby = GameLobby
                            { gameLobbyGameName = createGameName
                            , gameLobbyConnectedPlayers = maybeToList $ connectionLoggedInPlayer connState
                            }
                    gameId <- addGameState serverStateVar $ GameLobby_ gameLobby
                    modifyClientState serverStateVar cId (setConnectionInGame gameId)
                    return [(cId, GameLobbyView_ $ getGameLobbyView gameLobby)]

                _ -> return [] -- TODO: error for unexpected

distributeGameViewsForGame
    :: IsConnection conn
    => ServerState conn
    -> GameState -- TODO: gameId instead?
    -> [(ConnectionId, MessageForClient)]
distributeGameViewsForGame serverState gameState =
    map
        (\(player, cId) -> (cId, viewForGameState gameState player)) .
        mapToList $
        serverStatePlayerMap serverState

-- TODO: fix player-map (use players from state)


updateGame
    :: IsConnection conn
    => TVar (ServerState conn)
    -> GameId
    -> Action
    -> STM (Either ServerError (Either GameError GameState))
updateGame stateVar gameId action = do
    state <- readTVar stateVar

    let gameMay = findGameStateById gameId $ serverStateGameStates state
    case gameMay of
        Nothing ->
            return . Left . NoSuchGame $ gameId
        Just gameState -> do
            let updateResult = Game.updateState action gameState
            case updateResult of
                Right newGameState ->
                    updateGameState stateVar gameId newGameState
                Left _ -> return ()

            return . Right $ updateResult

