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
import           Config.GameConfig
import           Control.Error.Safe         (tryRight)
import           Control.Error.Util         ((??))
import           Control.Monad.Extra        (whenJust)
import           Control.Monad.Trans.Except
import           Data.Easy                  (eitherToMaybe, mapLeft, mapRight)
import qualified GameNg                     as Game
import           GameState
import           Network.Protocol
import           System.Random              (RandomGen, newStdGen)

import           App.AppUtils

{- TODO: features
* message for leaving a game??
* validation playerId matches connection-origin
* ban lists and use sequences or Foldable / MonoFoldable
* exception handling using ExceptT in handleMsgStm
* move the utils to own modules
* maybe use state-monad instead of stm?
-}

handleClientMsg
    :: IsConnection conn
    => TVar (ServerState conn)
    -> ConnectionId
    -> MessageForServer
    -> IO ()
handleClientMsg stateVar cId msg = do
    putStrLn $ tshow cId ++ " -> " ++ tshow msg

    gen <- newStdGen

    handleResult <- atomically . runExceptT $ handleMsgStm gen stateVar cId msg
    let toSend = either (msgForOne cId . ServerError_) id handleResult

    ServerState{serverStateConnections} <- readTVarIO stateVar
    mapM_
        (\(connId, conn, m) -> do -- IO monad
            putStrLn $ tshow connId ++ " <- " ++ tshow m
            sendSendableMsg conn m
        ) .
        mapMaybe
            (\(cIdToSend, m) -> do -- maybe monad
                conn <- findConnectionById cIdToSend serverStateConnections
                return (cIdToSend, conn, m)
            ) $
        toSend


-- TODO: tests
handleMsgStm
    :: (IsConnection conn, RandomGen gen)
    => gen
    -> TVar (ServerState conn)
    -> ConnectionId
    -> MessageForServer
    -> ExceptT ServerError STM [(ConnectionId, MessageForClient)]
handleMsgStm gen serverStateVar cId msg = do
    serverState <- lift $ readTVar serverStateVar
    connState <- findConnectionStateById cId serverState ?? NoSuchConnection
    case msg of
        Action_ action -> do
            gameId <- getGameIdFromConnection connState

            updatedState <- updateGameStm serverStateVar gameId action
            return $ distributeGameViewsForGame serverState updatedState

        Login_ Login{ loginPlayer } -> do
            lift $ modifyClientState serverStateVar cId (setConnectionLoggedInPlayer loginPlayer)
            lift $ modifyTVar serverStateVar $ insertPlayer cId loginPlayer
            let (lobbies, games) = allPreviewInfos . mapToList . gameStates . serverStateGameStates $ serverState
            return $ msgForOne cId $
                PlayerHome_ PlayerHome
                    { playerHomePlayer = loginPlayer
                    , activeLobbies = lobbies
                    , activeGames = games
                    }

        Logout -> do
            _ <- connectionLoggedInPlayer connState ?? NotLoggedIn
            lift $ modifyClientState serverStateVar cId connectionLogoutPlayer
            return []

        PlayerHomeRefresh -> do
            loginPlayer <- connectionLoggedInPlayer connState ?? NotLoggedIn
            let (lobbies, games) = allPreviewInfos . mapToList . gameStates . serverStateGameStates $ serverState

            return $ msgForOne cId $
                PlayerHome_ PlayerHome
                    { playerHomePlayer = loginPlayer
                    , activeLobbies = lobbies
                    , activeGames = games
                    }

        CreateNewGame_ CreateNewGame{createGameName} -> do
            let gameLobby = GameLobby
                    { gameLobbyGameName = createGameName
                    , gameLobbyConnectedPlayers = maybeToList $ connectionLoggedInPlayer connState
                    }
            gameId <- lift $ addGameState serverStateVar $ GameLobby_ gameLobby
            lift $ modifyClientState serverStateVar cId (setConnectionInGame gameId)
            return [(cId, GameLobbyView_ $ getGameLobbyView gameLobby)]

        StartGame -> do
            gameId <- getGameIdFromConnection connState
            gameState <- findGameStateById gameId serverState ?? NoSuchGame gameId

            -- start game
            lobby <- getGameLobby gameState ?? GameAlreadyStarted
            gameRunning <- tryRight . mapLeft GameError_ . Game.startGame gen $ lobby
            lift $ updateGameState serverStateVar gameId $ GameRunning_ gameRunning

            return $ distributeInitialInfosForGameRunning serverState gameRunning

        JoinGame_ joinGame -> do
            player <- connectionLoggedInPlayer connState ?? NotLoggedIn
            let gameId = joinGameId joinGame
            gameState <- findGameStateById gameId serverState ?? NoSuchGame gameId
            lift $ modifyClientState serverStateVar cId (setConnectionInGame gameId)

            lobby <- getGameLobby gameState ?? GameAlreadyStarted
            let newLobby = lobbyAddPlayer player lobby
            let newGame = GameLobby_ newLobby
            lift $ updateGameState serverStateVar gameId newGame

            return $ distributeGameViewsForGame serverState newGame


distributeGameViewsForGame
    :: IsConnection conn
    => ServerState conn
    -> GameState -- TODO: gameId instead? analyze usage for that
    -> [(ConnectionId, MessageForClient)]
distributeGameViewsForGame serverState gameState =
    mapMaybe (\p -> do
            cId <- lookup p $ serverStatePlayerMap serverState
            return (cId, viewForGameState gameState p)) .
        gameStatePlayers $ gameState

distributeInitialInfosForGameRunning
    :: ServerState conn
    -> GameRunning
    -> [(ConnectionId, MessageForClient)]
distributeInitialInfosForGameRunning serverState gameRunning =
    mapMaybe (\p -> do
            cId <- lookup p $ serverStatePlayerMap serverState
            return (cId, InitialInfoGameActive_ $ initialInfoGameActiveFromGameRunning gameRunning p)
        ) .
        toList .
        toNullable .
        players .
        gameRunningGameConfig $
        gameRunning


updateGameStm
    :: IsConnection conn
    => TVar (ServerState conn)
    -> GameId
    -> Action
    -> ExceptT ServerError STM GameState
updateGameStm stateVar gameId action = do
    state <- lift $ readTVar stateVar

    gameState <- maybe (throwE $ NoSuchGame gameId) return $
        findGameStateById gameId $ serverStateGameStates state

    let updateResult = mapLeft GameError_ $ Game.updateState action gameState
    newGameState <- tryRight updateResult

    lift $ updateGameState stateVar gameId newGameState
    return newGameState
