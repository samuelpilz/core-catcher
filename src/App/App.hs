{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module App.App (handleClientMsg, handleMsgStm) where

import           App.Connection
import           App.ConnectionState
import           App.State
import           ClassyPrelude
import           Control.Error.Safe         (tryRight)
import           Control.Error.Util         ((??), (!?))
import           Control.Monad.State        (State, runState)
import qualified Control.Monad.State        as State
import           Control.Monad.Trans.Except
import           Data.Easy                  (mapLeft)
import           EntityMgnt
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
handleClientMsg serverStateVar cId msg = do
    putStrLn $ tshow cId ++ " -> " ++ tshow msg

    gen <- newStdGen
    (toSend, serverState) <- atomically $ handleMsgStm gen serverStateVar cId msg

    mapM_
        (\(connId, connInfo, m) -> do -- IO monad
            putStrLn $ tshow connId ++ " <- " ++ tshow m
            sendSendableMsg (connection connInfo) m
        ) .
        mapMaybe
            (\(cIdToSend, m) -> do -- maybe monad
                connInfo :: ConnectionInfo conn <- findEntityById cIdToSend serverState
                return (cIdToSend, connInfo, m)
            ) $
        toSend

-- |handles a single message and saves the new state
handleMsgStm :: RandomGen gen => gen -> TVar (ServerState conn) -> ConnectionId -> MessageForServer
    -> STM ([(ConnectionId, MessageForClient)], ServerState conn)
handleMsgStm gen serverStateVar cId msg = do -- STM monad
    serverState <- readTVar serverStateVar
    let (updateResult, newServerState) =
            runState (runExceptT $ handleMsgState gen cId msg) serverState
    case updateResult of
        Left err ->
            return (msgForOne cId $ ServerError_ err, serverState)
        Right toSend -> do
            writeTVar serverStateVar newServerState
            return (toSend, newServerState)

-- TODO: tests
handleMsgState
    :: RandomGen gen
    => gen
    -> ConnectionId
    -> MessageForServer
    -> ExceptT ServerError (State (ServerState conn)) [(ConnectionId, MessageForClient)]
handleMsgState gen cId msg = do -- monad: ExceptT ServerError (..) [..]
    connInfo <- findEntityByIdS cId !? NoSuchConnection
    let connState = connectionState connInfo

    case msg of
        Action_ action -> do
            gameId <- getGameIdFromConnection connState
            gameState <- findEntityByIdS gameId !? NoSuchGame gameId

            -- update game
            newGameState :: GameState <- tryRight . mapLeft GameError_ $ Game.updateState action gameState
            updateEntityS gameId newGameState
            State.gets $ distributeGameViewsForGame newGameState

        Login_ Login{ loginPlayer } -> do
            -- register login player
            modifyEntityS cId $ setConnectionLoggedInPlayer loginPlayer
            State.modify $ insertPlayer cId loginPlayer

            -- get playerHome info
            (lobbies, games) <- State.gets $ allPreviewInfos . entityAssocs
            return $ msgForOne cId $
                PlayerHome_ PlayerHome
                    { playerHomePlayer = loginPlayer
                    , activeLobbies = lobbies
                    , activeGames = games
                    }

        Logout -> do
            when (isNothing $ connectionLoggedInPlayer connState) $ throwE NotLoggedIn
            modifyEntityS cId connectionLogoutPlayer
            return []

        PlayerHomeRefresh -> do
            loginPlayer <- connectionLoggedInPlayer connState ?? NotLoggedIn
            (lobbies, games) <- State.gets $ allPreviewInfos . entityAssocs

            return $ msgForOne cId $
                PlayerHome_ PlayerHome
                    { playerHomePlayer = loginPlayer
                    , activeLobbies = lobbies
                    , activeGames = games
                    }

        CreateNewGame_ CreateNewGame{createGameName} -> do
            player <- connectionLoggedInPlayer connState ?? NotLoggedIn

            -- create game
            let gameLobby = GameLobby
                    { gameLobbyGameName = createGameName
                    , gameLobbyConnectedPlayers = [player]
                    }
            gameId <- addEntityS (GameLobby_ gameLobby)

            -- set game in connection-state
            modifyEntityS cId (setConnectionInGame gameId)

            return [(cId, GameLobbyView_ $ getGameLobbyView gameLobby)]

        StartGame -> do
            gameId <- getGameIdFromConnection connState
            gameState <- findEntityByIdS gameId !? NoSuchGame gameId

            -- start game
            lobby <- getGameLobby gameState ?? GameAlreadyStarted
            gameRunning <- tryRight . mapLeft GameError_ . Game.startGame gen $ lobby
            updateEntityS gameId $ GameRunning_ gameRunning

            State.gets $ distributeInitialInfosForGameRunning gameRunning

        JoinGame_ joinGame -> do
            -- get player and game information
            player <- connectionLoggedInPlayer connState ?? NotLoggedIn
            let gameId = joinGameId joinGame
            gameState <- findEntityByIdS gameId !? NoSuchGame gameId

            -- modify connection
            modifyEntityS cId $ setConnectionInGame gameId

            -- alter lobby
            lobby <- getGameLobby gameState ?? GameAlreadyStarted
            let newLobby = lobbyAddPlayer player lobby
            let newGame = GameLobby_ newLobby
            updateEntityS gameId newGame

            State.gets $ distributeGameViewsForGame newGame

