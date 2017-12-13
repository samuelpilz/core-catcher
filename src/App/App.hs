{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|

This module contain the main App-logic function.

The update-function is called every time the a client sends a message.

The return value is a monad which may contain an error and a state-transformation.

TODO: improve documentation

-}

module App.App (handleMsgState) where

import           App.ConnectionState
import           App.State
import           ClassyPrelude
import           Control.Error.MonadErrorInstance ()
import           Control.Error.Util               ((!?), (??))
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Except       (runExcept)
import           EntityMgnt
import qualified GameNg                           as Game
import           GameState
import           Network.Protocol
import           System.Random                    (RandomGen)

import           App.AppUtils

{- TODO: further features
* message for leaving a game??
-}


{- |basic update-function for the app.

This function handles a single message by a client.

-}
handleMsgState ::
    ( RandomGen gen
    , Monad m
    , MonadState m
    , StateType m ~ ServerState conn
    , MonadError m
    , ErrorType m ~ ServerError
    )
    => gen
    -> ConnectionId
    -> MessageForServer
    -> m [(ConnectionId, MessageForClient)]
handleMsgState gen cId msg = do
    connInfo <- findEntityByIdS cId !? NoSuchConnection
    let connState = connectionState connInfo

    case msg of
        Action_ action@Action{actionPlayer} -> do
            loginPlayer <- connectionLoggedInPlayer connState ?? NotLoggedIn
            gameId <- getConnectionInGameIdM connState
            gameState <- findEntityByIdS gameId !? NoSuchGame gameId

            -- check action integrity
            unless (loginPlayer == actionPlayer) $
                throwError $ ActionPlayerNotLoggedIn loginPlayer actionPlayer

            -- update game
            newGameState <-
                runExcept (Game.updateState action gameState) ?? GameError_
            updateEntityS gameId newGameState
            gets $ distributeGameViewsForGame newGameState

        Login_ Login{ loginPlayer } -> do
            -- register login player
            modifyEntityM cId $ connectionLoginPlayer loginPlayer
            modify $ insertPlayer cId loginPlayer

            -- get playerHome info
            (lobbies, games) <- gets $ allPreviewInfos . entityAssocs
            return $ msgForOne cId $
                PlayerHome_ PlayerHome
                    { playerHomePlayer = loginPlayer
                    , activeLobbies = lobbies
                    , activeGames = games
                    }

        Logout -> do
            when (isNothing $ connectionLoggedInPlayer connState) $ throwError NotLoggedIn
            modifyEntityM cId connectionLogoutPlayer
            return []

        PlayerHomeRefresh -> do
            loginPlayer <- connectionLoggedInPlayer connState ?? NotLoggedIn
            (lobbies, games) <- gets $ allPreviewInfos . entityAssocs

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
            modifyEntityM cId (connectionJoinGame gameId)

            return [(cId, GameLobbyView_ $ getGameLobbyView gameLobby)]

        StartGame -> do
            gameId <- getConnectionInGameIdM connState
            gameState <- findEntityByIdS gameId !? NoSuchGame gameId
            lobby <- getGameLobby gameState ?? GameAlreadyStarted

            -- start game
            gameRunning <-
                runExcept (Game.startGame gen lobby) ?? GameError_
            updateEntityS gameId $ GameRunning_ gameRunning

            gets $ distributeInitialInfosForGameRunning gameRunning

        JoinGame_ joinGame -> do
            -- get player and game information
            player <- connectionLoggedInPlayer connState ?? NotLoggedIn
            let gameId = joinGameId joinGame
            gameState <- findEntityByIdS gameId !? NoSuchGame gameId

            -- modify connection
            modifyEntityM cId $ connectionJoinGame gameId

            -- alter lobby
            lobby <- getGameLobby gameState ?? GameAlreadyStarted
            let newLobby = lobbyAddPlayer player lobby
            let newGame = GameLobby_ newLobby
            updateEntityS gameId newGame

            gets $ distributeGameViewsForGame newGame

