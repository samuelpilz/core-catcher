{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.AppUtils where

import           App.ConnectionState
import           App.State
import           ClassyPrelude
import           Config.GameConfig
import           Control.Error.Util         ((??))
import           Control.Monad.Trans.Except
import           GameState
import           Network.Protocol

-- TODO: refine these functions
getGameIdFromConnection :: Monad m => ConnectionState -> ExceptT ServerError m GameId
getGameIdFromConnection connState =
    connectionInGame connState ??
        maybe NotLoggedIn NotInGame (connectionLoggedInPlayer connState)

msgForOne :: ConnectionId -> MessageForClient -> [(ConnectionId, MessageForClient)]
msgForOne = singletonMap


distributeGameViewsForGame
    :: GameState
    -> ServerState conn
    -> [(ConnectionId, MessageForClient)]
distributeGameViewsForGame gameState serverState =
    mapMaybe (\p -> do
            cId <- lookup p $ serverStatePlayerMap serverState
            return (cId, viewForGameState gameState p)) .
        gameStatePlayers $ gameState

distributeInitialInfosForGameRunning
    :: GameRunning
    -> ServerState conn
    -> [(ConnectionId, MessageForClient)]
distributeInitialInfosForGameRunning gameRunning serverState =
    mapMaybe (\p -> do
            cId <- lookup p $ serverStatePlayerMap serverState
            return (cId, InitialInfoGameActive_ $ initialInfoGameActiveFromGameRunning gameRunning p)
        ) .
        toList .
        players .
        gameRunningGameConfig $
        gameRunning



