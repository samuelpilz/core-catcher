{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module App.AppUtils where

import           App.State
import           ClassyPrelude
import           Config.GameConfig
import           GameState
import           Network.Protocol


-- |create a singleton-list with one clientId-message pair
msgForOne :: ConnectionId -> MessageForClient -> [(ConnectionId, MessageForClient)]
msgForOne = singletonMap


distributeGameViewsForGame
    :: GameState
    -> ServerState conn
    -> [(ConnectionId, MessageForClient)]
distributeGameViewsForGame gameState serverState =
    mapMaybe (\p -> do
            cId <- lookupPlayerConnection p serverState
            return (cId, viewForGameState gameState p)) .
        gameStatePlayers $ gameState


distributeInitialInfosForGameRunning
    :: GameRunning
    -> ServerState conn
    -> [(ConnectionId, MessageForClient)]
distributeInitialInfosForGameRunning gameRunning serverState =
    mapMaybe (\p -> do
            cId <- lookupPlayerConnection p serverState
            return (cId, InitialInfoGameActive_ $ initialInfoGameActiveFromGameRunning gameRunning p)
        ) .
        toList .
        players .
        gameRunningGameConfig $
        gameRunning



