{-# LANGUAGE NoImplicitPrelude #-}

module State (ServerState(..), HasConnections, GameState) where

import           ClassyPrelude
import           ConnectionMgnt

type GameState = String

data ServerState  =
    ServerState
        { connections :: ClientConnections
        , gameState   :: GameState
        }

instance HasConnections ServerState where
    getConnections = connections
    setConnections conn state = state { connections = conn }
