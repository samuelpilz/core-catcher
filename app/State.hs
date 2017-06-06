{-# LANGUAGE NoImplicitPrelude #-}

module State (ServerState(..), HasConnections, GameState) where

import           ClassyPrelude
import           ConnectionMgnt
import qualified GameLogic      as GL

type GameState = GL.GameState

data ServerState  =
    ServerState
        { connections :: ClientConnections
        , gameState   :: GameState
        }

instance HasConnections ServerState where
    getConnections = connections
    setConnections conn state = state { connections = conn }
