{-# LANGUAGE NoImplicitPrelude #-}

module State (ServerState(..), HasConnections) where

import           ClassyPrelude
import           ConnectionMgnt

data ServerState  =
    ServerState
        { connections :: [ClientConnection]
        , gameState   :: String
        }

instance HasConnections ServerState where
    getConnections = connections
    setConnections conn state = state { connections = conn }
