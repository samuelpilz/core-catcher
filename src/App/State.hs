{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module App.State
  ( ServerState(..)
  , HasConnections
  , GameState
  , IsConnection(..)
  ) where

import           App.ConnectionMgnt
import           ClassyPrelude
import           GameNg             (GameState)

data ServerState conn =
    ServerState
        { stateConnections :: ClientConnections conn
        , gameState        :: GameState
        }

instance IsConnection conn => HasConnections (ServerState conn) where
    type Conn (ServerState conn) = conn
    getConnections =
        stateConnections

    setConnections conns state =
        state
            { stateConnections = conns
            }
