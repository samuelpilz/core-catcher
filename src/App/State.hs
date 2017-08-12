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
import           Network.Protocol   (Player)

data ServerState conn =
    ServerState
        { stateConnections :: ClientConnections conn
        , gameState        :: GameState
        , playerMap        :: Map Player ConnectionId
        }

instance IsConnection conn => HasConnections (ServerState conn) where
    type Conn (ServerState conn) = conn
    getConnections =
        stateConnections

    setConnections conns state =
        state
            { stateConnections = conns
            }
