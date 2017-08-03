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
        { connections :: Seq (ClientConnection conn)
        , gameState   :: GameState
        }

instance IsConnection conn => HasConnections (ServerState conn) where
    type Conn (ServerState conn) = conn
    getConnections =
        connections

    setConnections conn state =
        state
            { connections = conn
            }
