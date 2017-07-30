{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module App.State
  ( ServerState(..)
  , HasConnections
  , GameState
  , IsConnection(..)
  , GameConnection(GameConnection)
  ) where

import           App.Connection
import           App.ConnectionMgnt
import           ClassyPrelude
import           GameNg             (GameState)
import qualified Network.Protocol   ()
import qualified Network.WebSockets as WS

newtype GameConnection = GameConnection WS.Connection

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

instance IsConnection GameConnection where
    type Pending GameConnection = WS.PendingConnection

    sendData (GameConnection conn) =
        WS.sendTextData conn

    receiveData (GameConnection conn) =
        WS.receiveData conn

    acceptRequest pending =
        GameConnection `map` WS.acceptRequest pending
