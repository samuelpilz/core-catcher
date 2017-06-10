{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module State
  ( ServerState(..)
  , HasConnections
  , GameState
  , IsConnection(..)
  , GameConnection(GameConnection)
  ) where

import           ClassyPrelude
import           Connection
import           ConnectionMgnt
import qualified GameLogic          as GL
import qualified Network.WebSockets as WS

type GameState = GL.GameState

newtype GameConnection = GameConnection WS.Connection

data ServerState conn =
    ServerState
        { connections :: Seq (ClientConnection conn)
        , gameState   :: GameState
        }

instance IsConnection conn => HasConnections (ServerState conn) where
    type Conn (ServerState conn) = conn
    getConnections = connections
    setConnections conn state = state { connections = conn }

instance IsConnection GameConnection where
    type Pending GameConnection = WS.PendingConnection

    sendData (GameConnection conn) = WS.sendTextData conn
    receiveData (GameConnection conn) = WS.receiveData conn
    acceptRequest pending = do
        conn <- WS.acceptRequest pending
        return $ GameConnection conn
