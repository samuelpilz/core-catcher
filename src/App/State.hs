{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module App.State
  ( ServerState(..)
  , HasConnections
  , GameState
  , IsConnection(..)
  , defaultInitialState
  ) where

import           App.ConnectionMgnt
import           ClassyPrelude
import           Config.GameConfig  (defaultConfig)
import           GameNg             (GameState (..), initialState)
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

defaultInitialState :: ServerState conn
defaultInitialState =
    ServerState
        { stateConnections = ClientConnections mempty 0
        , gameState = GameRunning_ $ initialState defaultConfig
        , playerMap = mempty
        }
