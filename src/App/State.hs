{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module App.State
  ( ServerState(..)
  , HasConnections
  , GameState
  , IsConnection(..)
  , defaultInitialState
  , defaultInitialStateWithRandomPositions
  ) where

import           App.ConnectionMgnt
import           ClassyPrelude
import           Config.GameConfig  (defaultConfig, defaultConfigWithRandomPositions)
import           GameNg             (GameState (..), initialStateFromConfig)
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

defaultInitialStateWithRandomPositions :: IO (ServerState conn)
defaultInitialStateWithRandomPositions = do
    config <- defaultConfigWithRandomPositions
    return ServerState
        { stateConnections = ClientConnections mempty 0
        , gameState = GameRunning_ $ initialStateFromConfig config
        , playerMap = mempty
        }

defaultInitialState :: ServerState conn
defaultInitialState =
    ServerState
        { stateConnections = ClientConnections mempty 0
        , gameState = GameRunning_ $ initialStateFromConfig defaultConfig
        , playerMap = mempty
        }
