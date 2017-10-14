{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module App.State where

import           App.ConnectionState
import           ClassyPrelude
import           EntityMgnt
import           GameState
import           Network.Protocol

newtype ConnectionId = ConnectionId Int
    deriving (Show, Eq, Ord)

-- TODO: think about multi-connections
data ServerState conn =
    ServerState
        { serverStateConnections :: Entities ConnectionId (ConnectionInfo conn)
        , serverStateGameStates  :: Entities GameId GameState
        , serverStatePlayerMap   :: Map Player ConnectionId
        }

instance EntityId GameId where
    getFirstId = GameId 0
    getNextId (GameId n) = GameId $ n+1

instance EntityId ConnectionId where
    getFirstId = ConnectionId 0
    getNextId (ConnectionId n) = ConnectionId $ n+1

instance HasEntities (ServerState conn) ConnectionId where
    type Entity (ServerState conn) ConnectionId = ConnectionInfo conn
    getEntities = serverStateConnections
    setEntities conns state = state { serverStateConnections = conns }

instance HasEntities (ServerState conn) GameId where
    type Entity (ServerState conn) GameId = GameState
    getEntities = serverStateGameStates
    setEntities states state = state { serverStateGameStates = states }

defaultInitialState :: ServerState conn
defaultInitialState =
    ServerState
        { serverStateConnections = emptyEntities
        , serverStateGameStates = emptyEntities
        , serverStatePlayerMap = mempty
        }

insertPlayer :: ConnectionId -> Player -> ServerState conn -> ServerState conn
insertPlayer cId p state@ServerState{ serverStatePlayerMap } =
   state { serverStatePlayerMap = insertMap p cId serverStatePlayerMap }

-- TODO: removePlayer (maybe playerMgnt)
