{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.State where

import           App.ConnectionState
import           ClassyPrelude
import           Data.Bimap          (Bimap)
import qualified Data.Bimap          as BiMap
import           EntityMgnt
import           GameState
import           Network.Protocol


newtype ConnectionId = ConnectionId Int
    deriving (Show, Eq, Ord)


data ServerState conn =
    ServerState
        { serverStateConnections :: Entities ConnectionId (ConnectionInfo conn)
        , serverStateGameStates  :: Entities GameId GameState
        , serverStatePlayerMap   :: Bimap Player ConnectionId
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
        , serverStatePlayerMap = BiMap.empty
        }


playerConnectionsList :: ServerState conn -> [(Player, ConnectionId)]
playerConnectionsList = BiMap.toList . serverStatePlayerMap


lookupPlayerConnection :: Player -> ServerState conn -> Maybe ConnectionId
lookupPlayerConnection p ServerState{ serverStatePlayerMap } =
    BiMap.lookup p serverStatePlayerMap


insertPlayer :: ConnectionId -> Player -> ServerState conn -> ServerState conn
insertPlayer cId p state@ServerState{ serverStatePlayerMap } =
   state { serverStatePlayerMap = BiMap.insert p cId serverStatePlayerMap }


removePlayer :: Player -> ServerState conn -> ServerState conn
removePlayer p state@ServerState{ serverStatePlayerMap } =
    state { serverStatePlayerMap = BiMap.delete p serverStatePlayerMap }


removeConnection :: ConnectionId -> ServerState conn -> ServerState conn
removeConnection cId state@ServerState{ serverStatePlayerMap } =
    state { serverStatePlayerMap = BiMap.deleteR cId serverStatePlayerMap }
