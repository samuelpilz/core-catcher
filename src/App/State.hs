{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module App.State where

import           App.ConnectionMgnt
import           App.GameMgnt
import           ClassyPrelude
import           Network.Protocol   (Player)

data ServerState conn =
    ServerState
        { serverStateConnections :: ClientConnections conn ConnectionState
        , serverStateGameStates  :: GameStates
        , serverStatePlayerMap   :: Map Player ConnectionId
        }

data ConnectionState =
    ConnectionState
        { connectionLoggedInPlayer :: Maybe Player
        , connectionInGame         :: Maybe GameId
        }

setConnectionLoggedInPlayer :: Player -> ConnectionState -> ConnectionState
setConnectionLoggedInPlayer player state = state { connectionLoggedInPlayer = Just player }

setConnectionInGame :: GameId -> ConnectionState -> ConnectionState
setConnectionInGame gameId state = state { connectionInGame = Just gameId }

instance IsConnection conn => HasConnections (ServerState conn) where
    type Conn (ServerState conn) = conn
    type ConnState (ServerState conn) = ConnectionState

    getConnections =
        serverStateConnections

    setConnections conns state =
        state { serverStateConnections = conns }

instance HasGameStates (ServerState conn) where
    getGameStates = serverStateGameStates
    setGameStates states state = state { serverStateGameStates = states }

instance IsConnectionState ConnectionState where
    newConnectionState = ConnectionState Nothing Nothing

defaultInitialState :: ServerState conn
defaultInitialState =
    ServerState
        { serverStateConnections = ClientConnections mempty 0
        , serverStateGameStates = GameStates mempty 0
        , serverStatePlayerMap = mempty
        }

insertPlayer :: ConnectionId -> Player -> ServerState conn -> ServerState conn
insertPlayer cId p state@ServerState{ serverStatePlayerMap } =
   state { serverStatePlayerMap = insertMap p cId serverStatePlayerMap }

-- TODO: removePlayer (maybe playerMgnt)
