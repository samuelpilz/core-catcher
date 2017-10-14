{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module App.ConnectionState where

-- import           App.Connection
import           ClassyPrelude
import           Network.Protocol

data ConnectionInfo conn =
    ConnectionInfo
        { connection      :: conn
        , connectionState :: ConnectionState
        }

-- TODO: ADT for connection states
data ConnectionState =
    ConnectionState
        { connectionLoggedInPlayer :: Maybe Player
        , connectionInGame         :: Maybe GameId
        }

newConnectionInfo :: conn -> ConnectionInfo conn
newConnectionInfo conn = ConnectionInfo conn newConnectionState

newConnectionState :: ConnectionState
newConnectionState = ConnectionState Nothing Nothing

setConnectionLoggedInPlayer :: Player -> ConnectionInfo conn -> ConnectionInfo conn
setConnectionLoggedInPlayer player connInfo@ConnectionInfo{connectionState} =
    connInfo
        { connectionState =
            connectionState
                { connectionLoggedInPlayer = Just player
            }
        }


connectionLogoutPlayer :: ConnectionInfo conn -> ConnectionInfo conn
connectionLogoutPlayer connInfo@ConnectionInfo{connectionState} =
    connInfo
        { connectionState =
            connectionState
                { connectionLoggedInPlayer = Nothing }
        }


setConnectionInGame :: GameId -> ConnectionInfo conn -> ConnectionInfo conn
setConnectionInGame gameId connInfo@ConnectionInfo{connectionState} =
    connInfo
        { connectionState =
            connectionState
                { connectionInGame = Just gameId
            }
        }

