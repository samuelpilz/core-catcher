{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module App.ConnectionState where

import           ClassyPrelude
import           Control.Monad.Error.Class
import           Network.Protocol

-- |
data ConnectionInfo conn =
    ConnectionInfo
        { connection      :: conn
        , connectionState :: ConnectionState
        }

-- |Data type of
data ConnectionState
    = NewConnection
    | LoggedIn Player
    | InGame Player GameId


-- | create a new connection info with a given connection
newConnectionInfo :: conn -> ConnectionInfo conn
newConnectionInfo conn = ConnectionInfo conn NewConnection


connectionLoggedInPlayer :: ConnectionState -> Maybe Player
connectionLoggedInPlayer NewConnection = Nothing
connectionLoggedInPlayer (LoggedIn p)  = Just p
connectionLoggedInPlayer (InGame p _)  = Just p


connectionInGameId :: ConnectionState -> Maybe GameId
connectionInGameId (InGame _ gId) = Just gId
connectionInGameId _              = Nothing


-- |get the gameId from a connection and throw an error inside the monad
getConnectionInGameIdM ::
    ( MonadError m
    , ErrorType m ~ ServerError
    )
    => ConnectionState -> m GameId
getConnectionInGameIdM NewConnection =
            throwError NotLoggedIn
getConnectionInGameIdM (LoggedIn p) =
            throwError $ NotInGame p
getConnectionInGameIdM (InGame _ gId) =
            return gId


-- |login a player to a connectionInfo
connectionLoginPlayer ::
    ( Monad m
    , MonadError m
    , ErrorType m ~ ServerError
    )
    => Player
    -> ConnectionInfo conn
    -> m (ConnectionInfo conn)
connectionLoginPlayer p connInfo@ConnectionInfo{connectionState} =
    case connectionState of
        NewConnection ->
            return connInfo {connectionState = LoggedIn p }
        LoggedIn _ ->
            throwError $ AlreadyLoggedIn p
        InGame _ _ ->
            throwError $ AlreadyLoggedIn p


connectionLogoutPlayer ::
    ( Monad m
    , MonadError m
    , ErrorType m ~ ServerError
    )
    => ConnectionInfo conn
    -> m (ConnectionInfo conn)
connectionLogoutPlayer connInfo@ConnectionInfo{connectionState} =
    case connectionState of
        NewConnection ->
            throwError NotLoggedIn
        LoggedIn _ ->
            return connInfo {connectionState = NewConnection }
        InGame _ _ ->
            return connInfo {connectionState = NewConnection }

connectionJoinGame ::
    ( Monad m
    , MonadError m
    , ErrorType m ~ ServerError
    )
    => GameId
    -> ConnectionInfo conn
    -> m (ConnectionInfo conn)
connectionJoinGame gId connInfo@ConnectionInfo{connectionState} =
    case connectionState of
        NewConnection ->
            throwError NotLoggedIn
        LoggedIn p ->
            return connInfo {connectionState = InGame p gId }
        InGame _ gId' ->
            throwError $ AlreadyInGame gId'


