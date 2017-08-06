{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}

{-
This module implements functions managing Client Connections inside a bigger state-type.

The state-type has to be kept within an TVar variable and the update functions are done using IO

For instances, it is only necessary to TODO

Note: this is used for preventing mutually recursive modules.

TODO: write about what is exported and how to use this module
-}

module App.ConnectionMgnt (
    ConnectionId,
    ClientConnection(..),
    ClientConnections,
    HasConnections,
    Conn,
    connectClient,
    disconnectClient,
    getConnections,
    setConnections,
    findConnectionById,
    withoutClient,
    IsConnection,
    Pending,
    sendMsg,
    sendSendableMsg,
    recvMsg,
    acceptRequest,
    multicastMsg
    ) where

import           ClassyPrelude
import           Network.Protocol

type ConnectionId = Int
data ClientConnection conn =
    ClientConnection
        { connectionId :: ConnectionId
        , connection   :: conn
        }

-- TODO: newtype for that (and then remove wrap in mock)
type ClientConnections conn = (Seq (ClientConnection conn))

class IsConnection c where
    type Pending c :: *

    sendMsg :: c -> MessageForClient -> IO ()

    recvMsg :: c -> IO (Maybe MessageForServer)

    sendSendableMsg :: SendableToClient msg => c -> msg -> IO ()
    sendSendableMsg c msg = sendMsg c $ wrap msg

    acceptRequest ::  Pending c -> IO c

    multicastMsg ::
        (SendableToClient msg, MonoFoldable f, IsConnection c, c ~ Element f)
        => f -> msg -> IO ()
    multicastMsg cs msg = omapM_ (`sendSendableMsg` msg) cs

-- instance for the ClientConnection type which is just clientId together with
instance IsConnection conn => IsConnection (ClientConnection conn) where
    type Pending (ClientConnection conn) = (ConnectionId, Pending conn)

    sendMsg ClientConnection {connection} = sendMsg connection

    recvMsg ClientConnection {connection} = recvMsg connection

    acceptRequest (cId, pending) = do
        c <- acceptRequest pending
        return $ ClientConnection cId c



class HasConnections state where
    type Conn state :: *

    getConnections :: state -> ClientConnections (Conn state)

    setConnections :: ClientConnections (Conn state) -> state -> state

    connectClient :: Conn state -> TVar state -> IO ConnectionId
    connectClient conn stateVar = do
        clientId <- atomically $ addClient conn stateVar
        putStrLn $ "connect " ++ tshow clientId
        return clientId

    disconnectClient :: ConnectionId -> TVar state -> IO ()
    disconnectClient clientId stateVar = do
        atomically $ removeClient clientId stateVar
        putStrLn $ "disconnect " ++ tshow clientId

findConnectionById :: ConnectionId -> ClientConnections conn -> Maybe (ClientConnection conn)
findConnectionById cId =
    find ((==cId) . connectionId)

-- helper functions (not exported)

-- TODO: maybe acceptClient instead of addClient??
addClient :: HasConnections state => Conn state -> TVar state -> STM ConnectionId
addClient conn stateVar = do -- update connection list
    state <- readTVar stateVar
    let connections = getConnections state
    let newConnectionId = nextId connections
    let newConnections = ClientConnection newConnectionId conn `cons` getConnections state
    writeTVar stateVar (setConnections newConnections state)
    return newConnectionId

removeClient :: HasConnections state => ConnectionId -> TVar state -> STM ()
removeClient cId stateVar = do
    state <- readTVar stateVar
    let connections = getConnections state
    writeTVar stateVar (setConnections (withoutClient cId connections) state)

nextId :: ClientConnections conn -> ConnectionId
nextId = fromMaybe 0 . map (+1) . maximumMay . map connectionId

withoutClient :: ConnectionId -> ClientConnections conn -> ClientConnections conn
withoutClient cId = filter ((/=cId) . connectionId)
