{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
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
    ClientId,
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

type ClientId = Int
newtype ClientConnection conn = ClientConnection (ClientId, conn) -- TODO: make record

type ClientConnections conn = Seq (ClientConnection conn)
-- TODO: include id-sequence

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
    type Pending (ClientConnection conn) = (ClientId, Pending conn)

    sendMsg (ClientConnection (_,c)) = sendMsg c

    recvMsg (ClientConnection (_,c)) = recvMsg c

    acceptRequest (clientId, pending) = do
        c <- acceptRequest pending
        return $ ClientConnection (clientId, c)



class HasConnections state where
    type Conn state :: *

    getConnections :: state -> ClientConnections (Conn state)

    setConnections :: ClientConnections (Conn state) -> state -> state

    connectClient :: Conn state -> TVar state -> IO ClientId
    connectClient conn stateVar = do
        clientId <- atomically $ addClient conn stateVar
        putStrLn $ "connect " ++ tshow clientId
        return clientId

    disconnectClient :: ClientId -> TVar state -> IO ()
    disconnectClient clientId stateVar = do
        atomically $ removeClient clientId stateVar
        putStrLn $ "disconnect " ++ tshow clientId



findConnectionById :: ClientId -> ClientConnections conn -> Maybe (ClientConnection conn)
findConnectionById cid =
    find (\(ClientConnection (c, _)) -> c == cid)

-- helper functions (not exported)

-- TODO: maybe acceptClient instead of addClient??
addClient :: HasConnections state => Conn state -> TVar state -> STM ClientId
addClient conn stateVar = do -- update connection list
    state <- readTVar stateVar
    let connections = getConnections state
    let newClientId = nextId connections
    let newConnections = ClientConnection (newClientId, conn) `cons` getConnections state
    writeTVar stateVar (setConnections newConnections state)
    return newClientId

removeClient :: HasConnections state => ClientId -> TVar state -> STM ()
removeClient clientId stateVar = do
    state <- readTVar stateVar
    let connections = getConnections state
    writeTVar stateVar (setConnections (withoutClient clientId connections) state)

nextId :: ClientConnections conn -> ClientId
nextId = const 0 --fromMaybe 0 . map (+1) . maximumMay . map fst TODO: fix

withoutClient :: ClientId -> ClientConnections conn -> ClientConnections conn
withoutClient clientId = filter (\(ClientConnection (c, _)) -> c /= clientId)
