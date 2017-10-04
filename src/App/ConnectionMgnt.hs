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

Note: this is used for preventing mutually recursive modules.

TODO: write about what is exported and how to use this module
-}

module App.ConnectionMgnt where

import           ClassyPrelude
import           Network.Protocol

type ConnectionId = Int

-- TODO: instance MonoFoldable & MonoTraversable for ClientConnections
data ClientConnections conn connState =
    ClientConnections
        { connections :: Map ConnectionId (conn, connState)
        , nextId      :: ConnectionId
        }

class IsConnection c where
    type Pending c :: *

    sendMsg :: c -> MessageForClient -> IO ()

    recvMsg :: c -> IO (Maybe MessageForServer)

    sendSendableMsg :: SendableToClient msg => c -> msg -> IO ()
    sendSendableMsg c msg = sendMsg c $ wrapSendable msg

    acceptRequest ::  Pending c -> IO c

    -- TODO: optimal multicast signature if ClientConnections was MonoFoldable
--     multicastMsg ::
--         (SendableToClient msg, MonoFoldable f, IsConnection c, c ~ Element f)
--         => f -> msg -> IO ()
    multicastMsg :: (SendableToClient msg) => ClientConnections c s -> msg -> IO ()
    multicastMsg cs msg = omapM_ (`sendSendableMsg` msg) . map fst $ connections cs

-- instance for connections bundled with state information
instance (IsConnection c, IsConnectionState s) => IsConnection (c, s) where
    type Pending (c,s) = Pending c
    sendMsg (c,_) = sendMsg c
    recvMsg (c,_) = recvMsg c
    acceptRequest p = do
        c <- acceptRequest p
        return (c, newConnectionState)

class IsConnectionState connState where
    newConnectionState :: connState

class HasConnections state where
    type Conn state :: *
    type ConnState state :: *

    getConnections :: state -> ClientConnections (Conn state) (ConnState state)

    setConnections :: ClientConnections (Conn state) (ConnState state) -> state -> state


-- functions for connections

connectClient
    :: (HasConnections state, IsConnectionState connState, ConnState state ~ connState)
    => TVar state -> Conn state -> STM ConnectionId
connectClient stateVar conn = do -- update connection list
    state <- readTVar stateVar
    let conns@ClientConnections{connections} = getConnections state
    let newConnections =
            conns
                { connections =
                    insertMap (nextId conns) (conn, newConnectionState) connections
                , nextId = 1 + nextId conns
                }
    writeTVar stateVar $ setConnections newConnections state
    return $ nextId conns

modifyClientState
    :: (HasConnections state, IsConnectionState connState, ConnState state ~ connState)
    => TVar state -> ConnectionId -> (connState -> connState) -> STM ()
modifyClientState stateVar cId connStateF = do
    state <- readTVar stateVar
    let conns@ClientConnections{connections} = getConnections state
    case findConnectionById cId state of
        Nothing -> return ()
        Just (conn, connState) -> do
            let newConnections =
                    conns
                        { connections =
                            insertMap cId (conn, connStateF connState) connections
                        }
            writeTVar stateVar $ setConnections newConnections state

disconnectClient
    :: (HasConnections state, IsConnectionState connState, ConnState state ~ connState)
    => TVar state -> ConnectionId -> STM ()
disconnectClient stateVar cId = do
    state <- readTVar stateVar
    let connections = getConnections state
    writeTVar stateVar (setConnections (withoutClient cId connections) state)

-- implement HasConnections for ClientConnections themselves
instance HasConnections (ClientConnections conn connState) where
    type Conn (ClientConnections conn connState) = conn
    type ConnState (ClientConnections conn connState) = connState
    getConnections = id
    setConnections = const

-- extra functions

findConnectionStateById :: (HasConnections state) => ConnectionId -> state -> Maybe (ConnState state)
findConnectionStateById cId =
    map snd . findConnectionById cId

findConnectionById :: (HasConnections state) => ConnectionId -> state -> Maybe (Conn state, ConnState state)
findConnectionById cId =
    lookup cId . connections . getConnections

withoutClient :: ConnectionId -> ClientConnections conn connState -> ClientConnections conn connState
withoutClient cId conns =
    conns { connections = deleteMap cId . connections $ conns }
