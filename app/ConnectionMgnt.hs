{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-
This module implements helps managing Client Connections
-}

module ConnectionMgnt (
    ClientId,
    ClientConnection,
    ClientConnections,
    HasConnections,
    Conn,
    connectClient,
    disconnectClient,
    getConnections,
    setConnections,
    findConnectionById
    ) where

import           ClassyPrelude
import qualified Network.WebSockets as WS

type ClientId = Int
type ClientConnection conn = (ClientId, conn)

type ClientConnections conn = Seq (ClientConnection conn)

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

    findConnectionById :: ClientId -> state -> Maybe (ClientConnection (Conn state))
    findConnectionById cid state =
        find ((==cid) . fst) $ getConnections state



-- helper functions (not exported)
addClient :: HasConnections state => Conn state -> TVar state -> STM ClientId
addClient conn stateVar = do -- update connection list
    state <- readTVar stateVar
    let connections = getConnections state
    let newClientId = nextId connections
    let newConnections = (newClientId, conn) `cons` getConnections state
    writeTVar stateVar (setConnections newConnections state)
    return newClientId

removeClient :: HasConnections state => ClientId -> TVar state -> STM ()
removeClient clientId stateVar = do
    state <- readTVar stateVar
    let connections = getConnections state
    writeTVar stateVar (setConnections (withoutClient clientId connections) state)

nextId :: ClientConnections conn -> ClientId
nextId = fromMaybe 0 . map (+1) . maximumMay . map fst

withoutClient :: ClientId -> ClientConnections conn -> ClientConnections conn
withoutClient clientId = filter ((/=) clientId . fst)
