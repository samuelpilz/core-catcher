{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
This module implements helps managing Client Connections
-}

module ConnectionMgnt (
    ClientId,
    ClientConnection,
    ClientConnections,
    HasConnections,
    connectClient,
    disconnectClient,
    getConnections,
    setConnections
    ) where

import           ClassyPrelude
import qualified Network.WebSockets as WS

type ClientId     = Int
type ClientConnection = (ClientId, WS.Connection)
type ClientConnections = [ClientConnection]

class HasConnections state where
    getConnections :: state -> ClientConnections

    setConnections :: ClientConnections -> state -> state

    connectClient :: WS.Connection -> TVar state -> IO ClientId
    connectClient conn stateVar = do
        clientId <- atomically $ addClient conn stateVar
        putStrLn $ "connect " ++ tshow clientId
        return clientId

    disconnectClient :: ClientId -> TVar state -> IO ()
    disconnectClient clientId stateVar = do
        atomically $ removeClient clientId stateVar
        putStrLn $ "disconnect " ++ tshow clientId

-- helper functions (not exported)
addClient :: HasConnections state => WS.Connection -> TVar state -> STM ClientId
addClient conn stateVar = do -- update connection list
    state <- readTVar stateVar
    let connections = getConnections state
    let newClientId = nextId connections
    let newConnections = (newClientId, conn):getConnections state
    writeTVar stateVar (setConnections newConnections state)
    return newClientId

removeClient :: HasConnections state => ClientId -> TVar state -> STM ()
removeClient clientId stateVar = do
    state <- readTVar stateVar
    let connections = getConnections state
    writeTVar stateVar (setConnections (withoutClient clientId connections) state)

nextId :: ClientConnections -> ClientId
nextId = fromMaybe 0 . map (+1) . maximumMay . map fst

withoutClient :: ClientId -> ClientConnections -> ClientConnections
withoutClient clientId = filter ((/=) clientId . fst)

