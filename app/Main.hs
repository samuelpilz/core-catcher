{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
code taken from tutorial
https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm
and code from
https://gitlab.com/paramander/typesafe-websockets/blob/master/src/Main.hs
-}

module Main where

import           App.App
import           App.Connection
import           App.ConnectionState
import           App.State
import           ClassyPrelude
import qualified Control.Exception              as Exception
import           EntityMgnt
import qualified Network.Protocol               as Protocol
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
-- import           System.Random                  (newStdGen)
import           WsConnection

main :: IO ()
main = do
    let initialState = defaultInitialState
    stateVar <- newTVarIO initialState
    let port = 7999
    putStrLn $ "Starting Core-Catcher server on port " ++ tshow port

    Warp.run port $ WS.websocketsOr
        WS.defaultConnectionOptions
        (wsApp stateVar)
        httpApp

httpApp :: Wai.Application
httpApp = WaiStatic.staticApp (WaiStatic.defaultFileServerSettings "web")


wsApp :: TVar (ServerState WsConnection) -> WS.ServerApp
wsApp stateVar pendingConn = do
    conn <- WS.acceptRequest pendingConn
    let wsConn = WsConnection conn
    WS.forkPingThread conn 30
    -- TODO: replace
    cId <- atomically $ do
        state <- readTVar stateVar
        let (cId, newState) = addEntity (newConnectionInfo wsConn) state
        writeTVar stateVar newState
        return cId
    putStrLn $ "connect " ++ tshow cId
    sendSendableMsg wsConn Protocol.ServerHello

    Exception.finally
        (wsListen (cId, wsConn) stateVar)
        (cleanOnCloseConnection stateVar cId)

wsListen :: IsConnection conn => (ConnectionId, conn) -> TVar (ServerState conn) -> IO ()
wsListen (cId, client) stateVar =
    handle handler . forever $ do
        maybeMsg <- recvMsg client
        case maybeMsg of
            Just msg ->
                handleClientMsg stateVar cId msg

            Nothing -> do
                sendSendableMsg client Protocol.ClientMsgError
                putStrLn "ERROR: msg has invalid format"
    where
        handler :: WS.ConnectionException -> IO ()
        handler _ = return ()

cleanOnCloseConnection :: TVar (ServerState WsConnection) -> ConnectionId -> IO ()
cleanOnCloseConnection stateVar cId = do
    atomically $ do
        state <- readTVar stateVar
        let newState = removeEntity cId state
        writeTVar stateVar newState

    putStrLn $ "disconnect " ++ tshow cId
