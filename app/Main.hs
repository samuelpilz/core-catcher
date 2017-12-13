{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           App.AppRunner
import           App.Cli
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
import           WsConnection


main :: IO ()
main = do
    let initialState = defaultInitialState
    stateVar <- newTVarIO initialState

    let port = 7999
    putStrLn $ "Starting Core-Catcher server on port " ++ tshow port

    serverThreadId <- fork $ Warp.run port $ WS.websocketsOr
        WS.defaultConnectionOptions
        (wsApp stateVar)
        httpApp

    cliIO stateVar serverThreadId


httpApp :: Wai.Application
httpApp = WaiStatic.staticApp (WaiStatic.defaultFileServerSettings "web")


wsApp :: TVar (ServerState WsConnection) -> WS.ServerApp
wsApp stateVar pendingConn = do
    conn <- WS.acceptRequest pendingConn
    let wsConn = WsConnection conn
    WS.forkPingThread conn 30
    cId <- atomically $ stateToStm stateVar $ addEntityS (newConnectionInfo wsConn)
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
                handleMsgIO stateVar cId msg

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
