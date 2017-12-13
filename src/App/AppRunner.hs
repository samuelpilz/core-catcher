{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|

Utility functions for running the app.

This module provides functions for lifting the functions in App to the STM and IO monads
for use in a web-server.

This module does not handle connection-management

-}

module App.AppRunner (handleMsgIO, handleMsgStm, stateToStm) where

import           App.App
import           App.AppUtils
import           App.Connection
import           App.ConnectionState
import           App.State
import           ClassyPrelude
import           Control.Error.MonadErrorInstance ()
import           Control.Monad.State              (State, runState)
import           Control.Monad.Trans.Except       (runExceptT)
import           EntityMgnt
import           Network.Protocol
import           System.Random                    (RandomGen, newStdGen)


handleMsgIO ::
    ( IsConnection conn
    , MonadIO m
    )
    => TVar (ServerState conn)
    -> ConnectionId
    -> MessageForServer
    -> m ()
handleMsgIO serverStateVar cId msg = do
    putStrLn $ tshow cId ++ " -> " ++ tshow msg

    gen <- liftIO newStdGen
    (toSend, serverState) <- liftIO . atomically $ handleMsgStm gen serverStateVar cId msg

    mapM_
        (\(connId, connInfo, m) -> do -- IO monad
            putStrLn $ tshow connId ++ " <- " ++ tshow m
            sendSendableMsg (connection connInfo) m
        ) .
        mapMaybe
            (\(cIdToSend, m) -> do -- maybe monad
                connInfo :: ConnectionInfo conn <- findEntityById cIdToSend serverState
                return (cIdToSend, connInfo, m)
            ) $
        toSend


-- |handles a single message and saves the new state
handleMsgStm
    :: RandomGen gen
    => gen
    -> TVar (ServerState conn)
    -> ConnectionId
    -> MessageForServer
    -> STM ([(ConnectionId, MessageForClient)], ServerState conn)
handleMsgStm gen serverStateVar cId msg = do -- STM monad
    serverState <- readTVar serverStateVar
    let (updateResult, newServerState) =
            runState (runExceptT $ handleMsgState gen cId msg) serverState
    case updateResult of
        Left err ->
            return (msgForOne cId $ ServerError_ err, serverState)
        Right toSend -> do
            writeTVar serverStateVar newServerState
            return (toSend, newServerState)


-- |lifts a state-operation to a STM-variable
stateToStm :: TVar s -> State s a -> STM a
stateToStm var stateM = do
    val <- readTVar var
    let (a, newVal) = runState stateM val
    writeTVar var newVal
    return a
