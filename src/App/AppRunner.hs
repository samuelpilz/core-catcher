{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module App.AppRunner (handleMsgIO, handleMsgStm) where

import           App.App
import           App.AppUtils
import           App.Connection
import           App.ConnectionState
import           App.State
import           ClassyPrelude
import           Control.Error.MonadErrorInstance ()
import           Control.Monad.Error.Class
import           Control.Monad.State              (runState)
import           Control.Monad.Trans.Except       (runExceptT)
import           EntityMgnt
import           Network.Protocol
import           System.Random                    (RandomGen, newStdGen)

{- TODO: features
* message for leaving a game??
* validation playerId matches connection-origin
* ban lists and use sequences or Foldable / MonoFoldable
* exception handling using ExceptT in handleMsgStm
* move the utils to own modules
* maybe use state-monad instead of stm?
-}

-- TODO: rename to handleMsgIO
handleMsgIO
    :: IsConnection conn
    => TVar (ServerState conn)
    -> ConnectionId
    -> MessageForServer
    -> IO ()
handleMsgIO serverStateVar cId msg = do
    putStrLn $ tshow cId ++ " -> " ++ tshow msg

    gen <- newStdGen
    (toSend, serverState) <- atomically $ handleMsgStm gen serverStateVar cId msg

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
handleMsgStm :: RandomGen gen => gen -> TVar (ServerState conn) -> ConnectionId -> MessageForServer
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

