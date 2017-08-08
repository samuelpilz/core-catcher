{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mock.Connection (
    FakeConnection,
    newFakeConnection,
    prepareMsgToRead,
    getSentMsg,
    FakeConnections,
    sendBuffer,
    recvBuffer
    ) where

import           App.ConnectionMgnt
import           ClassyPrelude
import           Control.Monad.Extra (whenJust)
import           Network.Protocol
import           Test.Framework

data FakeConnection =
    FakeConnection
        { sendBuffer :: TVar (Maybe MessageForClient)
        , recvBuffer :: TVar (Maybe MessageForServer)
        }

type FakeConnections = ClientConnections FakeConnection

newFakeConnection :: IO FakeConnection
newFakeConnection = do
    sb <- newTVarIO Nothing
    rb <- newTVarIO Nothing
    return $ FakeConnection sb rb

prepareMsgToRead :: FakeConnection -> MessageForServer -> IO ()
prepareMsgToRead conn =
    atomically . writeTVar (recvBuffer conn) . Just

getSentMsg :: FakeConnection -> IO (Maybe MessageForClient)
getSentMsg = readTVarIO . sendBuffer

instance IsConnection FakeConnection where
    type Pending FakeConnection = Maybe MessageForServer

    sendMsg conn =
        atomically . writeTVar (sendBuffer conn) . Just

    recvMsg (FakeConnection _ rb) =
        atomically $ swapTVar rb Nothing

    acceptRequest msgForServer = do
        conn <- newFakeConnection
        whenJust msgForServer (prepareMsgToRead conn)
        return conn
