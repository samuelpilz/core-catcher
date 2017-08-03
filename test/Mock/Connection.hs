{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mock.Connection (
    FakeConnection,
    newFakeConnection,
    prepareMsgToRead,
    getSentMsg
    ) where

import           App.ConnectionMgnt
import           ClassyPrelude
import           Network.WebSockets

import           Data.Maybe         (fromJust)
import           Test.Framework

data FakeConnection =
    FakeConnection
        { sendBuffer :: TVar (Maybe Msg)
        , recvBuffer :: TVar (Maybe Msg)
        }

type Msg = LByteString

data PendingConn = PendingConn


newFakeConnection :: IO FakeConnection
newFakeConnection = do
    sb <- newTVarIO Nothing
    rb <- newTVarIO Nothing
    return $ FakeConnection sb rb

prepareMsgToRead :: Msg -> FakeConnection -> IO ()
prepareMsgToRead msg conn =
    atomically $ writeTVar (recvBuffer conn) $ Just msg

getSentMsg :: FakeConnection -> IO (Maybe Msg)
getSentMsg = readTVarIO . sendBuffer

instance IsConnection FakeConnection where
    type Pending FakeConnection = PendingConn

    sendData conn =
        atomically . writeTVar (sendBuffer conn) . Just . toLazyByteString

    receiveData (FakeConnection _ rb) = do
        msg <- atomically $ swapTVar rb Nothing
        return . fromLazyByteString $ fromJust msg

    acceptRequest PendingConn =
        newFakeConnection
