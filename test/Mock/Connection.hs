{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mock.Connection where

import           App.Connection
import           ClassyPrelude
import           Network.WebSockets

import           System.IO.Unsafe
import           Test.Framework

newtype FakeConnection =
    FakeConnection
        { contentMsg    :: MVar Msg
        }

newtype Msg =
    Msg LByteString

data PendingConn = PendingConn

emptyConnection :: FakeConnection
emptyConnection = FakeConnection (unsafePerformIO $ newMVar (Msg ""))

connectionWith :: LByteString -> FakeConnection
connectionWith msg = FakeConnection (unsafePerformIO $ newMVar (Msg msg))

instance IsConnection FakeConnection where
    type Pending FakeConnection = PendingConn

    sendData conn msg = do
        _ <- takeMVar (contentMsg conn)
        putMVar (contentMsg conn) (Msg $ toLazyByteString msg)
        return ()

    receiveData (FakeConnection content) = do
        (Msg msg') <- readMVar content
        return $ fromLazyByteString msg'

    acceptRequest PendingConn =
        return emptyConnection
