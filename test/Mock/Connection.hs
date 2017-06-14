{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mock.Connection where

import           App.Connection
import           ClassyPrelude
import           Network.WebSockets

import           Test.Framework

newtype FakeConnection =
    FakeConnection
        { contentMsg    :: MVar Msg
        }

newtype Msg =
    Msg LByteString

data PendingConn = PendingConn

emptyConnection :: IO FakeConnection
emptyConnection = do
    mvar <- newMVar (Msg "")
    return (FakeConnection mvar)

connectionWith :: LByteString -> IO FakeConnection
connectionWith msg = do
    mvar <- newMVar (Msg msg)
    return (FakeConnection mvar)

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
        emptyConnection
