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
        { msg    :: Msg
        }

newtype Msg =
    Msg LByteString

data PendingConn = PendingConn

emptyConnection :: FakeConnection
emptyConnection = FakeConnection (Msg "")

instance IsConnection FakeConnection where
    type Pending FakeConnection = PendingConn

    sendData _ = return . const ()
    receiveData (FakeConnection (Msg msg')) = return $ fromLazyByteString msg'

    acceptRequest PendingConn = return $ FakeConnection (Msg "")


instance Arbitrary FakeConnection where
    arbitrary = return $ FakeConnection (Msg "")
