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
        { msg    :: LByteString
        }

data PendingConn = PendingConn

emptyConnection :: FakeConnection
emptyConnection = FakeConnection ""

instance IsConnection FakeConnection where
    type Pending FakeConnection = PendingConn

    sendData _ = return . const ()

    receiveData fake = return . fromLazyByteString $ msg fake

    acceptRequest PendingConn = return $ FakeConnection ""
