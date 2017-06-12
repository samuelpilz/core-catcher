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
        { msgs    :: LByteString
        }

data PendingConn = PendingConn

instance IsConnection FakeConnection where
    type Pending FakeConnection = PendingConn

    sendData _ = return . const ()

    receiveData fake = return . fromLazyByteString $ msgs fake

    acceptRequest PendingConn = return $ FakeConnection ""
