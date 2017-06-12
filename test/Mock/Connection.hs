{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Mock.Connection where

import           App.Connection
import           ClassyPrelude
import           Network.WebSockets

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
