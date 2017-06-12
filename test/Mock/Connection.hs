{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Mock.Connection where

import           App.Connection
import           ClassyPrelude
import Test.Framework

data FakeConnection = FakeConnection

instance IsConnection FakeConnection where
    type Pending FakeConnection = ()

    sendData = const $ const $ return ()
    
    receiveData = const $ assertFailure "FakeConnection does not implement receiveData"

    acceptRequest = const $ assertFailure "FakeConnection does not implement acceptRequest"

