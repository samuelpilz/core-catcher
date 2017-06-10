{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-
This module abstracts connections
-}

module App.Connection where

import           ClassyPrelude
import qualified Network.WebSockets as WS

class IsConnection c where
    type Pending c :: *

    sendData :: WS.WebSocketsData a => c -> a -> IO ()

    receiveData :: WS.WebSocketsData a => c -> IO a

    acceptRequest ::  Pending c -> IO c
