{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

{- |Implementation of a connection using Websockets.
-}
module WsConnection (WsConnection(..)) where

import           App.ConnectionMgnt
import           ClassyPrelude
import qualified Data.Aeson         as Aeson
import qualified Network.WebSockets as WS


newtype WsConnection = WsConnection WS.Connection

instance IsConnection WsConnection where
    type Pending WsConnection = WS.PendingConnection

    sendMsg (WsConnection conn) =
        WS.sendTextData conn . Aeson.encode

    recvMsg (WsConnection conn) = do
        wsData <- WS.receiveData conn
        return $ Aeson.decode wsData

    acceptRequest pending =
        WsConnection `map` WS.acceptRequest pending
