{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

{- |Implementation of a connection using WebSockets.
-}
module WsConnection (WsConnection(..)) where

import           App.Connection
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

    acceptRequest =
        map WsConnection . WS.acceptRequest
