{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsAppUtils where

import           ClassyPrelude
import           Connection
import           ConnectionMgnt
import qualified Data.Aeson       as Aeson
import           Network.Protocol

sendView :: (IsConnection conn, GameView view) => view -> ClientConnection conn -> IO ()
sendView view ci =
    sendData (snd ci) (Aeson.encode view)

recvAction :: IsConnection conn => ClientConnection conn -> IO (Maybe Action)
recvAction (_,ws) = do
    wsData <- receiveData ws
    return (Aeson.decode wsData)

broadcast :: (IsConnection conn, GameView view) => view -> ClientConnections conn -> IO ()
broadcast view  =
    mapM_ (sendView view)

withoutClient :: ClientId -> ClientConnections conn -> ClientConnections conn
withoutClient cid =
    filter ((cid /=) . fst)
