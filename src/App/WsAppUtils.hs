{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.WsAppUtils where

import           App.Connection
import           App.ConnectionMgnt
import           ClassyPrelude
import qualified Data.Aeson         as Aeson
import           Network.Protocol

sendView :: (IsConnection conn, GameView view) => ClientConnection conn -> view -> IO ()
sendView ci view =
    sendData (snd ci) (Aeson.encode view)

sendError :: (IsConnection conn) => ClientConnection conn -> GameError -> IO ()
sendError ci err =
    sendData (snd ci) (Aeson.encode err)

recvAction :: IsConnection conn => ClientConnection conn -> IO (Maybe Action)
recvAction (_,ws) = do
    wsData <- receiveData ws
    return (Aeson.decode wsData)

broadcast :: (IsConnection conn, GameView view) => ClientConnections conn -> view -> IO ()
broadcast conns view =
    mapM_ (\conn -> sendView conn view) conns

withoutClient :: ClientId -> ClientConnections conn -> ClientConnections conn
withoutClient cid =
    filter ((cid /=) . fst)
