{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsAppUtils where

import           ClassyPrelude
import           ConnectionMgnt
import qualified Data.Aeson         as Aeson
import           Network.Protocol
import qualified Network.WebSockets as WS

sendView :: (GameView view) => view -> ClientConnection -> IO ()
sendView view ci =
    WS.sendTextData (snd ci) (Aeson.encode view)

recvAction :: ClientConnection -> IO (Maybe Action)
recvAction ci = do
    wsData <- WS.receiveData (snd ci)
    return (Aeson.decode wsData)

broadcast :: (GameView view) => view -> [ClientConnection] -> IO ()
broadcast view  =
    mapM_ (sendView view)
