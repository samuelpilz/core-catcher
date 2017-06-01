{-# LANGUAGE NoImplicitPrelude #-}
module Network.ClientConnection where

{-
This module manages one client connections.
-}

import           ClassyPrelude
import qualified Data.Aeson         as Aeson
import           Network.Protocol
import qualified Network.WebSockets as WS


data ClientConnection =
    Connection
        { playerId     :: Player
        , wsConnection :: WS.Connection
        }

sendView :: (GameView view) => view -> ClientConnection -> IO ()
sendView view ci =
    WS.sendTextData (wsConnection ci) (Aeson.encode view)

recvAction :: ClientConnection -> IO (Maybe Action)
recvAction ci = do
    wsData <- WS.receiveData (wsConnection ci)
    return (Aeson.decode wsData)

broadcast :: (GameView view) => view -> [ClientConnection] -> IO ()
broadcast view  =
    mapM_ (sendView view)
{- other maybe-useful functions ... -}
