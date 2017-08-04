{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.WsAppUtils where

import           App.Connection
import           App.ConnectionMgnt
import           ClassyPrelude
import qualified Data.Aeson         as Aeson
import           Network.Protocol

-- TODO: type-cass for send* functions
sendToClient :: IsConnection conn => ClientConnection conn -> MessageForClient -> IO ()
sendToClient ci msg =
    sendData (snd ci) (Aeson.encode msg)

sendInitialInfo :: IsConnection conn => ClientConnection conn -> InitialInfoForClient -> IO ()
sendInitialInfo ci info =
    sendToClient ci $ InitialInfoForClient_ info

sendView :: IsConnection conn => ClientConnection conn -> GameView -> IO ()
sendView ci view =
    sendToClient ci $ GameView_ view

sendRogueView :: IsConnection conn => ClientConnection conn -> RogueGameView -> IO ()
sendRogueView ci view =
    sendView ci $ RogueView view

sendCatcherView :: IsConnection conn => ClientConnection conn -> CatcherGameView -> IO ()
sendCatcherView ci view =
    sendView ci $ CatcherView view

sendGameError :: IsConnection conn => ClientConnection conn -> GameError -> IO ()
sendGameError ci err =
    sendToClient ci $ GameError_ err

sendError :: IsConnection conn => ClientConnection conn -> GameError -> IO ()
sendError ci err =
    sendToClient ci $ GameError_ err

recvMsgForServer :: IsConnection conn => ClientConnection conn -> IO (Maybe MessageForServer)
recvMsgForServer (_,ws) = do
    wsData <- receiveData ws
    return $ Aeson.decode wsData

multicastCatcherView :: IsConnection conn => ClientConnections conn -> CatcherGameView -> IO ()
multicastCatcherView conns view =
    mapM_ (`sendCatcherView` view) conns

withoutClient :: ClientId -> ClientConnections conn -> ClientConnections conn
withoutClient cid =
    filter ((cid /=) . fst)
