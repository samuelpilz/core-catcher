{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
code taken from tutorial
https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm
and code from
https://gitlab.com/paramander/typesafe-websockets/blob/master/src/Main.hs
-}

module Main where

import           ClassyPrelude
import           ConnectionMgnt
import qualified Control.Exception              as Exception
import qualified Data.Aeson                     as Aeson
import qualified Network.HTTP.Types             as Http
import           Network.Protocol
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import           State
import           Util                           (defaultGame)
import qualified WsApp
import qualified WsAppUtils

main :: IO ()
main = do
    putStrLn "Starting Core-Catcher server on port 3000"
    Warp.run 3000 $ WS.websocketsOr
        WS.defaultConnectionOptions
        wsApp
        httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"


wsApp :: WS.ServerApp
wsApp pendingConn = do
    stateVar <- newTVarIO ServerState {connections = empty, gameState = defaultGame}
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateVar -- call to ConnectionMgnt
    WS.forkPingThread conn 30
    Exception.finally
        (wsListen (clientId, conn) stateVar)
        (disconnectClient clientId stateVar) -- call to ConnectionMgnt

wsListen :: ClientConnection -> TVar ServerState -> IO ()
wsListen client stateVar = forever $ do
    maybeAction <- WsAppUtils.recvAction client
    case maybeAction of
        Just action -> do
            WS.sendTextData (snd client) ("1" :: Text)
            WsApp.handle stateVar action
            return ()
        Nothing     -> do
            WS.sendTextData (snd client) ("2" :: Text)
            putStrLn "ERROR: The message could not be decoded"
