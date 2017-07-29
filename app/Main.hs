{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-
code taken from tutorial
https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm
and code from
https://gitlab.com/paramander/typesafe-websockets/blob/master/src/Main.hs
-}

module Main where

import           App.ConnectionMgnt
import           App.State
import           App.WsApp
import qualified App.WsAppUtils                 as WsAppUtils
import           ClassyPrelude                  hiding (handle)
import qualified Control.Exception              as Exception
import qualified GameNg
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

main :: IO ()
main = do
    stateVar <- newTVarIO ServerState {connections = empty, gameState = GameNg.initialState }
    putStrLn "Starting Core-Catcher server on port 8000"
    Warp.run 8000 $ WS.websocketsOr
        WS.defaultConnectionOptions
        (wsApp stateVar)
        httpApp

httpApp :: Wai.Application
httpApp = WaiStatic.staticApp (WaiStatic.defaultFileServerSettings "web")
--     do
--         let getFile = concat . Wai.pathInfo $ request
--         putStrLn getFile
--         responseText <- readFileUtf8 . unpack . concat . Wai.pathInfo $ request
--         respond $ Wai.responseLBS Http.status200 []
--             (builderToLazy. byteString . encodeUtf8 $ responseText)


wsApp :: TVar (ServerState GameConnection) -> WS.ServerApp
wsApp stateVar pendingConn = do
    conn <- WS.acceptRequest pendingConn
    let gameConn = GameConnection conn
    clientId <- connectClient gameConn stateVar -- call to ConnectionMgnt
    WS.forkPingThread conn 30
    WsAppUtils.sendInitialInfo (clientId, gameConn) $ initialInfoForClient clientId
    Exception.finally
        (wsListen (clientId, gameConn) stateVar)
        (disconnectClient clientId stateVar) -- call to ConnectionMgnt

wsListen :: IsConnection conn => ClientConnection conn -> TVar (ServerState conn) -> IO ()
wsListen client stateVar = forever $ do
    maybeAction <- WsAppUtils.recvAction client
    case maybeAction of
        Just action -> do
            -- TODO: what about request forging? (send game-token to client using player-mgnt)
            -- TODO: validation playerId==clientId
            handle client stateVar action
            return ()
        Nothing     -> do
            putStrLn "ERROR: The message could not be decoded"
            -- TODO: send info back to client
