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
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
--import           WsApp

main :: IO ()
main = do
    putStrLn $ "Starting Core-Catcher server on port 3000"
    Warp.run 3000 $ WS.websocketsOr
        WS.defaultConnectionOptions
        wsApp
        httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"


data ServerState  =
    ServerState
        { connections :: [ClientConnection]
        , gameState   :: String
        }

instance HasConnections ServerState where
    getConnections = connections
    setConnections conn state = state { connections = conn }

wsApp :: WS.ServerApp
wsApp pendingConn = do
    stateVar <- newTVarIO $ ServerState {connections = [], gameState = ""}
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateVar
    WS.forkPingThread conn 30
    Exception.finally
        (wsListen conn clientId stateVar)
        (disconnectClient clientId stateVar)

wsListen :: WS.Connection -> ClientId -> TVar ServerState -> IO ()
wsListen conn clientId stateVar = forever $ do
    text <- WS.receiveData conn
    putStrLn $ "reveived \"" ++ text ++ "\" from client " ++ tshow clientId
    broadcast clientId stateVar text -- TODO: Hook for business logic here
    -- real business logic:
    --objFromClient :: UnionOfProtocolDataThatAClientCanSend <- Protocol....
    --handleObj objFomClient additionalData

broadcast :: ClientId -> TVar ServerState -> Text -> IO ()
broadcast clientId stateVar msg = do
    state <- readTVarIO stateVar
    let otherClients = filter ((/=) clientId.fst) $ connections state
    forM_ otherClients $ \(_, conn) ->
        WS.sendTextData conn msg
