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
import qualified Control.Exception              as Exception
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

main :: IO ()
main = do
    putStrLn "start websocket server on port 3000"
    state <- newTVarIO []
    Warp.run 3000 $ WS.websocketsOr
        WS.defaultConnectionOptions
        (wsApp state)
        httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

wsApp :: TVar State -> WS.ServerApp
wsApp stateVar pendingConn = do
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateVar
    WS.forkPingThread conn 30
    Exception.finally
        (listen conn clientId stateVar)
        (disconnectClient clientId stateVar)


type ClientId = Int
type Client     = (ClientId, WS.Connection)
type State      = [Client]

-- client management part

nextId :: State -> ClientId
nextId = fromMaybe 0 . map (+1) . maximumMay . map fst

connectClient :: WS.Connection -> TVar State -> IO ClientId
connectClient conn stateVar = do
    clientId <- atomically $ addClient conn stateVar
    putStrLn $ "connect " ++ tshow clientId
    return clientId

addClient :: WS.Connection -> TVar State -> STM ClientId
addClient conn stateVar = do
    state <- readTVar stateVar
    let newClientId = nextId state
    writeTVar stateVar ((newClientId, conn):state)
    return newClientId


withoutClient :: ClientId -> State -> State
withoutClient clientId = filter ((/=) clientId . fst)

disconnectClient :: ClientId -> TVar State -> IO ()
disconnectClient clientId stateVar = do
    atomically $ modifyTVar stateVar (withoutClient clientId)
    putStrLn $ "disconnect " ++ tshow clientId

listen :: WS.Connection -> ClientId -> TVar State -> IO ()
listen conn clientId stateRef = forever $ do
    text <- WS.receiveData conn
    putStrLn $ "reveived " ++ text
    broadcast clientId stateRef text

broadcast :: ClientId -> TVar State -> Text -> IO ()
broadcast clientId stateRef msg = do
    clients <- readTVarIO stateRef
    let otherClients = withoutClient clientId clients
    forM_ otherClients $ \(_, conn) ->
        WS.sendTextData conn msg
