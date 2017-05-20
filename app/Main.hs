{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           ClassyPrelude
import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.Maybe                     as Maybe
import qualified Data.Sequence                  as Seq
import qualified Data.Text                      as Text
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS


type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = Seq.Seq Client


nextId :: State -> ClientId
nextId =
    Maybe.maybe 0 (1+) . maximumMay . map fst

main :: IO ()
main = do
    state <- Concurrent.newMVar Seq.empty
    Warp.run 3000 $ WS.websocketsOr
      WS.defaultConnectionOptions
      (wsApp state)
      httpApp

httpApp :: Wai.Application
httpApp _ respond =
    respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef =
    Concurrent.modifyMVar stateRef
        $ \state -> do
            let clientId = nextId state
            return ((clientId, conn) `cons` state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId =
    filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef =
    Concurrent.modifyMVar_ stateRef
        $ \state ->
            return $ withoutClient clientId state

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef =
    Monad.forever $
        WS.receiveData conn >>= broadcast clientId stateRef

broadcast :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
broadcast clientId stateRef msg = do
    clients <- Concurrent.readMVar stateRef
    let otherClients = withoutClient clientId clients
    Monad.forM_ otherClients $ \(_, conn) ->
        WS.sendTextData conn msg

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
    state <- readMVar stateRef
    -- TODO: not thread safe
    if length state < 4 then do
        putStrLn "Accept request"
        conn <- WS.acceptRequest pendingConn
        clientId <- connectClient conn stateRef
        WS.forkPingThread conn 30
        WS.sendTextData conn ("You have been added to the game\n" :: Text.Text)
        informOthersAbout clientId stateRef
        Exception.finally
            (listen conn clientId stateRef)
            (disconnectClient clientId stateRef)
    else do
        putStrLn "Reject request"
        WS.rejectRequest pendingConn "Too many players have joined the game\n"


informOthersAbout :: ClientId -> Concurrent.MVar State  -> IO ()
informOthersAbout clientId stateRef =
      broadcast clientId stateRef ("Player " ++ tshow clientId ++ " joined the Game!")
