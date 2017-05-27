{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           ClassyPrelude
import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Control.Monad.Trans            as Trans
import qualified Control.Monad.Trans.Maybe      as MaybeT
import qualified Data.Aeson                     as Aeson
import qualified Data.Maybe                     as Maybe
import qualified Data.Sequence                  as Seq
import qualified Data.Text                      as Text
import qualified GameLogic
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS


type ClientId = Int
type Client   = (ClientId, WS.Connection)
type Clients    = Seq.Seq Client
data Game = Game {
    gameClients :: Clients,
    gameState   :: GameLogic.GameState
}
newtype State = State {
    stateGame :: Game
}

initialState :: State
initialState =
    State { stateGame =
        Game {
            gameClients = Seq.empty,
            gameState = GameLogic.GameState {GameLogic.start = fromList [1,2,3], GameLogic.network = GameLogic.someNet, GameLogic.actions = []}
        }
    }

nextId :: Clients -> ClientId
nextId =
    Maybe.maybe 0 (1+) . maximumMay . map fst

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Starting Core-Catcher server on port " ++ tshow port
    state <- Concurrent.newMVar initialState
    Warp.run port $ WS.websocketsOr
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
            let clients = gameClients $ stateGame state
            let clientId = nextId clients
            return (state { stateGame = (stateGame state) { gameClients = (clientId, conn) `cons` clients } }, clientId)

withoutClient :: ClientId -> Clients -> Clients
withoutClient clientId =
    filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef =
    Concurrent.modifyMVar_ stateRef
        $ \state ->
            return State { stateGame = (stateGame state) { gameClients = withoutClient clientId $ gameClients $ stateGame state } }

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef =
    Monad.forever $ Concurrent.modifyMVar_ stateRef $ \state -> do
        wsdata <- liftIO $ WS.receiveData conn
        case Aeson.decode wsdata of
            Just action -> do
                -- FIXME: check if player's turn
                let st = GameLogic.addAction (gameState $ stateGame state) action
                broadcast (gameClients $ stateGame state) (Aeson.encode $ GameLogic.flattenState st)
                -- FIXME: don't add action if rollback or flattenState failed
                return State { stateGame = (stateGame state) { gameState = st } }
            Nothing -> return state

broadcast :: WS.WebSocketsData a => Clients -> a -> IO ()
broadcast clients msg =
    Monad.forM_ clients $ \(_, conn) ->
        WS.sendTextData conn msg

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
    state <- Concurrent.readMVar stateRef
    let clients = gameClients $ stateGame state
    -- TODO: not thread safe
    if length clients < 4 then do
        putStrLn "Accept request"
        conn <- WS.acceptRequest pendingConn
        clientId <- connectClient conn stateRef
        WS.forkPingThread conn 30
        WS.sendTextData conn ("You have been added to the game\n" :: Text.Text)
        broadcast clients ("Player " ++ tshow clientId ++ " joined the Game!")
        Exception.finally
            (listen conn clientId stateRef)
            (disconnectClient clientId stateRef)
    else do
        putStrLn "Reject request"
        WS.rejectRequest pendingConn "Too many players have joined the game\n"
