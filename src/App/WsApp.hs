{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.WsApp (handle, initialInfoForClient) where

import           App.Connection
import           App.ConnectionMgnt
import           App.State
import           App.WsAppUtils
import           ClassyPrelude       hiding (handle)
import           Config.GameConfig   (GameConfig)
import qualified Config.GameConfig   as Game
import           Control.Monad.Extra (whenJust)
import           GameNg              (GameState (..))
import qualified GameNg              as Game
import           Network.Protocol

handle
    :: IsConnection conn
    => ClientConnection conn
    -> TVar (ServerState conn)
    -> MessageForServer
    -> IO ()
handle client stateVar msg = do
    putStrLn $ tshow msg
    case msg of
        Action_ action -> do
            (newState, updateResult) <- atomically $ updateGameAtomically stateVar action
            case updateResult of
                Right newGameState -> sendGameViews newGameState (connections newState)
                Left gameError -> do
                    sendError client gameError
                    putStrLn $ "invalid action " ++ tshow gameError
    return ()

updateGameAtomically
    :: IsConnection conn
    =>  TVar (ServerState conn)
    -> Action
    -> STM (ServerState conn, Either GameError GameState)
updateGameAtomically stateVar action = do
    state <- readTVar stateVar

    let updateResult = Game.updateState action $ gameState state

    newState <- case updateResult of
        Right newGameState -> do
            let newState = state { gameState = newGameState }
            writeTVar stateVar newState
            return newState
        Left _ -> return state -- let state stay the same

    return (newState, updateResult)

sendGameViews :: IsConnection conn => GameState -> ClientConnections conn -> IO ()
sendGameViews (GameRunning_ game) conn = do
    let (rogueGameView, catcherGameView) = Game.getViews game
    multicastCatcherView (withoutClient 0 conn) catcherGameView
    whenJust (findConnectionById 0 conn) (`sendRogueView` rogueGameView)
sendGameViews (GameOver_ game) conn =
    broadcastGameOverView conn $ Game.getGameOverView game


initialInfoForClient :: GameConfig -> ClientId -> InitialInfoForClient
initialInfoForClient config clientId =
    InitialInfoForClient
        { initialPlayer = Player clientId
        , networkForGame = network
        , initialGameView = initialView
        }
    where
        initialState = Game.initialState config
        initialView = (if clientId == 0 then RogueView . fst else CatcherView . snd)
            (Game.getViews initialState)
        network = Game.network config

