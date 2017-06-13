{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.WsApp (App.WsApp.handle) where

import           App.Connection
import           App.ConnectionMgnt
import           App.State
import           App.WsAppUtils
import           ClassyPrelude
import qualified Glue               as Glue
import           Network.Protocol

handle :: IsConnection conn => ClientConnection conn -> TVar (ServerState conn) -> Action -> IO ()
handle client stateVar action = do
    (newState, updateResult) <- atomically $ updateGame stateVar action

    case updateResult of
        Right (_, rogueGameView, catcherGameView) -> do

            -- send game views
            let catchers = withoutClient 0 (connections newState)
            let maybeRogue = findConnectionById 0 newState
            broadcast catchers catcherGameView
            case maybeRogue of
                Just rogue -> sendView rogue rogueGameView
                Nothing    -> putStrLn "There is no rogue connected"

        Left gameError -> do
            sendError client gameError
            putStrLn $ "invalid action " ++ tshow (myError gameError)

    return ()

updateGame ::  IsConnection conn =>  TVar (ServerState conn) -> Action
    -> STM (ServerState conn, Either GameError (GameState, RogueGameView, CatcherGameView))
updateGame stateVar action = do
    state <- readTVar stateVar
    let updateResult = Glue.updateState action $ gameState state

    newState <- case updateResult of
        Right (newGame, _, _) -> do
            let newState = state { gameState = newGame }
            writeTVar stateVar newState
            return newState
        Left _ -> do
            return state

    return (newState, updateResult)
