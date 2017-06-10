{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsApp (WsApp.handle) where

import           ClassyPrelude
import           Connection
import           ConnectionMgnt
import qualified GlueMock         as Glue
import           Network.Protocol
import           State
import           WsAppUtils

handle :: IsConnection conn => TVar (ServerState conn) -> Action -> IO ()
handle stateVar action = do
    updateResult <- atomically $ updateGame stateVar action

    case updateResult of
        Right (newGame, rogueGameView, catcherGameView) -> do

            -- send game views
            let catchers = withoutClient 0 (connections state)
            let maybeRogue = findConnectionById 0 state
            broadcast catcherGameView catchers
            case maybeRogue of
                Just rogue -> sendView rogueGameView rogue
                Nothing    -> putStrLn "There is no rogue connected"

        Left gameError -> do
            putStrLn $ "invalid action (" ++ tshow (myError gameError) ++ ")"
             -- TODO: reply to sender

    return ()

updateGame :: TVar ServerState -> Action
    -> STM (Either GameError (GameState, RogueGameView, CatcherGameView))
updateGame stateVar action = do
    state <- readTVar stateVar
    let updateResult = Glue.updateState action $ gameState state
    case updateResult of
        Right (newGame, _, _) -> do
            writeTVar stateVar $ state { gameState = newGame }
        Left _ -> return ()
    return updateResult
