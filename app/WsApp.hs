{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsApp (WsApp.handle) where

import           ClassyPrelude
import           ConnectionMgnt
import qualified Glue
import qualified Network.Protocol   as Protocol
import qualified Network.WebSockets as WS
import           State
import           WsAppUtils
-- TODO: refactor this function as a whole
handle :: TVar ServerState -> Protocol.Action -> IO ()
handle stateVar action = do
    state <- readTVarIO stateVar-- TODO: do STM
    let updateResult = Glue.updateState action $ gameState state

    case updateResult of
        Right (newGame, rogueGameView, catcherGameView) -> do
            -- update game state
            atomically . writeTVar stateVar $ state { gameState = newGame }

            -- send game views
            let catchers = withoutClient 0 (connections state) :: ClientConnections GameConnection
            let maybeRogue = findConnectionById 0 state
            broadcast catcherGameView catchers
            case maybeRogue of
                Just rogue -> sendView rogueGameView rogue
                Nothing    -> putStrLn "There is no rogue connected"

        Left gameError -> do
            putStrLn $ "invalid action (" ++ tshow (Protocol.myError gameError) ++ ")"
             -- TODO: reply to sender

    return ()
