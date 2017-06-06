{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsApp (WsApp.handle) where

import           ClassyPrelude
import           ConnectionMgnt
import qualified Glue
import qualified Network.Protocol as Protocol
import           State
import           WsAppUtils

handle :: TVar ServerState -> Protocol.Action -> IO ()
handle serverVar action = do
    state <- readTVarIO serverVar
    let game = Glue.updateState action $ gameState state
    let catchers = withoutClient 0 (connections state)
    let maybeRogue = findConnectionById 0 state
    -- TODO: check flattened
    let rogueGameView = Glue.gameStateToRogueView game
    let catcherGameView = Glue.gameStateToCatcherView game
    -- TODO: do STM
    -- TODO: error handling
    case maybeRogue of
        Just rogue -> sendView rogueGameView rogue
        Nothing    -> putStrLn "There is no rogue connected"

    broadcast catcherGameView catchers

    return ()
