{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsApp (WsApp.handle) where

import           ClassyPrelude
import           ConnectionMgnt
import           Network.Protocol (Action (..), CatcherGameView, GameView,
                                   RogueGameView)
import           State
import           WsAppUtils

handle :: TVar ServerState -> Action -> IO ()
handle serverVar action = do
    state <- readTVarIO serverVar
    let newGameState = updateState action $ gameState state
    case newGameState of
        Just game -> do
            let catchers = withoutClient 0 (connections state)
            let maybeRogue = findConnectionById 0 state
            let rogueGameView = gameStateToRogueView game
            let catcherGameView = gameStateToCatcherView game
            broadcast catcherGameView catchers

            case maybeRogue of
                Just rogue -> sendView rogueGameView rogue
                Nothing    -> putStrLn "There is no rogue connected"
            return ()

        Nothing ->
            putStrLn $ "The sent move was apparently not a valid move.\nInform the player: " ++ tshow (player action)

updateState :: Action -> GameState -> Maybe GameState
-- TODO: someone implement
updateState  _ = undefined

gameStateToCatcherView :: GameState -> CatcherGameView
gameStateToCatcherView state = undefined

gameStateToRogueView :: GameState -> RogueGameView
gameStateToRogueView state = undefined
