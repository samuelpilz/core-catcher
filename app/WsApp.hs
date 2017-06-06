{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsApp (WsApp.handle) where

import           ClassyPrelude
import           Network.Protocol   (Action, CatcherGameView, GameView,
                                     RogueGameView)
import qualified Network.WebSockets as WS


handle :: TVar state -> Action -> IO ()
handle _ _ = return ()


updateState :: Action -> state -> STM ()
updateState _ _ = undefined

stateToCatcherGameView :: gameState -> STM CatcherGameView
stateToCatcherGameView = undefined

stateToRogueGameView :: gameState -> STM RogueGameView
stateToRogueGameView = undefined
