{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GlueMock
    (updateState
    ) where

import           ClassyPrelude
import qualified Data.Map                as Map
import qualified GameLogic
import           Network.ExampleGameView
import           Network.Protocol

updateState :: Action -> GameLogic.GameState ->
    Either GameError (GameLogic.GameState, RogueGameView, CatcherGameView)
updateState action state = Right
    ( state
    , movePlayerInRogueView exampleRogueGameView action
    , movePlayerInCatcherView exampleCatcherGameView action
    )


movePlayerInCatcherView :: CatcherGameView -> Action -> CatcherGameView
movePlayerInCatcherView gameView action =
    gameView {  catcherPlayerPositions =
            movePlayerInPlayerPositions (catcherPlayerPositions gameView) action
    }

movePlayerInRogueView :: RogueGameView -> Action -> RogueGameView
movePlayerInRogueView gameView action =
    gameView {  roguePlayerPositions =
            movePlayerInPlayerPositions (roguePlayerPositions gameView) action
    }

movePlayerInPlayerPositions :: PlayerPositions -> Action -> PlayerPositions
movePlayerInPlayerPositions playerPositions action =
    playerPositions { playerPositions_ =
        Map.insert (player action) (node action) (playerPositions_ playerPositions)
    }
