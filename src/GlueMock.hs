{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GlueMock
    (updateState
    ) where

import           ClassyPrelude
import qualified Data.Map                as Map
--import qualified GameLogic
import           Network.ExampleGameView
import           Network.Protocol

type GameState = RogueGameView -- TODO: change back to GameLogic.GameState as state

updateState :: Action -> GameState ->
    Either GameError (GameState, RogueGameView, CatcherGameView)
updateState action state = Right
    ( newState
    , newState -- TODO: maybe dispatch?
    , exampleCatcherGameView -- note: out of date
    )
    where
        newState = addAction action state

{-
movePlayerInCatcherView :: Action -> CatcherGameView -> CatcherGameView
movePlayerInCatcherView action gameView =
    gameView
    { catcherPlayerPositions =
        movePlayerInPlayerPositions action $ catcherPlayerPositions gameView
    , catcherRogueHistory =
        addToHistory (transport action) $ catcherRogueHistory gameView
    }
-}

addAction :: Action -> RogueGameView -> RogueGameView
addAction action gameView =
    gameView
    { roguePlayerPositions =
        movePlayerInPlayerPositions action $ roguePlayerPositions gameView
    , rogueOwnHistory =
        addToHistory (transport action) $ rogueOwnHistory gameView
    }

movePlayerInPlayerPositions :: Action -> PlayerPositions -> PlayerPositions
movePlayerInPlayerPositions action playerPositions =
    playerPositions { playerPositions_ =
        Map.insert (player action) (node action) (playerPositions_ playerPositions)
    }


addToHistory :: Transport -> RogueTransportHistory -> RogueTransportHistory
addToHistory transport history = history
    {rogueTransportHistory =
        rogueTransportHistory history ++ [transport]
    }
