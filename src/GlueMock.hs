{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GlueMock
    ( GameState
    , updateState
    , initialState
    ) where

import           ClassyPrelude
import qualified Data.Map                as Map
--import qualified GameLogic
import           Network.ExampleGameView
import           Network.Protocol

type GameState = RogueGameView

initialState :: GameState
initialState = exampleRogueGameView

updateState :: Action -> GameState ->
    Either GameError (GameState, RogueGameView, CatcherGameView)
updateState action state = Right
    ( newState
    , newState -- TODO: maybe dispatch?
    , exampleCatcherGameView -- note: out of date
    )
    where
        newState = addAction action state

addAction :: Action -> RogueGameView -> RogueGameView
addAction action gameView =
    gameView
    { roguePlayerPositions =
        movePlayerInPlayerPositions action $ roguePlayerPositions gameView
    , rogueOwnHistory =
        addToHistory (transport action) $ rogueOwnHistory gameView
    , rogueEnergies = subtractEnergyFromPlayer action $ rogueEnergies gameView
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

subtractEnergyFromPlayer :: Action -> PlayerEnergies -> PlayerEnergies
subtractEnergyFromPlayer action energies = energies
    {
        playerEnergies = Map.update (Just . subtractEnergy action) (player action)
            $ playerEnergies energies
    }

subtractEnergy :: Action -> EnergyMap -> EnergyMap
subtractEnergy action eMap = eMap
    { energyMap = Map.update (\v -> Just (v-1))
            (transport action) $ energyMap eMap
    }
