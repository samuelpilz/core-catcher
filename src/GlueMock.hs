{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GlueMock
    (updateState
    ) where

import           ClassyPrelude
import qualified Data.Map                as Map
--import qualified GameLogic
import           Debug.Trace             (trace)
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
    { energyMap = trace ("containsTaxi: " ++ show (Map.member (Transport {transportName="taxi"}) (energyMap eMap)) ++ ", see: " ++ show (energyMap eMap))
        $ Map.update (\v ->
                trace (show (transport action) ++ ": " ++ show v ++ "->" ++ show (v-1))
                $ Just (v-1))
            (transport action) $ energyMap eMap
    }
