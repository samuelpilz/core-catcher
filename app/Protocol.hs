{-# LANGUAGE NoImplicitPrelude #-}
module Protocol where

import           ClassyPrelude

type Player = Int
type Node = Int
type Transport = String
type EnergyMap = Map String Int
type PlayerEnergies = Map Player EnergyMap

data Action =
    Move Player Transport Node
    deriving (Show, Eq)

data PlayerPositions =
    Map Player Node -- player 0 is the rogue core

type RogueHistory =
    [Transport] -- display infos

{--
data GameState =
    State
        { playerPositions :: PlayerPositions
        , energyMap       :: EnergyMap
        , rogueHistory    :: RogueHistory
        {- ... gamestate fields -}
        }
--}

class GameView view where
    playerPositions :: view -> PlayerPositions
    energyMap :: view -> EnergyMap
    rogueHistory :: view -> RogueHistory
    rogueLastSeen :: view -> Maybe Node


data RogueGameView =
    RogueView
        { roguePlayerPositions :: PlayerPositions
        , rogueEnergyMap       :: PlayerEnergies
        , rogueOwnHistory      :: RogueHistory
        }

data CatcherGameView =
    CatcherView
        { catcherPlayerPositions :: PlayerPositions
        , catcherEenergyMap      :: PlayerEnergies
        , catcherRogueHistory    :: RogueHistory
        , catcherRogueLastSeen   :: Maybe Node
        }

instance GameView RogueGameView where
