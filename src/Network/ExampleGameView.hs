{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ExampleGameView where

import           ClassyPrelude    ()
import qualified Data.Map         as Map
import           Network.Protocol

exampleCatcherGameView :: CatcherGameView
exampleCatcherGameView =
    CatcherGameView { catcherPlayerPositions = examplePlayerPositions
    , catcherEnergies = examplePlayerEnergies
    , catcherRogueHistory = RogueHistory {rogueHistory_ = []}
    , catcherNextPlayer = Player {playerId = 0}
    }

exampleRogueGameView :: RogueGameView
exampleRogueGameView =
    RogueGameView { roguePlayerPositions = examplePlayerPositions
    , rogueEnergies = examplePlayerEnergies
    , rogueOwnHistory = RogueHistory {rogueHistory_ = []}
    , rogueNextPlayer = Player {playerId = 0}
    }


examplePlayerPositions :: PlayerPositions
examplePlayerPositions =
    PlayerPositions { playerPositions_ =
        Map.fromList [ ( Player { playerId = 0 }, Node { nodeId = 1 } )
        , ( Player { playerId = 1 }, Node { nodeId = 4 } )
        , ( Player { playerId = 2 }, Node { nodeId = 2 } )
        ]
    }


examplePlayerEnergies :: PlayerEnergies
examplePlayerEnergies =
    PlayerEnergies {playerEnergies =
        Map.fromList [ ( Player { playerId = 0 }, examplePlayer0Energies )
        ]
    }


examplePlayer0Energies :: EnergyMap
examplePlayer0Energies =
    EnergyMap { energyMap =
        Map.fromList [ ( Transport { transportName = "taxi" }, 5 )
        , ( Transport { transportName = "bus" }, 3 )
        , ( Transport { transportName = "underground" }, 2 )
        ]
    }

