{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ExampleGameView where

import           ClassyPrelude
import qualified Data.Map         as Map
import           Network.Protocol

exampleCatcherGameView :: CatcherGameView
exampleCatcherGameView =
    CatcherView { catcherPlayerPositions = examplePlayerPositions
    , catcherEnergies = examplePlayerEnergies
    , catcherRogueHistory = RogueTransportHistory {rogueTransportHistory = []}
    , catcherRogueLastSeen = Nothing
    , catcherViewError = Nothing
    }

exampleRogueGameView :: RogueGameView
exampleRogueGameView =
    RogueView { roguePlayerPositions = examplePlayerPositions
    , rogueEnergies = examplePlayerEnergies
    , rogueOwnHistory = RogueTransportHistory {rogueTransportHistory = []}
    , rogueRogueLastSeen = Nothing
    , rogueViewError = Nothing
    }


examplePlayerPositions :: PlayerPositions
examplePlayerPositions =
    PlayerPositions { playerPositions_ =
        Map.fromDistinctAscList [ ( Player { playerId = 1 }, Node { nodeId = 1 } )
        , ( Player { playerId = 2 }, Node { nodeId = 4 } )
        , ( Player { playerId = 3 }, Node { nodeId = 2 } )
        ]
    }


examplePlayerEnergies :: PlayerEnergies
examplePlayerEnergies =
    PlayerEnergies {playerEnergies =
        Map.fromDistinctAscList [ ( Player { playerId = 1 }, examplePlayer1Energies )
        ]
    }


examplePlayer1Energies :: EnergyMap
examplePlayer1Energies =
    EnergyMap { energyMap =
        Map.fromDistinctAscList [ ( Transport { transportName = "taxi" }, 5 )
        , ( Transport { transportName = "bus" }, 3 )
        , ( Transport { transportName = "underground" }, 2 )
        ]
    }
