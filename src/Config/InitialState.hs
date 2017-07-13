{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.InitialState where
-- TODO: remove
import           ClassyPrelude    ()
import qualified Data.Map         as Map
import           Network.Protocol

initialRogueHistory :: RogueHistory
initialRogueHistory = RogueHistory []

startPlayer :: Player
startPlayer = Player 0

initialPlayerPositions :: PlayerPositions
initialPlayerPositions =
    PlayerPositions { playerPositions =
        Map.fromList [ ( Player { playerId = 0 }, Node { nodeId = 1 } )
        , ( Player { playerId = 1 }, Node { nodeId = 4 } )
        , ( Player { playerId = 2 }, Node { nodeId = 2 } )
        ]
    }


initialPlayerEnergies :: PlayerEnergies
initialPlayerEnergies =
    PlayerEnergies {playerEnergies =
        Map.fromList [ ( Player { playerId = 0 }, initialPlayer0Energies )
        ]
    }


initialPlayer0Energies :: EnergyMap
initialPlayer0Energies =
    EnergyMap { energyMap =
        Map.fromList [ ( Transport { transportName = "taxi" }, 5 )
        , ( Transport { transportName = "bus" }, 3 )
        , ( Transport { transportName = "underground" }, 2 )
        ]
    }

