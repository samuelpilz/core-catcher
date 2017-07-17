{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.InitialState where
-- TODO: remove this hard-coded starting-state
import           ClassyPrelude
import qualified Data.Map         as Map
import           Network.Protocol

initialRogueHistory :: RogueHistory
initialRogueHistory = RogueHistory []

startPlayer :: Player
startPlayer = Player 0

initialPlayerPositions :: PlayerPositions
initialPlayerPositions =
    PlayerPositions { playerPositions =
        Map.fromList
            [ ( Player { playerId = 0 }, Node { nodeId = 1 } )
            , ( Player { playerId = 1 }, Node { nodeId = 4 } )
            , ( Player { playerId = 2 }, Node { nodeId = 2 } )
            , ( Player { playerId = 3 }, Node { nodeId = 14 } )
            ]
    }


initialPlayerEnergies :: PlayerEnergies
initialPlayerEnergies =
    PlayerEnergies .
        Map.fromList $
            map (\playerId -> (Player playerId, initialEnergiesPerPlayer)) [0..3]


initialEnergiesPerPlayer :: EnergyMap
initialEnergiesPerPlayer =
    EnergyMap { energyMap =
        Map.fromList
            [ ( Transport { transportName = "orange" }, 5 )
            , ( Transport { transportName = "blue" }, 3 )
            , ( Transport { transportName = "red" }, 2 )
            ]
    }

