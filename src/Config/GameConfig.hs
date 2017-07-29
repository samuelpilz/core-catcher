{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.GameConfig (GameConfig(..), defaultConfig) where

import           ClassyPrelude
import qualified Config.Network   as Network
import           Network.Protocol

data GameConfig =
    GameConfig
        { players                :: [Player]
        , initialPlayerEnergies  :: PlayerEnergies
        , initialPlayerPositions :: PlayerPositions
        , maxRounds              :: Int
        , rogueShowsAt           :: [Int]
        , network                :: Network
        }
    deriving (Eq, Show, Read)

-- | The default GameConfig
defaultConfig :: GameConfig
defaultConfig = GameConfig
    { players = defaultPlayers
    , initialPlayerEnergies = defaultInitialPlayerEnergies
    , initialPlayerPositions = defaultInitialPlayerPositions
    , maxRounds = 10
    , rogueShowsAt = [2,5,8,10]
    , network = Network.network
    }

defaultPlayers :: [Player]
defaultPlayers = fromList . map Player $ [0..3]

defaultInitialPlayerPositions :: PlayerPositions
defaultInitialPlayerPositions =
    mapFromList . zip defaultPlayers . map Node $ [1, 4, 2, 14]


defaultInitialPlayerEnergies :: PlayerEnergies
defaultInitialPlayerEnergies =
    mapFromList . zip defaultPlayers . repeat $ initialEnergiesPerPlayer

initialEnergiesPerPlayer :: EnergyMap
initialEnergiesPerPlayer =
    mapFromList
        [ ( Orange, 5 )
        , ( Blue, 3 )
        , ( Red, 2 )
        ]
