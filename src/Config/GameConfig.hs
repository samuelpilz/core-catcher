{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.GameConfig (GameConfig(..), defaultConfig, getRogue) where

import           ClassyPrelude
import qualified Config.Network   as Network
import           Network.Protocol

data GameConfig =
    GameConfig
        { players                :: NonNull (Seq Player)
        , initialPlayerEnergies  :: PlayerEnergies
        , initialPlayerPositions :: PlayerPositions
        , maxRounds              :: Int
        , rogueShowsAt           :: [Int]
        , network                :: Network
        }
    deriving (Eq, Show, Read)

getRogue :: GameConfig -> Player
getRogue = head . players

-- | The default GameConfig
defaultConfig :: GameConfig
defaultConfig = GameConfig
    { players = impureNonNull $ fromList defaultPlayers
    , initialPlayerEnergies = defaultInitialPlayerEnergies
    , initialPlayerPositions = defaultInitialPlayerPositions
    , maxRounds = 10
    , rogueShowsAt = [2,5,8,10]
    , network = Network.network
    }

defaultPlayers :: [Player]
defaultPlayers = map Player ["Alice", "Bob", "Charlie"]

defaultInitialPlayerPositions :: PlayerPositions
defaultInitialPlayerPositions =
    mapFromList . zip defaultPlayers . map Node $ [1, 4, 12]


defaultInitialPlayerEnergies :: PlayerEnergies
defaultInitialPlayerEnergies =
    mapFromList . zip defaultPlayers . repeat $ initialEnergiesPerPlayer

initialEnergiesPerPlayer :: EnergyMap
initialEnergiesPerPlayer =
    mapFromList
        [ ( Orange, 7 )
        , ( Blue, 4 )
        , ( Red, 2 )
        ]
