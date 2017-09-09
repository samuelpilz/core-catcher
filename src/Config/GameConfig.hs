{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.GameConfig
  ( GameConfig(..)
  , defaultConfig
  , defaultConfigWithRandomPositions
  , getRogue
  ) where

import           ClassyPrelude
import qualified Config.Network   as Network
import           Data.List        (nub)
import           Network.Protocol
import           System.Random    (RandomGen, randomRs)

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

-- |Generates gameConfig data from a generator from which to generate positions
defaultConfigWithRandomPositions :: (RandomGen gen) => gen -> GameConfig
defaultConfigWithRandomPositions gen =
    defaultConfig { initialPlayerPositions = randomPositions (length $ nodes Network.network) gen }


-- |Generates a playerPositions map from the given generator and the given max-amount of nodes
randomPositions :: (RandomGen gen) => Int -> gen -> PlayerPositions
randomPositions nodeNum =
    mapFromList . zip defaultPlayers . map Node . nub . randomRs (1, nodeNum)



-- | The default GameConfig
defaultConfig :: GameConfig
defaultConfig = GameConfig
    { players = impureNonNull $ fromList defaultPlayers
    , initialPlayerEnergies = defaultInitialPlayerEnergies
    , initialPlayerPositions = defaultInitialPlayerPositions
    , maxRounds = 10
    , rogueShowsAt = [1,4,7,10]
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
