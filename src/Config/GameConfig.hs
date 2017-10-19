{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Config.GameConfig
  ( GameConfig(..)
  , defaultConfig
  , defaultConfigForPlayers
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
        , gameName               :: Text
        }
    deriving (Eq, Show)

getRogue :: GameConfig -> Player
getRogue = head . players


-- |Generates a playerPositions map from the given generator and the given max-amount of nodes
randomPositions :: (RandomGen gen) => gen -> [Player] -> Int -> PlayerPositions
randomPositions gen players nodeNum =
    mapFromList . zip players . map Node . nub . randomRs (1, nodeNum) $ gen

defaultConfigForPlayers :: (RandomGen gen) => gen -> Text -> NonNull (Seq Player) -> GameConfig
defaultConfigForPlayers gen gameName players =
    GameConfig
        { players = players
        , initialPlayerEnergies = defaultInitialPlayerEnergies players
        , initialPlayerPositions = randomPositions gen (otoList players) . length $ nodes Network.network
        , maxRounds = 10
        , rogueShowsAt = [1,4,7,10]
        , network = Network.network
        , gameName = gameName
        }

-- | The default GameConfig
defaultConfig :: GameConfig
defaultConfig =
    GameConfig
        { players = impureNonNull $ fromList defaultPlayers
        , initialPlayerEnergies = defaultInitialPlayerEnergies defaultPlayers
        , initialPlayerPositions = defaultInitialPlayerPositions
        , maxRounds = 10
        , rogueShowsAt = [1,4,7,10]
        , network = Network.network
        , gameName = "Test Game"
        }

defaultInitialPlayerEnergies :: (MonoFoldable ps, Player ~ Element ps) => ps -> PlayerEnergies
defaultInitialPlayerEnergies ps =
    mapFromList . zip (otoList ps) . repeat $ initialEnergiesPerPlayer

defaultPlayers :: [Player]
defaultPlayers = map Player ["Alice", "Bob", "Charlie"]

defaultInitialPlayerPositions :: PlayerPositions
defaultInitialPlayerPositions =
    mapFromList . zip defaultPlayers . map Node $ [1, 4, 12]


initialEnergiesPerPlayer :: EnergyMap
initialEnergiesPerPlayer =
    mapFromList
        [ ( Orange, 7 )
        , ( Blue, 4 )
        , ( Red, 2 )
        ]
