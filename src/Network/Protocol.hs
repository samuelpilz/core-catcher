{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Protocol where

import           ClassyPrelude
import           Data.Aeson                as Aeson
import           Elm.Derive
import           GHC.Generics              ()
import           Test.QuickCheck.Arbitrary
{-
This module provides data-types that are sent to game-clients and bots as messages.
This class is a semantic protocol definition. The data-types are sent in json format.
-}

-- |Players and Nodes are Ints (=Ids). The rouge-player has id 0
newtype Player =
    Player { playerId :: Int }
    deriving (Show, Read, Eq, Ord, Generic)

-- |Node representation
newtype Node =
    Node { nodeId :: Int }
    deriving (Show, Read, Eq, Ord, Generic)

-- |Edge is a tuple of two Nodes
newtype Edge =
    Edge { edge :: (Node, Node) }
    deriving (Show, Read, Eq, Ord, Generic)

-- |Transport is a string
newtype Transport =
    Transport { transportName :: String }
    deriving (Show, Read, Eq, Ord, Generic)

{- |A engergy-map is keeps track how much energy per transport a player has left.
-}
newtype EnergyMap =
    EnergyMap
        { energyMap :: Map Transport Int
        }
        deriving (Show, Read, Eq, Generic)

{- |The playerEnergies Map keeps track of the EnergyMaps for all players.
-}
newtype PlayerEnergies =
    PlayerEnergies
        { playerEnergies :: Map Player EnergyMap
        }
        deriving (Show, Read, Eq, Generic)

{- |An action is something one of the players can do.
Currently this is only a move, but this may be expanded in the future.
-}
data Action =
    Move
      { player    :: Player
      , transport :: Transport
      , node      :: Node
      }
    deriving (Show, Read, Eq, Generic)

{- |The playerPositions map keeps track of the current nodes each player is on.

It is possible that the map is not complete.
This should be the case if the missing player should not be seen.
-}
newtype PlayerPositions =
    PlayerPositions
      { playerPositions_ :: Map Player Node -- ^player 0 is the rogue core
      }
    deriving (Show, Read, Eq, Generic)

{- |The history of transports used by the rouge core.
-}
newtype RogueTransportHistory =
    RogueTransportHistory
        { rogueTransportHistory :: [Transport]  -- ^display infos
        }
      deriving (Show, Read, Eq, Generic)

{--
data GameState =
    State
        { playerPositions :: PlayerPositions
        , energyMap       :: EnergyMap
        , rogueHistory    :: RogueHistory
        {- ... gamestate fields -}
        }
--}

{- |A game view is a subset of the game-State as seen by one of the players.
A game view should be determined by the player it is constructed for and a game state
-}
class (FromJSON view, ToJSON view) => GameView view where
    playerPositions :: view -> PlayerPositions
    energies :: view -> EnergyMap
    rogueHistory :: view -> RogueTransportHistory
    rogueLastSeen :: view -> Maybe Node

{- |A game view as seen by the rouge-core
-}
data RogueGameView =
    RogueView
        { roguePlayerPositions :: PlayerPositions
        , rogueEnergyMap       :: EnergyMap
        , rogueOwnHistory      :: RogueTransportHistory
        , rogueRogueLastSeen   :: Maybe Node
        }
        deriving (Show, Read,  Eq, Generic)

{- |A game view as seen by the catchers
-}
data CatcherGameView =
    CatcherView
        { catcherPlayerPositions :: PlayerPositions
        , catcherEenergyMap      :: EnergyMap
        , catcherRogueHistory    :: RogueTransportHistory
        , catcherRogueLastSeen   :: Maybe Node
        }
        deriving (Show, Read, Eq, Generic)

instance GameView RogueGameView where
    playerPositions = roguePlayerPositions
    energies = rogueEnergyMap
    rogueHistory = rogueOwnHistory
    rogueLastSeen = rogueRogueLastSeen

instance GameView CatcherGameView where
    playerPositions = catcherPlayerPositions
    energies = catcherEenergyMap
    rogueHistory = catcherRogueHistory
    rogueLastSeen = catcherRogueLastSeen


{- |Network: Nodes and Map Transport to Overlay.

The overlays contain the actual Edges

The network itself has no information about its representation.
Representation is handled via NetworkDisplayInfo

-}
data Network =
    Network
        { nodes    :: [Node]
        , overlays :: Map Transport NetworkOverlay
        }
        deriving (Show, Read, Eq, Generic)


{- |NetworkOverlay: Sub-Graph that contains several nodes

-}
data NetworkOverlay =
    NetworkOverlay
        { overlayNodes :: [Node] -- ^the contained nodes in the Overlay.
        , edges        :: [Edge] -- ^The edges must only connect the nodes contained in the first list.
        }
        deriving (Show, Read, Eq, Generic)

instance FromJSONKey Player where

instance FromJSONKey Node where

instance FromJSONKey Transport where

instance ToJSONKey Player where

instance ToJSONKey Transport where

instance Arbitrary Player  where
    arbitrary =
        Player <$> arbitrary

instance Arbitrary Node  where
    arbitrary =
        Node <$> arbitrary

instance Arbitrary Edge  where
    arbitrary =
        Edge <$> ((,) <$> arbitrary <*> arbitrary)

instance Arbitrary Transport  where
    arbitrary =
        Transport <$> arbitrary

instance Arbitrary Action where
    arbitrary = do
        player <- arbitrary
        transport <- arbitrary
        node <- arbitrary
        return Move { player, transport, node}

instance Arbitrary PlayerPositions where
    arbitrary = do
        playerPos <- arbitrary
        return PlayerPositions { playerPositions_ = playerPos }


deriveBoth Elm.Derive.defaultOptions ''Action
deriveBoth Elm.Derive.defaultOptions ''PlayerPositions
deriveBoth Elm.Derive.defaultOptions ''RogueGameView
deriveBoth Elm.Derive.defaultOptions ''CatcherGameView
deriveBoth Elm.Derive.defaultOptions ''PlayerEnergies
deriveBoth Elm.Derive.defaultOptions ''EnergyMap
deriveBoth Elm.Derive.defaultOptions ''Network
deriveBoth Elm.Derive.defaultOptions ''NetworkOverlay
deriveBoth Elm.Derive.defaultOptions ''Player
deriveBoth Elm.Derive.defaultOptions ''Edge
deriveBoth Elm.Derive.defaultOptions ''Node
deriveBoth Elm.Derive.defaultOptions ''Transport
deriveBoth Elm.Derive.defaultOptions ''RogueTransportHistory
