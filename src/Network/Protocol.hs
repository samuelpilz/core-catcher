{-# LANGUAGE NoImplicitPrelude #-}
module Network.Protocol where

import           ClassyPrelude

{-
This module provides data-types that are sent to game-clients and bots as messages.
This class is a semantic protocol definition. The data-types are sent in json format.


-}

-- |Players and Nodes are Ints (=Ids). The rouge-player has id 0
type Player = Int
-- |Node representation
type Node = Int
-- |Edge is a tuple of two Nodes
type Edge = (Node, Node)
-- |Transport is a string
type Transport = String
{- |A engergy-map is keeps track how much energy per transport a player has left.
-}
type EnergyMap = Map Transport Int
{- |The playerEnergies Map keeps track of the EnergyMaps for all players.
-}
type PlayerEnergies = Map Player EnergyMap

{- |An action is something one of the players can do.
Currently this is only a move, but this may be expanded in the future.
-}
data Action =
    Move Player Transport Node
    deriving (Show, Eq)

{- |The playerPositions map keeps track of the current nodes each player is on.

It is possible that the map is not complete.
This should be the case if the missing player should not be seen.
-}
data PlayerPositions =
    Map Player Node -- player 0 is the rogue core

{- |The history of transports used by the rouge core.
-}
type RogueTransportHistory =
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

{- |A game view is a subset of the game-State as seen by one of the players.
A game view should be determined by the player it is constructed for and a game state
-}
class GameView view where
    playerPositions :: view -> PlayerPositions
    energyMap :: view -> EnergyMap
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

{- |A game view as seen by the catchers
-}
data CatcherGameView =
    CatcherView
        { catcherPlayerPositions :: PlayerPositions
        , catcherEenergyMap      :: EnergyMap
        , catcherRogueHistory    :: RogueTransportHistory
        , catcherRogueLastSeen   :: Maybe Node
        }

instance GameView RogueGameView where
    playerPositions = roguePlayerPositions
    energyMap = rogueEnergyMap
    rogueHistory = rogueOwnHistory
    rogueLastSeen = rogueRogueLastSeen

instance GameView CatcherGameView where
    playerPositions = catcherPlayerPositions
    energyMap = catcherEenergyMap
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


{- |NetworkOverlay: Sub-Graph that contains several nodes

First part: the contained nodes in the Overlay.
The nodes have to be contained in the nodes of the enclosing network
Second part: the edges of the
The edges must only connect the nodes contained in the first list.

-}
data NetworkOverlay =
    NetworkOverlay
        { overlayNodes :: [Node]
        , edges        :: [Edge]
        }
