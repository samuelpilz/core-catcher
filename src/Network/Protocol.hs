{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Text has no arbitrary instance, defined here
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Protocol where

import           ClassyPrelude
import           Data.Aeson                as Aeson
import           Elm.Derive
import           GHC.Generics              ()
import           Test.QuickCheck.Arbitrary
import qualified TH.MonoDerive             as Derive
import qualified Test.QuickCheck.Gen       as Gen

{-
This module provides data-types that are sent to game-clients and bots as messages.
This class is a semantic protocol definition. The data-types are sent in json format.
-}

-- |Players have names
newtype Player =
    Player { playerName :: Text }
    deriving (Show, Read, Eq, Ord, Generic)

-- |Nodes are ints (ids) representation
newtype Node =
    Node { nodeId :: Int }
    deriving (Show, Read, Eq, Ord, Generic)

-- |Edge is a tuple of two Nodes
newtype Edge =
    Edge { edge :: (Node, Node) }
    deriving (Show, Read, Eq, Ord, Generic)

-- |Energy is a enum of possible energies.
data Energy = Red | Blue | Orange
    deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)

{- |An energy-map keeps track how much energy per energy a player has left.
-}
newtype EnergyMap =
    EnergyMap
        { energyMap :: Map Energy Int
        }
        deriving (Show, Read, Eq, Generic)

-- |A GameError is a enum of possible errors
data GameError
    = NotTurn Player
    | PlayerNotFound Player
    | EnergyNotFound Energy
    | NotReachable Node Energy Node
    | NodeBlocked Player
    | NotEnoughEnergy
    | GameIsOver
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
-} -- TODO: write into design document that an action may be more than a move. Maybe change?
data Action =
    Move
        { actionPlayer :: Player
        , actionEnergy :: Energy
        , actionNode   :: Node
        }
    deriving (Show, Read, Eq, Generic)

{- |The playerPositions map keeps track of the current nodes each player is on.

It is possible that the map is not complete.
This should be the case if the missing player should not be seen.
-}
newtype PlayerPositions =
    PlayerPositions
        { playerPositions :: Map Player Node -- ^player 0 is the rogue core
        }
    deriving (Show, Read, Eq, Generic)

-- TODO: Seq instead of list?
-- TODO: rename to shadowRogueHistory?
{- |The history of energies used by the rouge core.
-}
newtype RogueHistory =
    RogueHistory
        { rogueHistory :: [(Energy, Maybe Node)]
        }
    deriving (Show, Read, Eq, Generic)
-- TODO: derive type-classes like monoFunctor, ...

{- |The history of energies used by the rogue together with all nodes

The bool flag indicates whether the node in the history-entry is shown to the catchers
 during the game
-}
newtype OpenRogueHistory =
    OpenRogueHistory
        { openRogueHistory :: [(Energy, Node, Bool)]
        }
    deriving (Show, Read, Eq, Generic)


{- |A game view as seen by the rouge-core.
-}
data RogueGameView =
    RogueGameView
        { roguePlayerPositions :: PlayerPositions
        , rogueEnergies        :: PlayerEnergies
        , rogueOwnHistory      :: RogueHistory
        , rogueNextPlayer      :: Player
        }
    deriving (Show, Read,  Eq, Generic)

{- |A game view as seen by the catchers
-}
data CatcherGameView =
    CatcherGameView
        { catcherPlayerPositions :: PlayerPositions
        , catcherEnergies        :: PlayerEnergies
        , catcherRogueHistory    :: RogueHistory
        , catcherNextPlayer      :: Player
        }
    deriving (Show, Read, Eq, Generic)

{- |A view for the game-over screen
-}
data GameOverView =
    GameOverView
        { gameOverViewPlayerPositions :: PlayerPositions
        , gameOverViewPlayerEnergies  :: PlayerEnergies
        , gameOverViewRogueHistory    :: OpenRogueHistory
        }
    deriving (Show, Read, Eq, Generic)

{- |A game view is a subset of the game-State as seen by one of the players.
A game view should be determined by the player it is constructed for and a game state.
GameView is glue code for the game state. No actual game state is sent between
the fronend and the backend but only the views.
Views can contain different information based on the receiver.
-}
data GameView =
    RogueView RogueGameView | CatcherView CatcherGameView
    deriving (Show, Read,  Eq, Generic)

viewPlayerPositions :: GameView -> PlayerPositions
viewPlayerPositions (CatcherView view) = catcherPlayerPositions view
viewPlayerPositions (RogueView view)   = roguePlayerPositions view

viewEnergies :: GameView -> PlayerEnergies
viewEnergies (CatcherView view) = catcherEnergies view
viewEnergies (RogueView view)   = rogueEnergies view

viewRogueHistory :: GameView -> RogueHistory
viewRogueHistory (CatcherView view) = catcherRogueHistory view
viewRogueHistory (RogueView view)   = rogueOwnHistory view

viewNextPlayer :: GameView -> Player
viewNextPlayer (CatcherView view) = catcherNextPlayer view
viewNextPlayer (RogueView view)   = rogueNextPlayer view

{- |Network: Nodes and Map Energy to Overlay.

The overlays contain the actual Edges

The network itself has no information about its representation.
Representation is handled via NetworkDisplayInfo

-}
data Network =
    Network
        { nodes    :: [Node]
        , overlays :: Map Energy NetworkOverlay
        }
        deriving (Show, Read, Eq, Generic)


{- |NetworkOverlay: Sub-Graph that contains several nodes

-}
data NetworkOverlay =
    NetworkOverlay
        { overlayNodes :: [Node] -- ^the contained nodes in the Overlay.
        , overlayEdges :: [Edge] -- ^The edges must only connect the nodes contained in the first list.
        }
        deriving (Show, Read, Eq, Generic)

{- | InitialDataForClient the initial info the client gets after login

-}-- TODO: support login when game-over?
data InitialInfoForClient =
    InitialInfoForClient
        { networkForGame  :: Network
        , initialGameView :: GameView
        , initialPlayer   :: Player
        , allPlayers      :: [Player]
        , allEnergies     :: [Energy]
        }
        deriving (Show, Read, Eq, Generic)

newtype Login =
    Login
        { loginPlayer :: Player
        }
    deriving (Show, Read, Eq, Generic)

data MessageForServer
    = Action_ Action
    | Login_ Login
    deriving (Show, Read, Eq, Generic)

data MessageForClient
    = ServerHello
    | InitialInfoForClient_ InitialInfoForClient
    | GameView_ GameView
    | GameError_ GameError
    | GameOverView_ GameOverView
    deriving (Show, Read, Eq, Generic)

class SendableToClient msg where
    wrap :: msg -> MessageForClient


instance SendableToClient MessageForClient where
    wrap = id
instance SendableToClient GameError where
    wrap = GameError_
instance SendableToClient GameView where
    wrap = GameView_
instance SendableToClient GameOverView where
    wrap = GameOverView_
instance SendableToClient RogueGameView where
    wrap = GameView_ . RogueView
instance SendableToClient CatcherGameView where
    wrap = GameView_ . CatcherView
instance SendableToClient InitialInfoForClient where
    wrap = InitialInfoForClient_

instance FromJSONKey Player where

instance FromJSONKey Node where

instance FromJSONKey Energy where

instance ToJSONKey Player where

instance ToJSONKey Energy where

instance Arbitrary Text where
    arbitrary = pack <$> arbitrary

instance Arbitrary Player where
    arbitrary =
        Player <$> arbitrary

instance Arbitrary Node  where
    arbitrary =
        Node <$> arbitrary

instance Arbitrary Edge  where
    arbitrary =
        Edge <$> ((,) <$> arbitrary <*> arbitrary)

instance Arbitrary Energy  where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Action where
    arbitrary =
        Move <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PlayerPositions where
    arbitrary =
        PlayerPositions <$> arbitrary

instance Arbitrary PlayerEnergies where
    arbitrary =
        PlayerEnergies <$> arbitrary

instance Arbitrary EnergyMap where
    arbitrary =
        EnergyMap <$> arbitrary

instance Arbitrary RogueHistory where
    arbitrary =
        RogueHistory <$> arbitrary

instance Arbitrary OpenRogueHistory where
    arbitrary =
        OpenRogueHistory <$> arbitrary

instance Arbitrary GameError where
    arbitrary = undefined -- TODO: implement arbitrary for game-error??

instance Arbitrary CatcherGameView where
    arbitrary =
        CatcherGameView <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RogueGameView where
    arbitrary =
        RogueGameView <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Network where
    arbitrary =
        Network <$> arbitrary <*> arbitrary

instance Arbitrary NetworkOverlay where
    arbitrary =
        NetworkOverlay <$> arbitrary <*> arbitrary

instance Arbitrary GameView where
    arbitrary = do
        rogue <- arbitrary
        if rogue then
            RogueView <$> arbitrary
        else
            CatcherView <$> arbitrary

instance Arbitrary MessageForServer where
    arbitrary = Action_ <$> arbitrary

instance Arbitrary MessageForClient where
    arbitrary = GameView_ <$> arbitrary
    -- TODO: implement arbitrary with more constructors

deriveBoth Elm.Derive.defaultOptions ''Action
deriveBoth Elm.Derive.defaultOptions ''PlayerPositions
deriveBoth Elm.Derive.defaultOptions ''GameView
deriveBoth Elm.Derive.defaultOptions ''RogueGameView
deriveBoth Elm.Derive.defaultOptions ''CatcherGameView
deriveBoth Elm.Derive.defaultOptions ''GameError
deriveBoth Elm.Derive.defaultOptions ''PlayerEnergies
deriveBoth Elm.Derive.defaultOptions ''EnergyMap
deriveBoth Elm.Derive.defaultOptions ''Network
deriveBoth Elm.Derive.defaultOptions ''NetworkOverlay
deriveBoth Elm.Derive.defaultOptions ''Player
deriveBoth Elm.Derive.defaultOptions ''Edge
deriveBoth Elm.Derive.defaultOptions ''Node
deriveBoth Elm.Derive.defaultOptions ''Energy
deriveBoth Elm.Derive.defaultOptions ''RogueHistory
deriveBoth Elm.Derive.defaultOptions ''OpenRogueHistory
deriveBoth Elm.Derive.defaultOptions ''GameOverView
deriveBoth Elm.Derive.defaultOptions ''InitialInfoForClient
deriveBoth Elm.Derive.defaultOptions ''Login
deriveBoth Elm.Derive.defaultOptions ''MessageForServer
deriveBoth Elm.Derive.defaultOptions ''MessageForClient

-- IsMap implementation for PlayerPositions
Derive.deriveMap ''PlayerPositions
-- IsMap implementation for EnergyMap
Derive.deriveMap ''EnergyMap
-- IsMap implementation for PlayerEnergies
Derive.deriveMap ''PlayerEnergies
-- IsSequence for RogueHistory
Derive.deriveSequence ''RogueHistory
-- IsSequence for RogueHistory
Derive.deriveSequence ''OpenRogueHistory
