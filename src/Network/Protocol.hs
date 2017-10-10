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
import           Test.QuickCheck.Gen
import qualified TH.MonoDerive             as Derive

{-
This module provides data-types that are sent to game-clients and bots as messages.
This class is a semantic protocol definition. The data-types are sent in json format.
-}


-- |Players have names
newtype Player =
    Player { playerName :: Text }
    deriving (Show, Read, Eq, Ord, Generic)

-- |Nodes are ints (ids)
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

-- TODO: implement Num for that
newtype GameId = GameId { gameId :: Int }
    deriving (Show, Read, Eq, Ord, Generic)

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
    | GameNotStarted
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

{- |The history of energies used by the rouge core where the position is sometimes revealed.

This message is sent to the catchers

-}
newtype ShadowRogueHistory =
    ShadowRogueHistory
        { shadowRogueHistory :: [(Energy, Maybe Node)]
        }
    deriving (Show, Read, Eq, Generic)

{- |The history of energies used by the rogue together with all nodes

The bool flag indicates whether the node in the history-entry is shown to the catchers
 during the game
-}
newtype OpenRogueHistory =
    OpenRogueHistory
        { openRogueHistory :: [(Energy, Node, Bool)]
        }
    deriving (Show, Read, Eq, Generic)


data RogueHistory
    = OpenHistory OpenRogueHistory
    | ShadowHistory ShadowRogueHistory
    deriving (Show, Read, Eq, Generic)

{- |A game view as seen by the rouge-core.
-}
data RogueGameView =
    RogueGameView
        { roguePlayerPositions :: PlayerPositions
        , rogueEnergies        :: PlayerEnergies
        , rogueOwnHistory      :: ShadowRogueHistory
        , rogueNextPlayer      :: Player
        }
    deriving (Show, Read,  Eq, Generic)

{- |A game view as seen by the catchers
-}
data CatcherGameView =
    CatcherGameView
        { catcherPlayerPositions :: PlayerPositions
        , catcherEnergies        :: PlayerEnergies
        , catcherRogueHistory    :: ShadowRogueHistory
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
        , gameOverViewWinningPlayer   :: Player
        , gameOverViewNetwork         :: Network
        , gameOverViewAllPlayers      :: [Player]
        , gameOverViewAllEnergies     :: [Energy]
        , gameOverViewGameName        :: Text
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

viewRogueHistory :: GameView -> ShadowRogueHistory
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

If the client wants to log in when the game is over, the client gets sent a GameOverView instead

-}
data InitialInfoGameActive =
    InitialInfoGameActive
        { networkForGame         :: Network
        , initialGameView        :: GameView
        , startingPlayer         :: Player
        , initialInfoAllPlayers  :: [Player]
        , initialInfoAllEnergies :: [Energy]
        , initialInfoGameName    :: Text
        }
        deriving (Show, Read, Eq, Generic)


newtype Login =
    Login
        { loginPlayer :: Player
        }
    deriving (Show, Read, Eq, Generic)


newtype LoginFail =
    LoginFail
        { loginFailPlayer :: Player
        }
    deriving (Show, Read, Eq, Generic)

data GameLobbyView =
    GameLobbyView
        { gameLobbyViewGameName :: Text
        , gameLobbyViewPlayers  :: [Player]
        }
    deriving (Show, Read, Eq, Generic)

data GameLobbyPreview =
    GameLobbyPreview
        { gameLobbyPreviewGameId   :: GameId
        , gameLobbyPreviewGameName :: Text
        , gameLobbyPreviewPlayers  :: [Player]
        }
    deriving (Show, Read, Eq, Generic)

data GamePreview =
    GamePreview
        { gamePreviewGameId   :: GameId
        , gamePreviewGameName :: Text
        , gamePreviewPlayers  :: [Player]
        }
    deriving (Show, Read, Eq, Generic)

data PlayerHome =
    PlayerHome
        { playerHomePlayer :: Player
        , activeGames      :: [GamePreview]
        , activeLobbies    :: [GameLobbyPreview]
        }
    deriving (Show, Read, Eq, Generic)

newtype CreateNewGame =
    CreateNewGame
        { createGameName :: Text
        }
    deriving (Show, Read, Eq, Generic)

newtype JoinGame =
    JoinGame
        { joinGameId :: GameId
        }
    deriving (Show, Read, Eq, Generic)

data ServerError
    = NoSuchGame GameId
    | NotInGame Player
    | ClientMsgError
    | NotLoggedIn
    | GameAlreadyStarted
    | NoSuchConnection
    | GameError_ GameError
    deriving (Show, Read, Eq, Generic)


data MessageForServer
    = Login_ Login
    | CreateNewGame_ CreateNewGame
    | Action_ Action
    | StartGame
    | JoinGame_ JoinGame
    | PlayerHomeRefresh
    | Logout
    deriving (Show, Read, Eq, Generic)

data MessageForClient
    = ServerHello
    | LoginFail_ LoginFail
    | PlayerHome_ PlayerHome
    | InitialInfoGameActive_ InitialInfoGameActive
    | GameView_ GameView
    | GameOverView_ GameOverView
    | GameLobbyView_ GameLobbyView
    | ServerError_ ServerError
    deriving (Show, Read, Eq, Generic)

-- |class for messages sendable to the client
class SendableToClient msg where
    wrapSendable :: msg -> MessageForClient


instance SendableToClient MessageForClient where
    wrapSendable = id
instance SendableToClient GameError where
    wrapSendable = wrapSendable . GameError_
instance SendableToClient GameView where
    wrapSendable = GameView_
instance SendableToClient GameOverView where
    wrapSendable = GameOverView_
instance SendableToClient RogueGameView where
    wrapSendable = GameView_ . RogueView
instance SendableToClient CatcherGameView where
    wrapSendable = GameView_ . CatcherView
instance SendableToClient InitialInfoGameActive where
    wrapSendable = InitialInfoGameActive_
instance SendableToClient GameLobbyView where
    wrapSendable = GameLobbyView_
instance SendableToClient PlayerHome where
    wrapSendable = PlayerHome_
instance SendableToClient ServerError where
    wrapSendable = ServerError_
instance (SendableToClient a, SendableToClient b) => SendableToClient (Either a b) where
    wrapSendable = either wrapSendable wrapSendable

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

instance Arbitrary GameId where
    arbitrary =
        GameId <$> arbitrary

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

instance Arbitrary ShadowRogueHistory where
    arbitrary =
        ShadowRogueHistory <$> arbitrary

instance Arbitrary OpenRogueHistory where
    arbitrary =
        OpenRogueHistory <$> arbitrary

instance Arbitrary RogueHistory where
    arbitrary =
        oneof
            [ OpenHistory <$> arbitrary
            , ShadowHistory <$> arbitrary
            ]

instance Arbitrary GameError where
    arbitrary =
        oneof
            [ NotTurn <$> arbitrary
            , PlayerNotFound <$> arbitrary
            , EnergyNotFound <$> arbitrary
            , NotReachable <$> arbitrary <*> arbitrary <*> arbitrary
            , NodeBlocked <$> arbitrary
            , return NotEnoughEnergy
            , return GameIsOver
            ]

instance Arbitrary CatcherGameView where
    arbitrary =
        CatcherGameView <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RogueGameView where
    arbitrary =
        RogueGameView <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GameOverView where
    arbitrary =
        GameOverView <$>
            arbitrary <*>
            arbitrary <*>
            arbitrary <*>
            arbitrary <*>
            arbitrary <*>
            arbitrary <*>
            arbitrary <*>
            arbitrary

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


instance Arbitrary InitialInfoGameActive where
    arbitrary = InitialInfoGameActive <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary GameLobbyView where
    arbitrary = GameLobbyView <$> arbitrary <*> arbitrary

instance Arbitrary GameLobbyPreview where
    arbitrary = GameLobbyPreview <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PlayerHome where
    arbitrary = PlayerHome <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GamePreview where
    arbitrary = GamePreview <$> arbitrary <*> arbitrary <*> arbitrary

-- TODO: update arbitraries
instance Arbitrary MessageForServer where
    arbitrary = Action_ <$> arbitrary


instance Arbitrary MessageForClient where
    arbitrary =
        oneof
            [ return ServerHello
            , InitialInfoGameActive_ <$> arbitrary
            , GameView_ <$> arbitrary
            , GameOverView_ <$> arbitrary
            , GameLobbyView_ <$> arbitrary
            , PlayerHome_ <$> arbitrary
            ]


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
deriveBoth Elm.Derive.defaultOptions ''ShadowRogueHistory
deriveBoth Elm.Derive.defaultOptions ''OpenRogueHistory
deriveBoth Elm.Derive.defaultOptions ''RogueHistory
deriveBoth Elm.Derive.defaultOptions ''GameOverView
deriveBoth Elm.Derive.defaultOptions ''InitialInfoGameActive
deriveBoth Elm.Derive.defaultOptions ''Login
deriveBoth Elm.Derive.defaultOptions ''JoinGame
deriveBoth Elm.Derive.defaultOptions ''LoginFail
deriveBoth Elm.Derive.defaultOptions ''GameLobbyView
deriveBoth Elm.Derive.defaultOptions ''GameLobbyPreview
deriveBoth Elm.Derive.defaultOptions ''GamePreview
deriveBoth Elm.Derive.defaultOptions ''PlayerHome
deriveBoth Elm.Derive.defaultOptions ''CreateNewGame
deriveBoth Elm.Derive.defaultOptions ''MessageForServer
deriveBoth Elm.Derive.defaultOptions ''MessageForClient
deriveBoth Elm.Derive.defaultOptions ''ServerError
deriveBoth Elm.Derive.defaultOptions ''GameId

-- IsMap implementation for PlayerPositions
Derive.deriveMap ''PlayerPositions
-- IsMap implementation for EnergyMap
Derive.deriveMap ''EnergyMap
-- IsMap implementation for PlayerEnergies
Derive.deriveMap ''PlayerEnergies
-- IsSequence for RogueHistory
Derive.deriveSequence ''ShadowRogueHistory
-- IsSequence for RogueHistory
Derive.deriveSequence ''OpenRogueHistory
