{-# LANGUAGE NoImplicitPrelude #-}

module Transport where

import           ClassyPrelude
import           Control.Monad

type PlayerId = Int
type PlayersPos = Vector Vertex
type Start = PlayersPos

data Color = Blue | Orange | Red deriving (Show, Eq, Ord, Read, Enum)
data Energy = BlackEnergy | Energy Color deriving (Show, Eq, Ord, Read)

data GameState =
    GameState
        { start   :: Start
        , actions :: [Action]
        }

data Move
    = Move Energy Vertex
    | Pass

data Action
    = OneMove Move
    | TwoMoves Move Move

applyAction :: Monad m => (a -> Move -> m a) -> a -> Action -> m a
applyAction f x (OneMove a)    = f x a
applyAction f x (TwoMoves a b) = do
    r <- f x a
    f r b

newtype Vertex =
    Vertex
        { vId :: VertexId
        }
        deriving (Show, Eq, Read, Ord)

newtype VertexId =
    V Int
    deriving (Eq, Show, Ord, Read)

players :: GameState -> [PlayerId]
players s = [0..(length (start s) - 1)]


addAction :: Action -> GameState -> GameState
addAction a s =
    s { actions = a : actions s }

