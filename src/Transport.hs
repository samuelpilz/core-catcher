{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Transport where

import           ClassyPrelude
import           Control.Monad
import           Data.Foldable (foldrM)
import           Data.List (cycle)

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
    deriving (Show, Eq, Ord, Read)

data Action
    = OneMove Move
    | TwoMoves Move Move
    deriving (Show, Eq, Ord, Read)

data Rollback = Rollback { state :: GameState, error :: Text } | Fatal { error :: Text }
type Result a = Either Rollback a

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

players :: Start -> [PlayerId]
players s = [0..(length s - 1)]


addAction :: Action -> GameState -> GameState
addAction a s =
    s { actions = a : actions s }

turns :: GameState -> Maybe [PlayerId]
turns state =
  do
    (_:ls) <- scanState updateTurn initialTurn state
    sequence $ map (\x -> case x of (h:_) -> Just h; _ -> Nothing) ls

initialTurn :: Start -> Maybe [PlayerId]
initialTurn start = case players start of [] -> Nothing; xs -> Just (cycle xs)
updateTurn :: [PlayerId] -> Action -> Maybe [PlayerId]
updateTurn (_:xs) _ = Just xs
updateTurn _ _ = Nothing

foldState :: (a -> Action -> Maybe a) -> (Start -> Maybe a) -> GameState -> Maybe a
foldState update initial (GameState start actions) =
  do
    s <- initial start
    foldrM (flip update) s actions

foldStateWithTurn :: (a -> (Action, PlayerId) -> Maybe a) -> (Start -> Maybe a) -> GameState -> Maybe a
foldStateWithTurn update initial state =
  do
    ts <- turns state
    s <- initial (start state)
    foldrM (flip update) s (zip (actions state) ts)

scanState :: (a -> Action -> Maybe a) -> (Start -> Maybe a) -> GameState -> Maybe [a]
scanState update initial (GameState start actions) =
  scanrM update (initial start) actions

printStatesWithTurns :: GameState -> IO ()
printStatesWithTurns state = fromMaybe (putStrLn "Error while printing States") $ foldStateWithTurn (\y x -> Just $ y >> (print x)) (const $ Just $ return ()) state

scanrM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> m a -> mono -> m [a]
scanrM fn z0 =
  let
    f b ma = do (a:as) <- ma; n <- (fn a b); return (n:a:as)
    start = do n <- z0; return [n]
  in
    foldr f start
