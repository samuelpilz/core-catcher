{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies      #-}

module GameLogic where

import           ClassyPrelude
import           Control.Monad                     as Monad
import qualified Data.Aeson                        as Aeson
import           Data.Foldable                     (foldrM)
import qualified Data.Graph.Inductive.Graph        as Graph
import qualified Data.Graph.Inductive.PatriciaTree as PTree
import qualified Data.List                         as List
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import           Data.Vector                       ((!?), (//))
import qualified GHC.Generics                      ()
import           Lib                               (mapLeft, maybeToEither,
                                                    scanrTailM, scanrTailM)
import           Safe                              (tailSafe)

type PlayerId = Int
roguePid :: PlayerId
roguePid = 0
data Party = Rogue | Catchers
type VertexId = Graph.Node
-- For each player: Position in the graph
type PlayersPos = Vector VertexId
-- For each player: For each Energy: Amount of remaining energy
type PlayersEnergies = Vector (Map.Map Energy Int)
-- Initial state that every intermediate state is based on
type Start = PlayersPos

-- Possible energy colors
data Color = Blue | Orange | Red deriving (Show, Eq, Ord, Read, Enum, Generic)
instance Aeson.ToJSONKey Color where
instance Aeson.ToJSON Color where
instance Aeson.FromJSON Color where
-- Energy types, where BlackEnergy allows traversing an edge of any energy color
data Energy = BlackEnergy | Energy Color deriving (Show, Eq, Ord, Read, Generic)
instance Aeson.ToJSONKey Energy where
instance Aeson.ToJSON Energy where
instance Aeson.FromJSON Energy where

data GameState =
    GameState
        { start   :: Start
        , network :: Network
        , actions :: [Action] -- Actions performed by clients
        }
        deriving (Show, Generic)

-- Graph the players play in
newtype Network =
    Network (PTree.Gr Vertex Edge)
    deriving (Show)

data Move
    = Move Energy VertexId -- A player is moving towards VertexId using Energy
    | Pass                 -- A player is not moving in this Move
    deriving (Show, Eq, Ord, Read, Generic)
instance Aeson.FromJSON Move where
instance Aeson.ToJSON Move where

data Action
    = OneMove Move       -- A player performs a single move
    | TwoMoves Move Move -- Rogue Core is performing two moves in a single turn
    deriving (Show, Eq, Ord, Read, Generic)
instance Aeson.FromJSON Action where
instance Aeson.ToJSON Action where

-- When clients send invalid actions or the initial state is invalid
-- a FlatGameState cannot be derived causing the derivation function
-- to return a textual error message
type Error = Text
-- Recover from errors
data ErrorHandler
  = Rollback { state :: GameState, error :: Error } -- Contains last valid GameState
  | Fatal { error :: Error }                        -- There is no valid GameState
  deriving (Show, Generic)

type Result a = Either ErrorHandler a

-- Returns verticies that can be reached in a network from a vertex using any energy in a list of energies
adjacentWithEnergy :: Network -> VertexId -> [Energy] -> [VertexId]
adjacentWithEnergy (Network gr) v es =
    map snd $ filter (\(c,_) -> any  (`Set.member` reach c) es) $ Graph.lneighbors gr v

-- Computes if a player can move in a network from a vertex using a specific energy to a vertex
canMove :: Network -> VertexId -> Energy -> VertexId -> Bool
canMove net from ticket to =
    to `elem` adjacentWithEnergy net from [ticket]

-- Example network
someNet :: Network
someNet =
    Network $ buildGr
        [([(mkEdge [Energy Red, Energy Blue], 2), (mkEdge [Energy Orange], 3), (mkEdge [Energy Orange], 6)], 1, Vertex)
        ,([(mkEdge [BlackEnergy], 3), (mkEdge [Energy Red, Energy Orange, Energy Blue], 5)], 2, Vertex)
        ,([(mkEdge [Energy Red], 4)], 3, Vertex)
        ,([], 4, Vertex)
        ,([(mkEdge [Energy Red, Energy Blue], 6)], 5, Vertex)
        ,([], 6, Vertex)
        ]

samNet :: Network
samNet =
  Network $ buildGr
    [([(mkEdge [Energy Red], 6), (mkEdge [Energy Orange], 11)], 1, Vertex)
    ,([(mkEdge [Energy Orange], 5), (mkEdge [Energy Orange], 8)], 2, Vertex)
    ,([(mkEdge [Energy Blue], 4), (mkEdge [Energy Orange], 5), (mkEdge [Energy Orange], 6), (mkEdge [Energy Orange], 7), (mkEdge [Energy Blue], 8)], 3, Vertex)
    ,([(mkEdge [Energy Orange], 7), (mkEdge [Energy Orange], 10), (mkEdge [Energy Blue], 15)], 4, Vertex)
    ,([(mkEdge [Energy Orange], 8), (mkEdge [Energy Orange], 12)], 5, Vertex)
    ,([(mkEdge [Energy Orange], 10), (mkEdge [Energy Orange], 11), (mkEdge [Energy Red], 13)], 6, Vertex)
    ,([(mkEdge [Energy Orange], 16)], 7, Vertex)
    ,([(mkEdge [Energy Orange], 9), (mkEdge [Energy Blue], 12)], 8, Vertex)
    ,([(mkEdge [Energy Red], 13)], 9, Vertex)
    ,([], 10, Vertex)
    ,([], 11, Vertex)
    ,([(mkEdge [Energy Orange], 13), (mkEdge [Energy Blue], 15)], 12, Vertex)
    ,([(mkEdge [Energy Red], 14)], 13, Vertex)
    ,([(mkEdge [Energy Orange], 15)], 14, Vertex)
    ,([(mkEdge [Energy Orange], 16)], 15, Vertex)
    ,([], 16, Vertex)
    ]

exampleInvalidMove0 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Orange) 1, OneMove $ Move (Energy Blue) 1, OneMove $ Move (Energy Orange) 3]

exampleInvalidMove1 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Orange) 1, OneMove $ Move BlackEnergy 3, OneMove $ Move (Energy Orange) 3]

exampleInvalidMove2 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Orange) 1, OneMove $ Move (Energy Blue) 1, OneMove $ Move (Energy Orange) 2]

exampleValidMove0 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Blue) 5, OneMove $ Move (Energy Orange) 6]

-- Graph context for unidirected graphs
type UniContext a b = (Graph.Adj b, Graph.Node, a)

-- buildGr for unidirected contexts
buildGr :: Graph.DynGraph gr => [UniContext a b] -> gr a b
buildGr ctx = Graph.buildGr [(e, n, v, e) | (e, n, v) <- ctx]

-- Apply a function computing intermediate state to each Move in an Action in sequence and return the result
applyAction :: Monad m => (a -> Move -> m a) -> a -> Action -> m a
applyAction f x (OneMove a)    = f x a
applyAction f x (TwoMoves a b) = do
    r <- f x a
    f r b

-- Information about a Vertex in the graph
-- Does not contain any information right now
-- Will contain position and name in the future
data Vertex = Vertex deriving (Show, Eq, Read, Ord)

-- An Edge between Vertices
newtype Edge =
    Edge
        { reach :: Set.Set Energy -- Energies that can be used to traverse this edge
        }
    deriving (Show, Eq, Read, Ord)

-- Utility function to create an edge from a list of energies
mkEdge :: [Energy] -> Edge
mkEdge energies = Edge { reach = Set.fromList energies }

-- Given an initial state, returns a list of PlayerIds
players :: Start -> [PlayerId]
players s = [0..(length s - 1)]

-- Given a GameState, prepends an action to it
addAction :: GameState -> Action -> GameState
addAction s a =
    s { actions = a : actions s }

turns :: GameState -> Result [PlayerId]
turns st = do
    ls <- scanState updateTurn initialTurn st :: Result [(PlayerId, [PlayerId])]
    return $ map fst ls

-- Executes a function for each GameState obtained from a list of action
stateStream :: Monad m => (GameState -> m ()) -> GameState -> [Action] -> m ()
stateStream f initial = Monad.mapM_ f . List.scanl addAction initial

initialTurn :: Start -> Either Error (PlayerId, [PlayerId])
initialTurn strt = case players strt of [] -> Left "0 Players"; (x:xs) -> Right (x, List.cycle (x:xs))

updateTurn :: (PlayerId, [PlayerId]) -> Action -> Either Error (PlayerId, [PlayerId])
updateTurn (_,_:(x:xs)) _ = Right (x, x:xs)
updateTurn _ _            = Left "0 Players"

unwrap :: GameState -> [(Action, GameState)]
unwrap s = case actions s of
    []     -> []
    (x:xs) -> let prev = s { actions = xs } in (x, prev) : unwrap prev

foldState :: (a -> Action -> Either Error a) -> (Start -> Either Error a) -> GameState -> Result a
foldState update initial st =
  do
    s <- mapLeft Fatal $ initial $ start st
    foldrM fn s (unwrap st)
      where
        fn (action, ste) result = mapLeft (Rollback ste) (update result action)

foldStateWithTurn :: (a -> (Action, PlayerId) -> Either Error a) -> (Start -> Either Error a) -> GameState -> Result a
foldStateWithTurn update initial st =
  do
    ts <- turns st
    s <- mapLeft Fatal $ initial $ start st
    foldrM fn s (zip (unwrap st) ts)
      where
        fn ((action, ste),turn) result = mapLeft (Rollback ste) (update result (action, turn))

scanState :: (a -> Action -> Either Error a) -> (Start -> Either Error a) -> GameState -> Result [a]
scanState update initial st =
  scanrTailM fn (mapLeft Fatal $ initial $ start st) (unwrap st)
    where
      fn result (action, s) = mapLeft (Rollback s) (update result action)

printStatesWithTurns :: GameState -> IO ()
printStatesWithTurns st = case foldStateWithTurn (\y x -> Right $ y >> print x) (const $ Right $ return ()) st of
  Right io -> io
  Left err -> putStrLn (GameLogic.error err)


playersState :: GameState -> Result (PlayerId, PlayersEnergies, PlayersPos, Int)
playersState gs = do
  (ts, ps) <- foldStateWithTurn (updateEnergies gs =>> updatePositions gs) (initialEnergies >>| initialPositions) gs
  tm <- foldStateWithTurn (updateTwoMove gs) initialTwoMove gs
  turn <- case turns gs of
        Right t -> maybeToEither (headMay t) (Fatal "Failed to derive turns")
        Left eh -> Left eh
  return ((turn+1) `mod` (length . start $ gs), ts, ps, tm)

initialPositions :: PlayersEnergies -> Start -> Either Error PlayersPos
initialPositions _ = Right

updatePositions :: GameState -> PlayersEnergies -> PlayersPos -> (Action, PlayerId) -> Either Error PlayersPos
updatePositions gs t v (a, pid) = applyAction (updatePositions' gs pid t) v a

updatePositions' :: GameState -> PlayerId -> PlayersEnergies -> PlayersPos -> Move -> Either Error PlayersPos
updatePositions' gs pid _ v (Move e vtx) = do
              pos <- maybeToEither (v !? pid) "Player not found or Player has no position"
              unless (vtx `List.elem` adjacentWithEnergy (network gs) pos [e]) $ Left "Player moved incorrectly"
              unless notOccupied $ Left "Player collided with another one"
              Right $ v // [(pid, vtx)]
                where notOccupied = (vtx `notElem` v) || ((pid /= roguePid) && (vtx `notElem` (tailSafe . toList) v))
updatePositions' gs pid t v Pass         =  do
              pos <- maybeToEither (v !? pid) "Player not found or Player has no position"
              energy <- maybeToEither (t !? pid) "Player not found or Player has no energy"
              let colors = map fst $ Map.toList $ Map.filter (>0) energy
              let valid = allOccupied $ adjacentWithEnergy (network gs) pos colors
              if valid then Right v else Left "Player did not move"
                where allOccupied = all (\avail -> any (\other -> avail == other) v)

-- energies (GameState (fromList $ map (Vertex . V) [1,2,3]) [OneMove $ Move (Energy Blue) (Vertex $ V 2), OneMove $ Move (Energy Orange) (Vertex $ V 9)])
--energies :: GameState -> Result PlayersEnergies
--energies gs = foldStateWithTurn (updateEnergies gs) initialEnergies gs

initialEnergies :: Start -> Either Error PlayersEnergies
initialEnergies ls = Right $ fromList $ take (length ls) $ repeat initialEnergyPerPlayer

initialEnergyPerPlayer :: Map.Map Energy Int
initialEnergyPerPlayer = Map.fromList [(Energy Orange, 15), (Energy Blue, 8), (Energy Red, 3)]

updateEnergies :: GameState -> PlayersEnergies -> (Action, PlayerId) -> Either Error PlayersEnergies
updateEnergies gs v (a, pid) = applyAction (updateEnergies' gs pid) v a

updateEnergies' :: GameState -> PlayerId -> PlayersEnergies -> Move -> Either Error PlayersEnergies
updateEnergies' _ pid v (Move t _) = do
  old <- maybeToEither (v !? pid) "Player not found or Player has no energy"
  let ts = findWithDefault 0 t old
  new <- if ts - 1 >= 0 then return $ Map.insert t (ts - 1) old else Left "No more energies"
  ls <- if roguePid == pid then return [] else do
    oldm <- maybeToEither (v !? roguePid) "Error updating corrupted core"
    let oldv = 1 + findWithDefault 0 t oldm
    return [(roguePid, Map.insert t oldv oldm)]
  return $ v // ((pid, new):ls)
updateEnergies' _ _ v Pass = Right v

initialTwoMove :: Start -> Either Error Int
initialTwoMove _ = Right 2

updateTwoMove :: GameState -> Int -> (Action, PlayerId) -> Either Error Int
updateTwoMove _ m (TwoMoves a b, pid) = do
  when (a == Pass || b == Pass) $ Left "Can not pass a move within a two-moves"
  unless (pid == roguePid) $ Left "Player can not move with two-moves"
  unless (m > 0) $ Left "The corrupted core already used all its two-moves "
  return $ m - 1
updateTwoMove _ m (OneMove _, _) = Right m

example :: Map.Map Energy Int
example = Map.insert (Energy Orange) 5 Map.empty

example2 :: Map.Map Energy Int
example2 = Map.insert (Energy Blue) 5 Map.empty

(=>>) :: (Monad m) => (a -> b -> m a) -> (a -> c -> b -> m c) -> (a, c) -> b -> m (a, c)
(=>>) g h (a, c) b =
  do
    newA <- g a b
    newC <- h newA c b
    return (newA, newC)

(>>|) :: (Monad m) => (b -> m a) -> (a -> b -> m c) -> b -> m (a, c)
(>>|) g h b =
  do
    newA <- g b
    newC <- h newA b
    return (newA, newC)
