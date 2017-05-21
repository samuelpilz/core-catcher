{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies      #-}

module Transport where

import           ClassyPrelude
import           Control.Monad
import           Data.Foldable                     (foldrM)
import qualified Data.Graph.Inductive.Graph        as Graph
import qualified Data.Graph.Inductive.PatriciaTree as PTree
import qualified Data.List                         as List
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import           Data.Vector                       ((!?), (//))
import           Lib                               (mapLeft, mapRight, scanrTailM,
                                                    scanrTailM, maybeToEither)

type PlayerId = Int
type VertexId = Graph.Node
type PlayersPos = Vector VertexId
type PlayersTickets = Vector (Map.Map Energy Int)
type Start = PlayersPos

data Color = Blue | Orange | Red deriving (Show, Eq, Ord, Read, Enum)
data Energy = BlackEnergy | Energy Color deriving (Show, Eq, Ord, Read)

data GameState =
    GameState
        { start   :: Start
        , network :: Network
        , actions :: [Action]
        }
        deriving (Show)

newtype Network =
    Network (PTree.Gr Vertex Edge)
    deriving (Show)

adjacentWithEnergy :: Network -> VertexId -> [Energy] -> [VertexId]
adjacentWithEnergy (Network gr) v es =
    map snd $ filter (\(c,_) -> any  (`Set.member` reach c) es) $ Graph.lneighbors gr v

canMove :: Network -> VertexId -> Energy -> VertexId -> Bool
canMove net from ticket to =
    to `elem` adjacentWithEnergy net from [ticket]

someGraph :: Graph.DynGraph gr => gr Vertex Edge
someGraph =
    Graph.buildGr
        [([(mkEdge [Energy Blue],3)],
        1,
        Vertex,
        [(mkEdge [Energy Red],2)])]

data Move
    = Move Energy VertexId
    | Pass
    deriving (Show, Eq, Ord, Read)

data Action
    = OneMove Move
    | TwoMoves Move Move
    deriving (Show, Eq, Ord, Read)

type Error = Text
data ErrorHandler
  = Rollback { state :: GameState, error :: Error }
  | Fatal { error :: Error }
  deriving (Show)

type Result a = Either ErrorHandler a

applyAction :: Monad m => (a -> Move -> m a) -> a -> Action -> m a
applyAction f x (OneMove a)    = f x a
applyAction f x (TwoMoves a b) = do
    r <- f x a
    f r b

data Vertex = Vertex deriving (Show, Eq, Read, Ord)

newtype Edge =
    Edge
        { reach :: Set.Set Energy
        }
    deriving (Show)

mkEdge :: [Energy] -> Edge
mkEdge energies = Edge { reach = Set.fromList energies }

players :: Start -> [PlayerId]
players s = [0..(length s - 1)]

addAction :: Action -> GameState -> GameState
addAction a s =
    s { actions = a : actions s }

turns :: GameState -> Result [PlayerId]
turns st =
  do
    ls <- scanState updateTurn initialTurn st :: Result [(PlayerId, [PlayerId])]
    return $ map fst ls

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
  Left err -> putStrLn (Transport.error err)

playerState :: GameState -> Result (PlayersTickets, PlayersPos)
playerState gs = foldStateWithTurn (updateTickets gs =>> updatePositions gs) (initialTickets >>| initialPositions) gs

initialPositions :: PlayersTickets -> Start -> Either Error PlayersPos
initialPositions _ = Right

updatePositions :: GameState -> PlayersTickets -> PlayersPos -> (Action, PlayerId) -> Either Error PlayersPos
updatePositions gs t v (a, pid) = applyAction (updatePositions' gs pid t) v a

updatePositions' :: GameState -> PlayerId -> PlayersTickets -> PlayersPos -> Move -> Either Error PlayersPos
updatePositions' gs pid t v (Move e vtx) = do
              pos <- maybeToEither (v !? pid) "Player not found or Player has no position"
              let valid = vtx `List.elem` adjacentWithEnergy (network gs) pos [e]
              if valid then Right $ v // [(pid, vtx)] else Left "Player moved incorrectly"
updatePositions' gs pid t v Pass         =  do
              pos <- maybeToEither (v !? pid) "Player not found or Player has no position"
              energy <- maybeToEither (t !? pid) "Player not found or Player has no energy"
              let colors = map fst $ Map.toList $ Map.filter (> 0) energy
              let valid = null $ adjacentWithEnergy (network gs) pos colors
              if valid then Right v else Left "Player did not move"

-- tickets (GameState (fromList $ map (Vertex . V) [1,2,3]) [OneMove $ Move (Energy Blue) (Vertex $ V 2), OneMove $ Move (Energy Orange) (Vertex $ V 9)])
--tickets :: GameState -> Result PlayersTickets
--tickets gs = foldStateWithTurn (updateTickets gs) initialTickets gs

initialTickets :: Start -> Either Error PlayersTickets
initialTickets ls = Right $ fromList $ take (length ls) $ concat $ repeat [example, example2]

updateTickets :: GameState -> PlayersTickets -> (Action, PlayerId) -> Either Error PlayersTickets
updateTickets gs v (a, pid) = applyAction (updateTickets' gs pid) v a

corruptedPid :: GameState -> PlayerId
corruptedPid _ = 0

updateTickets' :: GameState -> PlayerId -> PlayersTickets -> Move -> Either Error PlayersTickets
updateTickets' gs pid v (Move t _) = do
  old <- maybeToEither (v !? pid) "Player not found or Player has no energy"
  let ts = findWithDefault 0 t old
  new <- if ts - 1 >= 0 then return $ Map.insert t (ts - 1) old else Left "No more tickets"
  ls <- if corruptedPid gs == pid then return [] else do
    oldm <- maybeToEither (v !? corruptedPid gs) "Error updating corrupted core"
    let oldv = 1 + findWithDefault 0 t oldm
    return [(corruptedPid gs, Map.insert t oldv oldm)]
  return $ v // ((pid, new):ls)
updateTickets' _ _ v Pass = Right v

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
