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
import qualified Data.Set                          as Set
import           Data.Vector                       ((//), (!?))
import           Lib                               (scanrTailM, mapLeft, scanrTailM)
import qualified Data.Map as Map

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
        , actions :: [Action]
        }
        deriving (Show)

newtype Network = Network (PTree.Gr Vertex Edge)

adjacentWithEnergy :: Network -> VertexId -> Energy -> [VertexId]
adjacentWithEnergy (Network gr) v e =
    map snd $ filter (\(c,_) -> Set.member e (reach c)) $ Graph.lneighbors gr v

canMove :: Network -> VertexId -> Energy -> VertexId -> Bool
canMove network from ticket to =
    to `elem` adjacentWithEnergy network from ticket

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

mkEdge :: [Energy] -> Edge
mkEdge energies = Edge { reach = Set.fromList energies }

players :: Start -> [PlayerId]
players s = [0..(length s - 1)]

addAction :: Action -> GameState -> GameState
addAction a s =
    s { actions = a : actions s }

turns :: GameState -> Result [PlayerId]
turns state =
  do
    ls <- scanState updateTurn initialTurn state :: Result [(PlayerId, [PlayerId])]
    return $ map fst ls

initialTurn :: Start -> Either Error (PlayerId, [PlayerId])
initialTurn start = case players start of [] -> Left "0 Players"; (x:xs) -> Right (x, List.cycle (x:xs))

updateTurn :: (PlayerId, [PlayerId]) -> Action -> Either Error (PlayerId, [PlayerId])
updateTurn (_,_:(x:xs)) _ = Right (x, x:xs)
updateTurn _ _            = Left "0 Players"

unwrap :: GameState -> [(Action, GameState)]
unwrap (GameState _ []) = []
unwrap s@(GameState _ (x:xs)) = let next = s { actions = xs } in (x, next) : unwrap next

foldState :: (a -> Action -> Either Error a) -> (Start -> Either Error a) -> GameState -> Result a
foldState update initial st@(GameState start _) =
  do
    s <- mapLeft Fatal $ initial start
    foldrM fn s (unwrap st)
      where
        fn (action,state) result = mapLeft (Rollback state) (update result action)

foldStateWithTurn :: (a -> (Action, PlayerId) -> Either Error a) -> (Start -> Either Error a) -> GameState -> Result a
foldStateWithTurn update initial st@(GameState start _) =
  do
    ts <- turns st
    s <- mapLeft Fatal $ initial start
    foldrM fn s (zip (unwrap st) ts)
      where
        fn ((action,state),turn) result = mapLeft (Rollback state) (update result (action, turn))

scanState :: (a -> Action -> Either Error a) -> (Start -> Either Error a) -> GameState -> Result [a]
scanState update initial st@(GameState start actions) =
  scanrTailM fn (mapLeft Fatal $ initial start) (unwrap st)
    where
      fn result (action, state) = mapLeft (Rollback state) (update result action)

printStatesWithTurns :: GameState -> IO ()
printStatesWithTurns state = case foldStateWithTurn (\y x -> Right $ y >> print x) (const $ Right $ return ()) state of
  Right io -> io
  Left err -> putStrLn (Transport.error err)

positions :: GameState -> Result PlayersPos
positions = foldStateWithTurn updatePositions initialPositions

initialPositions :: Start -> Either Error PlayersPos
initialPositions = Right

updatePositions :: PlayersPos -> (Action, PlayerId) -> Either Error PlayersPos
updatePositions v (a, pid) = applyAction (updatePositions' pid) v a

updatePositions' :: PlayerId -> PlayersPos -> Move -> Either Error PlayersPos
updatePositions' pid v (Move _ vtx) = Right $ v // [(pid, vtx)]
updatePositions' pid v Pass         = Right v

-- tickets (GameState (fromList $ map (Vertex . V) [1,2,3]) [OneMove $ Move (Energy Blue) (Vertex $ V 2), OneMove $ Move (Energy Orange) (Vertex $ V 9)])
tickets :: GameState -> Result PlayersTickets
tickets gs = foldStateWithTurn (updateTickets gs) initialTickets gs

initialTickets :: Start -> Either Error PlayersTickets
initialTickets ls = Right $ fromList $ take (length ls) $ concat $ repeat [example, example2]

updateTickets :: GameState -> PlayersTickets -> (Action, PlayerId) -> Either Error PlayersTickets
updateTickets gs v (a, pid) = applyAction (updateTickets' gs pid) v a

corruptedPid :: GameState -> PlayerId
corruptedPid _ = 0

updateTickets' :: GameState -> PlayerId -> PlayersTickets -> Move -> Either Error PlayersTickets
updateTickets' gs pid v (Move t _) =
  let
    subTicket old = let ts = findWithDefault 0 t old in (if (ts - 1) >= 0 then Just else const Nothing) $ Map.insert t (ts - 1) old
    addTicket old = Map.insert t (1 + findWithDefault 0 t old) old
    in
      case v !? pid of
        Nothing -> Left "Player not found or Player has no tickets"
        Just old -> case subTicket old of
          Nothing -> Left "No more tickets"
          Just new -> case if corruptedPid gs == pid then return [] else do {
            old <- v !? corruptedPid gs;
            return [(corruptedPid gs, addTicket old)]
          } of
            Nothing -> Left "Error updating corrupted core"
            Just ls -> Right $ v // ((pid, new):ls)
updateTickets' _ _ v Pass = Right v

example :: Map.Map Energy Int
example = Map.insert (Energy Orange) 5 Map.empty

example2 :: Map.Map Energy Int
example2 = Map.insert (Energy Blue) 5 Map.empty
