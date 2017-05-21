{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Transport where

import           ClassyPrelude
import           Control.Monad
import           Data.Foldable (foldrM)
import           Data.List     (cycle)
import           Data.Vector   ((//))
import           Lib           (scanrTailM, mapLeft)

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

type Error = Text
data ErrorHandler = Rollback { state :: GameState, error :: Error } | Fatal { error :: Error }
type Result a = Either ErrorHandler a

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

turns :: GameState -> Result [PlayerId]
turns state =
  do
    ls <- scanState updateTurn initialTurn state :: Result [(PlayerId, [PlayerId])]
    return $ map fst ls

initialTurn :: Start -> Either Error (PlayerId, [PlayerId])
initialTurn start = case players start of [] -> Left "0 Players"; (x:xs) -> Right (x, cycle (x:xs))

updateTurn :: (PlayerId, [PlayerId]) -> Action -> Either Error (PlayerId, [PlayerId])
updateTurn (_,_:(x:xs)) _ = Right (x, x:xs)
updateTurn _ _ = Left "0 Players"

unwrap :: GameState -> [(Action, GameState)]
unwrap (GameState _ []) = []
unwrap s@(GameState _ (x:xs)) = (x, s) : unwrap (s { actions = xs })

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

updatePositions :: PlayersPos -> (Action, PlayerId) -> Either Error PlayersPos
updatePositions v (a, pid) = applyAction (updatePositions' pid) v a

updatePositions' :: PlayerId -> PlayersPos -> Move -> Either Error PlayersPos
updatePositions' pid v (Move _ vtx) = Right $ v // [(pid, vtx)]
updatePositions' pid v Pass         = Right v
