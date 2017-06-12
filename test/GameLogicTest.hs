{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GameLogicTest where

import           ClassyPrelude
import           GameLogic hiding (Result)
import qualified GameLogic (Result)
import           Test.Framework

exampleInvalidMove0 :: GameLogic.Result (PlayerId, PlayersEnergies, PlayersPos, Int, RogueHistory)
exampleInvalidMove0 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Orange) 1, OneMove $ Move (Energy Blue) 1, OneMove $ Move (Energy Orange) 3]

exampleInvalidMove1 :: GameLogic.Result (PlayerId, PlayersEnergies, PlayersPos, Int, RogueHistory)
exampleInvalidMove1 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Orange) 1, OneMove $ Move BlackEnergy 3, OneMove $ Move (Energy Orange) 3]

exampleInvalidMove2 :: GameLogic.Result (PlayerId, PlayersEnergies, PlayersPos, Int, RogueHistory)
exampleInvalidMove2 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Orange) 1, OneMove $ Move (Energy Blue) 1, OneMove $ Move (Energy Orange) 2]

exampleValidMove0 :: GameLogic.Result (PlayerId, PlayersEnergies, PlayersPos, Int, RogueHistory)
exampleValidMove0 = playersState $ (\x -> GameState {start = fromList [1,2,3], network = someNet, actions = x} )
  [OneMove $ Move (Energy Blue) 5, OneMove $ Move (Energy Orange) 6]

test_valid0 :: IO ()
test_valid0 = case exampleValidMove0 of (Right _) -> return (); _ -> assertFailure "exampleValidMove0 not valid"

test_invalid0 :: IO ()
test_invalid0 = case exampleInvalidMove0 of (Right _) -> assertFailure "exampleInvalidMove0 not invalid"; _ -> return ()

test_invalid1 :: IO ()
test_invalid1 = case exampleInvalidMove1 of (Right _) -> assertFailure "exampleInvalidMove1 not invalid"; _ -> return ()

test_invalid2 :: IO ()
test_invalid2 = case exampleInvalidMove2 of (Right _) -> assertFailure "exampleInvalidMove2 not invalid"; _ -> return ()

