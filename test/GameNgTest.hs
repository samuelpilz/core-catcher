{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GameNgTest where

import           ClassyPrelude
import           Data.Map.Strict  (insert)
import           GameNg
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base
import Config.GameConfig

-- (@?=) = assertEqual
test_defaultInitialStateHasStartingPlayer0 :: IO ()
test_defaultInitialStateHasStartingPlayer0 = Player 0 @?= stateNextPlayer defaultInitialState

test_defaultInitialStateHasEmptyHistory :: IO ()
test_defaultInitialStateHasEmptyHistory =
    RogueHistory [] @?= stateRogueHistory defaultInitialState

-- TODO: split in more test cases such that one test-case asserts one thing
test_player0ValidMove :: IO ()
test_player0ValidMove =
    case updateState (Move (Player 0) (Transport "red") (Node 6)) defaultInitialState of
        Left (GameError err) -> assertFailure . unpack $ "action failed: " ++ err
        Right newState -> do
            (insert (Player 0) (Node 6) . playerPositions . statePlayerPositions $
             defaultInitialState) @?=
                (playerPositions . statePlayerPositions $ newState)
            -- test that energy has been used
            let energyLeft = remainingEnergy newState
            Just 1 @?= energyLeft

            -- test that history has been appended
            RogueHistory [(Transport "red", Just $ Node 6)] @?= stateRogueHistory newState
    where
        remainingEnergy state = do
            eMap <- lookup (Player 0) . statePlayerEnergies $ state
            lookup (Transport "red") eMap

test_player1ValidMove_historyNotUpdated :: IO ()
test_player1ValidMove_historyNotUpdated =
    case utilGameMoves defaultInitialState [(6, "red"), (3, "blue")] of
        Left (GameError err) -> assertFailure . unpack $ "action failed: " ++ err
        Right newState ->
            RogueHistory [(Transport "red", Just $ Node 6)] @?= stateRogueHistory newState


test_notPlayer1Turn :: IO ()
test_notPlayer1Turn =
    case updateState (Move (Player 1) (Transport "orange") (Node 5)) defaultInitialState of
        Left (GameError err) ->
            unless ("turn" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not be player 1's turn"

test_playerNotFoundInPositions :: IO ()
test_playerNotFoundInPositions =
    case updateState (Move (Player 0) (Transport "yellow") (Node 5)) $
        defaultInitialState { statePlayerPositions = mempty } of
        Left (GameError err) ->
            unless ("not found" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not find player"

test_playerNotFoundInEnergies :: IO ()
test_playerNotFoundInEnergies =
    case updateState (Move (Player 0) (Transport "yellow") (Node 5)) $
        defaultInitialState { statePlayerEnergies = mempty } of
        Left (GameError err) ->
            unless ("not found" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not find player"


test_energyNotFound :: IO ()
test_energyNotFound =
    case updateState (Move (Player 0) (Transport "green") (Node 5)) defaultInitialState of
        Left (GameError err) ->
            unless ("not found" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not find energy"

test_cannotMoveTo :: IO ()
test_cannotMoveTo =
    case updateState (Move (Player 0) (Transport "orange") (Node 1)) defaultInitialState of
        Left (GameError err) ->
            unless ("node" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not be allowed to move"

test_noEnergyLeft :: IO ()
test_noEnergyLeft =
    case updateState (Move (Player 0) (Transport "orange") (Node 6)) noEnergyState of
        Left (GameError err)  ->
            unless ("not enough energy" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _ -> assertFailure "should not allow move with no energy"
    where
        noEnergyState =
            defaultInitialState
                { statePlayerEnergies =
                    PlayerEnergies $
                        singletonMap (Player 0) (EnergyMap $ singletonMap (Transport "orange") 0)
                }

test_getViews_allViewsEqualToFieldInGameState :: IO ()
test_getViews_allViewsEqualToFieldInGameState = do
    let state = defaultInitialState
    let (rogueView, catcherView) = getViews state
    roguePlayerPositions rogueView @?= statePlayerPositions state
    rogueEnergies rogueView @?= statePlayerEnergies state
    rogueOwnHistory rogueView @?= stateRogueHistory state
    rogueNextPlayer rogueView @?= stateNextPlayer state
    catcherPlayerPositions catcherView @?= statePlayerPositions state
    catcherEnergies catcherView @?= statePlayerEnergies state
    catcherRogueHistory catcherView @?= stateRogueHistory state
    catcherNextPlayer catcherView @?= stateNextPlayer state



test_gameRound :: IO ()
test_gameRound =
    do
        let updated = utilGameMoves defaultInitialState
                   [ (11, "orange")
                   , (3, "blue")
                   , (5, "orange")
                   , (13, "orange")
                   ]
        state <- case updated of
            Left (GameError err) -> assertFailure $ unpack err
            Right newState -> return newState

        Just (Node 11) @?= (lookup (Player 0) . playerPositions . statePlayerPositions $ state)
        Just (Node 3) @?= (lookup (Player 1) . playerPositions . statePlayerPositions $ state)
        Just (Node 5) @?= (lookup (Player 2) . playerPositions . statePlayerPositions $ state)
        Just (Node 13) @?= (lookup (Player 3) . playerPositions . statePlayerPositions $ state)
        Player 0 @?= stateNextPlayer state

-- Utility functions for shortening tests

utilGameMoves :: GameState -> [(Int, Text)] -> Either GameError GameState
utilGameMoves =
    foldM
        (\s (n, e) -> updateState (Move (stateNextPlayer s) (Transport e) (Node n)) s)

defaultInitialState :: GameState
defaultInitialState = initialState defaultConfig
