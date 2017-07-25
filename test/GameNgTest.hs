{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GameNgTest where

import           ClassyPrelude
import           Config.GameConfig
import           GameNg
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base

-- (@?=) = assertEqual
test_defaultInitialStateHasStartingPlayer0 :: IO ()
test_defaultInitialStateHasStartingPlayer0 = Player 0 @?= stateNextPlayer defaultInitialState

test_defaultInitialStateHasEmptyHistory :: IO ()
test_defaultInitialStateHasEmptyHistory =
    RogueHistory [] @?= stateRogueHistory defaultInitialState

test_player0ValidMove_playerPositionUpdated :: IO ()
test_player0ValidMove_playerPositionUpdated =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left (GameError err) -> assertFailure . unpack $ "action failed: " ++ err
        Right newState ->
            Just (Node 6) @?= (lookup (Player 0) . statePlayerPositions $ newState)

test_player0ValidMove_energyDrained :: IO ()
test_player0ValidMove_energyDrained =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left (GameError err) ->
            assertFailure . unpack $ "action failed: " ++ err
        Right newState ->
            Just 1 @?= remainingEnergy newState
    where
        remainingEnergy state = do
            eMap <- lookup (Player 0) . statePlayerEnergies $ state
            lookup Red eMap


test_player0ValidMove_historyUpdated :: IO ()
test_player0ValidMove_historyUpdated =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left (GameError err) ->
            assertFailure . unpack $ "action failed: " ++ err
        Right newState ->
            RogueHistory [(Red, Nothing)] @?= stateRogueHistory newState

test_player0ValidMoveWithShow_historyUpdate :: IO ()
test_player0ValidMoveWithShow_historyUpdate =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left (GameError err) ->
            assertFailure . unpack $ "action failed: " ++ err
        Right newState ->
            RogueHistory [(Red, Nothing)] @?= stateRogueHistory newState

test_player1ValidMove_historyNotUpdated :: IO ()
test_player1ValidMove_historyNotUpdated =
    case utilGameMoves defaultInitialState [(6,Red), (3,Blue)] of
        Left (GameError err) ->
            assertFailure . unpack $ "action failed: " ++ err
        Right newState ->
            RogueHistory [(Red, Nothing)] @?= stateRogueHistory newState


test_notPlayer1Turn :: IO ()
test_notPlayer1Turn =
    case updateState (Move (Player 1) Orange (Node 5)) defaultInitialState of
        Left (GameError err) ->
            unless ("turn" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not be player 1's turn"

test_playerNotFoundInPositions :: IO ()
test_playerNotFoundInPositions =
    case updateState (Move (Player 0) Red (Node 5)) $
        defaultInitialState { statePlayerPositions = mempty } of
        Left (GameError err) ->
            unless ("not found" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not find player"

test_playerNotFoundInEnergies :: IO ()
test_playerNotFoundInEnergies =
    case updateState (Move (Player 0) Red (Node 5)) $
        defaultInitialState { statePlayerEnergies = mempty } of
        Left (GameError err) ->
            unless ("not found" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not find player"


test_energyNotFound :: IO ()
test_energyNotFound =
    case updateState (Move (Player 0) Red (Node 5)) $
        defaultInitialState { statePlayerEnergies = singletonMap (Player 0) mempty }of
        Left (GameError err) ->
            unless ("not found" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not find energy"

test_cannotMoveTo :: IO ()
test_cannotMoveTo =
    case updateState (Move (Player 0) Orange (Node 1)) defaultInitialState of
        Left (GameError err) ->
            unless ("node" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _              -> assertFailure "should not be allowed to move"

test_noEnergyLeft :: IO ()
test_noEnergyLeft =
    case updateState (Move (Player 0) Orange (Node 6)) noEnergyState of
        Left (GameError err)  ->
            unless ("not enough energy" `isInfixOf` err) $
            assertFailure $ "wrong error msg: " ++ unpack err
        Right _ -> assertFailure "should not allow move with no energy"
    where
        noEnergyState =
            defaultInitialState
                { statePlayerEnergies =
                    PlayerEnergies $
                        singletonMap
                            (Player 0)
                            (EnergyMap $
                                singletonMap Orange 0
                            )
                }
test_getViews_rogueViewEqualToFieldInGameState :: IO ()

test_getViews_rogueViewEqualToFieldInGameState = do
    let state = defaultInitialState
    let (rogueView, _) = getViews state
    roguePlayerPositions rogueView @?= statePlayerPositions state
    rogueEnergies rogueView @?= statePlayerEnergies state
    rogueOwnHistory rogueView @?= stateRogueHistory state
    rogueNextPlayer rogueView @?= stateNextPlayer state

test_getViews_catcherViewDoesNotContainRogue :: IO ()
test_getViews_catcherViewDoesNotContainRogue = do
    let (_, catcherView) = getViews defaultInitialState
    Nothing @?= (lookup (Player 0) . catcherPlayerPositions $ catcherView)

test_getViews_someFieldsEqualToGameState :: IO ()
test_getViews_someFieldsEqualToGameState = do
    let (_, catcherView) = getViews defaultInitialState
    catcherEnergies catcherView @?= statePlayerEnergies defaultInitialState
    catcherRogueHistory catcherView @?= stateRogueHistory defaultInitialState
    catcherNextPlayer catcherView @?= stateNextPlayer defaultInitialState



test_gameRound :: IO ()
test_gameRound =
    do
        let updated = utilGameMoves defaultInitialState
                   [ (11, Orange)
                   , (3, Blue)
                   , (5, Orange)
                   , (13, Orange)
                   ]
        state <- case updated of
            Left (GameError err) -> assertFailure $ unpack err
            Right newState       -> return newState

        Just (Node 11) @?= (lookup (Player 0) . playerPositions . statePlayerPositions $ state)
        Just (Node 3) @?= (lookup (Player 1) . playerPositions . statePlayerPositions $ state)
        Just (Node 5) @?= (lookup (Player 2) . playerPositions . statePlayerPositions $ state)
        Just (Node 13) @?= (lookup (Player 3) . playerPositions . statePlayerPositions $ state)
        Player 0 @?= stateNextPlayer state

-- Utility functions for shortening tests

utilGameMoves :: GameState -> [(Int, Transport)] -> Either GameError GameState
utilGameMoves =
    foldM
        (\s (n, e) -> updateState (Move (stateNextPlayer s) e (Node n)) s)

defaultInitialState :: GameState
defaultInitialState = initialState defaultConfig
