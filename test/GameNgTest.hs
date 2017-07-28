{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GameNgTest where

import           ClassyPrelude
import           Config.GameConfig
import           Data.Easy
import           GameNg
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base

-- (@?=) = assertEqual
test_defaultInitialStateHasStartingPlayer0 :: IO ()
test_defaultInitialStateHasStartingPlayer0 = Player 0 @?= gameRunningNextPlayer defaultInitialState

test_defaultInitialStateHasEmptyHistory :: IO ()
test_defaultInitialStateHasEmptyHistory =
    RogueHistory [] @?= gameRunningRogueHistory defaultInitialState

test_player0ValidMove_playerPositionUpdated :: IO ()
test_player0ValidMove_playerPositionUpdated =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left err -> assertFailure $ "action failed: " ++ show err
        Right newState ->
            Just (Node 6) @?= (lookup (Player 0) . gameRunningPlayerPositions $ newState)

test_player0ValidMove_energyDrained :: IO ()
test_player0ValidMove_energyDrained =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left err ->
            assertFailure $ "action failed: " ++ show err
        Right newState ->
            Just 1 @?= remainingEnergy newState
    where
        remainingEnergy state = do
            eMap <- lookup (Player 0) . gameRunningPlayerEnergies $ state
            lookup Red eMap


test_player0ValidMove_historyUpdated :: IO ()
test_player0ValidMove_historyUpdated =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left err ->
            assertFailure $ "action failed: " ++ show err
        Right newState ->
            RogueHistory [(Red, Nothing)] @?= gameRunningRogueHistory newState

test_player0ValidMoveWithShow_historyUpdate :: IO ()
test_player0ValidMoveWithShow_historyUpdate =
    case utilGameMoves defaultInitialState [(6,Red)] of
        Left err ->
            assertFailure $ "action failed: " ++ show err
        Right newState ->
            RogueHistory [(Red, Nothing)] @?= gameRunningRogueHistory newState

test_player1ValidMove_historyNotUpdated :: IO ()
test_player1ValidMove_historyNotUpdated =
    case utilGameMoves defaultInitialState [(6,Red), (3,Blue)] of
        Left err ->
            assertFailure $ "action failed: " ++ show err
        Right newState ->
            RogueHistory [(Red, Nothing)] @?= gameRunningRogueHistory newState


test_notPlayer1Turn :: IO ()
test_notPlayer1Turn =
    case updateState (Move (Player 1) Orange (Node 5)) defaultInitialState of
        Left err ->
            NotTurn @?= err
        Right _              -> assertFailure "should not be player 1's turn"

test_playerNotFoundInPositions :: IO ()
test_playerNotFoundInPositions =
    case updateState (Move (Player 0) Red (Node 5)) $
        defaultInitialState { gameRunningPlayerPositions = mempty } of
        Left err ->
            PlayerNotFound @?= err
        Right _              -> assertFailure "should not find player"

test_playerNotFoundInEnergies :: IO ()
test_playerNotFoundInEnergies =
    case updateState (Move (Player 0) Red (Node 5)) $
        defaultInitialState { gameRunningPlayerEnergies = mempty } of
        Left err ->
            PlayerNotFound @?= err
        Right _              -> assertFailure "should not find player"


test_energyNotFound :: IO ()
test_energyNotFound =
    case updateState (Move (Player 0) Red (Node 5)) $
        defaultInitialState { gameRunningPlayerEnergies = singletonMap (Player 0) mempty }of
        Left err ->
            EnergyNotFound @?= err
        Right _              -> assertFailure "should not find energy"

test_cannotMoveTo :: IO ()
test_cannotMoveTo =
    case updateState (Move (Player 0) Orange (Node 1)) defaultInitialState of
        Left err ->
            NotReachable @?= err
        Right _              -> assertFailure "should not be allowed to move"

test_noEnergyLeft :: IO ()
test_noEnergyLeft =
    case updateState (Move (Player 0) Orange (Node 6)) noEnergyState of
        Left err  ->
            NotEnoughEnergy @?= err
        Right _ -> assertFailure "should not allow move with no energy"
    where
        noEnergyState =
            defaultInitialState
                { gameRunningPlayerEnergies =
                    PlayerEnergies $
                        singletonMap (Player 0) (EnergyMap $ singletonMap Orange 0 )
                }
test_getViews_rogueViewEqualToFieldInGameState :: IO ()

test_getViews_rogueViewEqualToFieldInGameState = do
    let state = defaultInitialState
    let (rogueView, _) = getViews state
    roguePlayerPositions rogueView @?= gameRunningPlayerPositions state
    rogueEnergies rogueView @?= gameRunningPlayerEnergies state
    rogueOwnHistory rogueView @?= gameRunningRogueHistory state
    rogueNextPlayer rogueView @?= gameRunningNextPlayer state

test_getViews_catcherViewDoesNotContainRogue :: IO ()
test_getViews_catcherViewDoesNotContainRogue = do
    let (_, catcherView) = getViews defaultInitialState
    Nothing @?= (lookup (Player 0) . catcherPlayerPositions $ catcherView)

test_getViews_someFieldsEqualToGameState :: IO ()
test_getViews_someFieldsEqualToGameState = do
    let (_, catcherView) = getViews defaultInitialState
    catcherEnergies catcherView @?= gameRunningPlayerEnergies defaultInitialState
    catcherRogueHistory catcherView @?= gameRunningRogueHistory defaultInitialState
    catcherNextPlayer catcherView @?= gameRunningNextPlayer defaultInitialState



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
            Left err       -> assertFailure $ "action failed: " ++ show err
            Right newState -> return newState

        Just (Node 11) @?= (lookup (Player 0) . playerPositions . gameRunningPlayerPositions $ state)
        Just (Node 3) @?= (lookup (Player 1) . playerPositions . gameRunningPlayerPositions $ state)
        Just (Node 5) @?= (lookup (Player 2) . playerPositions . gameRunningPlayerPositions $ state)
        Just (Node 13) @?= (lookup (Player 3) . playerPositions . gameRunningPlayerPositions $ state)
        Player 0 @?= gameRunningNextPlayer state

-- Utility functions for shortening tests

utilGameMoves :: GameRunning -> [(Int, Energy)] -> Either GameError GameRunning
utilGameMoves =
    foldM
        (\s (n, e) ->
            join .
            mapRight (mapLeft . const $ GameIsOver) .
            updateState (Move (gameRunningNextPlayer s) e (Node n)) $
            s
        )


defaultInitialState :: GameRunning
defaultInitialState = initialState defaultConfig
