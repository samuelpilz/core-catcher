{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- |Module for testing the gameNg module.

The main function to execute test cases is the gameNgTestCase function defined in this module.

-}
module GameNgTest where

import           ClassyPrelude
import           Config.GameConfig
import           GameNg
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base

-- (@?=) = assertEqual
test_defaultInitialStateHasStartingPlayer0 :: IO ()
test_defaultInitialStateHasStartingPlayer0 =
    Player 0 @?= (gameRunningNextPlayer . initialState $ defaultConfig)

test_defaultInitialStateHasEmptyHistory :: IO ()
test_defaultInitialStateHasEmptyHistory = do
    RogueHistory [] @?= (gameRunningRogueHistory . initialState $ defaultConfig)
    OpenRogueHistory [] @?= (gameRunningOpenRogueHistory . initialState $ defaultConfig)

test_player0ValidMove_playerPositionUpdated :: IO ()
test_player0ValidMove_playerPositionUpdated =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Move (Player 0) Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            Just (Node 6) @?= lookup (Player 0) gameRunningPlayerPositions

test_player0ValidMove_energyDrained :: IO ()
test_player0ValidMove_energyDrained =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Move (Player 0) Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ game)) =
            Just 1 @?= remainingEnergy game

        remainingEnergy GameRunning {gameRunningPlayerEnergies} = do -- maybe monad
            eMap <- lookup (Player 0) gameRunningPlayerEnergies
            lookup Red eMap


test_player0ValidMove_historyUpdated :: IO ()
test_player0ValidMove_historyUpdated =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Move (Player 0) Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ game)) = do
            RogueHistory [(Red, Nothing)] @?= gameRunningRogueHistory game
            OpenRogueHistory [(Red, Node 6, False)] @?= gameRunningOpenRogueHistory game

test_player0ValidMove_historyUpdatedWithShow :: IO ()
test_player0ValidMove_historyUpdatedWithShow =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig { rogueShowsAt = [0] }
        moves =
            [ Move (Player 0) Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ game)) = do
            RogueHistory [(Red, Just $ Node 6)] @?= gameRunningRogueHistory game
            OpenRogueHistory [(Red, Node 6, True)] @?= gameRunningOpenRogueHistory game


test_player0ValidMove_historyUpdate :: IO ()
test_player0ValidMove_historyUpdate =
    gameNgTestCase
            defaultConfig
            moves
            assertions
    where
        moves =
            [ Move (Player 0) Red (Node 6) ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ game)) = do
            RogueHistory [(Red, Nothing)] @?= gameRunningRogueHistory game
            OpenRogueHistory [(Red, Node 6, False)] @?= gameRunningOpenRogueHistory game

test_player1ValidMove_historyNotUpdated :: IO ()
test_player1ValidMove_historyNotUpdated =
    gameNgTestCase
            defaultConfig
            moves
            assertions
    where
        moves =
            [ Move (Player 0) Red (Node 6)
            , Move (Player 1) Blue (Node 3)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ game)) = do
            RogueHistory [(Red, Nothing)] @?= gameRunningRogueHistory game
            OpenRogueHistory [(Red, Node 6, False)] @?= gameRunningOpenRogueHistory game


test_notPlayer1Turn :: IO ()
test_notPlayer1Turn =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves = [ Move (Player 1) Orange (Node 5) ]
        assertions (Left err) = NotTurn @?= err
        assertions (Right _) = assertFailure "should not be player 1's turn"

-- game over tests

test_rogueCaught_gameOverWinningPlayer1 :: IO ()
test_rogueCaught_gameOverWinningPlayer1 =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerPositions =
                insertMap (Player 1) (Node 3) $ initialPlayerPositions defaultConfig
            }
        moves =
            [ Move (Player 0) Red (Node 6)
            , Move (Player 1) Orange (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            Player 1 @?= gameOverWinningPlayer
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            assertFailure $ "game should be over, was: " ++ show gameRunningPlayerPositions

test_rogueWins_gameOverWinningPlayer0 :: IO ()
test_rogueWins_gameOverWinningPlayer0 =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig { maxRounds = 0 }
        moves = [ Move (Player 0) Red (Node 6) ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            Player 0 @?= gameOverWinningPlayer
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            assertFailure $ "game should be over, was: " ++ show gameRunningPlayerPositions

test_getGameOverView_sameConfigAndSamePositionsAndEnergies :: IO ()
test_getGameOverView_sameConfigAndSamePositionsAndEnergies =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerPositions =
                insertMap (Player 1) (Node 3) $ initialPlayerPositions defaultConfig
            }
        moves =
            [ Move (Player 0) Red (Node 6)
            , Move (Player 1) Orange (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver
            {gameOverGameConfig, gameOverPlayerPositions, gameOverPlayerEnergies})) = do
            Just (Node 6) @?= lookup (Player 0) gameOverPlayerPositions
            Just (Node 6) @?= lookup (Player 1) gameOverPlayerPositions
            Just 1 @?= (join . map (lookup Red) . lookup (Player 0) $ gameOverPlayerEnergies)
            config @?= gameOverGameConfig
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            assertFailure $ "game should be over, was: " ++ show gameRunningPlayerPositions

test_getGameOverView_openRogueHistory :: IO ()
test_getGameOverView_openRogueHistory =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerPositions =
                insertMap (Player 1) (Node 3) $ initialPlayerPositions defaultConfig
            }
        moves =
            [ Move (Player 0) Red (Node 6)
            , Move (Player 1) Orange (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverRogueHistory})) =
            OpenRogueHistory [ (Red, Node 6, False) ] @?= gameOverRogueHistory
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            assertFailure $ "game should be over, was: " ++ show gameRunningPlayerPositions

-- Test correct gameErrors

test_playerNotFoundInPositions :: IO ()
test_playerNotFoundInPositions =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerPositions = mempty }
        moves = [ Move (Player 0) Red (Node 6) ]
        assertions (Left err) =
            PlayerNotFound (Player 0) @?= err
        assertions (Right _) =
            assertFailure "should not find player"

test_playerNotFoundInEnergies :: IO ()
test_playerNotFoundInEnergies =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerEnergies = mempty }
        moves = [ Move (Player 0) Red (Node 6) ]
        assertions (Left err) =
            PlayerNotFound (Player 0) @?= err
        assertions (Right _) =
            assertFailure "should not find player"


test_energyNotFound :: IO ()
test_energyNotFound =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerEnergies = singletonMap (Player 0) mempty }
        moves = [ Move (Player 0) Red (Node 6) ]
        assertions (Left err) =
            EnergyNotFound Red @?= err
        assertions (Right _) =
            assertFailure "should not find energy"

test_cannotMoveTo :: IO ()
test_cannotMoveTo =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves = [ Move (Player 0) Orange (Node 1) ]
        assertions (Left err) =
            NotReachable @?= err
        assertions (Right _) =
            assertFailure "should not be allowed to move"

test_noEnergyLeft :: IO ()
test_noEnergyLeft =
    gameNgTestCase
        config
        moves
        assertions
    where
        config =
            defaultConfig
                { initialPlayerEnergies =
                    updateMap (Just . insertMap Red 0) (Player 0) .
                    initialPlayerEnergies $ defaultConfig
                }
        moves =
            [ Move (Player 0) Red (Node 6) ]
        assertions (Left err) =
            NotEnoughEnergy @?= err
        assertions (Right _) =
            assertFailure "should not allow move with no energy"

test_nodeBlocked :: IO ()
test_nodeBlocked =
    gameNgTestCase
        config
        moves
        assertions
    where
        config =
            defaultConfig
                { initialPlayerPositions =
                    insertMap (Player 3) (Node 15) . initialPlayerPositions $ defaultConfig
                , firstPlayer = Player 1
                }
        moves =
            [ Move (Player 1) Blue (Node 15) ]
        assertions (Left err) =
            NodeBlocked (Player 3) @?= err
        assertions (Right _) =
            assertFailure "should not allow move to blocked node"

test_nodeBlockedForRogue :: IO ()
test_nodeBlockedForRogue =
    gameNgTestCase
        config
        moves
        assertions
    where
        config =
            defaultConfig
                { initialPlayerPositions =
                    insertMap (Player 1) (Node 6) . initialPlayerPositions $ defaultConfig
                }
        moves =
            [ Move (Player 0) Red (Node 6) ]
        assertions (Left err) =
            NodeBlocked (Player 1) @?= err
        assertions (Right _) =
            assertFailure "should not allow move to blocked node"

-- Test getViews

test_getViews_rogueViewEqualToFieldInGameState :: IO ()
test_getViews_rogueViewEqualToFieldInGameState = do
    let game = initialState defaultConfig
    let (rogueView, _) = getViews game
    roguePlayerPositions rogueView @?= gameRunningPlayerPositions game
    rogueEnergies rogueView @?= gameRunningPlayerEnergies game
    rogueOwnHistory rogueView @?= gameRunningRogueHistory game
    rogueNextPlayer rogueView @?= gameRunningNextPlayer game


test_getViews_catcherViewDoesNotContainRogue :: IO ()
test_getViews_catcherViewDoesNotContainRogue = do
    let (_, catcherView) = getViews $ initialState defaultConfig
    Nothing @?= (lookup (Player 0) . catcherPlayerPositions $ catcherView)


test_getViews_someFieldsEqualToGameState :: IO ()
test_getViews_someFieldsEqualToGameState = do
    let game = initialState defaultConfig
    let (_, catcherView) = getViews game
    catcherEnergies catcherView @?= gameRunningPlayerEnergies game
    catcherRogueHistory catcherView @?= gameRunningRogueHistory game
    catcherNextPlayer catcherView @?= gameRunningNextPlayer game


test_getViews_rogueHiddenInCatcherView :: IO ()
test_getViews_rogueHiddenInCatcherView = do
    let (_, catcherView) = getViews $ initialState defaultConfig
    Nothing @?= (lookup (Player 0) . catcherPlayerPositions $ catcherView)


test_getViews_rogueShownAtCurrentPositionInCatcherView :: IO ()
test_getViews_rogueShownAtCurrentPositionInCatcherView =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig { rogueShowsAt = [0] }
        moves =
            [ Move (Player 0) Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ game)) = do
            let (_, catcherView) = getViews game
            Just (Node 6) @?= (lookup (Player 0) . catcherPlayerPositions $ catcherView)

test_getViews_rogueShownAtLastPositionInCatcherView :: IO ()
test_getViews_rogueShownAtLastPositionInCatcherView =
    gameNgTestCase
        config
        moves
        assertions
   where
        config = defaultConfig { rogueShowsAt = [0] }
        moves =
            [ Move (Player 0) Red (Node 6)
            , Move (Player 1) Blue (Node 3)
            , Move (Player 2) Orange (Node 5)
            , Move (Player 3) Orange (Node 13)
            , Move (Player 0) Orange (Node 11)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameRunning_ game)) = do
            let (_, catcherView) = getViews game
            Just (Node 6) @?= (lookup (Player 0) . catcherPlayerPositions $ catcherView)

-- TODO: move above views
test_gameRound :: IO ()
test_gameRound =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Move (Player 0) Orange (Node 11)
            , Move (Player 1) Blue (Node 3)
            , Move (Player 2) Orange (Node 5)
            , Move (Player 3) Orange (Node 13)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right
            (GameRunning_ GameRunning
                { gameRunningPlayerPositions
                , gameRunningNextPlayer
                }))
            = do
            Just (Node 11) @?=
                (lookup (Player 0) . playerPositions $ gameRunningPlayerPositions)
            Just (Node 3) @?=
                (lookup (Player 1) . playerPositions $ gameRunningPlayerPositions)
            Just (Node 5) @?=
                (lookup (Player 2) . playerPositions $ gameRunningPlayerPositions)
            Just (Node 13) @?=
                (lookup (Player 3) . playerPositions $ gameRunningPlayerPositions)
            Player 0 @?= gameRunningNextPlayer

{- |Function for testing the gameNg.

A test case consists of preparation, execution and assertions.
In this case, the preparation is done in the configuration, the execution consists of actions to execute and
the assertions evaluate the result.

-}
gameNgTestCase :: GameConfig -> [Action] -> (Either GameError GameState -> IO ()) -> IO ()
gameNgTestCase config  moves assertions =
    assertions $
        foldM
            (flip updateState)
            (GameRunning_ $ initialState config)
            moves
