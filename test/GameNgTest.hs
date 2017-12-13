{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- |Module for testing the gameNg module.

The main function to define test cases is the gameNgTestCase defined in this module.
This function combines configuration and actions to execute using the GameNg module
together with assertions that perform checks on the result of updates

-}
module GameNgTest where

import           ClassyPrelude
import           Config.GameConfig
import           Control.Error.MonadErrorInstance ()
import           Control.Monad.Trans.Except
import           GameNg
import           GameState
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base

-- (@?=) = assertEqual
test_defaultInitialStateHasStartingPlayer0 :: IO ()
test_defaultInitialStateHasStartingPlayer0 =
    (headEx . gameRunningNextPlayers . initialStateFromConfig $ defaultConfig) @?= alice

test_defaultInitialStateHasEmptyHistory :: IO ()
test_defaultInitialStateHasEmptyHistory = do
    (gameRunningRogueHistory . initialStateFromConfig $ defaultConfig) @?= ShadowRogueHistory []
    (gameRunningOpenRogueHistory . initialStateFromConfig $ defaultConfig) @?= OpenRogueHistory []

test_player0ValidMove_playerPositionUpdated :: IO ()
test_player0ValidMove_playerPositionUpdated =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Action alice Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            lookup alice gameRunningPlayerPositions @?= Just (Node 6)

test_player0ValidMove_energyDrained :: IO ()
test_player0ValidMove_energyDrained =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Action alice Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ game)) =
            remainingEnergy game @?= Just 1

        remainingEnergy GameRunning {gameRunningPlayerEnergies} = do -- maybe monad
            eMap <- lookup alice gameRunningPlayerEnergies
            lookup Red eMap


test_player0ValidMove_historyUpdated :: IO ()
test_player0ValidMove_historyUpdated =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Action alice Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ game)) = do
            gameRunningRogueHistory game @?= ShadowRogueHistory [(Red, Nothing)]
            gameRunningOpenRogueHistory game @?= OpenRogueHistory [(Red, Node 6, False)]


test_player0ValidMove_historyUpdatedWithShow :: IO ()
test_player0ValidMove_historyUpdatedWithShow =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig { rogueShowsAt = [0] }
        moves =
            [ Action alice Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ game)) = do
            gameRunningRogueHistory game @?= ShadowRogueHistory [(Red, Just $ Node 6)]
            gameRunningOpenRogueHistory game @?= OpenRogueHistory [(Red, Node 6, True)]


test_player0ValidMove_historyUpdate :: IO ()
test_player0ValidMove_historyUpdate =
    gameNgTestCase
            defaultConfig
            moves
            assertions
    where
        moves =
            [ Action alice Red (Node 6) ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ game)) = do
            gameRunningRogueHistory game @?= ShadowRogueHistory [(Red, Nothing)]
            gameRunningOpenRogueHistory game @?= OpenRogueHistory [(Red, Node 6, False)]

test_player1ValidMove_historyNotUpdated :: IO ()
test_player1ValidMove_historyNotUpdated =
    gameNgTestCase
            defaultConfig
            moves
            assertions
    where
        moves =
            [ Action alice Red (Node 6)
            , Action bob Blue (Node 3)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ game)) = do
            gameRunningRogueHistory game @?= ShadowRogueHistory [(Red, Nothing)]
            gameRunningOpenRogueHistory game @?= OpenRogueHistory [(Red, Node 6, False)]


test_notPlayer1Turn :: IO ()
test_notPlayer1Turn =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves = [ Action bob Orange (Node 5) ]
        assertions (Left err) = err @?= NotTurn alice
        assertions (Right _)  = assertFailure "should not be player 1's turn"

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
                insertMap bob (Node 3) $ initialPlayerPositions defaultConfig
            }
        moves =
            [ Action alice Red (Node 6)
            , Action bob Orange (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            gameOverWinningPlayer @?= bob
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
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
        moves = [ Action alice Red (Node 6) ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            gameOverWinningPlayer @?= alice
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            assertFailure $ "game should be over, was: " ++ show gameRunningPlayerPositions


test_gameOver_fieldsTakenFromGame :: IO ()
test_gameOver_fieldsTakenFromGame =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerPositions =
                insertMap bob (Node 3) $ initialPlayerPositions defaultConfig
            }
        moves =
            [ Action alice Red (Node 6)
            , Action bob Orange (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ gameOver)) = do
            lookup alice (gameOverPlayerPositions gameOver) @?= Just (Node 6)
            lookup bob (gameOverPlayerPositions gameOver) @?= Just (Node 6)
            (join . map (lookup Red) . lookup alice . gameOverPlayerEnergies $ gameOver) @?= Just 1
            gameOverGameConfig gameOver @?= config
            gameOverRogueHistory gameOver @?= OpenRogueHistory [(Red, Node 6, False)]
            gameOverWinningPlayer gameOver @?= bob
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ GameRunning {gameRunningPlayerPositions})) =
            assertFailure $ "game should be over, was: " ++ show gameRunningPlayerPositions


test_getGameOverView_fieldsSet :: IO ()
test_getGameOverView_fieldsSet =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig
            { initialPlayerPositions =
                insertMap bob (Node 3) $ initialPlayerPositions defaultConfig
            }
        moves =
            [ Action alice Red (Node 6)
            , Action bob Orange (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameOver_ gameOver)) =
            let
                GameOverView
                    { gameOverViewRogueHistory
                    , gameOverViewPlayerPositions
                    , gameOverViewPlayerEnergies
                    , gameOverViewWinningPlayer
                    } = getGameOverView gameOver
            in do
                gameOverViewRogueHistory @?= OpenRogueHistory [ (Red, Node 6, False) ]
                gameOverViewWinningPlayer @?= bob
                when (gameOverViewPlayerPositions == mempty) $ assertFailure "player positions empty"
                when (gameOverViewPlayerEnergies == mempty) $ assertFailure "player energies empty"
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
        moves = [ Action alice Red (Node 6) ]
        assertions (Left err) =
            err @?= PlayerNotFound alice
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
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
        moves = [ Action alice Red (Node 6) ]
        assertions (Left err) =
            err @?= PlayerNotFound alice
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
            { initialPlayerEnergies = singletonMap alice mempty }
        moves = [ Action alice Red (Node 6) ]
        assertions (Left err) =
            err @?= EnergyNotFound Red
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right _) =
            assertFailure "should not find energy"

test_cannotMoveTo :: IO ()
test_cannotMoveTo =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves = [ Action alice Orange (Node 13) ]
        assertions (Left err) =
            err @?= NotReachable (Node 1) Orange (Node 13)
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
                    updateMap (Just . insertMap Red 0) alice .
                    initialPlayerEnergies $ defaultConfig
                }
        moves =
            [ Action alice Red (Node 6) ]
        assertions (Left err) =
            err @?= NotEnoughEnergy
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
                    insertMap charlie (Node 15) . initialPlayerPositions $ defaultConfig
                }
        moves =
            [ Action alice Red (Node 6) -- mandatory first move from rogue
            , Action bob Blue (Node 15) ]
        assertions (Left err) =
            err @?= NodeBlocked charlie
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
                    insertMap bob (Node 6) . initialPlayerPositions $ defaultConfig
                }
        moves =
            [ Action alice Red (Node 6) ]
        assertions (Left err) =
            err @?= NodeBlocked bob
        assertions (Right _) =
            assertFailure "should not allow move to blocked node"

-- Test getViews

test_getViews_rogueViewEqualToFieldInGameState :: IO ()
test_getViews_rogueViewEqualToFieldInGameState = do
    let game = initialStateFromConfig defaultConfig
    let (rogueView, _) = getViews game
    gameRunningPlayerPositions game @?= roguePlayerPositions rogueView
    gameRunningPlayerEnergies game @?= rogueEnergies rogueView
    gameRunningRogueHistory game @?= rogueOwnHistory rogueView
    headEx (gameRunningNextPlayers game) @?= rogueNextPlayer rogueView


test_getViews_catcherViewDoesNotContainRogue :: IO ()
test_getViews_catcherViewDoesNotContainRogue = do
    let (_, catcherView) = getViews $ initialStateFromConfig defaultConfig
    (lookup alice . catcherPlayerPositions $ catcherView) @?= Nothing


test_getViews_someFieldsEqualToGameState :: IO ()
test_getViews_someFieldsEqualToGameState = do
    let game = initialStateFromConfig defaultConfig
    let (_, catcherView) = getViews game
    gameRunningPlayerEnergies game @?= catcherEnergies catcherView
    gameRunningRogueHistory game @?= catcherRogueHistory catcherView
    headEx (gameRunningNextPlayers game) @?= catcherNextPlayer catcherView


test_getViews_rogueHiddenInCatcherView :: IO ()
test_getViews_rogueHiddenInCatcherView = do
    let (_, catcherView) = getViews $ initialStateFromConfig defaultConfig
    (lookup alice . catcherPlayerPositions $ catcherView) @?= Nothing


test_getViews_rogueShownAtCurrentPositionInCatcherView :: IO ()
test_getViews_rogueShownAtCurrentPositionInCatcherView =
    gameNgTestCase
        config
        moves
        assertions
    where
        config = defaultConfig { rogueShowsAt = [0] }
        moves =
            [ Action alice Red (Node 6)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ game)) = do
            let (_, catcherView) = getViews game
            (lookup alice . catcherPlayerPositions $ catcherView) @?= Just (Node 6)


test_getViews_rogueShownAtLastPositionInCatcherView :: IO ()
test_getViews_rogueShownAtLastPositionInCatcherView =
    gameNgTestCase
        config
        moves
        assertions
   where
        config = defaultConfig { rogueShowsAt = [0] }
        moves =
            [ Action alice Red (Node 6)
            , Action bob Blue (Node 3)
            , Action charlie Orange (Node 13)
            , Action alice Orange (Node 11)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right (GameRunning_ game)) = do
            let (_, catcherView) = getViews game
            (lookup alice . catcherPlayerPositions $ catcherView) @?= Just (Node 6)


test_gameRound :: IO ()
test_gameRound =
    gameNgTestCase
        defaultConfig
        moves
        assertions
    where
        moves =
            [ Action alice Orange (Node 11)
            , Action bob Blue (Node 3)
            , Action charlie Orange (Node 13)
            ]
        assertions (Left err) =
            assertFailure $ "action failed: " ++ show err
        assertions (Right (GameOver_ GameOver {gameOverWinningPlayer})) =
            assertFailure $ "Game Over by " ++ show gameOverWinningPlayer
        assertions (Right (GameLobby_ _)) =
            assertFailure "Game still in lobby"
        assertions (Right
            (GameRunning_ GameRunning
                { gameRunningPlayerPositions
                , gameRunningNextPlayers
                }))
            = do
            Just (Node 11) @?=
                (lookup alice . playerPositions $ gameRunningPlayerPositions)
            Just (Node 3) @?=
                (lookup bob . playerPositions $ gameRunningPlayerPositions)
            Just (Node 13) @?=
                (lookup charlie . playerPositions $ gameRunningPlayerPositions)
            headEx gameRunningNextPlayers @?= alice



{- |Function for testing the gameNg.

A test case consists of preparation, execution and assertions.
In this case, the preparation is done in the configuration, the execution consists of actions to execute and
the assertions evaluate the result.

-}
gameNgTestCase :: GameConfig -> [Action] -> (Either GameError GameState -> IO ()) -> IO ()
gameNgTestCase config  moves assertions =
    assertions . runExcept $
        foldM
            (flip updateState)
            (GameRunning_ $ initialStateFromConfig config)
            moves

alice :: Player
alice = Player "Alice"

bob :: Player
bob = Player "Bob"

charlie :: Player
charlie = Player "Charlie"
