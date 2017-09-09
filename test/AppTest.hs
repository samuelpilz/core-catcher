{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- |Module for testing the app module.

The main function to execute test cases is the gameNgTestCase function defined in this module.
TODO: fix documentation

-}
module AppTest where

import           App.App
import           App.ConnectionMgnt
import           App.State
import           ClassyPrelude      hiding (handle)
import           Config.GameConfig  (GameConfig (..), defaultConfig)
import           Data.Maybe         (fromJust)
import           Mock.Connection
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base



test_loginRogue_initialInfo :: IO ()
test_loginRogue_initialInfo = do
    initialState <- initialStateWith3FakeConnections
    appTestCase
        initialState
        [(0,Login_ . Login $ alice)]
        assertions
    where
        assertions state = do
            msgSent <- getSentMsg $ getConnectionById 0 state
            case msgSent of
                Nothing -> assertFailure "should have sent initial info to alice"
                Just (InitialInfoForClient_ InitialInfoForClient
                    { initialPlayer
                    , initialGameView
                    , networkForGame
                    , allPlayers
                    , allEnergies
                    }) -> do
                    -- assertions on all fields of the initialInfoForClient
                    alice @?= initialPlayer
                    network defaultConfig @?= networkForGame
                    [alice, bob, charlie] @?= allPlayers
                    [Red, Blue, Orange] @?= allEnergies
                    case initialGameView of
                        CatcherView _ -> assertFailure "expeced rogue view"
                        RogueView _ -> return ()

                Just msg -> assertFailure $ "expected initialInfo, got " ++ show msg

test_loginCatcher_initialInfo :: IO ()
test_loginCatcher_initialInfo = do
    stateVar <- initialStateWith3FakeConnections
    appTestCase
        stateVar
        [(1,Login_ . Login $ bob)]
        assertions
    where
        assertions state = do
            msgSent <- getSentMsg $ getConnectionById 1 state
            case msgSent of
                Nothing -> assertFailure "should have sent to bob"
                Just (InitialInfoForClient_ info) -> do
                    bob @?= initialPlayer info
                    network defaultConfig @?= networkForGame info
                Just msg -> assertFailure $ "expected initialInfo, got " ++ show msg


test_playerMoved_responseToAllWithCorrectGameView :: IO ()
test_playerMoved_responseToAllWithCorrectGameView = do
    stateVar <- initialStateWith3Logins
    appTestCase
        stateVar
        [(0, Action_ $ Move alice Red (Node 6))]
        assertions
    where
        assertions state = do
            -- assertion for rogue
            msgSentToRogue <- getSentMsg $ getConnectionById 0 state
            case msgSentToRogue of
                Nothing ->
                    assertFailure "should have sent to rogue"
                Just (GameView_ (RogueView RogueGameView {rogueNextPlayer})) ->
                    bob @?= rogueNextPlayer
                Just msg ->
                    assertFailure $ "expected gameView, got " ++ show msg
            -- assertions for catcher
            mapM_
                (\cId -> do
                    msgSent <- getSentMsg $ getConnectionById cId state
                    case msgSent of
                        Nothing ->
                            assertFailure $ "should have sent to " ++ show cId
                        Just (GameView_ (CatcherView CatcherGameView {catcherNextPlayer})) ->
                            bob @?= catcherNextPlayer
                        Just msg ->
                            assertFailure $ "expected gameView, got " ++ show msg
                )
                [1,2]

test_playerMovedIncorrectly_gameErrorToOnlyOne :: IO ()
test_playerMovedIncorrectly_gameErrorToOnlyOne = do
    stateVar <- initialStateWith3Logins
    appTestCase
        stateVar
        [(1, Action_ $ Move bob Red (Node 6))]
        assertions
    where
        assertions state = do
            msgSentToBob <- getSentMsg $ getConnectionById 1 state
            case msgSentToBob of
                Nothing -> assertFailure "should have sent gameError to bob"
                Just (GameError_ err) ->
                    NotTurn alice @?= err
                Just msg -> assertFailure $ "expected gameError, got " ++ show msg

            mapM_
                (\cId -> do
                    msgSent <- getSentMsg $ getConnectionById cId state
                    Nothing @?= msgSent
                )
                [0,2]


test_playerMovedIncorrectly_stateSillOld :: IO ()
test_playerMovedIncorrectly_stateSillOld = do
    stateVar <- initialStateWith3Logins
    appTestCase
        stateVar
        [ (1, Action_ $ Move bob Red (Node 6))
        , (0, Action_ $ Move alice Red (Node 6))]
        assertions
    where
        assertions state = do
            -- test that alice did move indeed
            msgSentToAlice <- getSentMsg $ getConnectionById 0 state
            case msgSentToAlice of
                Nothing -> assertFailure "should have sent to rogue"
                Just (GameView_ _) -> return ()
                Just msg -> assertFailure $ "expected gameView, got " ++ show msg


test_playerMovedCorrectly_stateUpdatedTwice :: IO ()
test_playerMovedCorrectly_stateUpdatedTwice = do
    stateVar <- initialStateWith3Logins
    appTestCase
        stateVar
        [ (0, Action_ $ Move alice Red (Node 6))
        , (1, Action_ $ Move bob Orange (Node 10))
        ]
        assertions
    where
        assertions state = do
            -- test that alice did move indeed
            -- TODO: test that 2 messages have been sent?
            msgSentToAlice <- getSentMsg $ getConnectionById 0 state
            case msgSentToAlice of
                Nothing -> assertFailure "should have sent to rogue"
                Just (GameView_ (RogueView RogueGameView {rogueNextPlayer})) ->
                    charlie @?= rogueNextPlayer
                Just msg -> assertFailure $ "expected gameView, got " ++ show msg


-- TODO: tests for gameOver

{- |Function for testing the app-module.

A test case consists of preparation, execution and assertions.
In this case, the preparation is done in the configuration, the execution consists of actions to execute and
the assertions evaluate the result.

-}
appTestCase
    :: TVar (ServerState FakeConnection)
    -> [(ConnectionId, MessageForServer)]
    -> (ServerState FakeConnection -> IO ())
    -> IO ()
appTestCase stateVar msgs assertions = do
    handleMultipleMsgs stateVar msgs
    state <- atomically $ readTVar stateVar
    assertions state

-- |Helper functions to handle multiple messages for a stateVar
handleMultipleMsgs
    :: TVar (ServerState FakeConnection)
    -> [(ConnectionId, MessageForServer)]
    -> IO ()
handleMultipleMsgs stateVar = mapM_ (handleMsg stateVar)

-- |Helper function to handle one message for a stateVar
handleMsg :: TVar (ServerState FakeConnection) -> (ConnectionId, MessageForServer) -> IO ()
handleMsg stateVar (cId, msg) = do
   state <- atomically $ readTVar stateVar
   handle (cId, getConnectionById cId state) stateVar msg

initialStateWith3FakeConnections :: IO (TVar (ServerState FakeConnection))
initialStateWith3FakeConnections = do

    stateVar <- newTVarIO defaultInitialState
    connA <- newFakeConnection
    connB <- newFakeConnection
    connC <- newFakeConnection
    _ <- connectClient connA stateVar
    _ <- connectClient connB stateVar
    _ <- connectClient connC stateVar
    return stateVar

initialStateWith3Logins :: IO (TVar (ServerState FakeConnection))
initialStateWith3Logins = do
    stateVar <- initialStateWith3FakeConnections
    handleMultipleMsgs stateVar
        [ (0, Login_ $ Login alice)
        , (1, Login_ $ Login bob)
        , (2, Login_ $ Login charlie)
        ]
    state <- readTVarIO stateVar
    mapM_ (resetSendBuffer . snd) . mapToList . connections . stateConnections $ state
    return stateVar


getConnectionById :: ConnectionId -> ServerState FakeConnection -> FakeConnection
getConnectionById cId = fromJust . findConnectionById cId . stateConnections

alice :: Player
alice = Player "Alice"

bob :: Player
bob = Player "Bob"

charlie :: Player
charlie = Player "Charlie"
