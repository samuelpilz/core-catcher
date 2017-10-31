{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |Module for testing the app module.

The main function to execute test cases is the gameNgTestCase function defined in this module.
TODO: fix documentation

TODO: fix tests with new app interface

-}
module AppTest where

import           App.App
import           App.AppUtils
import           App.ConnectionState
import           App.State
import           ClassyPrelude              hiding (handle)
import           Config.GameConfig
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.State
import           Data.Easy                  (tripleToPair)
import           Data.Maybe                 (fromJust)
import           EntityMgnt
import           GameState
import           Network.Protocol
import           System.Random              (RandomGen, mkStdGen)
import qualified System.Random              as Random
import           Test.Framework
import           Test.HUnit.Base


test_noSuchConnection :: IO ()
test_noSuchConnection =
    appTestCase
        initialStateWithConnection
        [(ConnectionId 1, StartGame)]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 1, ServerError_ NoSuchConnection)]


test_login_playerHomeSent :: IO ()
test_login_playerHomeSent =
    appTestCase
        initialStateWithConnection
        [(ConnectionId 0, Login_ $ Login alice)]
        assertions
    where
        assertions (msgs, _) =
            msgs @?=
                [
                    ( ConnectionId 0
                    , PlayerHome_ PlayerHome
                        { playerHomePlayer = alice
                        , activeLobbies = []
                        , activeGames = []}
                    )
                ]

test_login_playerMapIncludesNewPlayer :: IO ()
test_login_playerMapIncludesNewPlayer =
    appTestCase
        initialStateWithConnection
        [(ConnectionId 0, Login_ $ Login alice)]
        assertions
    where
        assertions (_, state) =
            serverStatePlayerMap state @?= mapFromList [(alice, ConnectionId 0)]


test_playerHomeRefresh_notLoggedIn_errorMsg :: IO ()
test_playerHomeRefresh_notLoggedIn_errorMsg =
    appTestCase
        initialStateWithConnection
        [(ConnectionId 0, PlayerHomeRefresh)]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ NotLoggedIn)]


test_playerHomeRefresh :: IO ()
test_playerHomeRefresh =
    appTestCase
        initialStateWithLogin
        [(ConnectionId 0, PlayerHomeRefresh)]
        assertions
    where
        assertions (msgs, state) = do
            map fst msgs @?= [ConnectionId 0]
            serverStatePlayerMap state @?= mapFromList [(alice, ConnectionId 0)]
            case snd . headEx $ msgs of
                PlayerHome_ playerHome ->
                    PlayerHome
                        { playerHomePlayer = alice
                        , activeLobbies = []
                        , activeGames = []}
                        @?=
                        playerHome
                msg ->
                    assertFailure $
                        "should have sent PlayerHome to alice, but sent "
                        ++ show msg


test_logout_notLoggedIn :: IO ()
test_logout_notLoggedIn =
    appTestCase
        initialStateWithConnection
        [(ConnectionId 0, Logout)]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ NotLoggedIn)]


test_logout_loggedOut :: IO ()
test_logout_loggedOut =
    appTestCase
        initialStateWithLogin
        [ (ConnectionId 0, Logout)
        , (ConnectionId 0, Logout) -- to test that logout worked
        ]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ NotLoggedIn)]


test_createGame :: IO ()
test_createGame =
    appTestCase
        initialStateWithLogin
        [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"}) ]
        assertions
    where
        assertions (msgs, state) = do
            msgs @?= [(ConnectionId 0, GameLobbyView_ $ GameLobbyView "new-game" [alice])]

            findEntityById (GameId 0) state @?=
                Just (GameLobby_
                    GameLobby
                        { gameLobbyGameName = "new-game"
                        , gameLobbyConnectedPlayers = [alice]
                         }
                    )


test_createGame_notLoggedIn :: IO ()
test_createGame_notLoggedIn =
    appTestCase
        initialStateWithConnection
        [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"}) ]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ NotLoggedIn)]


test_joinGame_lobbySentToAll :: IO ()
test_joinGame_lobbySentToAll =
    appTestCase
        initialStateWith2Logins
        [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"})
        , (ConnectionId 1, JoinGame_ . JoinGame $ GameId 0)
        ]
        assertions
    where
        assertions (allMsgs, _) = do
            -- discard first msg because is response to created game
            let msgs = tailEx allMsgs
            mapM_ (\(cId, msg) ->
                case msg of
                    GameLobbyView_ lobby ->
                        (cId, gameLobbyViewPlayers lobby) @?= (cId, [alice, bob])
                    _ ->
                        assertFailure $ "expected lobby but got " ++ show msg ++ " to " ++ show cId
                )
                msgs
            map fst msgs @?= [ConnectionId 0, ConnectionId 1]


test_joinGame_NoSuchGame :: IO ()
test_joinGame_NoSuchGame =
    appTestCase
        initialStateWithLogin
        [ (ConnectionId 0, JoinGame_ . JoinGame $ GameId 0) ]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ $ NoSuchGame $ GameId 0)]


test_joinGame_notLoggedIn_serverErrorMsg :: IO ()
test_joinGame_notLoggedIn_serverErrorMsg =
    appTestCase
        initialStateWithConnection
        [ (ConnectionId 0, JoinGame_ . JoinGame $ GameId 0) ]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ NotLoggedIn)]


test_joinGame_playerAddedToLobby :: IO ()
test_joinGame_playerAddedToLobby =
    appTestCase
        initialStateWith2Logins
        [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"})
        , (ConnectionId 1, JoinGame_ . JoinGame $ GameId 0)
        ]
        assertions
    where
        assertions (_, state) = do
            let game = getEntityById (GameId 0) state
            case game of
                GameLobby_ lobby ->
                    gameLobbyConnectedPlayers lobby @?= [alice, bob]
                _ ->
                    assertFailure $ "game with id 0 should be in lobby-state, but is " ++ show game


test_joinGame_gameAlreadyStarted_errorMsg :: IO ()
test_joinGame_gameAlreadyStarted_errorMsg =
    appTestCase
        initialStateWithGame
        [ (ConnectionId 0, JoinGame_ . JoinGame $ GameId 0) ]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ GameAlreadyStarted)]


test_startGame_gameAlreadyStarted_errorMsg :: IO ()
test_startGame_gameAlreadyStarted_errorMsg =
    appTestCase
        initialStateWithGame
        [ (ConnectionId 0, StartGame) ]
        assertions
    where
        assertions (msgs, _) =
            msgs @?= [(ConnectionId 0, ServerError_ GameAlreadyStarted)]



test_startGame_initialInfoSent :: IO ()
test_startGame_initialInfoSent =
    appTestCase
        initialStateWithLogin
        [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"})
        , (ConnectionId 0, StartGame)
        ]
        assertions
    where
        assertions (msgs, _) =do
            case headEx msgs of
                (ConnectionId 0, GameLobbyView_ _) ->
                    return ()
                _ -> assertFailure "not sent lobby view"

            case msgs of
                [_, _] -> return ()
                [_] -> assertFailure "only got lobby-view-message"
                _ -> assertFailure $ "expected exactly 2 messages but got: " ++ show msgs

            case lastEx msgs of
                (ConnectionId 0, InitialInfoGameActive_ _) -> return ()
                (ConnectionId cId, msg) -> assertFailure $
                    "expect initial info to client 0 but got "
                    ++ show msg
                    ++ " to client "
                    ++ show cId


test_startGame_gameStartedInServerState :: IO ()
test_startGame_gameStartedInServerState =
    appTestCase
        initialStateWithLogin
        [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"})
        , (ConnectionId 0, StartGame)
        ]
        assertions
    where
        assertions (_, state) =
            case findEntityById (GameId 0) state :: Maybe GameState of
                Just (GameRunning_ gameRunning) -> do
                    (toList . players . gameRunningGameConfig $ gameRunning) @?= [alice]
                    [ConnectionId 0] @?=
                        (map fst .
                            distributeInitialInfosForGameRunning gameRunning $
                            state
                        )
                Just game ->
                    assertFailure $ "game in wrong state" ++ show game
                Nothing -> assertFailure "no game added"



-- test_startGame_multiplePlayers_initialInfoSentToAll :: IO ()
-- test_startGame_multiplePlayers_initialInfoSentToAll =
--     appTestCase
--         initialStateWith3Logins
--         [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"})
--         ,
--         , (ConnectionId 0, StartGame)
--         ]
--         assertions
--     where
--         assertions (_, state) =
--             return ()

-- TODO: reuse assertions for other cases
--         assertions state = do
--             msgSent <- getSentMsg $ getConnectionById 0 state
--             case msgSent of
--                 Nothing -> assertFailure "should have sent initial info to alice"
--                 Just (InitialInfoGameActive_ InitialInfoGameActive
--                     { initialPlayer
--                     , initialGameView
--                     , networkForGame
--                     , allPlayers
--                     , allEnergies
--                     }) -> do
--                     -- assertions on all fields of the initialInfoForClient
--                     alice @?= initialPlayer
--                     network defaultConfig @?= networkForGame
--                     [alice, bob, charlie] @?= allPlayers
--                     [Red, Blue, Orange] @?= allEnergies
--                     case initialGameView of
--                         CatcherView _ -> assertFailure "expeced rogue view"
--                         RogueView _   -> return ()
--
--                 Just msg -> assertFailure $ "expected initialInfo, got " ++ show msg
--
-- test_loginCatcher_initialInfo :: IO ()
-- test_loginCatcher_initialInfo = do
--     stateVar <- initialStateWith3FakeConnections
--     appTestCase
--         stateVar
--         [(1,Login_ . Login $ bob)]
--         assertions
--     where
--         assertions state = do
--             msgSent <- getSentMsg $ getConnectionById 1 state
--             case msgSent of
--                 Nothing -> assertFailure "should have sent to bob"
--                 Just (InitialInfoGameActive_ info) -> do
--                     bob @?= initialPlayer info
--                     network defaultConfig @?= networkForGame info
--                 Just msg -> assertFailure $ "expected initialInfo, got " ++ show msg
--
--
-- test_playerMoved_responseToAllWithCorrectGameView :: IO ()
-- test_playerMoved_responseToAllWithCorrectGameView = do
--     stateVar <- initialStateWith3Logins
--     appTestCase
--         stateVar
--         [(0, Action_ $ Move alice Red (Node 6))]
--         assertions
--     where
--         assertions state = do
--             -- assertion for rogue
--             msgSentToRogue <- getSentMsg $ getConnectionById 0 state
--             case msgSentToRogue of
--                 Nothing ->
--                     assertFailure "should have sent to rogue"
--                 Just (GameView_ (RogueView RogueGameView {rogueNextPlayer})) ->
--                     bob @?= rogueNextPlayer
--                 Just msg ->
--                     assertFailure $ "expected gameView, got " ++ show msg
--             -- assertions for catcher
--             mapM_
--                 (\cId -> do
--                     msgSent <- getSentMsg $ getConnectionById cId state
--                     case msgSent of
--                         Nothing ->
--                             assertFailure $ "should have sent to " ++ show cId
--                         Just (GameView_ (CatcherView CatcherGameView {catcherNextPlayer})) ->
--                             bob @?= catcherNextPlayer
--                         Just msg ->
--                             assertFailure $ "expected gameView, got " ++ show msg
--                 )
--                 [1,2]
--
-- test_playerMovedIncorrectly_gameErrorToOnlyOne :: IO ()
-- test_playerMovedIncorrectly_gameErrorToOnlyOne = do
--     stateVar <- initialStateWith3Logins
--     appTestCase
--         stateVar
--         [(1, Action_ $ Move bob Red (Node 6))]
--         assertions
--     where
--         assertions state = do
--             msgSentToBob <- getSentMsg $ getConnectionById 1 state
--             case msgSentToBob of
--                 Nothing -> assertFailure "should have sent gameError to bob"
--                 Just (GameError_ err) ->
--                     NotTurn alice @?= err
--                 Just msg -> assertFailure $ "expected gameError, got " ++ show msg
--
--             mapM_
--                 (\cId -> do
--                     msgSent <- getSentMsg $ getConnectionById cId state
--                     Nothing @?= msgSent
--                 )
--                 [0,2]
--
--
-- test_playerMovedIncorrectly_stateSillOld :: IO ()
-- test_playerMovedIncorrectly_stateSillOld = do
--     stateVar <- initialStateWith3Logins
--     appTestCase
--         stateVar
--         [ (1, Action_ $ Move bob Red (Node 6))
--         , (0, Action_ $ Move alice Red (Node 6))]
--         assertions
--     where
--         assertions state = do
--             -- test that alice did move indeed
--             msgSentToAlice <- getSentMsg $ getConnectionById 0 state
--             case msgSentToAlice of
--                 Nothing -> assertFailure "should have sent to rogue"
--                 Just (GameView_ _) -> return ()
--                 Just msg -> assertFailure $ "expected gameView, got " ++ show msg
--
--
-- test_playerMovedCorrectly_stateUpdatedTwice :: IO ()
-- test_playerMovedCorrectly_stateUpdatedTwice = do
--     stateVar <- initialStateWith3Logins
--     appTestCase
--         stateVar
--         [ (0, Action_ $ Move alice Red (Node 6))
--         , (1, Action_ $ Move bob Orange (Node 10))
--         ]
--         assertions
--     where
--         assertions state = do
--             -- test that alice did move indeed
--             -- TODO: test that 2 messages have been sent?
--             msgSentToAlice <- getSentMsg $ getConnectionById 0 state
--             case msgSentToAlice of
--                 Nothing -> assertFailure "should have sent to rogue"
--                 Just (GameView_ (RogueView RogueGameView {rogueNextPlayer})) ->
--                     charlie @?= rogueNextPlayer
--                 Just msg -> assertFailure $ "expected gameView, got " ++ show msg
--

-- TODO: tests for gameOver

{- |Function for testing the app-module.

A test case consists of preparation, execution and assertions.
In this case, the preparation is done in the configuration, the execution consists of actions to execute and
the assertions evaluate the result.

-}
appTestCase
    :: ServerState conn
    -> [(ConnectionId, MessageForServer)]
    -> (([(ConnectionId, MessageForClient)], ServerState conn) -> IO ())
    -> IO ()
appTestCase initialState msgs assertions =
    assertions $ handleMultipleMsgs (mkStdGen 42) initialState msgs

-- |Helper functions to handle multiple messages
handleMultipleMsgs
    :: RandomGen gen
    => gen
    -> ServerState conn
    -> [(ConnectionId, MessageForServer)]
    -> ([(ConnectionId, MessageForClient)], ServerState conn)
handleMultipleMsgs gen initialState =
    tripleToPair . foldl' foldF ([], initialState, gen)
    where
        foldF
            :: RandomGen gen
            => ([(ConnectionId, MessageForClient)], ServerState conn, gen)
            -> (ConnectionId, MessageForServer)
            -> ([(ConnectionId, MessageForClient)], ServerState conn, gen)
        foldF (clientMsg, state, g) (cId,msg) =
            let
                (g1, g2) = Random.split g
                (newClientMsgs, newState) = handleOneMsg g1 cId msg state
            in (clientMsg ++ newClientMsgs, newState, g2)

handleOneMsg
    :: RandomGen gen
    => gen
    -> ConnectionId
    -> MessageForServer
    -> ServerState conn
    -> ([(ConnectionId, MessageForClient)], ServerState conn)
handleOneMsg gen cId msg serverState =
    let (updateResult, newServerState) =
            runState (runExceptT $ handleMsgState gen cId msg) serverState
    in case updateResult of
       Left err ->
           (msgForOne cId $ ServerError_ err, serverState)
       Right toSend ->
           (toSend, newServerState)


prepareState
    :: RandomGen gen
    => gen
    -> [(ConnectionId, MessageForServer)]
    -> ServerState conn
    -> ServerState conn
prepareState gen msgs state = snd . handleMultipleMsgs gen state $ msgs


initialStateWithConnection :: ServerState ()
initialStateWithConnection = execState (do
        ConnectionId _ <- addEntityS $ newConnectionInfo ()
        return ()
    ) defaultInitialState


initialStateWithLogin :: ServerState ()
initialStateWithLogin =
    prepareState (mkStdGen 44)
        [(ConnectionId 0, Login_ $ Login alice)]
        initialStateWithConnection


initialStateWith3Logins :: ServerState ()
initialStateWith3Logins =
    prepareState (mkStdGen 44)
        [ (ConnectionId 0, Login_ $ Login alice)
        , (ConnectionId 1, Login_ $ Login bob)
        , (ConnectionId 2, Login_ $ Login charlie)
        ]
        initialStateWith3Connections


initialStateWith2Logins :: ServerState ()
initialStateWith2Logins =
    prepareState (mkStdGen 44)
        [ (ConnectionId 0, Login_ $ Login alice)
        , (ConnectionId 1, Login_ $ Login bob)
        ]
        initialStateWith3Connections


initialStateWith3Connections :: ServerState ()
initialStateWith3Connections = execState (do
        ConnectionId _ <- addEntityS $ newConnectionInfo ()
        ConnectionId _ <- addEntityS $ newConnectionInfo ()
        ConnectionId _ <- addEntityS $ newConnectionInfo ()
        return ()
    ) defaultInitialState

initialStateWithGame :: ServerState ()
initialStateWithGame =
    prepareState (mkStdGen 44)
        [ (ConnectionId 0, StartGame)
        ]
        initialStateWithLobby

initialStateWithLobby :: ServerState ()
initialStateWithLobby =
    prepareState (mkStdGen 44)
        [ (ConnectionId 0, CreateNewGame_ CreateNewGame{createGameName="new-game"})
        , (ConnectionId 1, JoinGame_ . JoinGame $ GameId 0)
        , (ConnectionId 2, JoinGame_ . JoinGame $ GameId 0)
        ]
        initialStateWith3Logins

getEntityById :: HasEntities container i => i -> container -> Entity container i
getEntityById eId = fromJust . findEntityById eId


alice :: Player
alice = Player "Alice"

bob :: Player
bob = Player "Bob"

charlie :: Player
charlie = Player "Charlie"
