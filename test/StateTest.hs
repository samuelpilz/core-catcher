{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StateTest where

import           App.ConnectionMgnt
import           App.State
import           ClassyPrelude
import qualified Config.GameConfig  as Config
import qualified GameNg
import           Mock.Connection
import           Test.Framework

emptyServerIO :: IO (ServerState FakeConnection)
emptyServerIO =
    return
        ServerState
            { gameState = GameNg.GameRunning_ $ GameNg.initialState Config.defaultConfig
            , connections = empty
            }

nonEmptyServerIO :: IO (ServerState FakeConnection)
nonEmptyServerIO = do
    conns <- sequenceA . fromList $ map (\num -> emptyConnection >>= \conn -> return (num, conn)) [1, 2]
    return
        ServerState
            { gameState = GameNg.GameRunning_ $ GameNg.initialState Config.defaultConfig
            , connections = conns
            }

test_stateWithNoConnections :: IO ()
test_stateWithNoConnections = do
    emptyServer <- emptyServerIO
    assertEqual (length $ getConnections emptyServer) 0

test_stateWithConnections :: IO ()
test_stateWithConnections = do
    nonEmptyServer <- nonEmptyServerIO
    assertEqual (length $ getConnections nonEmptyServer) 2

test_setConnectionsToState :: IO ()
test_setConnectionsToState = do
    emptyServer <- emptyServerIO
    assertEqual (length $ getConnections emptyServer) 0
    conns <- sequenceA . fromList $ map (\num -> emptyConnection >>= \conn -> return (num, conn)) [1, 2]
    let newState = setConnections conns emptyServer
    assertEqual (length $ getConnections newState) 2

test_connectClientToState :: IO ()
test_connectClientToState = do
    emptyServer <- emptyServerIO
    stateRef <- newTVarIO emptyServer
    server <- readTVarIO stateRef
    assertEqual (length $ getConnections server) 0
    newConn <- emptyConnection
    _ <- connectClient newConn stateRef
    server' <- readTVarIO stateRef
    assertEqual (length $ getConnections server') 1

test_disconnectClientFromState :: IO ()
test_disconnectClientFromState = do
    nonEmptyServer <- nonEmptyServerIO
    stateRef <- newTVarIO nonEmptyServer
    server <- readTVarIO stateRef
    assertEqual (length $ getConnections server) 2
    disconnectClient 1 stateRef
    state' <- readTVarIO stateRef
    assertEqual (length $ getConnections state') 1

test_disconnectUnknownClientFromState :: IO ()
test_disconnectUnknownClientFromState = do
    nonEmptyServer <- nonEmptyServerIO
    stateRef <- newTVarIO nonEmptyServer
    server <- readTVarIO stateRef
    assertEqual (length $ getConnections server) 2
    disconnectClient 3 stateRef
    state' <- readTVarIO stateRef
    assertEqual (length $ getConnections state') 2

test_findClientById :: IO ()
test_findClientById = do
    nonEmptyServer <- nonEmptyServerIO
    assertEqual (length $ getConnections nonEmptyServer) 2
    let val = findConnectionById 1 (connections nonEmptyServer)
    assertEqual (Just 1) (fst <$> val)

test_findNonExistentClientById :: IO ()
test_findNonExistentClientById = do
    nonEmptyServer <- nonEmptyServerIO
    assertEqual (length $ getConnections nonEmptyServer) 2
    let val = findConnectionById 3 (connections nonEmptyServer)
    assertEqual True (isNothing val)
