{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StateTest where

import           App.ConnectionMgnt
import           App.State
import           ClassyPrelude
import           GameLogicTest
import           Glue               ()
import           Mock.Connection
import           Test.Framework

emptyServer :: ServerState FakeConnection
emptyServer =
    ServerState
        { gameState = exampleGameState
        , connections = empty
        }

nonEmptyServer :: ServerState FakeConnection
nonEmptyServer =
    ServerState
        { gameState = exampleGameState
        , connections = fromList
            [ (1, emptyConnection)
            , (2, emptyConnection)
            ]
        }

test_stateWithNoConnections :: IO ()
test_stateWithNoConnections =
    assertEqual (length $ getConnections emptyServer) 0

test_stateWithConnections :: IO ()
test_stateWithConnections =
    assertEqual (length $ getConnections nonEmptyServer) 2

test_setConnectionsToState :: IO ()
test_setConnectionsToState = do
    assertEqual (length $ getConnections emptyServer) 0
    let newState = setConnections (fromList [(1, emptyConnection), (2, emptyConnection)]) emptyServer
    assertEqual (length $ getConnections newState) 2

test_connectClientToState :: IO ()
test_connectClientToState = do
    stateRef <- newTVarIO emptyServer
    server <- readTVarIO stateRef
    assertEqual (length $ getConnections server) 0
    _ <- connectClient emptyConnection stateRef
    server' <- readTVarIO stateRef
    assertEqual (length $ getConnections server') 1

test_disconnectClientFromState :: IO ()
test_disconnectClientFromState = do
      stateRef <- newTVarIO nonEmptyServer
      server <- readTVarIO stateRef
      assertEqual (length $ getConnections server) 2
      disconnectClient 1 stateRef
      state' <- readTVarIO stateRef
      assertEqual (length $ getConnections state') 1

test_disconnectUnknownClientFromState :: IO ()
test_disconnectUnknownClientFromState = do
      stateRef <- newTVarIO nonEmptyServer
      server <- readTVarIO stateRef
      assertEqual (length $ getConnections server) 2
      disconnectClient 3 stateRef
      state' <- readTVarIO stateRef
      assertEqual (length $ getConnections state') 2

test_findClientById :: IO ()
test_findClientById = do
      assertEqual (length $ getConnections nonEmptyServer) 2
      let val = findConnectionById 1 nonEmptyServer
      assertEqual (Just 1) (fst <$> val)

test_findNonExistentClientById :: IO ()
test_findNonExistentClientById = do
      assertEqual (length $ getConnections nonEmptyServer) 2
      let val = findConnectionById 3 nonEmptyServer
      assertEqual True (isNothing val)
