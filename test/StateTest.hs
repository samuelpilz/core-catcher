{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StateTest where

import           App.ConnectionMgnt
import           App.State
import           ClassyPrelude
import           GameLogic
import           Mock.Connection
import           Test.Framework

emptyServer :: ServerState FakeConnection
emptyServer =
    ServerState
        { gameState =
            GameState
                { start = fromList [1,2,3]
                , network = someNet
                , actions = empty
                }
        , connections = empty
        }

nonEmptyServer :: ServerState FakeConnection
nonEmptyServer =
    ServerState
        { gameState =
            GameState
                { start = fromList [1,2,3]
                , network = someNet
                , actions = empty
                }
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
