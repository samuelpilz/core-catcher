{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConnectionMgntTest where

import           App.Connection
import           ClassyPrelude
import           Mock.Connection
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base

-- TODO: reuse tests for EntityMgnt

emptyFakeConnectionsTVar :: IO (TVar FakeConnections)
emptyFakeConnectionsTVar = newTVarIO $ ClientConnections  mempty 0


fakeConnectionsTVar :: IO (TVar FakeConnections)
fakeConnectionsTVar = do
    conn0 <- newFakeConnection
    conn1 <- newFakeConnection
    conn2 <- newFakeConnection

    newTVarIO ClientConnections
        { connections =
            mapFromList
                [ (0, (conn0, newConnectionState))
                , (1, (conn1, newConnectionState))
                , (2, (conn2, newConnectionState))
                ]
        , nextId = 3
        }


test_addConnection :: IO ()
test_addConnection = do
    conns <- emptyFakeConnectionsTVar
    conn <- newFakeConnection
    cId <- atomically $ connectClient conns conn
    0 @?= cId
    newConns <- readTVarIO conns
    1 @?= length (connections newConns)


test_addMultipleConnections :: IO ()
test_addMultipleConnections = do
    conns <- emptyFakeConnectionsTVar
    conn1 <- newFakeConnection
    conn2 <- newFakeConnection
    conn3 <- newFakeConnection
    cId1 <- atomically $ connectClient conns conn1
    cId2 <- atomically $ connectClient conns conn2
    cId3 <- atomically $ connectClient conns conn3

    0 @?= cId1
    1 @?= cId2
    2 @?= cId3


    newConns <- readTVarIO conns
    3 @?= length (connections newConns)

test_disconnectClient :: IO ()
test_disconnectClient = do
    conns <- fakeConnectionsTVar
    atomically $ disconnectClient conns 1

    newConns <- readTVarIO conns
    [0,2] @?= (map fst . mapToList . connections $ newConns)


prop_sendMsg :: MessageForClient -> Property
prop_sendMsg msg = assertionAsProperty $ do
    conn <- newFakeConnection
    sendMsg conn msg
    sentMsg <- readTVarIO $ sendBuffer conn
    Just msg @?= sentMsg


prop_recvMsg :: MessageForServer -> Property
prop_recvMsg msg = assertionAsProperty $ do
    conn <- newFakeConnection
    prepareMsgToRead conn msg
    readMsg <- recvMsg conn
    Just msg @?= readMsg


prop_multicastAction :: MessageForClient -> Property
prop_multicastAction msg = assertionAsProperty $ do
    connsTVar <- fakeConnectionsTVar
    conns <- readTVarIO connsTVar
    multicastMsg conns msg
    for_ (otoList $ connections conns) (\(conn,_) -> do
        sentMsg <- readTVarIO $ sendBuffer conn
        Just msg @?= sentMsg)

test_withoutClient :: IO ()
test_withoutClient = do
    connsTVar <- fakeConnectionsTVar
    conns <- readTVarIO connsTVar
    [0,2] @?= (map fst . mapToList . connections . withoutClient 1 $ conns)


test_withoutClientNotContained :: IO ()
test_withoutClientNotContained = do
    connsTVar <- fakeConnectionsTVar
    conns <- readTVarIO connsTVar
    [0,1,2] @?= (map fst . mapToList . connections . withoutClient 3 $ conns)

test_findConnectionById :: IO ()
test_findConnectionById = do
    connsTVar <- fakeConnectionsTVar
    conns <- readTVarIO connsTVar
    Just () @?= (map (const ()) . findConnectionById 1 $ conns)


test_findConnectionByIdNotContained :: IO ()
test_findConnectionByIdNotContained = do
    connsTVar <- fakeConnectionsTVar
    conns <- readTVarIO connsTVar
    Nothing @?= (map (const ()) . findConnectionById 4 $ conns)
