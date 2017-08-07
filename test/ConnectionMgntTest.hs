{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConnectionMgntTest where

import           App.ConnectionMgnt
import           ClassyPrelude
import           Mock.Connection
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base

emptyFakeConnectionsTVar :: IO (TVar FakeConnections)
emptyFakeConnectionsTVar = newTVarIO $ FakeConnections empty


fakeConnectionsTVar :: IO (TVar FakeConnections)
fakeConnectionsTVar = do
    conn0 <- newFakeConnection
    conn1 <- newFakeConnection
    conn2 <- newFakeConnection

    newTVarIO $ FakeConnections $ fromList
        [ ClientConnection 0 conn0
        , ClientConnection 1 conn1
        , ClientConnection 2 conn2
        ]


test_addConnection :: IO ()
test_addConnection = do
    conns <- emptyFakeConnectionsTVar
    conn <- newFakeConnection
    cId <- connectClient conn conns
    0 @?= cId
    FakeConnections newConns <- readTVarIO conns
    1 @?= length newConns


test_addMultipleConnections :: IO ()
test_addMultipleConnections = do
    conns <- emptyFakeConnectionsTVar
    conn1 <- newFakeConnection
    conn2 <- newFakeConnection
    conn3 <- newFakeConnection
    cId1 <- connectClient conn1 conns
    cId2 <- connectClient conn2 conns
    cId3 <- connectClient conn3 conns

    0 @?= cId1
    1 @?= cId2
    2 @?= cId3


    FakeConnections newConns <- readTVarIO conns
    3 @?= length newConns

test_disconnectClient :: IO ()
test_disconnectClient = do
    conns <- fakeConnectionsTVar
    disconnectClient 1 conns

    FakeConnections newConns <- readTVarIO conns
    fromList [0,2] @?= map connectionId newConns


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
    FakeConnections conns <- readTVarIO connsTVar
    multicastMsg conns msg
    for_ conns (\ClientConnection {connection} -> do
        sentMsg <- readTVarIO $ sendBuffer connection
        Just msg @?= sentMsg)

test_withoutClient :: IO ()
test_withoutClient = do
    connsTVar <- fakeConnectionsTVar
    FakeConnections conns <- readTVarIO connsTVar
    fromList [0,2] @?= (map connectionId . withoutClient 1 $ conns)


test_withoutClientNotContained :: IO ()
test_withoutClientNotContained = do
    connsTVar <- fakeConnectionsTVar
    FakeConnections conns <- readTVarIO connsTVar
    fromList [0,1,2] @?= (map connectionId . withoutClient 3 $ conns)

test_findConnectionById :: IO ()
test_findConnectionById = do
    connsTVar <- fakeConnectionsTVar
    FakeConnections conns <- readTVarIO connsTVar
    Just 1 @?= (map connectionId . findConnectionById 1 $ conns)


test_findConnectionByIdNotContained :: IO ()
test_findConnectionByIdNotContained = do
    connsTVar <- fakeConnectionsTVar
    FakeConnections conns <- readTVarIO connsTVar
    Nothing @?= (map connectionId . findConnectionById 4 $ conns)
