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

{-Tests
* withoutClient
-}

-- TODO: translate into ConnectionMgnt tests
-- nonEmptyServerIO :: IO (ServerState Fake.FakeConnection)
-- nonEmptyServerIO = do
--     conns <- sequenceA $ map (\num -> Fake.emptyConnection >>= \conn -> return (num, conn)) [1..10]
--     let sndMvar = Fake.contentMsg . snd $ conns `indexEx` 1
--     atomically $ writeTVar sndMvar (Fake.Msg $ Aeson.encode anAction)
--     return ServerState
--         { gameState = GameNg.GameRunning_ $ GameNg.initialState Config.defaultConfig
--         , connections = conns
--         }
--
-- test_receiveInvalidMessage :: IO ()
-- test_receiveInvalidMessage = do
--     nonEmptyServer <- nonEmptyServerIO
--     let (Just conn) = Mgnt.findConnectionById 1 (connections nonEmptyServer)
--     maybeMsg <- recvMsgForServer conn
--     assertEqual Nothing maybeMsg
--
-- prop_arbitraryActionIsParseable :: Protocol.Action -> Property
-- prop_arbitraryActionIsParseable action = assertionAsProperty $ do
--     nonEmptyServer <- nonEmptyServerIO
--     let (Just cli@(1, conn)) = Mgnt.findConnectionById 1 (connections nonEmptyServer)
--     atomically $ writeTVar (Fake.contentMsg conn) (Fake.Msg $ Aeson.encode action)
--     maybeMsg <- recvMsgForServer cli
--     assertEqual True (isJust maybeMsg)
--     let Just (Protocol.Action_ realAction) = maybeMsg
--     assertEqual action realAction
--
-- prop_withoutClient :: Mgnt.ClientId -> Property
-- prop_withoutClient cid = assertionAsProperty $ do
--     nonEmptyServer <- nonEmptyServerIO
--     let conns = Mgnt.getConnections nonEmptyServer
--     assertEqual True (cid `notElem` map fst (Mgnt.withoutClient cid conns))
--
-- prop_sendArbitraryRogueGameView :: Protocol.RogueGameView -> Property
-- prop_sendArbitraryRogueGameView rgv = assertionAsProperty $ do
--     nonEmptyServer <- nonEmptyServerIO
--     let (Just cli@(_, conn)) = Mgnt.findConnectionById 1 (connections nonEmptyServer)
--     sendToClient cli rgv
--     -- small hack, sendView saves the sent message which can be read out with receiveData
--     Just (Protocol.GameView_ (Protocol.RogueView rgv')) <- Aeson.decode `map` receiveData conn
--     assertEqual rgv rgv'
--
-- prop_sendArbitraryCatcherGameView :: Protocol.CatcherGameView -> Property
-- prop_sendArbitraryCatcherGameView cgv = assertionAsProperty $ do
--     nonEmptyServer <- nonEmptyServerIO
--     let (Just cli@(_, conn)) = Mgnt.findConnectionById 1 (connections nonEmptyServer)
--     sendToClient cli cgv
--     -- small hack, sendView saves the sent message which can be read out with receiveData
--     Just (Protocol.GameView_ (Protocol.CatcherView cgv')) <- Aeson.decode `map` receiveData conn
--     assertEqual cgv cgv'
--
--
-- prop_broadcastCatcherGameView :: WithQCArgs (Protocol.CatcherGameView -> Property)
-- prop_broadcastCatcherGameView =
--     withQCArgs
--         (\args -> args { maxSuccess = 5 })
--         prop_broadcastArbitraryCatcherGameView
--     where
--       prop_broadcastArbitraryCatcherGameView :: Protocol.CatcherGameView -> Property
--       prop_broadcastArbitraryCatcherGameView cgv =
--           assertionAsProperty
--               $ do
--                   nonEmptyServer <- nonEmptyServerIO
--                   multicastToClients (Mgnt.getConnections nonEmptyServer) cgv
--                   -- small hack, sendView saves the sent message which can be read out with receiveData
--                   res <- mapM (receiveData . snd) (Mgnt.getConnections nonEmptyServer) :: IO (Seq LByteString)
--                   let answers = map Aeson.decode res
--                   assertBool (all (\(Just (Protocol.GameView_ (Protocol.CatcherView v))) -> v == cgv) answers)
--
--
-- test_receiveValidMessage :: IO ()
-- test_receiveValidMessage = do
--     nonEmptyServer <- nonEmptyServerIO
--     let (Just conn) = Mgnt.findConnectionById 2 (connections nonEmptyServer)
--     maybeMsg <- recvMsgForServer conn
--     assertEqual True (isJust maybeMsg)
--     let Just (Protocol.Action_ realConn) = maybeMsg
--     assertEqual anAction realConn


-- TODO: translate state-tests

-- emptyServerIO :: IO (ServerState FakeConnection)
-- emptyServerIO =
--     return
--         ServerState
--             { gameState = GameNg.GameRunning_ $ GameNg.initialState Config.defaultConfig
--             , connections = empty
--             }
--
-- nonEmptyServerIO :: IO (ServerState FakeConnection)
-- nonEmptyServerIO = do
--     conns <- sequenceA . fromList $ map (\num -> newFakeConnection >>= \conn -> return (num, conn)) [1, 2]
--     return
--         ServerState
--             { gameState = GameNg.GameRunning_ $ GameNg.initialState Config.defaultConfig
--             , connections = conns
--             }
--
-- test_stateWithNoConnections :: IO ()
-- test_stateWithNoConnections = do
--     emptyServer <- emptyServerIO
--     assertEqual (length $ getConnections emptyServer) 0
--
-- test_stateWithConnections :: IO ()
-- test_stateWithConnections = do
--     nonEmptyServer <- nonEmptyServerIO
--     assertEqual (length $ getConnections nonEmptyServer) 2
--
-- test_setConnectionsToState :: IO ()
-- test_setConnectionsToState = do
--     emptyServer <- emptyServerIO
--     assertEqual (length $ getConnections emptyServer) 0
--     conns <- sequenceA . fromList $ map (\num -> newFakeConnection >>= \conn -> return (num, conn)) [1, 2]
--     let newState = setConnections conns emptyServer
--     assertEqual (length $ getConnections newState) 2
--
-- test_connectClientToState :: IO ()
-- test_connectClientToState = do
--     emptyServer <- emptyServerIO
--     stateRef <- newTVarIO emptyServer
--     server <- readTVarIO stateRef
--     assertEqual (length $ getConnections server) 0
--     newConn <- newFakeConnection
--     _ <- connectClient newConn stateRef
--     server' <- readTVarIO stateRef
--     assertEqual (length $ getConnections server') 1
--
-- test_disconnectClientFromState :: IO ()
-- test_disconnectClientFromState = do
--     stateRef <- newTVarIO nonEmptyServerIO
--     server <- readTVarIO stateRef
--     assertEqual (length $ getConnections server) 2
--     disconnectClient 1 stateRef
--     state' <- readTVarIO stateRef
--     assertEqual (length $ getConnections state') 1
--
-- test_disconnectUnknownClientFromState :: IO ()
-- test_disconnectUnknownClientFromState = do
--     stateRef <- newTVarIO nonEmptyServerIO
--     server <- readTVarIO stateRef
--     assertEqual (length $ getConnections server) 2
--     disconnectClient 3 stateRef
--     state' <- readTVarIO stateRef
--     assertEqual (length $ getConnections state') 2
--
-- test_findClientById :: IO ()
-- test_findClientById = do
--     nonEmptyServer <- nonEmptyServerIO
--     assertEqual (length $ getConnections nonEmptyServer) 2
--     let val = findConnectionById 1 (connections nonEmptyServer)
--     assertEqual (Just 1) (fst <$> val)
--
-- test_findNonExistentClientById :: IO ()
-- test_findNonExistentClientById = do
--     nonEmptyServer <- nonEmptyServerIO
--     assertEqual (length $ getConnections nonEmptyServer) 2
--     let val = findConnectionById 3 (connections nonEmptyServer)
--     assertEqual True (isNothing val)


