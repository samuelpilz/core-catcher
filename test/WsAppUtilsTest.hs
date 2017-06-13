{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module WsAppUtilsTest where

import qualified App.ConnectionMgnt as Mgnt
import           App.State
import           App.WsAppUtils
import           ClassyPrelude
import qualified Data.Aeson         as Aeson
import           GameLogicTest
import qualified Glue               ()
import qualified Mock.Connection    as Fake
import qualified Network.Protocol   as Protocol
import           Test.Framework

anAction :: Protocol.Action
anAction =
    Protocol.Move
        (Protocol.Player 1)
        (Protocol.Transport "green")
        (Protocol.Node 1)


nonEmptyServerIO :: IO (ServerState Fake.FakeConnection)
nonEmptyServerIO = do
    conns <- sequenceA . fromList $ map (\num -> Fake.emptyConnection >>= \conn -> return (num, conn)) [1..100]
    let sndMvar = (Fake.contentMsg . snd $ conns `indexEx` 1)
    _ <- takeMVar sndMvar
    putMVar sndMvar (Fake.Msg $ Aeson.encode anAction)
    return ServerState
        { gameState = exampleGameState
        , connections = conns
        }

test_receiveInvalidMessage :: IO ()
test_receiveInvalidMessage = do
    nonEmptyServer <- nonEmptyServerIO
    let (Just conn) = Mgnt.findConnectionById 1 nonEmptyServer
    maybeAction <- recvAction conn
    assertEqual True (isNothing maybeAction)

prop_arbitraryActionIsParsable :: Protocol.Action -> Property
prop_arbitraryActionIsParsable action = assertionAsProperty $ do
    nonEmptyServer <- nonEmptyServerIO
    let (Just cli@(1, conn)) = Mgnt.findConnectionById 1 nonEmptyServer
    _ <- swapMVar (Fake.contentMsg conn) (Fake.Msg $ Aeson.encode action)
    maybeAction <- recvAction cli
    assertEqual True (isJust maybeAction)
    let Just realAction = maybeAction
    assertEqual action realAction

prop_withoutClient :: Mgnt.ClientId -> Property
prop_withoutClient cid = assertionAsProperty $ do
    nonEmptyServer <- nonEmptyServerIO
    let conns = Mgnt.getConnections nonEmptyServer
    assertEqual True (cid `notElem` map fst (withoutClient cid conns))

prop_sendArbitraryRogueGameView :: Protocol.RogueGameView -> Property
prop_sendArbitraryRogueGameView rgv = assertionAsProperty $ do
    nonEmptyServer <- nonEmptyServerIO
    let (Just cli@(_, conn)) = Mgnt.findConnectionById 1 nonEmptyServer
    sendView cli rgv
    -- small hack, sendView saves the sent message which can be read out with receiveData
    Just rgv' <- Aeson.decode `fmap` receiveData conn
    assertEqual rgv rgv'

prop_sendArbitraryCatcherGameView :: Protocol.CatcherGameView -> Property
prop_sendArbitraryCatcherGameView cgv = assertionAsProperty $ do
    nonEmptyServer <- nonEmptyServerIO
    let (Just cli@(_, conn)) = Mgnt.findConnectionById 1 nonEmptyServer
    sendView cli cgv
    -- small hack, sendView saves the sent message which can be read out with receiveData
    Just cgv' <- Aeson.decode `fmap` receiveData conn
    assertEqual cgv cgv'


prop_broadcastCatcherGameView :: WithQCArgs (Protocol.CatcherGameView -> Property)
prop_broadcastCatcherGameView =
    withQCArgs
        (\args -> args { maxSuccess = 10 })
        prop_broadcastArbitraryCatcherGameView
    where
      prop_broadcastArbitraryCatcherGameView :: Protocol.CatcherGameView -> Property
      prop_broadcastArbitraryCatcherGameView cgv =
          assertionAsProperty
              $ do
                  nonEmptyServer <- nonEmptyServerIO
                  broadcast (Mgnt.getConnections nonEmptyServer) cgv
                  -- small hack, sendView saves the sent message which can be read out with receiveData
                  res <- mapM (receiveData . snd) (Mgnt.getConnections nonEmptyServer) :: IO (Seq LByteString)
                  let answers = map Aeson.decode res
                  assertBool (all (\(Just v) -> v == cgv) answers)

prop_broadcastRogueGameView :: WithQCArgs (Protocol.RogueGameView -> Property)
prop_broadcastRogueGameView =
    withQCArgs
        (\args -> args { maxSuccess = 10 })
        prop_broadcastArbitraryRogueGameView
    where
        prop_broadcastArbitraryRogueGameView :: Protocol.RogueGameView -> Property
        prop_broadcastArbitraryRogueGameView cgv =
            assertionAsProperty
                $ do
                    nonEmptyServer <- nonEmptyServerIO
                    broadcast (Mgnt.getConnections nonEmptyServer) cgv
                    -- small hack, sendView saves the sent message which can be read out with receiveData
                    res <- mapM (receiveData . snd) (Mgnt.getConnections nonEmptyServer) :: IO (Seq LByteString)
                    let answers = map Aeson.decode res
                    assertBool (all (\(Just v) -> v == cgv) answers)


test_receiveValidMessage :: IO ()
test_receiveValidMessage = do
    nonEmptyServer <- nonEmptyServerIO
    let (Just conn) = Mgnt.findConnectionById 2 nonEmptyServer
    maybeAction <- recvAction conn
    assertEqual True (isJust maybeAction)
    let Just realConn = maybeAction
    assertEqual anAction realConn
