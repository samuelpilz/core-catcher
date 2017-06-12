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


nonEmptyServer :: ServerState Fake.FakeConnection
nonEmptyServer =
    ServerState
        { gameState = exampleGameState
        , connections = fromList $
            [ (1, Fake.connectionWith "We are Legion")
            , (2, Fake.connectionWith
                ( Aeson.encode anAction
                )
            )
            ]
            ++
            [ (i, Fake.emptyConnection)
            | i <- [3..100]
            ]
        }

test_receiveValidMessage :: IO ()
test_receiveValidMessage = do
    let (Just conn) = Mgnt.findConnectionById 2 nonEmptyServer
    maybeAction <- recvAction conn
    assertEqual True (isJust maybeAction)
    let Just realConn = maybeAction
    assertEqual anAction realConn

test_receiveInvalidMessage :: IO ()
test_receiveInvalidMessage = do
    let (Just conn) = Mgnt.findConnectionById 1 nonEmptyServer
    maybeAction <- recvAction conn
    assertEqual True (isNothing maybeAction)

prop_arbitraryActionIsParsable :: Protocol.Action -> Property
prop_arbitraryActionIsParsable action = assertionAsProperty $ do
    let (Just cli@(1, conn)) = Mgnt.findConnectionById 1 nonEmptyServer
    _ <- swapMVar (Fake.contentMsg conn) (Fake.Msg $ Aeson.encode action)
    maybeAction <- recvAction cli
    assertEqual True (isJust maybeAction)
    let Just realAction = maybeAction
    assertEqual action realAction

prop_withoutClient :: Mgnt.ClientId -> Property
prop_withoutClient cid = assertionAsProperty $ do
    let conns = Mgnt.getConnections nonEmptyServer
    assertEqual True (cid `notElem` map fst (withoutClient cid conns))

prop_sendArbitraryRogueGameView :: Protocol.RogueGameView -> Property
prop_sendArbitraryRogueGameView rgv = assertionAsProperty $ do
    let (Just cli@(_, conn)) = Mgnt.findConnectionById 1 nonEmptyServer
    sendView rgv cli
    -- small hack, sendView saves the sent message which can be read out with receiveData
    Just rgv' <- Aeson.decode `fmap` receiveData conn
    assertEqual rgv rgv'

prop_sendArbitraryCatcherGameView :: Protocol.CatcherGameView -> Property
prop_sendArbitraryCatcherGameView cgv = assertionAsProperty $ do
    let (Just cli@(_, conn)) = Mgnt.findConnectionById 1 nonEmptyServer
    sendView cgv cli
    -- small hack, sendView saves the sent message which can be read out with receiveData
    Just cgv' <- Aeson.decode `fmap` receiveData conn
    assertEqual cgv cgv'

prop_broadcastArbitraryCatcherGameView :: Protocol.CatcherGameView -> Property
prop_broadcastArbitraryCatcherGameView cgv =
    assertionAsProperty
        $ do
            broadcast cgv (Mgnt.getConnections nonEmptyServer)
            -- small hack, sendView saves the sent message which can be read out with receiveData
            res <- mapM (receiveData . snd) (Mgnt.getConnections nonEmptyServer) :: IO (Seq LByteString)
            let answers = map Aeson.decode res
            assertBool (all (\(Just v) -> v == cgv) answers)
