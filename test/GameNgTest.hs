{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GameNgTest where

import ClassyPrelude
import Data.Map.Strict (insert)
import GameNg
import Network.Protocol
import Test.Framework
import Test.HUnit.Base

-- (@?=) = assertEqual
test_initialStateHasStartingPlayer0 :: IO ()
test_initialStateHasStartingPlayer0 = Player 0 @?= stateNextPlayer initialState

test_initialStateHasEmptyHistory :: IO ()
test_initialStateHasEmptyHistory =
    RogueHistory [] @?= stateRogueHistory initialState

test_player0ValidMove :: IO ()
test_player0ValidMove =
    case updateState (Move (Player 0) (Transport "red") (Node 6)) initialState of
        Left (GameError err) -> assertFailure . unpack $ "action failed: " ++ err
        Right (newState, _, _) ->
            (insert (Player 0) (Node 6) . playerPositions . statePlayerPositions $
             initialState) @?=
            (playerPositions . statePlayerPositions $ newState)
