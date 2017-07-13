{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GameNgTest where

import           ClassyPrelude
import           GameNg
import           Network.Protocol
import           Test.Framework
import           Test.HUnit.Base

test_initialStateHasStartingPlayer0 :: IO ()
test_initialStateHasStartingPlayer0 =
    Player 0 @?= stateNextPlayer initialState

test_initialStateHasEmptyHistory :: IO ()
test_initialStateHasEmptyHistory =
    RogueHistory [] @?= stateRogueHistory initialState

prop_networkStaysSameAfterActions :: Action -> Bool
prop_networkStaysSameAfterActions a =
    case updateState a initialState of
        Left (GameError _) -> True
        Right (newState, _, _) ->
            stateNetwork newState == stateNetwork initialState
