{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- |Module for testing the gameNg module.

The main function to execute test cases is the gameNgTestCase function defined in this module.

-}
module GameConfigTest where

import           ClassyPrelude
import           Config.GameConfig
import qualified Config.Network    as Network
import           Data.List         (nub)
import           Network.Protocol
import           System.Random     (mkStdGen)
import           Test.Framework

-- TODO: redo
-- prop_randomPlayerPositions_noOverlap :: Int -> Bool
-- prop_randomPlayerPositions_noOverlap seed =
--     posNodes == nub posNodes
--     where
--         posNodes = posNodesOfRandom seed
--
-- prop_randomPlayerPositions_noOutOfBound :: Int -> Bool
-- prop_randomPlayerPositions_noOutOfBound seed =
--     all (\(Node n) -> 1 <= n && n <= defaultNodeNum) posNodes
--     where
--         posNodes = posNodesOfRandom seed
--
-- defaultNodeNum :: Int
-- defaultNodeNum = length . nodes $ Network.network
--
-- posNodesOfRandom :: Int -> [Node]
-- posNodesOfRandom =
--     map snd .
--     mapToList .
--     initialPlayerPositions .
--     defaultConfigWithRandomPositions .
--     mkStdGen
--
