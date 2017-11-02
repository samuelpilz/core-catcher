{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EntityMgntTest where

import           ClassyPrelude
import           EntityMgnt
import           Test.Framework
import           Test.HUnit.Base

-- wrapper for Entities to reduce the need of type-annotations in tests

newtype IntEntities = IntEntities (Entities Int Int)

instance HasEntities IntEntities Int where
    type Entity IntEntities Int = Int
    getEntities = getIntEntities
    setEntities = const . IntEntities

getIntEntities :: IntEntities -> Entities Int Int
getIntEntities (IntEntities e) = e

findEntityByIntId :: Int -> IntEntities -> Maybe Int
findEntityByIntId = findEntityById

addIntEntity :: Int -> IntEntities -> (Int, IntEntities)
addIntEntity = addEntity

removeIntEntity :: Int -> IntEntities -> IntEntities
removeIntEntity = removeEntity

allIntEntities :: IntEntities -> [(Int, Int)]
allIntEntities = allEntities

emptyIntEntities :: IntEntities
emptyIntEntities = IntEntities emptyEntities

singletonEntities :: Int -> IntEntities
singletonEntities i = snd . addIntEntity i $ emptyIntEntities


-- Test cases

test_emptyEntities :: IO ()
test_emptyEntities =
    let
        es :: Entities Int Int
        es = emptyEntities
    in do
    nextId es @?= 0
    entities es @?= mapFromList []


test_addEntity :: IO ()
test_addEntity =
    let
        (eId, es) = addIntEntity 1 emptyIntEntities
    in do
        eId @?= 0
        findEntityByIntId 0 es @?= Just 1
        nextId (getIntEntities es) @?= 1


test_removeEntity :: IO ()
test_removeEntity =
    let
        es = removeIntEntity 0 $ singletonEntities 2
    in do
        findEntityByIntId 0 es @?= Nothing
        nextId (getIntEntities es) @?= 1


test_removeEntity_notContained :: IO ()
test_removeEntity_notContained =
    let
        es = removeIntEntity 1 $ singletonEntities 2
    in
        -- mapping 0->2 is still there
        findEntityByIntId 0 es @?= Just 2


test_findEntityById_notContained :: IO ()
test_findEntityById_notContained =
    findEntityByIntId 1 (singletonEntities 2) @?= Nothing


-- test_addMultipleConnections :: IO ()
-- test_addMultipleConnections = do
--     conns <- emptyFakeConnectionsTVar
--     conn1 <- newFakeConnection
--     conn2 <- newFakeConnection
--     conn3 <- newFakeConnection
--     cId1 <- atomically $ connectClient conns conn1
--     cId2 <- atomically $ connectClient conns conn2
--     cId3 <- atomically $ connectClient conns conn3
--
--     0 @?= cId1
--     1 @?= cId2
--     2 @?= cId3
--
--
--     newConns <- readTVarIO conns
--     3 @?= length (connections newConns)
--
--
--
-- test_withoutClientNotContained = do
--     connsTVar <- fakeConnectionsTVar
--     conns <- readTVarIO connsTVar
--     [0,1,2] @?= (map fst . mapToList . connections . withoutClient 3 $ conns)
