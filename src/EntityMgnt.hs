{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

-- TODO: remake comment
-- TODO: test

module EntityMgnt where

import           ClassyPrelude

-- TODO: instance MonoFoldable & MonoTraversable for e
data Entities i e =
    Entities
        { entities :: Map i e
        , nextId   :: i
        }

emptyEntities :: EntityId i => Entities i e
emptyEntities =
    Entities
        { entities = mempty
        , nextId = getFirstId
        }

class Ord i => EntityId i where
    getFirstId :: i
    getNextId :: i -> i

class EntityId i => HasEntities state i where
    type Entity state i :: *

    getEntities :: state -> Entities i (Entity state i)
    setEntities :: Entities i (Entity state i) -> state -> state

    -- |add an entity with the next id. The given id is returned.
    addEntity :: Entity state i -> state -> (i, state)
    addEntity entity state =
        let
            es@Entities{entities, nextId} = getEntities state
            newEntities =
                es
                    { entities =
                        insertMap nextId entity entities
                    , nextId = getNextId nextId
                    }
        in
            ( nextId
            , setEntities newEntities state
            )


    -- |set an entity value. The entity should already exist in the map.
    updateEntity :: i -> Entity state i -> state -> state
    updateEntity eId entity state =
        let
            es@Entities{entities} = getEntities state
            newEntities =
                es
                    { entities =
                        insertMap eId entity entities
                    }
        in
            setEntities newEntities state

    -- |Modifies an entity according to a given function.
    modifyEntity :: i -> (Entity state i -> Entity state i) -> state -> state
    modifyEntity eId f state =
        let
            es@Entities{entities} = getEntities state
            newEntities = case findEntityById eId state of
                Nothing -> es
                Just e ->
                    es
                        { entities =
                            insertMap eId (f e) entities
                        }
        in
            setEntities newEntities state

    -- TODO: split into remove and removeAndGet
    -- |Remove an entity from the map and return the assigned connection
    removeEntity :: i -> state -> (state, Maybe (Entity state i))
    removeEntity eId state =
        let
            es@Entities{entities} = getEntities state
            (oldValMay, newMap) = updateLookupWithKey (\_ _ -> Nothing) eId entities
            newEntities = es { entities = newMap }
        in
            (setEntities newEntities state, oldValMay)

    -- extra functions
    findEntityById :: i -> state -> Maybe (Entity state i)
    findEntityById gId =
        lookup gId . entities . getEntities

    entityMap :: state -> Map i (Entity state i)
    entityMap = entities . getEntities

    entityAssocs :: state -> [(i, Entity state i)]
    entityAssocs = mapToList . entityMap

-- implement HasEntities for Entities themselves
-- instance EntityId i => HasEntities (Entities i e) i where
--     type Entity (Entities i e) i = e
--     getEntities = id
--     setEntities = flip const
