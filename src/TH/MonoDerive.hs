{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module TH.MonoDerive
  ( Test(..)
  , monoidTh
  , semigroupTh
  , growingAppendTh
  , setContainerTh
  , monoFoldableTh
  , monoTraversableTh
  , monoFunctorTh
  ) where

import           ClassyPrelude
import           Language.Haskell.TH


newtype Test = Test { mapp :: Map Int String } deriving Show

monoidTh :: Name -> DecsQ
monoidTh name = do
    mem <- memptyTh
    man <- mappendTh
    let instanceType           = AppT (ConT ''Monoid) (ConT name)
    return [InstanceD Nothing [] instanceType [mem, man] ]
    where
      memptyTh :: DecQ
      memptyTh = do
          TyConI mn <- reify name
          con <- getNewTypeCon mn
          let mempty_ = mkName "mempty"
          let bodyExpr = [e| $(conE con) $(varE 'mempty) |]
          let cl = clause [] (normalB bodyExpr) []
          funD mempty_ [cl]

      mappendTh :: DecQ
      mappendTh = do
          TyConI mn <- reify name
          con <- getNewTypeCon mn
          firstName <- newName "a"
          secondName <- newName "b"
          let firstPat = conP con [varP firstName]
          let secondPat = conP con [varP secondName]
          let mappend_ = mkName "mappend"
          let bodyExpr = [e| $(conE con) $ $(varE 'mappend) $(varE firstName) $(varE secondName) |]
          let cl = clause [firstPat, secondPat] (normalB bodyExpr) []
          funD mappend_ [cl]

emptyDerive :: Name -> Name-> DecsQ
emptyDerive typeclass name = do
    let instanceType           = AppT (ConT typeclass) (ConT name)
    return [InstanceD Nothing [] instanceType [] ]

semigroupTh :: Name -> DecsQ
semigroupTh = emptyDerive ''Semigroup

growingAppendTh :: Name -> DecsQ
growingAppendTh = emptyDerive ''GrowingAppend

-- TODO: this is wrong
setContainerTh :: Name -> DecsQ
setContainerTh = emptyDerive ''SetContainer

monoTraversableTh :: Name -> DecsQ
monoTraversableTh name = do
    otraverse' <- otraverseTh
    let instanceType           = AppT (ConT ''MonoTraversable) (ConT name)
    return [InstanceD Nothing [] instanceType [otraverse'] ]
    where
      otraverseTh :: DecQ
      otraverseTh = do
          TyConI mn <- reify name
          con <- getNewTypeCon mn
          conVar <- newName "a"
          funName <- newName "f"
          let funcPat = varP funName
          let typePat = conP con [varP conVar]
          let otraverseName = mkName "otraverse"
          let bodyExpr = [e| $(varE 'map) $(conE con) $ $(varE 'otraverse) $(varE funName) $(varE conVar) |]
          let cl = clause [funcPat, typePat] (normalB bodyExpr) []
          funD otraverseName [cl]

monoFunctorTh :: Name -> DecsQ
monoFunctorTh name = do
    omapTh' <- omapTh
    let instanceType           = AppT (ConT ''MonoFunctor) (ConT name)
    return [InstanceD Nothing [] instanceType [omapTh'] ]
    where
      omapTh :: DecQ
      omapTh = do
          TyConI mn <- reify name
          con <- getNewTypeCon mn
          conVar <- newName "a"
          funName <- newName "f"
          let funcPat = varP funName
          let typePat = conP con [varP conVar]
          let otraverseName = mkName "omap"
          let bodyExpr = [e| $(conE con) $ $(varE 'omap) $(varE funName) $(varE conVar) |]
          let cl = clause [funcPat, typePat] (normalB bodyExpr) []
          funD otraverseName [cl]


monoFoldableTh :: Name -> DecsQ
monoFoldableTh typeName = do
    funcs <- sequenceA [ofoldMapTh, ofoldrTh, ofoldlTh', olengthTh, olength64Th, ofoldr1ExTh, ofoldl1ExTh']
    let instanceType           = AppT (ConT ''MonoFoldable) (ConT typeName)
    return [InstanceD Nothing [] instanceType funcs]

    where
      ofoldMapTh :: DecQ
      ofoldMapTh = do
          TyConI mn <- reify typeName
          con <- getNewTypeCon mn
          conVar <- newName "a"
          funName <- newName "f"
          let otraverseName = mkName "ofoldMap"
          let bodyExpr = [e| $(varE 'ofoldMap) $(varE funName) $(varE conVar) |]
          let cl = clause
                  [ varP funName
                  , conP con [varP conVar]
                  ]
                  (normalB bodyExpr)
                  []
          funD otraverseName [cl]

      ofoldrTh :: DecQ
      ofoldrTh = do
          TyConI mn <- reify typeName
          con <- getNewTypeCon mn
          conVar <- newName "a"
          funName <- newName "f"
          accName <- newName "acc"
          let funcName = mkName "ofoldr"
          let bodyExpr = [e| $(varE 'ofoldr) $(varE funName) $(varE accName) $(varE conVar) |]
          let cl = clause
                  [ varP funName
                  , conP con [varP conVar]
                  , varP accName
                  ]
                  (normalB bodyExpr)
                  []
          funD funcName [cl]

      ofoldlTh' :: DecQ
      ofoldlTh' = do
          TyConI mn <- reify typeName
          con <- getNewTypeCon mn
          conVar <- newName "a"
          funName <- newName "f"
          accName <- newName "acc"
          let funcName = mkName "ofoldl'"
          let bodyExpr = [e| $(varE 'ofoldl') $(varE funName) $(varE accName) $(varE conVar) |]
          let cl = clause
                  [ varP funName
                  , conP con [varP conVar]
                  , varP accName
                  ]
                  (normalB bodyExpr)
                  []
          funD funcName [cl]

      olengthTh :: DecQ
      olengthTh = do
          TyConI mn <- reify typeName
          con <- getNewTypeCon mn
          conVar <- newName "a"
          let funcName = mkName "olength"
          let bodyExpr = [e| $(varE 'olength) $(varE conVar) |]
          let cl = clause
                  [ conP con [varP conVar]
                  ]
                  (normalB bodyExpr)
                  []
          funD funcName [cl]

      olength64Th :: DecQ
      olength64Th = do
          TyConI mn <- reify typeName
          con <- getNewTypeCon mn
          conVar <- newName "a"
          let funcName = mkName "olength64"
          let bodyExpr = [e| $(varE 'olength64) $(varE conVar) |]
          let cl = clause
                  [ conP con [varP conVar]
                  ]
                  (normalB bodyExpr)
                  []
          funD funcName [cl]

      ofoldr1ExTh :: DecQ
      ofoldr1ExTh = do
          TyConI mn <- reify typeName
          con <- getNewTypeCon mn
          conVar <- newName "a"
          funName <- newName "f"
          let otraverseName = mkName "ofoldr1Ex"
          let bodyExpr = [e| $(varE 'ofoldr1Ex) $(varE funName) $(varE conVar) |]
          let cl = clause
                  [ varP funName
                  , conP con [varP conVar]
                  ]
                  (normalB bodyExpr)
                  []
          funD otraverseName [cl]

      ofoldl1ExTh' :: DecQ
      ofoldl1ExTh' = do
          TyConI mn <- reify typeName
          con <- getNewTypeCon mn
          conVar <- newName "a"
          funName <- newName "f"
          let otraverseName = mkName "ofoldl1Ex'"
          let bodyExpr = [e| $(varE 'ofoldl1Ex') $(varE funName) $(varE conVar) |]
          let cl = clause
                  [ varP funName
                  , conP con [varP conVar]
                  ]
                  (normalB bodyExpr)
                  []
          funD otraverseName [cl]

getNewTypeCon :: Dec -> Q Name
getNewTypeCon dec =
    case dec of
      (NewtypeD _ _ _ _ (RecC con _) _) -> return con
      (NewtypeD _ _ _ _ (NormalC con _) _) -> return con
      _ -> fail "Only Newtype datastructures are allowed"

{--
TODO: can this instance be derived?
-- answer: probably
type instance Element PlayerPositions = Node

instance SetContainer PlayerPositions where
    -- TODO: could this be somehow done?
    type ContainerKey PlayerPositions = Player
    member p = member p . playerPositions
    notMember p = notMember p . playerPositions
    union pp1 pp2 = PlayerPositions $ union (playerPositions pp1) (playerPositions pp2)
    difference pp1 pp2 = PlayerPositions $ difference (playerPositions pp1) (playerPositions pp2)
    intersection pp1 pp2 = PlayerPositions $ intersection (playerPositions pp1) (playerPositions pp2)
    keys = keys . playerPositions
instance IsMap PlayerPositions where
    -- TODO: could this be somehow done?
    type MapValue PlayerPositions = Node
    lookup k = lookup k . playerPositions
    insertMap k v = PlayerPositions . insertMap k v . playerPositions
    deleteMap k = PlayerPositions . deleteMap k . playerPositions
    singletonMap k v = PlayerPositions $ singletonMap k v
    mapFromList = PlayerPositions . mapFromList
    mapToList = mapToList . playerPositions
--}
