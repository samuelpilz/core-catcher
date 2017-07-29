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
      memptyTh = simpleWrap "mempty" name

      mappendTh :: DecQ
      mappendTh = simpleBinOp "mappend" name

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
          con <- getNewTypeCon name
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
      omapTh = simpleMap "omap" name


monoFoldableTh :: Name -> DecsQ
monoFoldableTh typeName = do
    funcs <- sequenceA [ofoldMapTh, ofoldrTh, ofoldlTh', olengthTh, olength64Th, ofoldr1ExTh, ofoldl1ExTh']
    let instanceType           = AppT (ConT ''MonoFoldable) (ConT typeName)
    return [InstanceD Nothing [] instanceType funcs]
    where
      ofoldMapTh :: DecQ
      ofoldMapTh = simpleFold1 "ofoldMap" typeName

      ofoldrTh :: DecQ
      ofoldrTh = simpleFold "ofoldr" typeName

      ofoldlTh' :: DecQ
      ofoldlTh' = simpleFold "ofoldl'" typeName

      olengthTh :: DecQ
      olengthTh = simplePattern "olength" typeName

      olength64Th :: DecQ
      olength64Th = simplePattern "olength64" typeName

      ofoldr1ExTh :: DecQ
      ofoldr1ExTh = simpleFold1 "ofoldr1Ex" typeName

      ofoldl1ExTh' :: DecQ
      ofoldl1ExTh' = simpleFold1 "ofoldl1Ex'" typeName

simpleWrap :: String -> Name -> DecQ
simpleWrap realFunName typeName = do
    con <- getNewTypeCon typeName
    let funcName = mkName realFunName
    let bodyExpr = [e| $(conE con) $(varE funcName) |]
    let cl = clause
            []
            (normalB bodyExpr)
            []
    funD funcName [cl]

simplePattern :: String -> Name -> DecQ
simplePattern realFunName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    let funcName = mkName realFunName
    let bodyExpr = [e| $(varE funcName) $(varE conVar) |]
    let cl = clause
            [ conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleFold :: String -> Name -> DecQ
simpleFold realFuncName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    funName <- newName "f"
    accName <- newName "acc"
    let funcName = mkName realFuncName
    let bodyExpr = [e| $(varE funcName) $(varE funName) $(varE accName) $(varE conVar) |]
    let cl = clause
            [ varP funName
            , varP accName
            , conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleFold1 :: String -> Name -> DecQ
simpleFold1 realFuncName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    funName <- newName "f"
    let funcName = mkName realFuncName
    let bodyExpr = [e| $(varE funcName) $(varE funName) $(varE conVar) |]
    let cl = clause
            [ varP funName
            , conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleMap :: String -> Name -> DecQ
simpleMap realFuncName name = do
    con <- getNewTypeCon name
    conVar <- newName "a"
    funName <- newName "f"
    let funcPat = varP funName
    let typePat = conP con [varP conVar]
    let funcName = mkName realFuncName
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE funName) $(varE conVar) |]
    let cl = clause [funcPat, typePat] (normalB bodyExpr) []
    funD funcName [cl]

simpleBinOp :: String -> Name -> DecQ
simpleBinOp realFuncName name = do
    con <- getNewTypeCon name
    firstName <- newName "a"
    secondName <- newName "b"
    let firstPat = conP con [varP firstName]
    let secondPat = conP con [varP secondName]
    let funcName = mkName realFuncName
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE firstName) $(varE secondName) |]
    let cl = clause [firstPat, secondPat] (normalB bodyExpr) []
    funD funcName [cl]


emptyDerive :: Name -> Name-> DecsQ
emptyDerive typeclass name = do
    let instanceType           = AppT (ConT typeclass) (ConT name)
    return [InstanceD Nothing [] instanceType [] ]

getNewTypeCon :: Name -> Q Name
getNewTypeCon typeName = do
    TyConI mn <- reify typeName
    case mn of
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
