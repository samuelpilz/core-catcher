{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module TH.MonoFunctions where


import           ClassyPrelude
import           Language.Haskell.TH

simpleWrap :: Name -> Name -> DecQ
simpleWrap funcName typeName = do
    con <- getNewTypeCon typeName
    let bodyExpr = [e| $(conE con) $(varE funcName) |]
    let cl = clause
            []
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleWrap1 :: Name -> Name -> DecQ
simpleWrap1 funcName typeName = do
    con <- getNewTypeCon typeName
    let bodyExpr = [e| $(conE con) . $(varE funcName) |]
    let cl = clause
            []
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleWrap2 :: Name -> Name -> DecQ
simpleWrap2 funcName typeName = do
    con <- getNewTypeCon typeName
    keyVar <- newName "k"
    valueVar <- newName "v"
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE keyVar) $(varE valueVar) |]
    let cl = clause
            [ varP keyVar
            , varP valueVar
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simplePattern :: Name -> Name -> DecQ
simplePattern funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    let bodyExpr = [e| $(varE funcName) $(varE conVar) |]
    let cl = clause
            [ conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleUnwrap :: Name -> Name -> DecQ
simpleUnwrap funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    otherVar <- newName "p"
    let bodyExpr = [e| $(varE funcName) $(varE otherVar) $(varE conVar) |]
    let cl = clause
            [ varP otherVar
            , conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

-- TODO: naming is fucked up
simpleUnwrap1 :: Name -> Name -> DecQ
simpleUnwrap1 funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    let bodyExpr = [e| $(varE funcName) $(varE conVar) |]
    let cl = clause
            [ conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]


simpleUnwrapWrap :: Name -> Name -> DecQ
simpleUnwrapWrap funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE conVar) |]
    let cl = clause
            [ conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleUnwrapWrap1' :: Name -> Name -> DecQ
simpleUnwrapWrap1' funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    keyVar <- newName "k"
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE conVar) $(varE keyVar) |]
    let cl = clause
            [ conP con [varP conVar]
            , varP keyVar
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleUnwrapWrap1 :: Name -> Name -> DecQ
simpleUnwrapWrap1 funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    keyVar <- newName "k"
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE keyVar) $(varE conVar) |]
    let cl = clause
            [ varP keyVar
            , conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleUnwrapWrap2 :: Name -> Name -> DecQ
simpleUnwrapWrap2 funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    keyVar <- newName "k"
    valueVar <- newName "v"
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE keyVar) $(varE valueVar) $(varE conVar) |]
    let cl = clause
            [ varP keyVar
            , varP valueVar
            , conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleFold :: Name -> Name -> DecQ
simpleFold funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    funName <- newName "f"
    accName <- newName "acc"
    let bodyExpr = [e| $(varE funcName) $(varE funName) $(varE accName) $(varE conVar) |]
    let cl = clause
            [ varP funName
            , varP accName
            , conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleFold1 :: Name -> Name -> DecQ
simpleFold1 funcName typeName = do
    con <- getNewTypeCon typeName
    conVar <- newName "a"
    funName <- newName "f"
    let bodyExpr = [e| $(varE funcName) $(varE funName) $(varE conVar) |]
    let cl = clause
            [ varP funName
            , conP con [varP conVar]
            ]
            (normalB bodyExpr)
            []
    funD funcName [cl]

simpleMap :: Name -> Name -> DecQ
simpleMap funcName name = do
    con <- getNewTypeCon name
    conVar <- newName "a"
    funName <- newName "f"
    let typePat = conP con [varP conVar]
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE funName) $(varE conVar) |]
    let cl = clause [varP funName, typePat] (normalB bodyExpr) []
    funD funcName [cl]

simpleBinOp :: Name -> Name -> DecQ
simpleBinOp funcName name = do
    con <- getNewTypeCon name
    firstName <- newName "a"
    secondName <- newName "b"
    let firstPat = conP con [varP firstName]
    let secondPat = conP con [varP secondName]
    let bodyExpr = [e| $(conE con) $ $(varE funcName) $(varE firstName) $(varE secondName) |]
    let cl = clause [firstPat, secondPat] (normalB bodyExpr) []
    funD funcName [cl]


getNewTypeCon :: Name -> Q Name
getNewTypeCon typeName = do
    TyConI mn <- reify typeName
    case mn of
      (NewtypeD _ _ _ _ (RecC con _) _) -> return con
      (NewtypeD _ _ _ _ (NormalC con _) _) -> return con
      _ -> fail "Only Newtype datastructures are allowed"
