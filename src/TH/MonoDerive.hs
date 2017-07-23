{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TH.MonoDerive where

import           ClassyPrelude
import           Language.Haskell.TH

curryN :: Int -> ExpQ
curryN n = do
    f <- newName "f"
    xs <- replicateM n (newName "x")
    let args = map VarP (f:xs)
        ntup = TupE (map VarE xs)
    return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where
    mkCurryDec :: Int -> Q Dec
    mkCurryDec ith = do
          cury <- curryN ith
          let name = mkName $ "curry" ++ show ith
          return $ FunD name [Clause [] (NormalB cury) []]
