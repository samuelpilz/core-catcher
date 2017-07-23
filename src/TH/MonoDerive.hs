{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module TH.MonoDerive where

import           ClassyPrelude
import           Language.Haskell.TH

{--
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


genAdd :: Int -> Q Exp
genAdd n = [| (+n) |]
--}
{--
mapM :: Int -> Q Dec
mapM n
    | n >= 1 = funD name [cl1, cl2]
    | otherwise = fail "mapN: argument n may not be <= 0."
    where
        name = mkName $ "map" ++ show n
        cl1 = do
            f <- newName "f"
            xs <- replicateM n (newName "x")
            ys <- replicateM n (newName "ys")
            let   argPatts  = varP f : consPatts
                  consPatts = [ [p| $(varP x) : $(varP ys) |] | (x,ys) <- xs `zip` ys ]
                  apply     = foldl' (\ g x -> [| $g $(varE x) |])
                  first     = apply (varE f) xs
                  rest      = apply (varE name) (f:ys)
            clause argPatts (normalB [| $first : $rest |]) []


        cl2  = clause (replicate (n+1) wildP) (normalB (conE '[])) []
--}

newtype Test = Test { map :: Map Int String } deriving Show

f :: DecsQ
f = do
    Just mn <- lookupTypeName "Test"
    TyConI (NewtypeD _ name _ _ (RecC con _) _) <- reify mn
    firstName <- newName "a"
    secondName <- newName "b"
    let first' = conP con [varP firstName]
    let second' = conP con [varP secondName]

    t <- instanceD (cxt []) (return undefined) []
    return [t]
    --[d| instance Monoid $(conT name) where mempty = $(conE con) ClassyPrelude.mempty; mappend $first' $second' = $(conE con) $ ( $(varE firstName) ++ $(varE secondName) ) |]

{--
TyConI
  (NewtypeD
    []
    Network.Protocol.PlayerPositions
    []
    Nothing
    (RecC
      Network.Protocol.PlayerPositions
        [ (Network.Protocol.playerPositions
        , Bang NoSourceUnpackedness NoSourceStrictness
        , AppT (AppT (ConT Data.Map.Base.Map) (ConT Network.Protocol.Player)) (ConT Network.Protocol.Node))
        ]
    )
    []
    )

TyConI
  ( NewtypeD
      []
      TH.MonoDerive.Test
      []
      Nothing
      ( NormalC
        TH.MonoDerive.Test
          [(Bang NoSourceUnpackedness NoSourceStrictness
          , AppT (AppT (ConT Data.Map.Base.Map) (ConT GHC.Types.Int)) (ConT GHC.Base.String))
          ]
      )
      []
      )
--}
