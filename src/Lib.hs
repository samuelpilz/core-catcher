{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib
    ( someFunc
    , scanrM
    , scanrTailM
    , scanrInitM
    , mapLeft
    , mapRight
    , maybeToEither
    ) where

import           ClassyPrelude
import           Control.Monad ()
import           Data.Foldable () -- foldrM

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- maps a Left of an Either
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft fn (Left a) = Left $ fn a
mapLeft _ (Right c) = Right c

-- maps a Right of an Either
mapRight :: (a -> b) -> Either c a -> Either c b
mapRight fn (Right a) = Right $ fn a
mapRight _ (Left c) = Left c

-- like scanr but with Monad support
scanrM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> m a -> mono -> m [a]
scanrM fn z0 ls = do (x, xs) <- scanrM' fn z0 ls; return (x:xs)

-- like scanrM but skips first element
scanrTailM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> m a -> mono -> m [a]
scanrTailM fn z0 ls = do (_, xs) <- scanrM' fn z0 ls; return xs

-- like scanrM but skips last element
scanrInitM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> m a -> mono -> m [a]
scanrInitM fn z0 =
  let
    f b ma = do ls <- ma; a_ <- a ls; n <- fn a_ b; return (n:ls)
    a [] = z0
    a (a':_) = return a'
    start = return []
  in
    foldr f start

scanrM' :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> m a -> mono -> m (a, [a])
scanrM' fn z0 =
  let
    f b ma = do (a, as) <- ma; n <- fn a b; return (n, a:as)
    start = do n <- z0; return (n, [])
  in
    foldr f start

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing b = Left b
