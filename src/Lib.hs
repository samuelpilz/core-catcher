{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib
    ( scanrM
    , scanrTailM
    , scanrInitM
    ) where

import           ClassyPrelude
import           Control.Monad ()
import           Data.Foldable ()

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
    a []     = z0
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
