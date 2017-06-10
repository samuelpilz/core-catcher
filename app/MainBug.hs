{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
code taken from tutorial
https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm
and code from
https://gitlab.com/paramander/typesafe-websockets/blob/master/src/Main.hs
-}

module Main where

import           ClassyPrelude

import qualified Data.Map      as Map
import           GHC.Generics  ()

import           System.IO     (getLine)

-- |Transport is a text
newtype Transport = Transport String
    --Transport { transportName :: String }
    deriving (Show, Read, Eq, Ord, Generic)

main :: IO ()
main = do
    putStrLn "energyMap:"
    print energyMap
    putStrLn "using assocs"
    mapM_ (\(k,v) -> putStrLn (tshow k ++ " -> " ++ tshow v)) $ Map.assocs energyMap
    putStrLn ""

    putStrLn "using lookup"
    mapM_ (\k -> putStrLn (tshow k ++ " -> " ++ tshow (Map.lookup k energyMap))) $ Map.keys energyMap
    putStrLn ""

    mapM_ (\(t1, t2) -> print (t1, t2, compare t1 t2)) $ cartProd (Map.keys energyMap) (Map.keys energyMap)

energyMap :: Map Transport Int
energyMap =
    Map.fromDistinctAscList [ ( Transport "taxi", 5 )
        , ( Transport "bus", 3 )
        , ( Transport "underground", 2 )
        ]

cartProd xs ys = [(x,y) | x <- xs, y <- ys]
