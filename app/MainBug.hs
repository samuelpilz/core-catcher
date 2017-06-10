{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
code taken from tutorial
https://www.paramander.com/blog/playing-with-websockets-in-haskell-and-elm
and code from
https://gitlab.com/paramander/typesafe-websockets/blob/master/src/Main.hs
-}

module Main where

import           Prelude

import           Data.Map     (Map)
import qualified Data.Map     as Map
import           GHC.Generics (Generic)

import           System.IO    (getLine)


type Transport = String
-- |Transport is a text
--newtype Transport = Transport String
    --Transport { transportName :: String }
    --deriving (Show, Read, Eq, Ord, Generic)

main :: IO ()
main = do
    putStrLn "energyMap:"
    print energyMap
    putStrLn ""

    putStrLn "using assocs"
    mapM_ (\(k,v) -> putStrLn (show k ++ " -> " ++ show v)) $ Map.assocs energyMap
    putStrLn ""

    putStrLn "using lookup"
    mapM_ (\k -> putStrLn (show k ++ " -> " ++ show (Map.lookup k energyMap))) $ Map.keys energyMap
    putStrLn ""

    mapM_ (\(t1, t2) -> putStrLn $ t1 ++ (if t1 < t2 then "<" else if t1 > t2 then ">" else "=") ++ t2)
        $ cartProd (Map.keys energyMap) (Map.keys energyMap)

energyMap :: Map Transport Int
energyMap =
    Map.fromDistinctAscList
        [ ( "t", 0 )
        , ( "b", 0 )
        , ( "u", 0 )
        ]

cartProd xs ys = [(x,y) | x <- xs, y <- ys]
