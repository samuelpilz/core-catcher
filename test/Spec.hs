{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
import           ClassyPrelude
import           Lib
import           Test.Framework

main :: IO ()
main = htfMain htf_thisModulesTests

test_mapRightDoubleValue :: IO ()
test_mapRightDoubleValue =
    let
      x = Right 3 :: Either String Int
      y = Right 8 :: Either String Int
    in
      assertEqual y (mapRight (2*) x)

prop_mapRightWithIdFunction :: Either String Int -> Bool
prop_mapRightWithIdFunction x =
    x == mapRight id x
