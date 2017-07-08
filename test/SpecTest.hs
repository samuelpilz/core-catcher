{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SpecTest where
import           ClassyPrelude
import           GameLogic      ((>>|))

import           Test.Framework

test_dummy :: IO ()
test_dummy = do
    actual <- (return >>| (\a _ -> return a)) (1 :: Int)
    let expected  = (1,1)
    assertEqual expected actual
