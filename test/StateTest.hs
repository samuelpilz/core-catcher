{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StateTest where

import           ClassyPrelude
import           Test.Framework

test_stateDummy :: IO ()
test_stateDummy =
    assertEqual 1 1
