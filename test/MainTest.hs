{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MainTest where

import           ClassyPrelude
import           Mock.Connection
import           Test.Framework


test_mainDummy :: IO ()
test_mainDummy =
    assertEqual 1 1
