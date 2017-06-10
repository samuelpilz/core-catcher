{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MainTest where

import           ClassyPrelude
import           Test.Framework
import           Mock.Connection

test_dummy :: IO ()
test_dummy = func
