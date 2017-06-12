{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           ClassyPrelude

import           Test.Framework

import {-@ HTF_TESTS @-} ProtocolTest
import {-@ HTF_TESTS @-} SpecTest
import {-@ HTF_TESTS @-} MainTest
import {-@ HTF_TESTS @-} StateTest
import {-@ HTF_TESTS @-} GameLogicTest

main :: IO ()
main = htfMain htf_importedTests
