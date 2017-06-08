{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           ClassyPrelude

import           Test.Framework

import {-@ HTF_TESTS @-} ProtocolTest
import {-@ HTF_TESTS @-} SpecTest
import {-@ HTF_TESTS @-} MainTest

main :: IO ()
main = htfMain htf_importedTests
