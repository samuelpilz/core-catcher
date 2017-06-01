{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           ClassyPrelude

import           Test.Framework

import {-@ HTF_TESTS @-} ProtocolTest
import {-@ HTF_TESTS @-} SpecTest

main :: IO ()
main = htfMain htf_importedTests
