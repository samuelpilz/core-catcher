{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           ClassyPrelude

import           Test.Framework

import {-@ HTF_TESTS @-} EntityMgntTest
import {-@ HTF_TESTS @-} GameNgTest
import {-@ HTF_TESTS @-} AppTest

main :: IO ()
main = do
    args <- getCurrentArgs
    setDefaultArgs (args { maxSuccess = 25 } )
    htfMain htf_importedTests
