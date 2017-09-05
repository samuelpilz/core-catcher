{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           ClassyPrelude

import           Test.Framework

import {-@ HTF_TESTS @-} ProtocolTest
import {-@ HTF_TESTS @-} ConnectionMgntTest
import {-@ HTF_TESTS @-} GameNgTest

main :: IO ()
main = do
    args <- getCurrentArgs
    setDefaultArgs (args { maxSuccess = 15 } ) 
    htfMain htf_importedTests
