{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           ClassyPrelude

import           Test.Framework

-- TODO: reenable tests
import {-@ HTF_TESTS @-} ProtocolTest
--import {-@ HTF_TESTS @-} ConnectionMgntTest
import {-@ HTF_TESTS @-} GameNgTest
-- import {-@ HTF_TESTS @-} AppTest
import {-@ HTF_TESTS @-} GameConfigTest

main :: IO ()
main = do
    args <- getCurrentArgs
    setDefaultArgs (args { maxSuccess = 15 } )
    htfMain htf_importedTests
