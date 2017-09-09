{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Network.ElmDerive
import ClassyPrelude

main :: IO ()
main = do
    writeFileUtf8 "frontend/Protocol.elm" elmProtocolModule
    putStrLn "frontend/Protocol.elm updated"

