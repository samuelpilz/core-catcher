{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Network where

import           ClassyPrelude
import           Data.List        (nub)
import           Network.Protocol

network :: Network
network =
    Network { nodes = map Node [1..17]
    , overlays = mapFromList
        [ ( Red, redOverlay )
        , ( Blue, blueOverlay )
        , ( Orange, orangeOverlay )
        ]
    }


redOverlay :: NetworkOverlay
redOverlay =
    mkOverlayCompact
        [ [1, 6, 13, 9]
        ]


blueOverlay :: NetworkOverlay
blueOverlay =
    mkOverlayCompact
        [ [ 3, 4, 15, 12, 8, 17, 3 ]
        ]


orangeOverlay :: NetworkOverlay
orangeOverlay =
    mkOverlayCompact
        [ [ 6, 10, 4, 7, 16, 15, 14 ]
        , [ 1, 11, 6, 3, 5 ]
        , [ 2, 5, 8, 2 ]
        , [ 5, 12, 13, 14 ]
        , [ 8, 9 ]
        , [ 3, 7 ]
        , [ 1, 17, 2 ]
        ]


mkOverlayCompact :: [[Int]] -> NetworkOverlay
mkOverlayCompact lists =
    mkOverlay $
        concatMap
            (\l ->
                zip
                    l
                    (tailDef l)
            )
            lists


mkOverlay :: [(Int, Int)] -> NetworkOverlay
mkOverlay list =
    NetworkOverlay { overlayNodes =
        map Node
            . nub
            . unzipConcat
        $
            list
    , overlayEdges =  map (Edge . (***) Node Node) list
    }

unzipConcat :: [(a,a)] -> [a]
unzipConcat = uncurry (++) . unzip
