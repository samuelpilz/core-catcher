module Network exposing (..)

{- 
 - one possibility for Network type
-}

import List exposing (..)
import Dict exposing (..)


type alias Node =
    Int


type alias Edge =
    ( Node, Node )


type alias Transportation =
    String


type alias Color =
    String


type alias EdgeWidth =
    Int


type alias Network =
    Dict Transportation NetworkOverlay


type alias NetworkOverlay =
    ( List Node, List Edge )


type alias NetworkDisplayInfo =
    ( Dict Transportation Color, Dict Transportation EdgeWidth, Dict Node ( Int, Int ) )
