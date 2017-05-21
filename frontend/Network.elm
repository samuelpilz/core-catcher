module Network exposing (..)

{-
   - one possibility for Network type
-}

import List exposing (..)
import Dict exposing (..)
import Maybe exposing (..)


type alias Node =
    Int


type alias Edge =
    ( Node, Node )


type alias Transportation =
    String


type alias Network =
    ( List Node, Dict Transportation NetworkOverlay )


type alias NetworkOverlay =
    ( List Node, List Edge )


type alias Color =
    String


type alias EdgeWidth =
    Int


type alias NodeSize =
    Int


type alias ColorMap =
    Dict Transportation Color


type alias EdgeWidthMap =
    Dict Transportation EdgeWidth


type alias NodeSizeMap =
    Dict Transportation NodeSize


type alias NodeXyMap =
    Dict Node ( Int, Int )


type alias NetworkDisplayInfo =
    ( ColorMap, EdgeWidthMap, NodeSizeMap, NodeXyMap, List Transportation )


type alias OverlayDisplayInfo =
    ( Color, EdgeWidth, NodeSize, NodeXyMap )


displayInfoForOverlay : NetworkDisplayInfo -> Transportation -> Maybe OverlayDisplayInfo
displayInfoForOverlay ( cm, ewm, nsm, nxym, _ ) t =
    Maybe.map4 (\a b c d -> ( a, b, c, d ))
        (get t cm)
        (get t ewm)
        (get t nsm)
        (Just nxym)

-- net x coordinate of node
nodeX : NodeXyMap -> Node -> Int
nodeX nodeXyMap n =
    50
        + 100
        * (Maybe.withDefault 0
            << Maybe.map Tuple.first
            << get n
           <|
            nodeXyMap
          )

-- get y coordinate of node
nodeY : NodeXyMap -> Node -> Int
nodeY nodeXyMap n =
    50
        + 100
        * (Maybe.withDefault 0
            << Maybe.map Tuple.second
            << get n
           <|
            nodeXyMap
          )

getNodeXyMap : NetworkDisplayInfo -> NodeXyMap
getNodeXyMap (_,_,_,m,_) = m

indexOf : a -> List a -> Maybe Int
indexOf a l = case l of
  [] ->
    Nothing
  (x::xs) ->
    if a == x then Just 0 else  Maybe.map ((+)1) <| indexOf a xs

getPriority : NetworkDisplayInfo -> Transportation -> Int
getPriority (_,_,_,_, priorityList) t = Maybe.withDefault -1 << indexOf t <| priorityList