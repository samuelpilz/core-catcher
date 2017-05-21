module Network exposing (..)

{-
   - one possibility for Network type
-}

import Dict exposing (..)
import Maybe exposing (..)


{-| The type of node: Id by Int
-}
type alias Node =
    Int


{-| The type of edge is a connection between nodes.
-}
type alias Edge =
    ( Node, Node )


{-| The type of transportation is a string
-}
type alias Transportation =
    String


{-| Network: Nodes and Map Transportation to Overlay.

The overlays contain the actual Edges

The network itself has no information about its representation.
Representation is handled via NetworkDisplayInfo

-}
type alias Network =
    ( List Node, Dict Transportation NetworkOverlay )


{-| NetworkOverlay: Sub-Graph that contains several nodes

First part: the contained nodes in the Overlay.
The nodes have to be contained in the nodes of the enclosing network
Second part: the edges of the
The edges must only connect the nodes contained in the first list.

-}
type alias NetworkOverlay =
    ( List Node, List Edge )


{-| Types of colors possible for overlays. For now: a string
-}
type alias Color =
    String


{-| Type to describe the width of svg lines (the edges)
-}
type alias EdgeWidth =
    Int


{-| Type to describe the size of the nodes within the svg
-}
type alias NodeSize =
    Int


{-| A map that contains the mappings from transportation type to color.

Each transportation type should be present in this map.

-}
type alias ColorMap =
    Dict Transportation Color


{-| A map that contains the edge widths for each Transportation type.

Each transportation type should be present in this map.

-}
type alias EdgeWidthMap =
    Dict Transportation EdgeWidth


{-| A map that contains the node sizes for each Transportation type.
This is the size of the circle that lies behind a node to represent
that the node has a stop for the given transport

Each transportation type should be present in this map.

-}
type alias NodeSizeMap =
    Dict Transportation NodeSize


{-| A map that mapps nodes to coordinates
-}
type alias NodeXyMap =
    Dict Node ( Int, Int )


{-| A tuple that contains all information needed to display a network.

The first 3 entries are maps that store the color, edgeWidth and nodeSize per transport type

The 4th entry is a map of coordinates per node

The 5th entry is the list of transportations used in order which they should be rendered.
The smallest / thinnest transport type should be namen last.

-}
type alias NetworkDisplayInfo =
    ( ColorMap, EdgeWidthMap, NodeSizeMap, NodeXyMap, List Transportation )


{-| A tuple that contains all information needed to display a single networkOverlay.

The 1st entry is the color in which the edges and node rings are drawn.
The 2nd entry is the width of the edges, the 3rd is the size of the rings around the nodes.

The 4th entry is a map of coordinates per node, the same in the NetworkDisplayInfo type.

-}
type alias OverlayDisplayInfo =
    ( Color, EdgeWidth, NodeSize, NodeXyMap )


{-| extract the OverlayDisplayInfo from the NetworkDisplayInfo for one transportation type.

Returns `Nothing` if the given transportation type is missing in at least one of the
NetworkDisplayInfo maps is missing.

-}
displayInfoForOverlay : NetworkDisplayInfo -> Transportation -> Maybe OverlayDisplayInfo
displayInfoForOverlay ( cm, ewm, nsm, nxym, _ ) t =
    Maybe.map4 (\a b c d -> ( a, b, c, d ))
        (get t cm)
        (get t ewm)
        (get t nsm)
        (Just nxym)


{-| get the X coordinates of the node within the svg, given the map of coordinates
-}
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


{-| get the Y coordinates of the node within the svg, given the map of coordinates
-}
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


{-| gets the nodeXyMap from a NetworkDisplayTuple
-}
getNodeXyMap : NetworkDisplayInfo -> NodeXyMap
getNodeXyMap ( _, _, _, m, _ ) =
    m


{-| helper function to extract the index of an element in the list (not in core)
-}
indexOf : a -> List a -> Maybe Int
indexOf a l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            if a == x then
                Just 0
            else
                Maybe.map ((+) 1) <| indexOf a xs


{-| gets the priority value for the given transportation.

The overlays with higher priority are drawn first

This is the reverse order of the list in NetworkDisplayInfo
-}
getPriority : NetworkDisplayInfo -> Transportation -> Int
getPriority ( _, _, _, _, priorityList ) t =
    Maybe.withDefault -1 << indexOf t <| priorityList
