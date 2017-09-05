module View.GameViewDisplay exposing (..)

{-| data types for displaying game views
-}

import Protocol exposing (..)
import AllDict exposing (..)
import Maybe exposing (..)
import Time exposing (Time)



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


{-| A map that contains the mappings from energy type to color.

Each energy type should be present in this map.

-}
type alias ColorMap =
    AllDict Energy Color Int


{-| A map that contains the edge widths for each Energy type.

Each energy type should be present in this map.

-}
type alias EdgeWidthMap =
    AllDict Energy EdgeWidth Int


{-| A map that contains the node sizes for each Energy type.
This is the size of the circle that lies behind a node to represent
that the node has a stop for the given energy

Each energy type should be present in this map.

-}
type alias NodeSizeMap =
    AllDict Energy NodeSize Int


{-| A map that mapps nodes to coordinates
-}
type alias NodeXyMap =
    AllDict Node ( Int, Int ) Int


type alias PlayerColorMap =
    AllDict Player Color String


{-| An object that contains all information needed to display a network.

The first 3 fields are maps that store the color, edgeWidth and nodeSize per energy type

The nodeXyMap field is a map of coordinates per node

The energyPriorityList is the list of energies to the determine the order
in which they should be rendered.
The smallest / thinnest energy type should be named last.

The playerColorMap assigns each player a color

mapHeight and mapWidth contain the number of pixels the map is drawn

-}
type alias GameViewDisplayInfo =
    { colorMap : ColorMap
    , edgeWidthMap : EdgeWidthMap
    , nodeSizeMap : NodeSizeMap
    , nodeXyMap : NodeXyMap
    , energyPriorityList : List Energy
    , playerColorMap : PlayerColorMap
    , mapWidth : Int
    , mapHeight : Int
    , gridWidth : Int
    , gridHeight : Int
    , movementAnimationDuration : Time
    }


{-| An object that contains all information needed to display a single networkOverlay.

The color-property represents the color the overlay- node-rings and edges should be displayed in.

The nodeSize and edgeWidth properties determine the size of the circle of the rings around nodes
and the stroke-width of the edges

-}
type alias OverlayDisplayInfo =
    { color : Color
    , edgeWidth : EdgeWidth
    , nodeSize : NodeSize
    }


{-| extract the OverlayDisplayInfo from the NetworkDisplayInfo for one energy type.

Returns `Nothing` if the given energy type is missing in at least one of the
NetworkDisplayInfo maps is missing.

-}
displayInfoForEnergy : GameViewDisplayInfo -> Energy -> Maybe OverlayDisplayInfo
displayInfoForEnergy { colorMap, edgeWidthMap, nodeSizeMap } energy =
    Maybe.map3
        (\c e n ->
            { color = c
            , edgeWidth = e
            , nodeSize = n
            }
        )
        (get energy colorMap)
        (get energy edgeWidthMap)
        (get energy nodeSizeMap)



-- TODO: nodeXy : NodeXyMap -> Node -> Maybe (Int,Int) to use with let?
-- TODO: less Maybe.withDefault in general?? -> Fail in code and not visually


{-| get the X coordinates of the node within the svg, given the map of coordinates
-}
nodeX : NodeXyMap -> Node -> Int
nodeX nodeXyMap n =
    50
        + 100
        * (Maybe.withDefault 0
            << Maybe.map Tuple.first
            << AllDict.get n
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


{-| gets the priority value for the given energy.

The overlays with higher priority are drawn first

This is the reverse order of the list in NetworkDisplayInfo

-}
getPriority : GameViewDisplayInfo -> Energy -> Int
getPriority { energyPriorityList } t =
    Maybe.withDefault -1 << indexOf t <| energyPriorityList
