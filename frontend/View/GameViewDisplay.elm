module View.GameViewDisplay exposing (..)

{-| data types for displaying game views
-}

import Protocol exposing (..)
import AllDict exposing (..)
import Maybe exposing (..)


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
    AllDict Transport Color Int


{-| A map that contains the edge widths for each Transport type.

Each energy type should be present in this map.

-}
type alias EdgeWidthMap =
    AllDict Transport EdgeWidth Int


{-| A map that contains the node sizes for each Transport type.
This is the size of the circle that lies behind a node to represent
that the node has a stop for the given energy

Each energy type should be present in this map.

-}
type alias NodeSizeMap =
    AllDict Transport NodeSize Int


{-| A map that mapps nodes to coordinates
-}
type alias NodeXyMap =
    AllDict Node ( Int, Int ) Int


type alias PlayerColorMap =
    AllDict Player Color Int


{-| A tuple that contains all information needed to display a network.

The first 3 entries are maps that store the color, edgeWidth and nodeSize per energy type

The 4th entry is a map of coordinates per node

The 5th entry is the list of energies used in order which they should be rendered.
The smallest / thinnest energy type should be named last.

The 6th entry is a map that mapps each player to a color

TODO: rework

-}
type alias GameViewDisplayInfo =
    { colorMap : ColorMap
    , edgeWidthMap : EdgeWidthMap
    , nodeSizeMap : NodeSizeMap
    , nodeXyMap : NodeXyMap
    , energyPriorityList : List Transport
    , playerColorMap : PlayerColorMap
    , mapWidth : Int
    , mapHeight : Int
    , gridWidth : Int
    , gridHeight : Int
    }


{-| A tuple that contains all information needed to display a single networkOverlay.

The 1st entry is the color in which the edges and node rings are drawn.
The 2nd entry is the width of the edges, the 3rd is the size of the rings around the nodes.

The 4th entry is a map of coordinates per node, the same in the NetworkDisplayInfo type.

TODO: rework

-}
type alias OverlayDisplayInfo =
    { color : Color
    , edgeWidth : EdgeWidth
    , nodeSize : NodeSize
    , nodeXyMap : NodeXyMap
    }


{-| extract the OverlayDisplayInfo from the NetworkDisplayInfo for one energy type.

Returns `Nothing` if the given energy type is missing in at least one of the
NetworkDisplayInfo maps is missing.

-}
displayInfoForTransport : GameViewDisplayInfo -> Transport -> Maybe OverlayDisplayInfo
displayInfoForTransport { colorMap, edgeWidthMap, nodeSizeMap, nodeXyMap } energy =
    Maybe.map4
        (\c e n xy ->
            { color = c
            , edgeWidth = e
            , nodeSize = n
            , nodeXyMap = xy
            }
        )
        (get energy colorMap)
        (get energy edgeWidthMap)
        (get energy nodeSizeMap)
        (Just nodeXyMap)



-- TODO: nodeXy : NodeXyMap -> Node -> (Int,Int)?


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
getPriority : GameViewDisplayInfo -> Transport -> Int
getPriority { energyPriorityList } t =
    Maybe.withDefault -1 << indexOf t <| energyPriorityList



{-
   movePlayerInGame : Game -> Player -> Node -> Game
   movePlayerInGame game player newNode =
       { game | gameState = Dict.update player (\_ -> Just newNode) game.gameState }
-}
