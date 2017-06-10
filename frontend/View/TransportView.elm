module View.TransportView exposing (transportView)

import Html as Html
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import AllDict exposing (..)
import Data exposing (..)
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import GameViewDisplay exposing (..)
import Maybe exposing (..)
import Tuple as Tuple


transportView : Network -> GameViewDisplayInfo -> GameView -> Html.Html Msg
transportView _ displayInfo gameView =
    svg
        [ height (toString displayInfo.mapHeight)
        , width "400"
        , Html.style [ ( "border-size", "1" ) ]
        ]
    -- elements of svg now
    <|
        (List.map energyRecord
            << List.sortBy (\( priority, _, _ ) -> priority)
            << List.map
                (\( transport, color ) ->
                    ( getPriority displayInfo transport
                    , color
                    , getEnergyForTransportAndPlayer { playerId = 0 } transport gameView
                    )
                )
            << AllDict.toList
         <|
            displayInfo.colorMap
        )
            ++ (List.map2 rectForPosAndColor [ ( 50, 100 ), ( 100, 100 ), ( 150, 100 ) ]
                    -- TODO: expand and wrap
                    << List.map (Maybe.withDefault "black")
                    << List.map (\t -> AllDict.get t displayInfo.colorMap)
                <|
                    (rogueHistory gameView).rogueTransportHistory
               )



--| TODO: add number of available energies in boxes


energyRecord : ( Int, Color, Int ) -> Svg.Svg Msg
energyRecord (pos, color, count) =
    g []
        [ rectForPosAndColor ( 50 * pos + 50, 1 ) color
        , text_
            [ x << toString <| 50 * pos + 75
            , y << toString <| 30
            , fill "#ffffff"
            , Svg.Attributes.cursor "pointer"

            --, onClick
            , textAnchor "middle"
            ]
            [ text << toString <| count ]
        ]


rectForPosAndColor : ( Int, Int ) -> Color -> Svg.Svg Msg
rectForPosAndColor ( xPos, yPos ) color =
    rect
        [ x << toString <| xPos
        , y << toString <| yPos
        , width "49"
        , height "49"
        , Svg.Attributes.cursor "pointer"
        , fill color
        , stroke "#000000"
        , strokeWidth "1"
        ]
        []
