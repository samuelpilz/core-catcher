module View.EnergyView exposing (energyOverview)

import Html as Html
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import AllDict exposing (..)
import EveryDict exposing (..)
import ClientState exposing (..)
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import View.GameViewDisplay exposing (..)
import Maybe exposing (..)
import Tuple as Tuple


energyOverview : Network -> GameViewDisplayInfo -> GameState -> Html.Html Msg
energyOverview _ displayInfo gameState =
    svg
        [ height (toString displayInfo.mapHeight)
        , width "400"
        , Html.style [ ( "border-size", "1" ) ]
        ]
    <|
        playersView
            displayInfo
            gameState
            ++ historyView
                gameState.rogueHistory
                displayInfo


playersView : GameViewDisplayInfo -> GameState -> List (Svg.Svg Msg)
playersView displayInfo gameState =
    List.concat
        << List.map2
            (playerView displayInfo gameState)
            (range 0 <| List.length gameState.players)
    <|
        gameState.players


playerView : GameViewDisplayInfo -> GameState -> Int -> Player -> List (Svg.Svg Msg)
playerView displayInfo gameState pos player =
    energyForPlayerView displayInfo gameState pos player
        ++ turnIcon displayInfo gameState pos player


energyForPlayerView : GameViewDisplayInfo -> GameState -> Int -> Player -> List (Svg.Svg Msg)
energyForPlayerView displayInfo gameState pos player =
    (List.map
        (energyRecord gameState player)
        << List.sortBy (\( ( priority, _ ), _, _, _ ) -> priority)
        << List.map
            (\energy ->
                ( ( getPriority displayInfo energy, pos )
                , energy
                , Maybe.withDefault "white" <| AllDict.get energy displayInfo.colorMap
                , Maybe.withDefault 0
                    << Maybe.andThen (EveryDict.get energy)
                    << Maybe.map .energyMap
                    << EveryDict.get player
                  <|
                    gameState.playerEnergies.playerEnergies
                )
            )
     <|
        gameState.energies
    )
        ++ playerLabel gameState displayInfo pos player


energyRecord : GameState -> Player -> ( ( Int, Int ), Energy, Color, Int ) -> Svg.Svg Msg
energyRecord gameState forPlayer ( ( posX, posY ), energy, color, count ) =
    let
        ownPlayer =
            gameState.player == forPlayer

        highlightEnergy =
            ownPlayer && gameState.selectedEnergy == energy
    in
        g []
            [ rect
                ([ x << toString <| 50 * posX + 50
                 , y << toString <| 60 * posY + 10
                 , width "49"
                 , height "49"
                 , Svg.Attributes.cursor "pointer"
                 , fill color
                 , stroke "#000000"
                 , strokeWidth
                    << toString
                   <|
                    if highlightEnergy then
                        3
                    else
                        1
                 ]
                    ++ if ownPlayer then
                        [ onClick (SelectEnergy energy) ]
                       else
                        []
                )
                []
            , text_
                ([ x << toString <| 50 * posX + 75
                 , y << toString <| 60 * posY + 40
                 , fill "#ffffff"
                 , Svg.Attributes.cursor "pointer"
                 , textAnchor "middle"
                 ]
                    ++ if ownPlayer then
                        [ onClick (SelectEnergy energy) ]
                       else
                        []
                )
                [ text << toString <| count ]
            ]


playerLabel : GameState -> GameViewDisplayInfo -> Int -> Player -> List (Svg.Svg Msg)
playerLabel gameState displayInfo posY player =
    [ text_
        [ x << toString <| 210
        , y << toString <| 60 * posY + 40
        , textAnchor "start"
        , fill "#111111"
        , fontSize "20"
        ]
        [ text player.playerName ]
    , line
        [ x1 << toString <| 210
        , y1 << toString <| 60 * posY + 47
        , x2 << toString <| 300
        , y2 << toString <| 60 * posY + 47
        , fill "none"
        , strokeWidth << toString <| 3
        , stroke
            << Maybe.withDefault "white"
            << AllDict.get player
          <|
            displayInfo.playerColorMap
        , strokeDasharray "5,5"
        ]
        []
    ]


turnIcon : GameViewDisplayInfo -> GameState -> Int -> Player -> List (Svg.Svg Msg)
turnIcon displayInfo gameState posY forPlayer =
    if gameState.nextPlayer == Just forPlayer then
        [ g []
            [ circle
                [ cx << toString <| 30
                , cy << toString <| 60 * posY + 30
                , r "8"
                , stroke
                    << Maybe.withDefault "white"
                    << AllDict.get forPlayer
                  <|
                    displayInfo.playerColorMap
                , strokeWidth "2.5"
                , fill "none"
                , Svg.Attributes.strokeDasharray "3,2.7"
                ]
                []
            ]
        , line
            [ x1 << toString <| 4
            , y1 << toString <| 60 * posY + 30
            , x2 << toString <| 18
            , y2 << toString <| 60 * posY + 26
            , strokeWidth "1.5"
            , stroke
                << Maybe.withDefault "white"
                << AllDict.get forPlayer
              <|
                displayInfo.playerColorMap
            ]
            []
        , line
            [ x1 << toString <| 5
            , y1 << toString <| 60 * posY + 34
            , x2 << toString <| 18
            , y2 << toString <| 60 * posY + 30
            , strokeWidth "1.5"
            , stroke
                << Maybe.withDefault "white"
                << AllDict.get forPlayer
              <|
                displayInfo.playerColorMap
            ]
            []
        , line
            [ x1 << toString <| 6
            , y1 << toString <| 60 * posY + 38
            , x2 << toString <| 18
            , y2 << toString <| 60 * posY + 34
            , strokeWidth "1.5"
            , stroke
                << Maybe.withDefault "white"
                << AllDict.get forPlayer
              <|
                displayInfo.playerColorMap
            ]
            []
        ]
    else
        []


historyView : RogueHistory -> GameViewDisplayInfo -> List (Svg.Svg Msg)
historyView rogueHistory displayInfo =
    List.map2 historyRecord (range 0 100)
        << List.map
            (\( t, n ) ->
                ( Maybe.withDefault "black" << AllDict.get t <| displayInfo.colorMap
                , n
                )
            )
        << List.reverse
    <|
        rogueHistory.rogueHistory


historyRecord : Int -> ( Color, Maybe Node ) -> Svg.Svg Msg
historyRecord pos ( color, nodeMay ) =
    g []
        [ rect
            [ x << toString <| 50 + 50 * (pos % 5)
            , y << toString <| 250 + 50 * (pos // 5)
            , width "49"
            , height "49"
            , Svg.Attributes.cursor "pointer"
            , fill color
            , stroke "#000000"
            , strokeWidth "1"
            ]
            []
        , text_
            [ x << toString <| 50 + 50 * (pos % 5) + 25
            , y << toString <| 100 + 50 * (pos // 5) + 27
            , fill "#ffffff"
            , Svg.Attributes.cursor "pointer"
            , textAnchor "middle"
            ]
            [ text
                << Maybe.withDefault ""
                << Maybe.map toString
                << Maybe.map .nodeId
              <|
                nodeMay
            ]
        ]
