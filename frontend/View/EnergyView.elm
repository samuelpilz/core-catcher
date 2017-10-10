module View.EnergyView exposing (energyOverview)

import Html as Html
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import AllDict exposing (..)
import EveryDict exposing (..)
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import View.GameViewDisplay exposing (..)
import Maybe exposing (..)
import Tuple as Tuple
import Experimental.ClientState exposing (..)


energyOverview : NetworkModel -> Html.Html Msg
energyOverview networkModel =
    let
        displayInfo =
            networkModel.displayInfo
    in
        svg
            [ height (toString displayInfo.mapHeight)
            , width "400"
            , Html.style [ ( "border-size", "1" ) ]
            ]
        <|
            playersView
                networkModel
                ++ historyView
                    networkModel.rogueHistory
                    displayInfo


playersView : NetworkModel -> List (Svg.Svg Msg)
playersView networkModel =
    List.map2
        (playerView networkModel)
        (range 0 <| List.length networkModel.players)
    <|
        networkModel.players


playerView : NetworkModel -> Int -> Player -> Svg.Svg Msg
playerView networkModel pos player =
    g [] <|
        (if player == networkModel.player then
            [ rect
                [ x << toString <| 45
                , y << toString <| 5 + pos * 60
                , height "60"
                , width "350"
                , fill "#d0d0d0"
                ]
                []
            ]
         else
            []
        )
            ++ energyForPlayerView networkModel pos player
            ++ turnIcon networkModel pos player


energyForPlayerView : NetworkModel -> Int -> Player -> List (Svg.Svg Msg)
energyForPlayerView networkModel pos player =
    let
        displayInfo =
            networkModel.displayInfo
    in
        (List.map
            (energyRecord networkModel player)
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
                        networkModel.playerEnergies.playerEnergies
                    )
                )
         <|
            networkModel.energies
        )
            ++ playerLabel displayInfo pos player


energyRecord : NetworkModel -> Player -> ( ( Int, Int ), Energy, Color, Int ) -> Svg.Svg Msg
energyRecord networkModel forPlayer ( ( posX, posY ), energy, color, count ) =
    let
        ownPlayer =
            networkModel.player == forPlayer

        highlightEnergy =
            ownPlayer && networkModel.selectedEnergy == Just energy
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


playerLabel : GameViewDisplayInfo -> Int -> Player -> List (Svg.Svg Msg)
playerLabel displayInfo posY player =
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


turnIcon : NetworkModel -> Int -> Player -> List (Svg.Svg Msg)
turnIcon networkModel posY forPlayer =
    let
        displayInfo =
            networkModel.displayInfo
    in
        if networkModel.nextPlayer == Just forPlayer then
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
historyView rogueHistory =
    case rogueHistory of
        ShadowHistory h ->
            shadowHistoryView h

        OpenHistory h ->
            openHistoryView h


shadowHistoryView : ShadowRogueHistory -> GameViewDisplayInfo -> List (Svg.Svg Msg)
shadowHistoryView rogueHistory displayInfo =
    List.map2 historyRecord (range 0 100)
        << List.map
            (\( t, n ) ->
                ( Maybe.withDefault "black" << AllDict.get t <| displayInfo.colorMap
                , n
                )
            )
        << List.reverse
    <|
        rogueHistory.shadowRogueHistory


openHistoryView : OpenRogueHistory -> GameViewDisplayInfo -> List (Svg.Svg msg)
openHistoryView rogueHistory displayInfo =
    -- TODO: implement
    []


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
            , y << toString <| 250 + 50 * (pos // 5) + 27
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
