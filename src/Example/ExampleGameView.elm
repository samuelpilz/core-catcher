module Example.ExampleGameView exposing (..)

import Protocol exposing (..)
import List exposing (..)
import Maybe exposing (..)


catcherGameView : CatcherGameView
catcherGameView =
    { catcherPlayerPositions = playerPositions
    , catcherEnergies = playerEnergies
    , catcherRogueHistory = {rogueTransportHistory = []}
    , catcherRogueLastSeen = Nothing
    , catcherViewError = Nothing
    }


playerPositions : PlayerPositions
playerPositions =
    { playerPositions_ =
        [ ( { playerId = 1 }, { nodeId = 1 } )
        , ( { playerId = 2 }, { nodeId = 4 } )
        , ( { playerId = 3 }, { nodeId = 2 } )
        ]
    }


playerEnergies : PlayerEnergies
playerEnergies =
    { playerEnergies =
        [ ( { playerId = 1 }, player1Energies )
        ]
    }


player1Energies : EnergyMap
player1Energies =
    { energyMap =
        [ ( { transportName = "taxi" }, 5 )
        , ( { transportName = "bus" }, 3 )
        , ( { transportName = "underground" }, 2 )
        ]
    }
