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
    , catcherNextPlayer = {playerId = 0}
    }

rogueGameView : RogueGameView
rogueGameView =
    { roguePlayerPositions = playerPositions
    , rogueEnergies = playerEnergies
    , rogueOwnHistory = {rogueTransportHistory = []}
    , rogueRogueLastSeen = Nothing
    , rogueNextPlayer = {playerId = 0}
    }


playerPositions : PlayerPositions
playerPositions =
    { playerPositions_ =
        [ ( { playerId = 0 }, { nodeId = 1 } )
        , ( { playerId = 1 }, { nodeId = 4 } )
        , ( { playerId = 2 }, { nodeId = 2 } )
        ]
    }


playerEnergies : PlayerEnergies
playerEnergies =
    { playerEnergies =
        [ ( { playerId = 0 }, player0Energies )
        ]
    }


player0Energies : EnergyMap
player0Energies =
    { energyMap =
        [ ( { transportName = "taxi" }, 5 )
        , ( { transportName = "bus" }, 3 )
        , ( { transportName = "underground" }, 2 )
        ]
    }
