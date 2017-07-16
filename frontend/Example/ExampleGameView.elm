module Example.ExampleGameView exposing (..)

import Protocol exposing (..)
import List exposing (..)
import Maybe exposing (..)


catcherGameView : CatcherGameView
catcherGameView =
    { catcherPlayerPositions = playerPositions
    , catcherEnergies = playerEnergies
    , catcherRogueHistory = {rogueHistory_ = []}
    , catcherNextPlayer = {playerId = 0}
    }

rogueGameView : RogueGameView
rogueGameView =
    { roguePlayerPositions = playerPositions
    , rogueEnergies = playerEnergies
    , rogueOwnHistory = {rogueHistory_ = []}
    , rogueNextPlayer = {playerId = 0}
    }


playerPositions : PlayerPositions
playerPositions =
    { playerPositions_ = []
        --[ ( { playerId = 0 }, { nodeId = 1 } )
        --, ( { playerId = 1 }, { nodeId = 4 } )
        --, ( { playerId = 2 }, { nodeId = 2 } )
        --]
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
        [ ( { transportName = "orange" }, 15 )
        , ( { transportName = "blue" }, 8 )
        , ( { transportName = "red" }, 3 )
        ]
    }
