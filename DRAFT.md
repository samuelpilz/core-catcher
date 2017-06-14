# Data Structures / Information

- Map:

```haskell

newtype Graph = 
    Graph 
        ( Set Vertex
        , [NetworkOverlay]
        )
        deriving (Eq, Show, Read) 

newtype NetworkOverlay = 
    Overlay 
        ( Set Edge
        )
        deriving (Eq, Show, Read)

newtype Edge = 
    Edge 
        VertexId 
        VertexId
        deriving (Show, Read) 

-- unidirectional edge
instance Eq Edge where
  (==) (Edge u v) (Edge u' v') = 
      u == u' && v == v' 
      || u == v' && v == u'  

type Colors = [Color]
type Color = Int

black :: Colors
black = []

data Vertex = 
    Vertex 
        { identifier :: VertexId
        , position :: (Int, Int)
        }

newtype VertexId = 
    V Int 
    deriving (Eq, Show, Ord, Read) 
```

- Player:

```haskell
data Player = 
    Player 
        { name :: Data.Text
        , tickets :: Data.Map Color Int
        , location :: Maybe VertexIdentifier
        }
```

- Game-state:

```haskell
data Core = 
    Core 
        { player :: Player
        , history :: [VertexIdentifier]
        }

data GameState = 
    GameState 
        { graph :: Graph
        , players :: [Player]
        , core :: Core
        }
```
