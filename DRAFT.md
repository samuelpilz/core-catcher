# Data Structures / Information

- Map:

```haskell
newtype Graph = Graph ([Vertex], [(VertexIdentifier, VertexIdentifier, Colors)])
type Colors = [Color]
type Color = Int
black :: Colors
black = []
data Vertex = Vertex {
    identifier :: VertexIdentifier,
    position :: (Int, Int)
  }
type VertexIdentifier = Int
```

- Player:

```haskell
data Player = Player {
    name :: Data.Text,
    tickets :: Data.Map Color Int,
    location :: Maybe VertexIdentifier
  }
```

- Game-state:

```haskell
data Core = Core {
    player :: Player,
    history :: [VertexIdentifier]
  }

data GameState = GameState {
    graph :: Graph,
    players :: [Player],
    core :: Core
  }

```
