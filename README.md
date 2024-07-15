# powernet -- algebraic graphs with edges labeled by capacity

This package implements `Network`s, graph data structures where edges are labeled by capacity.

A `Network` is typically constructed algebraically using `-<`, `>-`, and `overlays`:

```haskell
import Data.Network

node1 = node 'a' 1
node2 = node 'b' 2
node3 = node 'c' 3

type NodeLabel = Char
type NodeContent = Int

network :: Network Int NodeLabel NodeContent
network = overlays [ node1 -< (10 :: Capacity Int) >- node2
                   , node2 -< (15 :: Capacity Int) >- node3
                   , node3 -< ( 7 :: Capacity Int) >- node1
                   ]
```

See the documentation for `Data.Network` (run `cabal haddock`) for documentation on the various operations available.

## Implementation details

This library builds on algebraic graphs provided by [`algebraic-graphs`](https://github.com/snowleopard/alga). 

Why another library? Not all instances of `algebraic-graphs` make sense at the time of writing these words. Consider this code:

```haskell
import Algebra.Graph.Labelled 

graph :: Graph (Capacity Int) Char
graph = edges [(0, 'a', 'b'), (0, 'b', 'c'), (0, 'c', 'a')]
```

What would you expect for `length graph` and `traverse print graph`? [The internal structure of `Algebra.Graph.Labelled.Graph` leaks such that the `Foldable` and `Traversable` instances may behave unintuitively](https://github.com/snowleopard/alga/issues/95).

`powernet` solves this problem by separating the topology of the network, and the nodes (and their contents). Roughly:

```haskell
type NodeID = Int

data Network e a 
    = MkNetwork { networkTopology :: Graph (Capacity e) NodeID
                , networkNodeMap  :: Bimap NodeID a
                }
```

In this implementation, `Network` has intuitive `Foldable` and `Traversable` instances.
