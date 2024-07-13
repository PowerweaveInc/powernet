{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave
-- License     :  MIT
-- Portability :  portable

module Data.Network ( 
    -- * Constructing 'Network's
    Network, empty, vertex, edge, overlays, edges, fromGraph,
    (-<), (>-), 

    -- ** Constructing nodes
    Node(nodeLabel, nodeData), node,

    -- * Deconstructing 'Network's
    vertices, topology,

    -- * 'Network' operations
    filter, map, mapWithNode, foldWithNode, traverseWithNode, 
    zipWith, zipWithNode, zipWith3, zipWithNode3, unzip,
    
    -- * Describing edges
    Capacity, getCapacity, capacity,
    NonNegative, finite, unsafeFinite, infinite,

    -- * Edge manipulations
    mapEdges, replaceEdge,

    -- * Export
    toDotNotation,
    ) where

import           Algebra.Graph.Label        ( Capacity, NonNegative, getCapacity, capacity, finite, unsafeFinite
                                            , infinite 
                                            )
import           Algebra.Graph.Labelled     ( Graph(..), emap )
import qualified Algebra.Graph.Labelled     as Graph
import           Algebra.Graph.Export.Dot   ( Style(..), Attribute(..), export, defaultStyle )
import           Data.Bifoldable
import           Data.Bifunctor             
import           Data.Bitraversable
import           Data.Bimap.Strict          ( Bimap )
import qualified Data.Bimap.Strict          as Bimap
import           Data.Maybe                 ( fromMaybe, fromJust )
import           Data.Set                   ( Set )
import qualified Data.Set                   as Set
import           Data.Text                  ( Text )
import           Prelude                    hiding ( filter, zipWith, zipWith3, unzip, map, traverse )
import qualified Prelude


-- Same precedence as Algebra.Graph.Labelled.(-<) and Algebra.Graph.Labelled.(>-)
infixl 5 -<, >-


-- | Pointer from a node to 
newtype NodeID = MkNodeId Int
    deriving (Eq, Show, Ord, Num, Real, Enum, Integral)

-- | A 'Node a b' is a graph node labeled with type 'a', and carrying
-- extra data of type 'b'.
--
-- Comparison between nodes is done on the node label, while mapping
-- and data transformations (e.g. the 'Functor' instance) is done on the node data.
data Node a b 
    = MkNode { nodeLabel :: !a
             , nodeData  :: !b
             }
    deriving (Show)

node :: a -> b -> Node a b
node = MkNode

instance Functor (Node a) where
  fmap f (MkNode x a) = MkNode x (f a)

instance Foldable (Node a) where
  foldMap f (MkNode _ a) = f a

instance Traversable (Node a) where
  traverse f (MkNode x a) = MkNode x <$> f a

instance Eq a => Eq (Node a b) where
  MkNode a _ == MkNode b _ = a == b

instance Ord a => Ord (Node a b) where
  MkNode a _ `compare` MkNode b _ = compare a b
  min x@(MkNode a _) y@(MkNode b _)
    | a <= b    = x
    | otherwise = y
  max x@(MkNode a _) y@(MkNode b _)
    | a >= b    = x
    | otherwise = y

instance Bifunctor Node where
  bimap f g (MkNode a b) = MkNode (f a) (g b)

instance Bifoldable Node where
  bifoldMap f g (MkNode a b) = f a <> g b

instance Bitraversable Node where
  bitraverse f g (MkNode a b) = MkNode <$> f a <*> g b


-- | A network is an algebraic graph with edges labeled by capacity.
data Network e n a 
    -- We chose to represent a Network while separating the network topology
    -- and the nodes.
    -- This is because using an algebraic graph leads to duplicating of nodes.
    -- For example:
    --
    -- >>> Graph.overlays ['a' -< 0 >- 'b', 'a' -< 0 >- 'c', 'b' -< 0 >- 'c' ]
    -- Connect 0 (Connect 0 (Vertex 'a') (Vertex 'b')) (Connect 0 (Connect 0 (Vertex 'a') (Vertex 'c')) (Connect 0 (Connect 0 (Vertex 'b') (Vertex 'c')) Empty))
    --
    -- Note that Vertex 'a' appears multiple times! Which means that when attaching data to vertices, we duplicate
    -- data for each node.
    --
    -- Therefore, we store the network topology separately, mapping each node to a NodeID
    --
    -- Having node labels and node data separated is also key to some instances;
    -- we can remove the constraints on the node data such that we can easily define structure
    -- preserving instances such as 'Functor' and 'Traversable'.
    = MkNetwork { networkTopology :: Graph (Capacity e) NodeID
                , networkNodeMap  :: Bimap NodeID (Node n a)
                }
    deriving (Eq, Ord, Show)


instance Foldable (Network e n) where
    foldMap f = foldWithNode (\_ x -> f x)
    {-# INLINEABLE foldMap #-}

instance (Ord e, Num e, Ord n) => Semigroup (Network e n a) where
    x <> y = fromGraph $ topology x `Graph.overlay` topology y

instance (Ord e, Ord n, Num e) => Monoid (Network e n a) where
    mempty = empty

instance Ord n => Functor (Network e n) where
    fmap f (MkNetwork top nds) = MkNetwork top (Bimap.mapR (second f) nds)
    {-# INLINEABLE fmap #-}

instance Ord n => Traversable (Network e n) where
    traverse f = traverseWithNode (\_ x -> f x)
    {-# INLINEABLE traverse #-}



-- | Map each node of a 'Network' 
map :: Ord n => (a -> b) -> Network e n a -> Network e n b
map = fmap


-- | Map each node of a 'Network', using both node labels and node data. 
mapWithNode :: Ord n => (n -> a -> b) -> Network e n a -> Network e n b
mapWithNode f (MkNetwork top nds) 
    = MkNetwork top (Bimap.mapR (\(MkNode label data') -> MkNode label $ f label data') nds)
{-# INLINEABLE mapWithNode #-}


-- | Fold over a 'Network', using both node labels and node data.
foldWithNode :: Monoid m 
             => (n -> a -> m) 
             -> Network e n a 
             -> m
foldWithNode f (MkNetwork _ nodes) 
    = foldMap (\(MkNode label data') -> f label data') 
              (Set.toAscList $ Bimap.keysSetR nodes)
{-# INLINEABLE foldWithNode #-}


-- | Traverse a 'Network', using both node labels and node data.
traverseWithNode :: (Applicative f)
                 => (n -> a -> f b)
                 -> Network e n a
                 -> f (Network e n b)
traverseWithNode f (MkNetwork top nodes) 
    = MkNetwork top 
    . Bimap.fromDistinctAscList <$> Prelude.traverse (\(nodeid, MkNode label data') -> (\newdata -> (nodeid, MkNode label newdata)) <$> f label data') 
                                                     (Bimap.toList nodes)


-- | Create a 'Network' from a graph topology.
fromGraph :: Ord n => Graph (Capacity e) (Node n a) -> Network e n a
fromGraph g = MkNetwork indirectTop nodeMap
    where
        uniqueNodes = Graph.foldg mempty Set.singleton (\_ x y -> x <> y)
        nodeMap     = Bimap.fromDistinctAscList $ zip [0..] $ Set.toList (uniqueNodes g)
        indirectTop = fmap (\nd -> fromJust $ Bimap.lookupR nd nodeMap) g


-- | Extract the topology of a network
topology :: Network e n a -> Graph (Capacity e) (Node n a)
topology (MkNetwork top nodes) 
    = fmap (\nodeid -> fromJust $ Bimap.lookup nodeid nodes) top


-- | Construct the empty graph. 
empty :: Network e n a
empty = MkNetwork Empty Bimap.empty


-- | Construct the graph comprising a single isolated vertex.
vertex :: Node n a -> Network e n a
vertex nd = MkNetwork (Vertex 0) (Bimap.singleton 0 nd)


-- | Construct the graph comprising a single labelled edge.
--
-- If both nodes are equal, then 'edge _ x x' is equivalent to 'vertex x'.
-- 
-- See also '-<' and '>-'.
edge :: Ord n => Capacity e -> Node n a -> Node n a -> Network e n a
edge e x y 
    = if x == y 
        then vertex x 
        else MkNetwork (Graph.connect e (Vertex 0) (Vertex 1)) (Bimap.fromDistinctAscList [(0, x), (1, y)])


-- | Overlay a given list of graphs. 
overlays :: (Num e, Ord e, Ord n) => [Network e n a] -> Network e n a
-- To minimize possibility of errors, I prefer converting from/to graphs
-- when combining using `overlays`. While this may be inefficient,
-- I assume that 'Network's are rarely constructed. 
overlays = fromGraph 
         . Graph.overlays 
         . fmap topology


-- | Construct the a 'Network' from a list of labelled edges. 
-- Self-referential edges are pruned, i.e.
--
-- @
-- edges [('a', 1, 'b'), ('a', 0, 'a')] == edges [('a', 1, 'b')]
-- @      
edges :: (Ord n, Ord e, Num e) => [(Node n a, Capacity e, Node n a)] -> Network e n a
edges = fromGraph 
      . Graph.edges 
      . Prelude.map (\(x, e, y) -> (e, x, y)) 
      . Prelude.filter (\(x, _, y) -> x /= y) -- removing self-referential edges


-- | Construct a 'Network' using '-<' and '>-':
--
-- >>> (node 'x' 0) -< (0::Capacity Int) >- (node 'y' 0)
(-<) :: Node n a -> Capacity e -> (Node n a, Capacity e)
(-<) = (,)


-- | Construct a 'Network' using '-<' and '>-':
--
-- >>> (node 'x' 0) -< (0::Capacity Int) >- (node 'y' 0)
(>-) :: Ord n => (Node n a, Capacity e) -> Node n a -> Network e n a
(x, e) >- y = edge e x y


-- | The set of vertices of a given network.
vertices :: Ord n => Network e n a -> Set (Node n a)
vertices (MkNetwork _ nds) = Set.fromList $ Bimap.elems nds


-- | Construct the induced subgraph of a given graph by removing the vertices that do not satisfy a given predicate. 
filter :: (Node n a -> Bool) -> Network e n a -> Network e n a
filter f (MkNetwork top nds) 
    = let newNodeMap    = Bimap.filter (\_ x -> f x) nds 
          remainingKeys = Bimap.keysSet newNodeMap
       in MkNetwork (Graph.induce (`Set.member` remainingKeys) top) newNodeMap


-- | Map the edges of a network.
mapEdges :: (Capacity e -> Capacity f) 
         -> Network e n a 
         -> Network f n a
mapEdges f (MkNetwork top nds) = MkNetwork (emap f top) nds


-- | Replace the edge between two nodes with the provided capacity.
-- If the edge didn't exist, it will be created.
replaceEdge :: (Num e, Ord e, Ord n) 
            => Capacity e
            -> n
            -> n
            -> Network e n a
            -> Network e n a
replaceEdge f x y (MkNetwork top nodes) 
    = MkNetwork newTopology nodes
    where
        mp = Bimap.mapR nodeLabel nodes
        newTopology = fromMaybe top 
                    $ Graph.replaceEdge f <$> Bimap.lookupR x mp
                                          <*> Bimap.lookupR y mp
                                          <*> pure top


-- | Zip two networks together with identical structures.
--
-- Note that this function might have unintended consequences. For example,
-- two 'Network's which are equivalent but not structurally equivalent may 
-- not zip properly.
--
-- Edge labels from the first 'Network' are preserved
--
-- >>> zipWith (,) ('a' -< (0::Capacity Int) >- 'b') ('c' -< (1::Capacity Int) >- 'd')
-- MkNetwork {unNetwork = Connect 0 (Vertex ('a','c')) (Vertex ('b','d'))}
zipWith :: (Ord e, Num e, Ord n) 
        => (a -> b -> c) 
        -> Network e n a 
        -> Network e n b 
        -> Network e n c
{-# INLINEABLE zipWith #-}
zipWith f (MkNetwork x xNodes) (MkNetwork y yNodes) 
    = if x == y
        then MkNetwork x (Bimap.zipWith (\(MkNode n1 i) (MkNode _ j) -> MkNode n1 $ f i j) xNodes yNodes)
        else empty 


-- | Zip two networks together with identical structures, taking node labels and node data
-- into account.
zipWithNode :: (Ord e, Num e, Ord n) 
            => (n -> a -> b -> c)
            -> Network e n a 
            -> Network e n b
            -> Network e n c
zipWithNode f (MkNetwork x xs) (MkNetwork y ys) 
    = if x == y
        then MkNetwork x (Bimap.zipWith (\(MkNode n i) (MkNode _ j) -> MkNode n $ f n i j) xs ys)
        else empty 


-- | Zip three networks together with identical structures, taking node labels and node data
-- into account.
zipWithNode3 :: (Ord e, Num e, Ord n) 
             => (n -> a -> b -> c -> d)
             -> Network e n a 
             -> Network e n b
             -> Network e n c
             -> Network e n d
zipWithNode3 f xs ys zs
    = let xsys = zipWith (,) xs ys
       in zipWithNode (\k (x, y) z -> f k x y z) xsys zs

-- | Zip three networks together with identical structures.
--
-- The caveats for 'zipWith' also apply to this function. 
zipWith3 :: (Ord e, Num e, Ord n) 
         => (a -> b -> c -> d)
         -> Network e n a 
         -> Network e n b
         -> Network e n c
         -> Network e n d
zipWith3 f a b c = zipWith (\i (j, k) -> f i j k) a $ zipWith (,) b c


-- | Unzip a 'Network'
unzip :: Ord n => Network e n (a, b) -> (Network e n a, Network e n b)
unzip n = (map fst n, map snd n)


-- | Represent a 'Network' using graphviz's dot notation.
--
-- >>> let network = 'a' -< (1 :: Capacity Int) >- 'b'
-- >>> toDotNotation show network
-- "digraph \n{\n  \"'a'\"\n  \"'b'\"\n  \"'a'\" -> \"'b'\" [label=\"1\"]\n}\n"
toDotNotation :: (Ord n, Ord e, Num e) 
              => (n -> Text)
              -> (Capacity e -> Text) 
              -> Network e n a 
              -> Text
toDotNotation showVertex showEdge (MkNetwork top nodes)
    = export style topology'
        where
            topology' = fmap (\nodeid -> nodeLabel $ nodes Bimap.! nodeid) top
            style = let def = defaultStyle showVertex
                     in def{ edgeAttributes = \x y -> ["label" := showEdge (Graph.edgeLabel x y topology')]
                           }
