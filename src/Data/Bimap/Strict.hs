-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave
-- License     :  BSD3
-- Portability :  portable
--
-- An implementation of bidirectional maps between values of two
-- key types. A 'Bimap' is a bijection between subsets of
-- its two argument types.

-- Each element of the left-hand type is associated with an element
-- of the right-hand type, and vice-versa, such that the two mappings
-- are inverses. Deleting an element will cause its twin to be deleted,
-- and inserting a pair of elements will cause any overlapping bindings
-- to be deleted.

-- Most functions implicitly consider the left-hand type to be the
-- key, and the right-hand type to be the value.
-- Functions with an @R@ suffix reverse this convention, treating the
-- right-hand type as the key and the left-hand type as the value.
--
-- This module was modified from the original 'bimap' package
-- here:
--  https://github.com/joelwilliamson/bimap
-- Modifications include using strict Maps and zipWith/zipWithR
module Data.Bimap.Strict (
    -- * Bimap type
    Bimap(),
    -- * Query
    null, size, member, memberR, 
    notMember, notMemberR,
    pairMember, pairNotMember,
    lookup, lookupR,
    (!), (!>), (!?), (!?>),
    -- * Construction
    empty, singleton,
    -- * Update
    insert, tryInsert,
    -- * Zippping
    zipWith, zipWithR,
    -- * Filter
    filter, partition,
    -- * Conversion\/traversal
    fromList, fromAList, fromDistinctAscList,
    toList, toAscList, toAscListR,
    keys, keysSet, keysR, keysSetR,
    elems,
    assocs,
    fold,
    map, mapR, mapMonotonic, mapMonotonicR,
    toMap, toMapR,
    -- * Miscellaneous
    valid,
    twist,
    twisted,
) where

import           Control.DeepSeq     ( NFData(rnf) )
import           Control.Monad.Catch ( Exception, MonadThrow(..) )

import           Data.List           ( foldl', sort )
import qualified Data.Map.Strict     as M
import qualified Data.Map.Merge.Strict as Merge
import           Data.Maybe          ( fromMaybe )
import           Data.Set            ( Set )
import           Data.Tuple          ( swap )
import           Data.Typeable       ( Typeable )
import           Prelude             hiding ( filter, lookup, null, pred, map, zipWith )
import qualified Prelude             as P


infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

{-|
A bidirectional map between values of types @a@ and @b@.
-}
data Bimap a b = MkBimap !(M.Map a b) !(M.Map b a)

instance (Show a, Show b) => Show (Bimap a b) where
    show x = "fromList " ++ (show . toList $ x)

instance (Eq a, Eq b) => Eq (Bimap a b) where
    (==) (MkBimap left1 _) (MkBimap left2 _) = left1 == left2

instance (Ord a, Ord b) => Ord (Bimap a b) where
    compare (MkBimap left1 _) (MkBimap left2 _) = compare left1 left2

instance (NFData a, NFData b) => NFData (Bimap a b) where
    rnf (MkBimap m1 m2) = rnf (m1, m2)

instance Foldable (Bimap a) where
    foldMap f (MkBimap left _) = foldMap f left


{-|
A 'Bimap' action failed.
-}
data BimapException = KeyNotFound String
  deriving(Eq, Show, Typeable)

instance Exception BimapException


{-| /O(1)/. The empty bimap.
-}
empty :: Bimap a b
empty = MkBimap M.empty M.empty

{-| /O(1)/. A bimap with a single element.
-}
singleton :: a -> b -> Bimap a b
singleton x y = MkBimap (M.singleton x y) (M.singleton y x)

{-| /O(1)/. Is the bimap empty?
-}
null :: Bimap a b -> Bool
null (MkBimap left _) = M.null left

{-| /O(1)/. The number of elements in the bimap.
-}
size :: Bimap a b -> Int
size (MkBimap left _) = M.size left

{-| /O(log n)/. Is the specified value a member of the bimap?
-}
member :: (Ord a) => a -> Bimap a b -> Bool
member x (MkBimap left _) = M.member x left

{-| /O(log n)/. A version of 'member' specialized to the right key.
-}
memberR :: (Ord b) => b -> Bimap a b -> Bool
memberR y (MkBimap _ right) = M.member y right

{-| /O(log n)/. Is the specified value not a member of the bimap?
-}
notMember :: (Ord a) => a -> Bimap a b -> Bool
notMember = not .: member

{-| /O(log n)/. A version of 'notMember' specialized to the right key.
-}
notMemberR :: (Ord b) => b -> Bimap a b -> Bool
notMemberR = not .: memberR

{-| /O(log n)/.
Are the two values associated /with each other/ in the bimap?

This function is uncurried in its first two arguments, so that it
can be used infix.

-}
pairMember :: (Ord a, Ord b)
           => (a, b) -> Bimap a b -> Bool
pairMember (x, y) (MkBimap left _) =
    maybe False (== y) (M.lookup x left)

{-| /O(log n)/.
Are the two values not in the bimap, or not associated
with each other? (Complement of 'pairMember'.)
-}
pairNotMember :: (Ord a, Ord b)
              => (a, b) -> Bimap a b -> Bool
pairNotMember = not .: pairMember

{-| /O(log n)/.
Insert a pair of values into the bimap, associating them.

If either of the values is already in the bimap, any overlapping
bindings are deleted.

-}
insert :: (Ord a, Ord b)
       => a -> b -> Bimap a b -> Bimap a b
insert x y = delete x >>> deleteR y >>> unsafeInsert x y
    where
    (>>>) = flip (.)

{-| /O(log n)/.
Insert a pair of values into the bimap, but only if neither is
already in the bimap.
-}
tryInsert :: (Ord a, Ord b)
          => a -> b -> Bimap a b -> Bimap a b
tryInsert x y bi
    | x `notMember` bi && y `notMemberR` bi = unsafeInsert x y bi
    | otherwise                               = bi

{-| /O(log n)/.
Insert a pair of values into the bimap, without checking for
overlapping bindings.

If either value is already in the bimap, and
is not bound to the other value, the bimap will become inconsistent.
-}
unsafeInsert :: (Ord a, Ord b)
             => a -> b -> Bimap a b -> Bimap a b
unsafeInsert x y (MkBimap left right) =
    MkBimap (M.insert x y left) (M.insert y x right)

{-| /O(log n)/. Common implementation for 'delete' and 'deleteR'. -}
deleteE :: (Ord a, Ord b)
       => Either a b -> Bimap a b -> Bimap a b
deleteE e (MkBimap left right) =
    MkBimap
        (perhaps M.delete x  left)
        (perhaps M.delete y  right)
    where
    perhaps = maybe id
    x = either Just (`M.lookup` right) e
    y = either (`M.lookup` left) Just  e

{-| /O(log n)/.
Delete a value and its twin from a bimap.

When the value is not a member of the bimap, the original bimap is
returned.

-}
delete :: (Ord a, Ord b) => a -> Bimap a b -> Bimap a b
delete = deleteE . Left

{-| /O(log n)/ A version of 'delete' specialized to the right key.
-}
deleteR :: (Ord a, Ord b) => b -> Bimap a b -> Bimap a b
deleteR = deleteE . Right


{-| /O(log n)/.
Lookup a left key in the bimap, returning the associated right key.

This function will @return@ the result in the monad, or @fail@ if
the value isn't in the bimap.

Note that the signature differs slightly from Data.Map's @lookup@. This one is more general -
it functions the same way as the "original" if @m@ is cast (or inferred) to Maybe.

-}
lookup :: (Ord a, MonadThrow m)
       => a -> Bimap a b -> m b
lookup x (MkBimap left _) =
    maybe (throwM $ KeyNotFound "Data.Bimap.lookup")
          return
          (M.lookup x left)

{-| /O(log n)/.
A version of 'lookup' that is specialized to the right key,
and returns the corresponding left key.


-}
lookupR :: (Ord b, MonadThrow m)
        => b -> Bimap a b -> m a
lookupR y (MkBimap _ right) =
    maybe (throwM $ KeyNotFound "Data.Bimap.lookupR")
          return
          (M.lookup y right)

{-| /O(log n)/.
Find the right key corresponding to a given left key.
Calls @'error'@ when the key is not in the bimap.
-}
(!) :: (Ord a) => Bimap a b -> a -> b
(!) bi x = fromMaybe (error "Data.Bimap.(!): Left key not found") $ lookup x bi

{-| /O(log n)/.
A version of @(!)@ that is specialized to the right key,
and returns the corresponding left key.
-}
(!>) :: (Ord b) => Bimap a b -> b -> a
(!>) bi y = fromMaybe (error "Data.Bimap.(!>): Right key not found") $ lookupR y bi

{-| /O(log n)/.
See 'lookup'. -}
(!?) :: (Ord a, MonadThrow m) => Bimap a b -> a -> m b
(!?) = flip lookup

{-| /O(log n)/.
See 'lookupR'. -}
(!?>) :: (Ord b, MonadThrow m) => Bimap a b -> b -> m a
(!?>) = flip lookupR

{-| /O(n*log n)/.
Build a map from a list of pairs. If there are any overlapping
pairs in the list, the later ones will override the earlier ones.
-}
fromList :: (Ord a, Ord b)
         => [(a, b)] -> Bimap a b
fromList = foldl' (flip . uncurry $ insert) empty

{-| /O(n*log n)/.
Build a map from a list of pairs. Unlike 'fromList', earlier pairs
will take precedence over later ones.

The name @fromAList@ is a reference to Lisp-style association
lists, where associations can be overridden by prepending new ones.

Note that when duplicates occur in both the keys and in the values,
@fromList xs /= fromAList (reverse xs)@. However, if either
contains no duplicates, then the equality holds.

-}
fromAList :: (Ord a, Ord b)
          => [(a, b)] -> Bimap a b
fromAList = foldl' (flip . uncurry $ tryInsert) empty


-- | /O(n)/. Build a bimap from a distinct list of pairs, where both the @fst@
-- and @snd@ halves of the list are in strictly ascending order.
-- This precondition is /not/ checked; an invalid list will produce a
-- malformed bimap.
fromDistinctAscList :: [(a, b)] -> Bimap a b
fromDistinctAscList xs = MkBimap (M.fromDistinctAscList xs)
                                 (M.fromDistinctAscList $ fmap swap xs)

{-| /O(n)/. Convert to a list of associated pairs.
-}
toList :: Bimap a b -> [(a, b)]
toList = toAscList


{-| /O(n)/.
Convert to a list of associated pairs, with the left-hand
values in ascending order.

Since pair ordering is lexical, the pairs will also be in
ascending order.

-}
toAscList :: Bimap a b -> [(a, b)]
toAscList (MkBimap left _) = M.toList left

{-| /O(n)/.
Convert to a list of associated pairs, with the right-hand
values first in the pair and in ascending order.

Since pair ordering is lexical, the pairs will also be in
ascending order.

-}
toAscListR :: Bimap a b -> [(b, a)]
toAscListR = toAscList . twist

{-| /O(n)/.
Return all associated pairs in the bimap, with the left-hand
values in ascending order.
-}
assocs :: Bimap a b -> [(a, b)]
assocs = toList

{-| /O(n)/.
Return all left-hand keys in the bimap in ascending order.
-}
keys :: Bimap a b -> [a]
keys (MkBimap left _) = M.keys left


{-| /O(n)/.
Return all left-hand keys in the bimap in a 'Set'.
-}
keysSet :: Bimap a b -> Set a
keysSet (MkBimap left _) = M.keysSet left


{-| /O(n)/.
Return all right-hand keys in the bimap in ascending order.
-}
keysR :: Bimap a b -> [b]
keysR (MkBimap _ right) = M.keys right


{-| /O(n)/.
Return all left-hand keys in the bimap in a 'Set'.
-}
keysSetR :: Bimap a b -> Set b
keysSetR (MkBimap _ right) = M.keysSet right


{-| /O(n)/. An alias for 'keysR'.
-}
elems :: Bimap a b -> [b]
elems = keysR

{-| /O(1)/. Extract only the left-to-right component of a bimap.
-}
toMap :: Bimap a b -> M.Map a b
toMap (MkBimap left _) = left

{-| /O(1)/. Extract only the right-to-left component of a bimap.
-}
toMapR :: Bimap a b -> M.Map b a
toMapR (MkBimap _ right) = right

{-| /O(n)/.
Filter all association pairs that satisfy the predicate.

Note that the predicate will be applied /twice/ for each association
in the bimap.

-}
filter :: (a -> b -> Bool) -> Bimap a b -> Bimap a b
filter pred (MkBimap left right) =
    MkBimap
        (M.filterWithKey pred left)
        (M.filterWithKey (flip pred) right)

{-| /O(n)/.
Partition the bimap according to a predicate.
The first bimap contains all associations that satisfy the predicate;
the second contains all associations that fail the predicate.

Note that the predicate will be applied /twice/ for each association
in the bimap.

-}
partition :: (a -> b -> Bool) -> Bimap a b -> (Bimap a b, Bimap a b)
partition pred (MkBimap left right) =
    (,) (MkBimap leftA rightA) (MkBimap leftB rightB)
    where
    (leftA, leftB) = M.partitionWithKey pred left
    (rightA, rightB) = M.partitionWithKey (flip pred) right


{-| /O(n*log n)/.
Test if the internal bimap structure is valid. This should be true
for any bimap created using the public interface, unless
'fromAscPairListUnchecked' has been used inappropriately.
-}
valid :: (Ord a, Ord b)
      => Bimap a b -> Bool
valid (MkBimap left right) = and
    [ M.valid left, M.valid right
    , (==)
        (sort .                M.toList $ left )
        (sort . P.map flipPair . M.toList $ right)
    ]
    where
    flipPair (x, y) = (y, x)

{-| /O(1)/.
Reverse the positions of the two element types in the bimap.
-}
twist ::  Bimap a b -> Bimap b a
twist (MkBimap left right) = MkBimap right left

{-| /O(1)/.
Reverse the positions of the two element types in a bimap
transformation.
-}
twisted :: (Bimap a b -> Bimap a b) -> (Bimap b a -> Bimap b a)
twisted f = twist . f . twist

{-| /O(n)/.
Fold the association pairs in the map, such that
@'fold' f z == 'foldr' f z . 'assocs'@.
-}
fold :: (a -> b -> c -> c) -> c -> Bimap a b -> c
fold f z = foldr (uncurry f) z . assocs

{-| /O(n*log n)/
Map a function over all the left keys in the map.
-}
map :: Ord c => (a -> c) -> Bimap a b -> Bimap c b
map f (MkBimap left right) =
    MkBimap (M.mapKeys f left) (M.map f right)

{-| /O(n*log n)/
Map a function over all the right keys in the map.
-}
mapR :: Ord c => (b -> c) -> Bimap a b -> Bimap a c
mapR f (MkBimap left right) =
    MkBimap (M.map f left) (M.mapKeys f right)

{-| /O(n)/.
Map a strictly increasing function over all left keys in the map.
/The precondition is not checked./
-}
mapMonotonic :: (a -> c) -> Bimap a b -> Bimap c b
mapMonotonic f (MkBimap left right) =
    MkBimap (M.mapKeysMonotonic f left) (M.map f right)

{-| /O(n)/.
Map a strictly increasing function over all right keys in the map.
/The precondition is not checked./
-}
mapMonotonicR :: (b -> c) -> Bimap a b -> Bimap a c
mapMonotonicR f (MkBimap left right) =
    MkBimap (M.map f left) (M.mapKeysMonotonic f right)


-- | Zip two 'Bimap's using a combination function on the left keys, dropping keys not in both maps.
zipWith :: (Ord c, Ord k) => (a -> b -> c) -> Bimap k a -> Bimap k b -> Bimap k c
zipWith f (MkBimap xleft _) (MkBimap yleft _) 
    = MkBimap left right
    where
        left  = Merge.merge Merge.dropMissing Merge.dropMissing (Merge.zipWithMatched (\_ x y -> f x y)) xleft yleft
        right = M.fromList $ fmap swap $ M.toList left 


-- | Zip two 'Bimap's using a combination function on the right keys, dropping keys not in both maps.
zipWithR :: (Ord c, Ord k) => (a -> b -> c) -> Bimap a k -> Bimap b k -> Bimap c k
zipWithR f (MkBimap _ xright) (MkBimap _ yright) 
    = MkBimap left right
    where
        right  = Merge.merge Merge.dropMissing Merge.dropMissing (Merge.zipWithMatched (\_ x y -> f x y)) xright yright
        left = M.fromList $ fmap swap $ M.toList right 