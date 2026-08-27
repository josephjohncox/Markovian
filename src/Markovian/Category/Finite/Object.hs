{- | Nonempty finite objects for probability-bearing interfaces.

A 'FiniteObject' is a nonempty refinement of 'FiniteSet'. It preserves layout
order and stores the equality evidence established at construction. The
ordinary 'Eq' instance is layout equality for compatibility with the existing
exact IR. Use 'sameFiniteSupport' for semantic support equality.
-}
module Markovian.Category.Finite.Object (
    FiniteObject,
    FiniteObjectError (..),
    finiteObject,
    finiteObjectValues,
    finiteObjectCardinality,
    sameFiniteSupport,
    sameFiniteObjectLayout,
    forgetNonempty,
    requireNonempty,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Set (FiniteSet, sameFiniteSet)
import Markovian.Category.Finite.Set.Internal (FiniteObject (..), FiniteSet (..))

-- | Errors from finite object construction or refinement.
data FiniteObjectError value
    = EmptyFiniteObject
    | DuplicateFiniteObjectValue !value
    deriving (Eq, Show)

-- | Validate a nonempty duplicate-free finite object.
finiteObject :: (Eq value) => [value] -> Either (FiniteObjectError value) (FiniteObject value)
finiteObject [] = Left EmptyFiniteObject
finiteObject values@(first : remaining) =
    case firstDuplicate values of
        Just duplicate -> Left (DuplicateFiniteObjectValue duplicate)
        Nothing -> Right (UnsafeFiniteObject (first :| remaining))

-- | Read object values in represented layout order.
finiteObjectValues :: FiniteObject value -> NonEmpty value
finiteObjectValues (UnsafeFiniteObject values) = values

-- | Read the number of represented values.
finiteObjectCardinality :: FiniteObject value -> Int
finiteObjectCardinality = NonEmpty.length . finiteObjectValues

-- | Compare semantic support without considering layout order.
sameFiniteSupport :: FiniteObject value -> FiniteObject value -> Bool
sameFiniteSupport left right = sameFiniteSet (forgetNonempty left) (forgetNonempty right)

-- | Compare support and layout order.
sameFiniteObjectLayout :: FiniteObject value -> FiniteObject value -> Bool
sameFiniteObjectLayout = (==)

-- | Forget the nonempty proof without changing layout.
forgetNonempty :: FiniteObject value -> FiniteSet value
forgetNonempty (UnsafeFiniteObject values) = UnsafeFiniteSet (NonEmpty.toList values)

-- | Require a finite set to be nonempty.
requireNonempty :: FiniteSet value -> Either (FiniteObjectError value) (FiniteObject value)
requireNonempty (UnsafeFiniteSet values) =
    case values of
        [] -> Left EmptyFiniteObject
        first : remaining -> Right (UnsafeFiniteObject (first :| remaining))

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining
