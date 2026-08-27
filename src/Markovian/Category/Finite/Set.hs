{- | Opaque duplicate-free finite sets with explicit layouts.

Support equality ignores layout order. Layout equality compares represented
order. The ordinary 'Eq' instance is layout equality for compatibility with
the existing exact IR. Use 'sameFiniteSet' for semantic support equality.
Empty finite sets are valid.
-}
module Markovian.Category.Finite.Set (
    FiniteSet,
    FiniteSetError (..),
    finiteSet,
    finiteSetValues,
    finiteSetCardinality,
    sameFiniteSet,
    sameFiniteLayout,
    sameFiniteSetLayout,
) where

import Markovian.Category.Finite.Set.Internal (FiniteSet (..))

-- | Failure to construct a duplicate-free finite set.
newtype FiniteSetError value = DuplicateFiniteSetValue value
    deriving (Eq, Show)

-- | Validate a finite set. The empty list is valid.
finiteSet :: (Eq value) => [value] -> Either (FiniteSetError value) (FiniteSet value)
finiteSet values =
    case firstDuplicate values of
        Just duplicate -> Left (DuplicateFiniteSetValue duplicate)
        Nothing -> Right (UnsafeFiniteSet values)

-- | Read values in represented layout order.
finiteSetValues :: FiniteSet value -> [value]
finiteSetValues (UnsafeFiniteSet values) = values

-- | Read the number of represented values.
finiteSetCardinality :: FiniteSet value -> Int
finiteSetCardinality = length . finiteSetValues

-- | Compare semantic supports without considering layout order.
sameFiniteSet :: FiniteSet value -> FiniteSet value -> Bool
sameFiniteSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    all (`elem` right) left && all (`elem` left) right

-- | Compare support and layout order.
sameFiniteLayout :: FiniteSet value -> FiniteSet value -> Bool
sameFiniteLayout = (==)

-- | Descriptive alias for 'sameFiniteLayout'.
sameFiniteSetLayout :: FiniteSet value -> FiniteSet value -> Bool
sameFiniteSetLayout = sameFiniteLayout

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining
