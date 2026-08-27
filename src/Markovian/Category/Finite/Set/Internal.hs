{-# LANGUAGE GADTs #-}

module Markovian.Category.Finite.Set.Internal (
    FiniteSet (..),
    FiniteObject (..),
) where

import Data.List.NonEmpty (NonEmpty)

{- | A finite-set representation with stored equality evidence.
Constructors are exposed only to finite-witness implementation modules.
-}
data FiniteSet value where
    UnsafeFiniteSet :: (Eq value) => [value] -> FiniteSet value

-- | A nonempty finite-object representation with stored equality evidence.
data FiniteObject value where
    UnsafeFiniteObject :: (Eq value) => NonEmpty value -> FiniteObject value

instance Eq (FiniteSet value) where
    UnsafeFiniteSet left == UnsafeFiniteSet right = left == right

instance (Show value) => Show (FiniteSet value) where
    showsPrec precedence (UnsafeFiniteSet values) =
        showParen (precedence > 10) (showString "FiniteSet " . showsPrec 11 values)

instance Eq (FiniteObject value) where
    UnsafeFiniteObject left == UnsafeFiniteObject right = left == right

instance (Show value) => Show (FiniteObject value) where
    showsPrec precedence (UnsafeFiniteObject values) =
        showParen (precedence > 10) (showString "FiniteObject " . showsPrec 11 values)
