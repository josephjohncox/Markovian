{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-indexed finite tensor shapes. A shape of @'[]@ is one scalar.
module Markovian.Tensor.Shape (
    SShape (SNil, SCons),
    KnownShape (knownShape),
    Vector,
    Matrix,
    shapeDimensions,
    shapeRank,
    shapeElements,
) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Numeric.Natural (Natural)

-- | Singleton evidence for a type-level list of dimensions.
data SShape (shape :: [Nat]) where
    SNil :: SShape '[]
    SCons :: (KnownNat dimension) => Proxy dimension -> SShape rest -> SShape (dimension ': rest)

type role SShape nominal

-- | Recover a singleton for a statically known shape.
class KnownShape shape where
    knownShape :: SShape shape

instance KnownShape '[] where
    knownShape = SNil

instance (KnownNat dimension, KnownShape rest) => KnownShape (dimension ': rest) where
    knownShape = SCons Proxy knownShape

-- | A rank-one shape.
type Vector n = '[n]

-- | A rank-two row-major logical shape.
type Matrix rows columns = '[rows, columns]

-- | Dimensions in outermost-to-innermost order.
shapeDimensions :: forall shape. SShape shape -> [Natural]
shapeDimensions SNil = []
shapeDimensions (SCons (_ :: Proxy dimension) rest) = fromInteger (natVal (Proxy @dimension)) : shapeDimensions rest

-- | Number of dimensions.
shapeRank :: SShape shape -> Natural
shapeRank = fromIntegral . length . shapeDimensions

-- | Number of represented scalar elements. Rank zero has one element.
shapeElements :: SShape shape -> Natural
shapeElements = product . shapeDimensions
