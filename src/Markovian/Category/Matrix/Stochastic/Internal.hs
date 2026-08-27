{-# LANGUAGE RoleAnnotations #-}

module Markovian.Category.Matrix.Stochastic.Internal (
    StochasticMatrix (..),
) where

import Markovian.Category.Matrix (Matrix)

{- | A normalized matrix representation.
The constructor is shared only by proof-preserving refinement modules.
-}
type role StochasticMatrix nominal nominal nominal

newtype StochasticMatrix scalar source target
    = UnsafeStochasticMatrix (Matrix scalar source target)
