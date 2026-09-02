{-# LANGUAGE DataKinds #-}

{- | Closed, finite, first-order differentiable programs.

There is intentionally no constructor for arbitrary Haskell callbacks. The
language has no recursion, branching, indexing callbacks, stochastic nodes,
@ReLU@, division, or higher-order functions.
-}
module Markovian.Autodiff.Language (
    Fragment (..),
    Program,
    identity,
    compose,
    parallel,
    fanout,
    shareParameters,
    constantScalar,
    constantVector,
    parameter,
    negateScalar,
    addScalar,
    multiplyScalar,
    addVector,
    hadamard,
    dot,
    sumVector,
    tanhScalar,
    tanhVector,
    first,
    second,
    liftPolynomial,
) where

import Markovian.Autodiff.Internal.Syntax
