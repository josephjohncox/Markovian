{-# LANGUAGE RoleAnnotations #-}

{- | Typed exact Bayesian channels with checked prior flow.

Composition verifies that the second channel's input prior is the first
channel's pushforward. This indexed operation is not a @Category@ instance and
Bayesian inversion is not a dagger.
-}
module Markovian.Bayesian.Channel.Exact (
    BayesianChannel,
    BayesianChannelError (..),
    bayesianChannel,
    bayesianChannelInputPrior,
    bayesianChannelOutputPrior,
    bayesianChannelForward,
    bayesianChannelInverse,
    composeBayesianChannel,
) where

import Markovian.Algebra.NonNegativeRational
import Markovian.Bayesian.Exact
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic

-- | A channel paired with its input prior and exact pushforward prior.
type role BayesianChannel nominal nominal

data BayesianChannel source target
    = BayesianChannel
        !(Prior source)
        !(Prior target)
        !(StochasticMatrix NonNegativeRational source target)

-- | Typed Bayesian-channel construction or composition failure.
data BayesianChannelError
    = BayesianChannelBayesianError !BayesianError
    | BayesianChannelPriorMismatch
    | BayesianChannelMatrixError !MatrixError
    deriving (Eq, Show)

-- | Attach an input prior and compute the exact output prior.
bayesianChannel ::
    (Eq target) =>
    Prior source ->
    StochasticMatrix NonNegativeRational source target ->
    Either BayesianChannelError (BayesianChannel source target)
bayesianChannel input forward = do
    output <- mapBayesian (pushforward input forward)
    Right (BayesianChannel input output forward)

-- | Read the input prior.
bayesianChannelInputPrior :: BayesianChannel source target -> Prior source
bayesianChannelInputPrior (BayesianChannel input _ _) = input

-- | Read the exact pushforward prior.
bayesianChannelOutputPrior :: BayesianChannel source target -> Prior target
bayesianChannelOutputPrior (BayesianChannel _ output _) = output

-- | Read the forward stochastic channel.
bayesianChannelForward ::
    BayesianChannel source target ->
    StochasticMatrix NonNegativeRational source target
bayesianChannelForward (BayesianChannel _ _ forward) = forward

-- | Compute the prior-indexed support-restricted inverse.
bayesianChannelInverse ::
    (Eq source, Eq target) =>
    BayesianChannel source target ->
    Either BayesianChannelError (BayesianInverse source target)
bayesianChannelInverse channel =
    mapBayesian
        ( bayesianInverse
            (bayesianChannelInputPrior channel)
            (bayesianChannelForward channel)
        )

-- | Compose channels only when the represented middle prior agrees exactly.
composeBayesianChannel ::
    (Eq source, Eq middle, Eq target) =>
    BayesianChannel source middle ->
    BayesianChannel middle target ->
    Either BayesianChannelError (BayesianChannel source target)
composeBayesianChannel first second
    | not
        ( priorEquivalent
            (bayesianChannelOutputPrior first)
            (bayesianChannelInputPrior second)
        ) =
        Left BayesianChannelPriorMismatch
    | otherwise = do
        forward <-
            either
                (Left . BayesianChannelMatrixError)
                Right
                ( composeStochastic
                    (bayesianChannelForward first)
                    (bayesianChannelForward second)
                )
        bayesianChannel (bayesianChannelInputPrior first) forward

mapBayesian :: Either BayesianError value -> Either BayesianChannelError value
mapBayesian = either (Left . BayesianChannelBayesianError) Right
