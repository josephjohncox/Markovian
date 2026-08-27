module RefinementRoles where

import Data.Coerce (coerce)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational)
import Markovian.Bayesian.Channel.Exact (BayesianChannel)
import Markovian.Bayesian.Exact (BayesianInverse, Posterior, Prior, Support)
import Markovian.Category.Convex.Exact (ConvexFamily)
import Markovian.Category.Matrix.Deterministic (DeterministicMatrix)
import Markovian.Category.Matrix.Stochastic (StochasticMatrix)
import Markovian.Open.Pushout (FinitePushout)
import Markovian.POMDP.Exact (ExactBelief)

newtype DifferentScalar = DifferentScalar NonNegativeRational
newtype DifferentSource = DifferentSource Bool

newtype CollapsedState = CollapsedState Bool

instance Eq CollapsedState where
    _ == _ = True

breakStochastic ::
    StochasticMatrix NonNegativeRational source target ->
    StochasticMatrix DifferentScalar source target
breakStochastic = coerce

breakDeterministic ::
    DeterministicMatrix NonNegativeRational source target ->
    DeterministicMatrix DifferentScalar source target
breakDeterministic = coerce

breakConvex ::
    ConvexFamily NonNegativeRational source target ->
    ConvexFamily DifferentScalar source target
breakConvex = coerce

breakStochasticSource ::
    StochasticMatrix NonNegativeRational Bool target ->
    StochasticMatrix NonNegativeRational DifferentSource target
breakStochasticSource = coerce

breakDeterministicSource ::
    DeterministicMatrix NonNegativeRational Bool target ->
    DeterministicMatrix NonNegativeRational DifferentSource target
breakDeterministicSource = coerce

breakConvexSource ::
    ConvexFamily NonNegativeRational Bool target ->
    ConvexFamily NonNegativeRational DifferentSource target
breakConvexSource = coerce

breakPriorSource :: Prior Bool -> Prior DifferentSource
breakPriorSource = coerce

breakSupportSource :: Support Bool -> Support DifferentSource
breakSupportSource = coerce

breakPosteriorSource :: Posterior Bool -> Posterior DifferentSource
breakPosteriorSource = coerce

breakInverseSource ::
    BayesianInverse Bool target ->
    BayesianInverse DifferentSource target
breakInverseSource = coerce

breakChannelSource ::
    BayesianChannel Bool target ->
    BayesianChannel DifferentSource target
breakChannelSource = coerce

breakPushoutCommon ::
    FinitePushout sort Bool left right ->
    FinitePushout sort DifferentSource left right
breakPushoutCommon = coerce

breakBeliefState :: ExactBelief Bool -> ExactBelief CollapsedState
breakBeliefState = coerce
