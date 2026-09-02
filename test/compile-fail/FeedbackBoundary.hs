module FeedbackBoundary where

import Data.Coerce (coerce)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational)
import Markovian.Category.Matrix.Stochastic (StochasticMatrix)
import Markovian.Feedback.Channel.Exact
import Markovian.Feedback.Delay.Exact
import Markovian.Feedback.Timed.Exact

badLoopLayoutConstructor :: LoopLayout owner loop
badLoopLayoutConstructor = UnsafeLoopLayout undefined undefined

badCheckedFeedbackConstructor :: CheckedFeedback owner input loop output
badCheckedFeedbackConstructor = UnsafeCheckedFeedback undefined undefined

badDelayedConstructor :: CheckedDelayedFeedback owner input state output
badDelayedConstructor = UnsafeCheckedDelayedFeedback undefined undefined undefined undefined undefined undefined

badTimedConstructor :: CheckedTimedFeedback owner input loop output
badTimedConstructor = UnsafeCheckedTimedFeedback undefined undefined

badFeedbackEndpointCoerce ::
    CheckedFeedback String Bool Bool Bool ->
    CheckedFeedback String () Bool Bool
badFeedbackEndpointCoerce = coerce

badFeedbackOwnerCoerce ::
    CheckedFeedback String Bool Bool Bool ->
    CheckedFeedback () Bool Bool Bool
badFeedbackOwnerCoerce = coerce

badDelayedStateCoerce ::
    CheckedDelayedFeedback String Bool Bool Bool ->
    CheckedDelayedFeedback String Bool () Bool
badDelayedStateCoerce = coerce

badTimedLoopCoerce ::
    CheckedTimedFeedback String Bool Bool Bool ->
    CheckedTimedFeedback String Bool () Bool
badTimedLoopCoerce = coerce

badWitnessAsChannel ::
    CheckedFeedback String Bool Bool Bool ->
    StochasticMatrix NonNegativeRational Bool Bool
badWitnessAsChannel = id
