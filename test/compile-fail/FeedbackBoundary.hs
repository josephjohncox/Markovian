module FeedbackBoundary where

import Data.Coerce (coerce)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet)
import Markovian.Category.Matrix.Stochastic (StochasticMatrix)
import Markovian.Feedback.Channel.Exact
import Markovian.Feedback.Delay.Exact
import Markovian.Feedback.Timed.Exact
import Markovian.Feedback.Value.Exact
import Markovian.Objective.Exact (ExactDiscount)

badLoopLayoutConstructor :: LoopLayout owner loop
badLoopLayoutConstructor = UnsafeLoopLayout undefined undefined

badCheckedFeedbackConstructor :: CheckedFeedback owner input loop output
badCheckedFeedbackConstructor = UnsafeCheckedFeedback undefined undefined

badDelayedConstructor :: CheckedDelayedFeedback owner input state output
badDelayedConstructor = UnsafeCheckedDelayedFeedback undefined undefined undefined undefined undefined undefined

badTimedConstructor :: CheckedTimedFeedback owner input loop output
badTimedConstructor = UnsafeCheckedTimedFeedback undefined undefined

badAffineCoefficientsConstructor :: AffineFeedbackCoefficients source output
badAffineCoefficientsConstructor = UnsafeAffineFeedbackCoefficients undefined undefined undefined undefined

badCheckedAffineFeedbackConstructor :: CheckedAffineFeedback owner input loop output
badCheckedAffineFeedbackConstructor = UnsafeCheckedAffineFeedback undefined undefined undefined

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

badAffineOwnerCoerce ::
    CheckedAffineFeedback String Bool Bool Bool ->
    CheckedAffineFeedback () Bool Bool Bool
badAffineOwnerCoerce = coerce

badAffineInputCoerce ::
    CheckedAffineFeedback String Bool Bool Bool ->
    CheckedAffineFeedback String () Bool Bool
badAffineInputCoerce = coerce

badAffineLoopCoerce ::
    CheckedAffineFeedback String Bool Bool Bool ->
    CheckedAffineFeedback String Bool () Bool
badAffineLoopCoerce = coerce

badAffineOutputCoerce ::
    CheckedAffineFeedback String Bool Bool Bool ->
    CheckedAffineFeedback String Bool Bool ()
badAffineOutputCoerce = coerce

badAffineCoefficientSourceCoerce ::
    AffineFeedbackCoefficients Bool Bool ->
    AffineFeedbackCoefficients () Bool
badAffineCoefficientSourceCoerce = coerce

badAffineCoefficientOutputCoerce ::
    AffineFeedbackCoefficients Bool Bool ->
    AffineFeedbackCoefficients Bool ()
badAffineCoefficientOutputCoerce = coerce

badWitnessAsChannel ::
    CheckedFeedback String Bool Bool Bool ->
    StochasticMatrix NonNegativeRational Bool Bool
badWitnessAsChannel = id

badAffineCoefficientsAsChannel ::
    AffineFeedbackCoefficients Bool Bool ->
    StochasticMatrix NonNegativeRational Bool Bool
badAffineCoefficientsAsChannel = id

badAffineFiniteDiscount ::
    (Eq input, Eq output) =>
    FeedbackLimits ->
    ExactDiscount ->
    FiniteSet input ->
    LoopLayout owner loop ->
    FiniteSet output ->
    StochasticMatrix NonNegativeRational (Either input loop) (FeedbackEvent loop output) ->
    Either (AffineFeedbackError loop output) (CheckedAffineFeedback owner input loop output)
badAffineFiniteDiscount = closeAffineFeedback
