{-# LANGUAGE RoleAnnotations #-}

{- | Exact finite payoffs and their contravariant action through channels.

A payoff is a total rational-valued function on an explicit finite set. Unlike
an exact state, a payoff need not be nonnegative or normalized. Pullback is
conditional expectation through a normalized stochastic matrix. It is not
Bayesian inversion: it needs no prior and produces no posterior.
-}
module Markovian.Category.Payoff.Exact (
    ExactPayoff,
    ExactPayoffError (..),
    exactPayoff,
    exactPayoffFromFunction,
    exactPayoffObject,
    exactPayoffValues,
    exactPayoffValue,
    exactPayoffEquivalent,
    PayoffPullbackError (..),
    pullbackPayoff,
    StatePayoffPairingError (..),
    pairStatePayoff,
) where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational, getNonNegativeRational)
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix (matrixEntry)
import Markovian.Category.Matrix.Stochastic

-- | A total exact rational payoff on an explicit finite set.
type role ExactPayoff nominal

data ExactPayoff value = ExactPayoff !(FiniteSet value) ![(value, Rational)]

-- | Failure to construct a total finite payoff table.
data ExactPayoffError value
    = DuplicatePayoffValue !value
    | PayoffValueOutsideObject !value
    | MissingPayoffValue !value
    deriving (Eq, Show)

-- | Failure to pull a payoff through a channel with a different target.
data PayoffPullbackError = PayoffPullbackTargetMismatch
    deriving (Eq, Show)

-- | Failure to pair a normalized state matrix with a payoff.
data StatePayoffPairingError
    = StatePayoffSourceNotSingleton
    | StatePayoffObjectMismatch
    deriving (Eq, Show)

{- | Validate a complete payoff table.

Each represented value must occur exactly once. Entries are stored in the
finite set's layout order, so input order has no semantic effect.
-}
exactPayoff ::
    (Eq value) =>
    FiniteSet value ->
    [(value, Rational)] ->
    Either (ExactPayoffError value) (ExactPayoff value)
exactPayoff object entries = do
    case firstDuplicate (map fst entries) of
        Just duplicate -> Left (DuplicatePayoffValue duplicate)
        Nothing -> Right ()
    case firstOutside entries of
        Just outside -> Left (PayoffValueOutsideObject outside)
        Nothing -> Right ()
    case firstMissing (finiteSetValues object) of
        Just missing -> Left (MissingPayoffValue missing)
        Nothing ->
            Right
                ( ExactPayoff
                    object
                    [ (value, fromMaybe 0 (lookup value entries))
                    | value <- finiteSetValues object
                    ]
                )
  where
    firstOutside [] = Nothing
    firstOutside ((value, _) : remaining)
        | value `elem` finiteSetValues object = firstOutside remaining
        | otherwise = Just value
    firstMissing [] = Nothing
    firstMissing (value : remaining)
        | value `elem` map fst entries = firstMissing remaining
        | otherwise = Just value

-- | Construct a total payoff by evaluating a function on every represented value.
exactPayoffFromFunction :: FiniteSet value -> (value -> Rational) -> ExactPayoff value
exactPayoffFromFunction object function =
    ExactPayoff object [(value, function value) | value <- finiteSetValues object]

-- | Read the represented finite set.
exactPayoffObject :: ExactPayoff value -> FiniteSet value
exactPayoffObject (ExactPayoff object _) = object

-- | Read labelled payoff values in object-layout order.
exactPayoffValues :: ExactPayoff value -> [(value, Rational)]
exactPayoffValues (ExactPayoff _ values) = values

-- | Read one represented payoff. Values outside the object return 'Nothing'.
exactPayoffValue :: (Eq value) => ExactPayoff value -> value -> Maybe Rational
exactPayoffValue (ExactPayoff _ values) = (`lookup` values)

-- | Compare payoff objects and labelled values extensionally, ignoring layout.
exactPayoffEquivalent :: (Eq value) => ExactPayoff value -> ExactPayoff value -> Bool
exactPayoffEquivalent left right =
    sameFiniteSet (exactPayoffObject left) (exactPayoffObject right)
        && all
            (\value -> exactPayoffValue left value == exactPayoffValue right value)
            (finiteSetValues (exactPayoffObject left))

{- | Pull a payoff backward through an exact stochastic matrix.

For @K : X -> Y@ and @u : Y -> Rational@, the result is
@x -> sum_y K(x,y) * u(y)@.
-}
pullbackPayoff ::
    (Eq target) =>
    StochasticMatrix NonNegativeRational source target ->
    ExactPayoff target ->
    Either PayoffPullbackError (ExactPayoff source)
pullbackPayoff channel payoff
    | not (sameFiniteSet (stochasticTarget channel) (exactPayoffObject payoff)) =
        Left PayoffPullbackTargetMismatch
    | otherwise =
        Right $
            exactPayoffFromFunction (stochasticSource channel) $ \sourceValue ->
                foldl'
                    (+)
                    0
                    [ channelMass sourceValue targetValue * payoffAt targetValue
                    | targetValue <- finiteSetValues (stochasticTarget channel)
                    ]
  where
    channelMass sourceValue targetValue =
        maybe 0 getNonNegativeRational (matrixEntry (forgetStochastic channel) sourceValue targetValue)
    payoffAt targetValue = fromMaybe 0 (exactPayoffValue payoff targetValue)

{- | Pair a normalized state matrix @1 -> X@ with a payoff on @X@.

The source witness must be the represented singleton unit set. The result is
the exact expectation @sum_x p(x) * u(x)@.
-}
pairStatePayoff ::
    (Eq value) =>
    StochasticMatrix NonNegativeRational () value ->
    ExactPayoff value ->
    Either StatePayoffPairingError Rational
pairStatePayoff state payoff
    | finiteSetValues (stochasticSource state) /= [()] = Left StatePayoffSourceNotSingleton
    | not (sameFiniteSet (stochasticTarget state) (exactPayoffObject payoff)) =
        Left StatePayoffObjectMismatch
    | otherwise =
        Right $
            foldl'
                (+)
                0
                [ stateMass value * payoffAt value
                | value <- finiteSetValues (stochasticTarget state)
                ]
  where
    stateMass value =
        maybe 0 getNonNegativeRational (matrixEntry (forgetStochastic state) () value)
    payoffAt value = fromMaybe 0 (exactPayoffValue payoff value)

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining
