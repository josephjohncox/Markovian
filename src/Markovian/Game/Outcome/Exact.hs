{-# LANGUAGE RoleAnnotations #-}

{- | Exact finite joint outcome laws for games.

A dynamic stage atom keeps its complete reward vector and successor together.
For additive risk-neutral value, the represented one-step expectation is fixed
by the two marginals.  The joint law still preserves pathwise pairs for
post-transition observations and future nonlinear or risk-sensitive semantics.
-}
module Markovian.Game.Outcome.Exact (
    ExactStageOutcome,
    exactStageOutcome,
    stageRewards,
    successorState,
    ExactOutcomeLaw,
    OutcomeLawError (..),
    exactOutcomeLaw,
    outcomeCarrier,
    outcomeEntries,
    outcomeMass,
    OutcomeExpectationError (..),
    outcomeExpectation,
    outcomePlayerValues,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

{- | One joint stage event.  Terminal value is deliberately not stored here;
it belongs to the reached state's terminal table and is paid on entry.
-}
data ExactStageOutcome owner state = ExactStageOutcome !(ExactPlayerValues owner) !state
    deriving (Eq, Show)

type role ExactStageOutcome nominal nominal

-- | Construct one joint reward/successor atom.
exactStageOutcome :: ExactPlayerValues owner -> state -> ExactStageOutcome owner state
exactStageOutcome = ExactStageOutcome

-- | Read the transition reward vector.
stageRewards :: ExactStageOutcome owner state -> ExactPlayerValues owner
stageRewards (ExactStageOutcome rewards _) = rewards

-- | Read the reached public state.
successorState :: ExactStageOutcome owner state -> state
successorState (ExactStageOutcome _ state) = state

-- | A complete literal probability table over an explicit finite carrier.
data ExactOutcomeLaw outcome = ExactOutcomeLaw !(FiniteObject outcome) ![(outcome, Rational)]
    deriving (Eq, Show)

type role ExactOutcomeLaw nominal

-- | Outcome-law construction failures.
data OutcomeLawError outcome
    = ExcessOutcomeEntries
    | DuplicateOutcomeEntry !outcome
    | MissingOutcomeEntry !outcome
    | OutcomeOutsideCarrier !outcome
    | NegativeOutcomeMass !outcome !Rational
    | OutcomeMassNotOne !Rational
    | OutcomeRationalLimitExceeded !outcome !Natural !Natural
    | OutcomeTotalRationalLimitExceeded !Natural !Natural
    | OutcomeWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Validate a complete exact law.  It never normalizes supplied masses.
exactOutcomeLaw :: (Eq outcome) => GameLimits -> FiniteObject outcome -> [(outcome, Rational)] -> Either (OutcomeLawError outcome) (ExactOutcomeLaw outcome)
exactOutcomeLaw limits carrier supplied = do
    let outcomes = NonEmpty.toList (finiteObjectValues carrier)
        bounded = take (length outcomes + 1) supplied
        work = fromIntegral (length outcomes)
    if work > maximumGameWork limits then Left (OutcomeWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    if length bounded > length outcomes then Left ExcessOutcomeEntries else pure ()
    case firstDuplicate (map fst bounded) of
        Just duplicate -> Left (DuplicateOutcomeEntry duplicate)
        Nothing -> pure ()
    case firstOutside outcomes (map fst bounded) of
        Just outside -> Left (OutcomeOutsideCarrier outside)
        Nothing -> pure ()
    entries <- traverse canonical outcomes
    total <- foldlM addMass 0 entries
    if total == 1 then Right (ExactOutcomeLaw carrier entries) else Left (OutcomeMassNotOne total)
  where
    canonical outcome = case lookup outcome supplied of
        Nothing -> Left (MissingOutcomeEntry outcome)
        Just mass
            | mass < 0 -> Left (NegativeOutcomeMass outcome mass)
            | rationalSizeBits mass > maximumGameRationalBits limits -> Left (OutcomeRationalLimitExceeded outcome (rationalSizeBits mass) (maximumGameRationalBits limits))
            | otherwise -> Right (outcome, mass)
    addMass accumulator (_, mass) =
        let total = accumulator + mass
            actual = rationalSizeBits total
         in if actual > maximumGameRationalBits limits
                then Left (OutcomeTotalRationalLimitExceeded actual (maximumGameRationalBits limits))
                else Right total

-- | Read the outcome carrier.
outcomeCarrier :: ExactOutcomeLaw outcome -> FiniteObject outcome
outcomeCarrier (ExactOutcomeLaw carrier _) = carrier

-- | Read the complete table.
outcomeEntries :: ExactOutcomeLaw outcome -> [(outcome, Rational)]
outcomeEntries (ExactOutcomeLaw _ entries) = entries

-- | Query one represented mass.
outcomeMass :: (Eq outcome) => ExactOutcomeLaw outcome -> outcome -> Maybe Rational
outcomeMass (ExactOutcomeLaw _ entries) outcome = lookup outcome entries

-- | Exact expectation failures.
data OutcomeExpectationError
    = OutcomeExpectationWorkLimitExceeded !Natural !Natural
    | OutcomeExpectationRationalLimitExceeded !Natural !Natural
    | OutcomeExpectationPlayerLayoutMismatch
    deriving (Eq, Show)

-- | Bounded exact scalar expectation over represented outcomes.
outcomeExpectation :: GameLimits -> ExactOutcomeLaw outcome -> (outcome -> Rational) -> Either OutcomeExpectationError Rational
outcomeExpectation limits law observation
    | work > maximumGameWork limits = Left (OutcomeExpectationWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = foldlM add 0 (outcomeEntries law)
  where
    work = fromIntegral (2 * length (outcomeEntries law))
    add accumulator (outcome, mass) = do
        _ <- checked mass
        observed <- checked (observation outcome)
        term <- checked (mass * observed)
        checked (accumulator + term)
    checked value = case checkRationalSize limits value of
        Left (actual, maximum_) -> Left (OutcomeExpectationRationalLimitExceeded actual maximum_)
        Right valid -> Right valid

-- | Expected player vector from one joint stage law.
outcomePlayerValues :: (Eq owner) => GameLimits -> ExactOutcomeLaw (ExactStageOutcome owner state) -> Either OutcomeExpectationError (ExactPlayerValues owner)
outcomePlayerValues limits law = case outcomeEntries law of
    [] -> error "outcomePlayerValues: nonempty law invariant"
    ((firstOutcome, _) : _) ->
        let entryCount = fromIntegral (length (outcomeEntries law))
            playerCount = fromIntegral (finiteObjectCardinality (playerValuesCarrier (stageRewards firstOutcome)))
            work = cappedGameProduct (maximumGameWork limits) entryCount (cappedGameAdd (maximumGameWork limits) (cappedGameProduct (maximumGameWork limits) playerCount 2) 1)
         in if work > maximumGameWork limits
                then Left (OutcomeExpectationWorkLimitExceeded work (maximumGameWork limits))
                else foldlM addWeighted (zeroPlayerValues (playerValuesCarrier (stageRewards firstOutcome))) (outcomeEntries law)
  where
    addWeighted accumulator (outcome, mass) = do
        weighted <- mapEvaluation (scalePlayerValues limits mass (stageRewards outcome))
        mapEvaluation (addPlayerValues limits accumulator weighted)
    mapEvaluation result = case result of
        Left (EvaluationRationalLimitExceeded _ actual maximum_) -> Left (OutcomeExpectationRationalLimitExceeded actual maximum_)
        Left _ -> Left OutcomeExpectationPlayerLayoutMismatch
        Right value -> Right value

foldlM :: (accumulator -> value -> Either error accumulator) -> accumulator -> [value] -> Either error accumulator
foldlM function = go
  where
    go accumulator [] = Right accumulator
    go accumulator (value : remaining) = case function accumulator value of
        Left problem -> Left problem
        Right next -> go next remaining

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

firstOutside :: (Eq value) => [value] -> [value] -> Maybe value
firstOutside _ [] = Nothing
firstOutside allowed (value : remaining)
    | value `notElem` allowed = Just value
    | otherwise = firstOutside allowed remaining
