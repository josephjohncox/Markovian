{-# LANGUAGE RoleAnnotations #-}

{- | Exact correlated- and coarse-correlated-equilibrium candidate checks and
first-witness solvers.

Correlated obedience uses unconditional slacks, so a recommendation of zero
probability needs no invented conditional belief.  CE and CCE remain distinct
reports and are not interchangeable with independent mixed profiles.

The solvers return the first deterministically checked witness in exact
'Rational' arithmetic.  They do not optimize an objective, solve Nash,
enumerate equilibria, or return all vertices.  Reservations are conservative
represented-work and logical-field blocks; they are not a physical time, heap,
or stack bound.
-}
module Markovian.Game.Correlated.Exact (
    ExactCorrelationDevice,
    CorrelationDeviceError (..),
    exactCorrelationDevice,
    correlationEntries,
    correlationMass,
    correlationExpectedUtility,
    RecommendationStatus (..),
    ObedienceCheck (..),
    CorrelatedEquilibriumReport (..),
    CorrelatedCheckError (..),
    checkCorrelatedEquilibrium,
    CoarseDeviationCheck (..),
    CoarseCorrelatedEquilibriumReport (..),
    checkCoarseCorrelatedEquilibrium,
    isIndependentCorrelation,

    -- * Exact first-witness CE and CCE solvers
    CorrelationSolveLimits,
    correlationSolveLimits,
    correlationSolveGameLimits,
    maximumCorrelationSolveInequalities,
    maximumCorrelationSolveCandidates,
    maximumCorrelationSolveMaterialization,
    CorrelationSolvePhase (..),
    CorrelationSolveResource (..),
    CorrelationRepresentation (..),
    CorrelationSolveInvariant (..),
    CorrelationSolveError (..),
    CorrelationSolveAccounting,
    correlationSolveReservedWork,
    correlationSolveReservedMaterialization,
    correlationSolveObservedRationalBits,
    correlationSolveCheckerCoveredRationalBits,
    correlationSolveCandidates,
    correlationSolveRankDeficientCandidates,
    correlationSolveInconsistentCandidates,
    correlationSolveInequalityRejectedCandidates,
    correlationSolveSelectedInequalities,
    CorrelatedEquilibriumSolution,
    correlatedSolutionGame,
    correlatedSolutionDevice,
    correlatedSolutionCheck,
    correlatedSolutionAccounting,
    solveCorrelatedEquilibrium,
    CoarseCorrelatedEquilibriumSolution,
    coarseCorrelatedSolutionGame,
    coarseCorrelatedSolutionDevice,
    coarseCorrelatedSolutionCheck,
    coarseCorrelatedSolutionAccounting,
    solveCoarseCorrelatedEquilibrium,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Game.Correlated.Exact.Internal qualified as Internal
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

-- The solver contract specifies the count fold used by its work schedule.
{-# ANN module ("HLint: ignore Use sum" :: String) #-}

-- | A complete joint distribution over pure profiles.
data ExactCorrelationDevice owner action
    = ExactCorrelationDevice
        !(OwnedProduct owner action)
        ![(OwnedProfile owner action, Rational)]
    deriving (Eq, Show)

type role ExactCorrelationDevice nominal nominal

-- | Correlation-device construction failures.
data CorrelationDeviceError owner action
    = ExcessCorrelationEntries
    | DuplicateCorrelationProfile !(OwnedProfile owner action)
    | MissingCorrelationProfile !(OwnedProfile owner action)
    | CorrelationProfileOutsideProduct !(OwnedProfile owner action)
    | NegativeCorrelationMass !(OwnedProfile owner action) !Rational
    | CorrelationMassNotOne !Rational
    | CorrelationRationalLimitExceeded !(OwnedProfile owner action) !Natural !Natural
    | CorrelationTotalRationalLimitExceeded !Natural !Natural
    | CorrelationWorkLimitExceeded !Natural !Natural
    | CorrelationProductError !(OwnedProductError owner)
    deriving (Eq, Show)

{- | Validate a literal complete joint profile distribution.  Input is not
normalized and duplicate labels are rejected.
-}
exactCorrelationDevice :: (Eq owner, Eq action) => GameLimits -> OwnedProduct owner action -> [(OwnedProfile owner action, Rational)] -> Either (CorrelationDeviceError owner action) (ExactCorrelationDevice owner action)
exactCorrelationDevice limits product_ supplied = do
    case validateOwnedProduct limits product_ of
        Left problem -> Left (CorrelationProductError problem)
        Right () -> pure ()
    let profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
        bounded = take (length profiles + 1) supplied
        work = fromIntegral (length profiles)
    if work > maximumGameWork limits then Left (CorrelationWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    if length bounded > length profiles then Left ExcessCorrelationEntries else pure ()
    case firstDuplicate (map fst bounded) of
        Just duplicate -> Left (DuplicateCorrelationProfile duplicate)
        Nothing -> pure ()
    case firstOutside profiles (map fst bounded) of
        Just outside -> Left (CorrelationProfileOutsideProduct outside)
        Nothing -> pure ()
    entries <- traverse canonical profiles
    total <- foldlM addMass 0 entries
    if total == 1
        then Right (ExactCorrelationDevice product_ entries)
        else Left (CorrelationMassNotOne total)
  where
    canonical profile = case lookup profile supplied of
        Nothing -> Left (MissingCorrelationProfile profile)
        Just mass
            | mass < 0 -> Left (NegativeCorrelationMass profile mass)
            | rationalSizeBits mass > maximumGameRationalBits limits -> Left (CorrelationRationalLimitExceeded profile (rationalSizeBits mass) (maximumGameRationalBits limits))
            | otherwise -> Right (profile, mass)
    addMass accumulator (_, mass) =
        let total = accumulator + mass
            actual = rationalSizeBits total
         in if actual > maximumGameRationalBits limits
                then Left (CorrelationTotalRationalLimitExceeded actual (maximumGameRationalBits limits))
                else Right total

-- | Read the canonical complete table.
correlationEntries :: ExactCorrelationDevice owner action -> [(OwnedProfile owner action, Rational)]
correlationEntries (ExactCorrelationDevice _ entries) = entries

-- | Query one represented profile mass.
correlationMass :: (Eq owner, Eq action) => ExactCorrelationDevice owner action -> OwnedProfile owner action -> Maybe Rational
correlationMass (ExactCorrelationDevice product_ entries) profile
    | profile `elem` NonEmpty.toList (finiteObjectValues (ownedProfiles product_)) = lookup profile entries
    | otherwise = Nothing

-- | Candidate-check failures.
data CorrelatedCheckError
    = CorrelatedGameDeviceMismatch
    | CorrelatedWorkLimitExceeded !Natural !Natural
    | CorrelatedRationalLimitExceeded !String !Natural !Natural
    | CorrelatedInternalLayoutMismatch
    deriving (Eq, Show)

-- | Exact utility under the joint device.
correlationExpectedUtility :: (Eq owner, Eq action) => GameLimits -> ExactNormalGame owner action -> ExactCorrelationDevice owner action -> Either CorrelatedCheckError (ExactPlayerValues owner)
correlationExpectedUtility limits game device@(ExactCorrelationDevice product_ _)
    | normalGameProduct game /= product_ = Left CorrelatedGameDeviceMismatch
    | work > maximumGameWork limits = Left (CorrelatedWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        validateDevice limits device
        foldlM addWeighted (zeroPlayerValues (ownedOwners product_)) (correlationEntries device)
  where
    entryCount = fromIntegral (length (correlationEntries device))
    playerSteps = cappedGameAdd (maximumGameWork limits) (fromIntegral (finiteObjectCardinality (ownedOwners product_))) 1
    work = cappedGameProduct (maximumGameWork limits) entryCount playerSteps
    addWeighted accumulator (profile, mass) = do
        values <- maybe (Left CorrelatedInternalLayoutMismatch) Right (lookup profile (normalGamePayoffs game))
        weighted <- mapEvaluation (scalePlayerValues limits mass values)
        mapEvaluation (addPlayerValues limits accumulator weighted)

-- | Whether an obedience row has positive recommendation probability.
data RecommendationStatus = PositiveRecommendation | NullRecommendation
    deriving (Eq, Show)

-- | One direct-recommendation obedience inequality.
data ObedienceCheck owner action = ObedienceCheck
    { recommendedFor :: !owner
    , recommendedAction :: !action
    , alternativeAction :: !action
    , recommendationMass :: !Rational
    , recommendationStatus :: !RecommendationStatus
    , obedienceSlack :: !Rational
    }
    deriving (Eq, Show)

-- | Deterministic CE candidate report.
data CorrelatedEquilibriumReport owner action = CorrelatedEquilibriumReport
    { correlatedEquilibriumSatisfied :: !Bool
    , correlatedProfileCount :: !Natural
    , correlatedObedienceCount :: !Natural
    , correlatedArithmeticWork :: !Natural
    , correlatedObedienceChecks :: ![ObedienceCheck owner action]
    }
    deriving (Eq, Show)

-- | Check all unconditional CE obedience inequalities.
checkCorrelatedEquilibrium :: (Eq owner, Eq action) => GameLimits -> ExactNormalGame owner action -> ExactCorrelationDevice owner action -> Either CorrelatedCheckError (CorrelatedEquilibriumReport owner action)
checkCorrelatedEquilibrium limits game device@(ExactCorrelationDevice product_ _)
    | normalGameProduct game /= product_ = Left CorrelatedGameDeviceMismatch
    | work > maximumGameWork limits = Left (CorrelatedWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        validateDevice limits device
        checks <- traverse check obedienceRows
        Right
            CorrelatedEquilibriumReport
                { correlatedEquilibriumSatisfied = all ((>= 0) . obedienceSlack) checks
                , correlatedProfileCount = profileCount
                , correlatedObedienceCount = fromIntegral (length checks)
                , correlatedArithmeticWork = work
                , correlatedObedienceChecks = checks
                }
  where
    profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
    profileCount = fromIntegral (length profiles)
    obedienceRows =
        [ (owner, recommended, alternative)
        | (owner, choices) <- ownedProductRows product_
        , recommended <- NonEmpty.toList (finiteObjectValues choices)
        , alternative <- NonEmpty.toList (finiteObjectValues choices)
        , alternative /= recommended
        ]
    obedienceCount = foldl (cappedGameAdd (maximumGameWork limits)) 0 [cappedGameProduct (maximumGameWork limits) count (if count == 0 then 0 else count - 1) | (_, choices) <- ownedProductRows product_, let count = fromIntegral (finiteObjectCardinality choices)]
    work = cappedGameProduct (maximumGameWork limits) obedienceCount (cappedGameProduct (maximumGameWork limits) profileCount 4)
    check (owner, recommended, alternative) = do
        let matching = [(profile, mass) | (profile, mass) <- correlationEntries device, profileChoice profile owner == Just recommended]
        recommendation <- foldlM addRecommendation 0 matching
        slack <- foldlM (contribution owner alternative) 0 matching
        Right
            ObedienceCheck
                { recommendedFor = owner
                , recommendedAction = recommended
                , alternativeAction = alternative
                , recommendationMass = recommendation
                , recommendationStatus = if recommendation == 0 then NullRecommendation else PositiveRecommendation
                , obedienceSlack = slack
                }
    addRecommendation accumulator (_, mass) = checked limits "CE recommendation mass" (accumulator + mass)
    contribution owner alternative accumulator (profile, mass) = do
        replacement <- either (const (Left CorrelatedInternalLayoutMismatch)) Right (replaceChoice product_ owner alternative profile)
        incumbent <- maybe (Left CorrelatedInternalLayoutMismatch) Right (normalPayoff game owner profile)
        deviating <- maybe (Left CorrelatedInternalLayoutMismatch) Right (normalPayoff game owner replacement)
        difference <- checked limits "CE payoff difference" (incumbent - deviating)
        term <- checked limits "CE weighted slack" (mass * difference)
        checked limits "CE slack accumulation" (accumulator + term)

-- | One constant pre-recommendation deviation inequality.
data CoarseDeviationCheck owner action = CoarseDeviationCheck
    { coarseDeviationOwner :: !owner
    , coarseAlternativeAction :: !action
    , coarseDeviationSlack :: !Rational
    }
    deriving (Eq, Show)

-- | Deterministic CCE candidate report.
data CoarseCorrelatedEquilibriumReport owner action = CoarseCorrelatedEquilibriumReport
    { coarseCorrelatedEquilibriumSatisfied :: !Bool
    , coarseCorrelatedProfileCount :: !Natural
    , coarseDeviationCount :: !Natural
    , coarseArithmeticWork :: !Natural
    , coarseDeviationChecks :: ![CoarseDeviationCheck owner action]
    }
    deriving (Eq, Show)

-- | Check all constant pre-recommendation deviations.
checkCoarseCorrelatedEquilibrium :: (Eq owner, Eq action) => GameLimits -> ExactNormalGame owner action -> ExactCorrelationDevice owner action -> Either CorrelatedCheckError (CoarseCorrelatedEquilibriumReport owner action)
checkCoarseCorrelatedEquilibrium limits game device@(ExactCorrelationDevice product_ _)
    | normalGameProduct game /= product_ = Left CorrelatedGameDeviceMismatch
    | work > maximumGameWork limits = Left (CorrelatedWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        validateDevice limits device
        checks <- traverse check rows
        Right
            CoarseCorrelatedEquilibriumReport
                { coarseCorrelatedEquilibriumSatisfied = all ((>= 0) . coarseDeviationSlack) checks
                , coarseCorrelatedProfileCount = profileCount
                , coarseDeviationCount = fromIntegral (length checks)
                , coarseArithmeticWork = work
                , coarseDeviationChecks = checks
                }
  where
    profileCount = fromIntegral (length (correlationEntries device))
    rowCount = foldl (cappedGameAdd (maximumGameWork limits)) 0 [fromIntegral (finiteObjectCardinality choices) | (_, choices) <- ownedProductRows product_]
    work = cappedGameProduct (maximumGameWork limits) rowCount (cappedGameProduct (maximumGameWork limits) profileCount 4)
    rows = [(owner, alternative) | (owner, choices) <- ownedProductRows product_, alternative <- NonEmpty.toList (finiteObjectValues choices)]
    check (owner, alternative) = do
        slack <- foldlM (contribution owner alternative) 0 (correlationEntries device)
        Right (CoarseDeviationCheck owner alternative slack)
    contribution owner alternative accumulator (profile, mass) = do
        replacement <- either (const (Left CorrelatedInternalLayoutMismatch)) Right (replaceChoice product_ owner alternative profile)
        incumbent <- maybe (Left CorrelatedInternalLayoutMismatch) Right (normalPayoff game owner profile)
        deviating <- maybe (Left CorrelatedInternalLayoutMismatch) Right (normalPayoff game owner replacement)
        difference <- checked limits "CCE payoff difference" (incumbent - deviating)
        term <- checked limits "CCE weighted slack" (mass * difference)
        checked limits "CCE slack accumulation" (accumulator + term)

{- | Check whether a device is exactly the product distribution of a supplied
mixed profile.
-}
isIndependentCorrelation :: (Eq owner, Eq action) => GameLimits -> ExactMixedProfile owner action -> ExactCorrelationDevice owner action -> Either CorrelatedCheckError Bool
isIndependentCorrelation limits mixed device@(ExactCorrelationDevice product_ entries)
    | mixedProfileProduct mixed /= product_ = Left CorrelatedGameDeviceMismatch
    | otherwise = do
        validateDevice limits device
        results <- traverse agrees entries
        Right (and results)
  where
    agrees (profile, mass) = do
        productMass <- mapEvaluation (mixedProfileProbability limits mixed profile) >>= maybe (Left CorrelatedGameDeviceMismatch) Right
        checkedMass <- checked limits "correlation mass" mass
        Right (productMass == checkedMass)

validateDevice :: GameLimits -> ExactCorrelationDevice owner action -> Either CorrelatedCheckError ()
validateDevice limits (ExactCorrelationDevice product_ entries) = do
    case validateOwnedProduct limits product_ of
        Left _ -> Left CorrelatedGameDeviceMismatch
        Right () -> pure ()
    total <- foldlM add 0 entries
    if total == 1 then Right () else Left CorrelatedInternalLayoutMismatch
  where
    add accumulator (_, mass)
        | mass < 0 = Left CorrelatedInternalLayoutMismatch
        | otherwise = do
            _ <- checked limits "correlation mass" mass
            checked limits "correlation total" (accumulator + mass)

checked :: GameLimits -> String -> Rational -> Either CorrelatedCheckError Rational
checked limits label value = case checkRationalSize limits value of
    Left (actual, maximum_) -> Left (CorrelatedRationalLimitExceeded label actual maximum_)
    Right valid -> Right valid

mapEvaluation :: Either ExactEvaluationError value -> Either CorrelatedCheckError value
mapEvaluation result = case result of
    Left (EvaluationRationalLimitExceeded label actual maximum_) -> Left (CorrelatedRationalLimitExceeded label actual maximum_)
    Left (EvaluationWorkLimitExceeded required limit) -> Left (CorrelatedWorkLimitExceeded required limit)
    Left _ -> Left CorrelatedInternalLayoutMismatch
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

-- * Exact first-witness CE and CCE solvers

{- | Configured solve caps.  The constructor performs no game traversal and
supplies no defaults.  Every cap is inclusive and zero is a valid cap.  Work and
'Rational' caps come from the embedded 'GameLimits', whose horizon field has no
solver use.
-}
newtype CorrelationSolveLimits = CorrelationSolveLimits Internal.CorrelationSolveLimits
    deriving (Eq, Show)

-- | Construct solve limits: inequality, candidate, then materialization cap.
correlationSolveLimits :: GameLimits -> Natural -> Natural -> Natural -> CorrelationSolveLimits
correlationSolveLimits limits inequalities candidates materialization =
    CorrelationSolveLimits (Internal.correlationSolveLimits limits inequalities candidates materialization)

-- | Read the embedded game limits.
correlationSolveGameLimits :: CorrelationSolveLimits -> GameLimits
correlationSolveGameLimits (CorrelationSolveLimits limits) =
    Internal.correlationSolveGameLimits' limits

-- | Read the inclusive inequality cap.
maximumCorrelationSolveInequalities :: CorrelationSolveLimits -> Natural
maximumCorrelationSolveInequalities (CorrelationSolveLimits limits) =
    Internal.maximumCorrelationSolveInequalities' limits

-- | Read the inclusive attempted-candidate cap.
maximumCorrelationSolveCandidates :: CorrelationSolveLimits -> Natural
maximumCorrelationSolveCandidates (CorrelationSolveLimits limits) =
    Internal.maximumCorrelationSolveCandidates' limits

-- | Read the inclusive cumulative materialization cap.
maximumCorrelationSolveMaterialization :: CorrelationSolveLimits -> Natural
maximumCorrelationSolveMaterialization (CorrelationSolveLimits limits) =
    Internal.maximumCorrelationSolveMaterialization' limits

{- | The cumulative solve account.  Accessors are functions, not record
selectors, so clients cannot construct or update an accounting value.
-}
newtype CorrelationSolveAccounting = CorrelationSolveAccounting Internal.CorrelationSolveAccounting
    deriving (Eq, Show)

-- | Cumulative reserved work.
correlationSolveReservedWork :: CorrelationSolveAccounting -> Natural
correlationSolveReservedWork (CorrelationSolveAccounting account) =
    Internal.correlationSolveReservedWork' account

-- | Cumulative reserved materialization credits.
correlationSolveReservedMaterialization :: CorrelationSolveAccounting -> Natural
correlationSolveReservedMaterialization (CorrelationSolveAccounting account) =
    Internal.correlationSolveReservedMaterialization' account

{- | The historical observed combined 'Rational'-bit maximum, including shadow
arithmetic and rejected candidates.
-}
correlationSolveObservedRationalBits :: CorrelationSolveAccounting -> Natural
correlationSolveObservedRationalBits (CorrelationSolveAccounting account) =
    Internal.correlationSolveObservedRationalBits' account

{- | The maximum covered by the shadow's sequence-equality proof.  This is not a
claim that the unmodified checker emitted tracing events.
-}
correlationSolveCheckerCoveredRationalBits :: CorrelationSolveAccounting -> Natural
correlationSolveCheckerCoveredRationalBits (CorrelationSolveAccounting account) =
    Internal.correlationSolveCheckerCoveredRationalBits' account

-- | Attempted candidate count.
correlationSolveCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveCandidates (CorrelationSolveAccounting account) =
    Internal.correlationSolveCandidates' account

-- | Rank-deficient rejections.
correlationSolveRankDeficientCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveRankDeficientCandidates (CorrelationSolveAccounting account) =
    Internal.correlationSolveRankDeficientCandidates' account

-- | Inconsistent rejections.
correlationSolveInconsistentCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveInconsistentCandidates (CorrelationSolveAccounting account) =
    Internal.correlationSolveInconsistentCandidates' account

-- | Inequality rejections.
correlationSolveInequalityRejectedCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveInequalityRejectedCandidates (CorrelationSolveAccounting account) =
    Internal.correlationSolveInequalityRejectedCandidates' account

-- | The selected zero-based inequality indices of the successful candidate.
correlationSolveSelectedInequalities :: CorrelationSolveAccounting -> [Natural]
correlationSolveSelectedInequalities (CorrelationSolveAccounting account) =
    Internal.correlationSolveSelectedInequalities' account

-- | Ordered solve phases.
data CorrelationSolvePhase
    = CorrelationAdmission
    | CorrelationConstraints
    | CorrelationCombination
    | CorrelationElimination
    | CorrelationInequalities
    | CorrelationVerification
    | CorrelationPublication
    deriving (Eq, Show)

-- | Metered resources.
data CorrelationSolveResource
    = CorrelationInequalityCount
    | CorrelationCandidateCount
    | CorrelationMaterialization
    | CorrelationWork
    | CorrelationRationalBits
    deriving (Eq, Show)

-- | Representational length ceilings.
data CorrelationRepresentation
    = CorrelationOwnerLength
    | CorrelationChoiceLength
    | CorrelationProfileLength
    | CorrelationReportLength
    | CorrelationRationalLength
    deriving (Eq, Show)

-- | Internal consistency failures.
data CorrelationSolveInvariant
    = CorrelationInputLayoutInvariant
    | CorrelationConstraintLayoutInvariant
    | CorrelationCandidateShapeInvariant
    | CorrelationShadowVerificationInvariant
    | CorrelationCheckerDisagreement
    | CorrelationCompletedSearchWithoutWitness
    deriving (Eq, Show)

{- | Solve failures.  For 'CorrelationSolveLimitExceeded' the two numbers are the
cap and the saturated required value, in that order; on failure the required
value is exactly @cap + 1@ and is not a claimed exact demand beyond the cap.
Errors carry no candidate, device, accounting snapshot, or resumable state.

There is no @NoEquilibrium@ constructor and no successful @Nothing@.  A
completed public search without a witness is
'CorrelationCompletedSearchWithoutWitness', an invariant failure rather than a
mathematical nonexistence result.
-}
data CorrelationSolveError owner action
    = CorrelationSolveProductError !(OwnedProductError owner)
    | CorrelationSolveLimitExceeded
        !CorrelationSolvePhase
        !CorrelationSolveResource
        !Natural
        !Natural
    | CorrelationSolveRepresentationExceeded !CorrelationRepresentation
    | CorrelationSolveDeviceError !(CorrelationDeviceError owner action)
    | CorrelationSolveCheckerError !CorrelatedCheckError
    | CorrelationSolveInvariantFailure !CorrelationSolveInvariant
    deriving (Eq, Show)

type role CorrelationSolveError nominal nominal

{- | A checked CE witness: the original game handle, the literal device, the
actual checker report, and the cumulative accounting.  Retaining the game handle
does not copy its payoff table.
-}
data CorrelatedEquilibriumSolution owner action
    = CorrelatedEquilibriumSolution
        !(ExactNormalGame owner action)
        !(ExactCorrelationDevice owner action)
        !(CorrelatedEquilibriumReport owner action)
        !CorrelationSolveAccounting
    deriving (Eq, Show)

type role CorrelatedEquilibriumSolution nominal nominal

-- | Read the original game handle.
correlatedSolutionGame :: CorrelatedEquilibriumSolution owner action -> ExactNormalGame owner action
correlatedSolutionGame (CorrelatedEquilibriumSolution game _ _ _) = game

-- | Read the checked literal device.
correlatedSolutionDevice :: CorrelatedEquilibriumSolution owner action -> ExactCorrelationDevice owner action
correlatedSolutionDevice (CorrelatedEquilibriumSolution _ device _ _) = device

-- | Read the actual CE checker report.
correlatedSolutionCheck :: CorrelatedEquilibriumSolution owner action -> CorrelatedEquilibriumReport owner action
correlatedSolutionCheck (CorrelatedEquilibriumSolution _ _ report _) = report

-- | Read the cumulative accounting.
correlatedSolutionAccounting :: CorrelatedEquilibriumSolution owner action -> CorrelationSolveAccounting
correlatedSolutionAccounting (CorrelatedEquilibriumSolution _ _ _ account) = account

-- | A checked CCE witness.  Not interchangeable with a CE solution.
data CoarseCorrelatedEquilibriumSolution owner action
    = CoarseCorrelatedEquilibriumSolution
        !(ExactNormalGame owner action)
        !(ExactCorrelationDevice owner action)
        !(CoarseCorrelatedEquilibriumReport owner action)
        !CorrelationSolveAccounting
    deriving (Eq, Show)

type role CoarseCorrelatedEquilibriumSolution nominal nominal

-- | Read the original game handle.
coarseCorrelatedSolutionGame :: CoarseCorrelatedEquilibriumSolution owner action -> ExactNormalGame owner action
coarseCorrelatedSolutionGame (CoarseCorrelatedEquilibriumSolution game _ _ _) = game

-- | Read the checked literal device.
coarseCorrelatedSolutionDevice :: CoarseCorrelatedEquilibriumSolution owner action -> ExactCorrelationDevice owner action
coarseCorrelatedSolutionDevice (CoarseCorrelatedEquilibriumSolution _ device _ _) = device

-- | Read the actual CCE checker report.
coarseCorrelatedSolutionCheck :: CoarseCorrelatedEquilibriumSolution owner action -> CoarseCorrelatedEquilibriumReport owner action
coarseCorrelatedSolutionCheck (CoarseCorrelatedEquilibriumSolution _ _ report _) = report

-- | Read the cumulative accounting.
coarseCorrelatedSolutionAccounting :: CoarseCorrelatedEquilibriumSolution owner action -> CorrelationSolveAccounting
coarseCorrelatedSolutionAccounting (CoarseCorrelatedEquilibriumSolution _ _ _ account) = account

{- | Search for the first deterministically checked CE witness in exact
'Rational' arithmetic.

This returns the first checked witness in the fixed candidate order.  It does
not optimize an objective, solve Nash, enumerate equilibria, or return all
vertices, and first active-set success makes no global witness-optimization
claim.  Resource exhaustion is a terminal error: it never skips a tuple, changes
the order, reports nonexistence, or returns a partial witness.
-}
solveCorrelatedEquilibrium ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    Either (CorrelationSolveError owner action) (CorrelatedEquilibriumSolution owner action)
solveCorrelatedEquilibrium (CorrelationSolveLimits limits) game =
    case Internal.runSolve (searchWitness limits Internal.CorrelatedMode game verify) of
        Left fault -> Left (mapFault fault)
        Right (Left problem, _) -> Left problem
        Right (Right (device, report), account) ->
            Right (CorrelatedEquilibriumSolution game device report (CorrelationSolveAccounting account))
  where
    embedded = Internal.correlationSolveGameLimits' limits
    verify shadow device = case checkCorrelatedEquilibrium embedded game device of
        Left problem -> Left (CorrelationSolveCheckerError problem)
        Right report -> case agreesWithCorrelatedShadow embedded game device shadow report of
            Just problem -> Left problem
            Nothing -> Right report

{- | Search for the first deterministically checked CCE witness in exact
'Rational' arithmetic.  Every CE is a CCE, but the two solution types are
distinct and not interchangeable.
-}
solveCoarseCorrelatedEquilibrium ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    Either (CorrelationSolveError owner action) (CoarseCorrelatedEquilibriumSolution owner action)
solveCoarseCorrelatedEquilibrium (CorrelationSolveLimits limits) game =
    case Internal.runSolve (searchWitness limits Internal.CoarseMode game verify) of
        Left fault -> Left (mapFault fault)
        Right (Left problem, _) -> Left problem
        Right (Right (device, report), account) ->
            Right (CoarseCorrelatedEquilibriumSolution game device report (CorrelationSolveAccounting account))
  where
    embedded = Internal.correlationSolveGameLimits' limits
    verify shadow device = case checkCoarseCorrelatedEquilibrium embedded game device of
        Left problem -> Left (CorrelationSolveCheckerError problem)
        Right report -> case agreesWithCoarseShadow embedded device shadow report of
            Just problem -> Left problem
            Nothing -> Right report

{- | The shared phase pipeline: admission, constraints, then streamed
combination, elimination, inequality checking, verification, and publication.
The mode supplies its row family and its verification callback.
-}
searchWitness ::
    (Eq owner, Eq action) =>
    Internal.CorrelationSolveLimits ->
    Internal.SolveMode ->
    ExactNormalGame owner action ->
    ([Internal.ShadowRow owner action] -> ExactCorrelationDevice owner action -> Either (CorrelationSolveError owner action) report) ->
    Internal.Solve owner (Either (CorrelationSolveError owner action) (ExactCorrelationDevice owner action, report))
searchWitness limits mode game verify = do
    (owners, profiles, locals) <- Internal.admitGame limits game
    let dimension = 1 + owners + foldl (+) 0 locals + profiles
    Internal.reserveBlock limits Internal.CorrelationConstraints dimension
    (_, rows) <- Internal.admitGeometry limits mode profiles locals
    Internal.observeRational limits Internal.CorrelationConstraints 0
    Internal.observeRational limits Internal.CorrelationConstraints 1
    let constraintDimension = dimension + rows
    constraints <- Internal.buildConstraints limits mode game constraintDimension
    stream constraints constraintDimension profiles rows
  where
    embedded = Internal.correlationSolveGameLimits' limits
    canonical = Internal.carrierValues (ownedProfiles (normalGameProduct game))
    labels = Internal.deviationLabels mode (normalGameProduct game)
    stream constraints dimension profiles rows = do
        Internal.reserveBlock limits Internal.CorrelationCombination dimension
        let selected = Internal.initialTuple (profiles - 1)
        Internal.creditCandidate limits
        go constraints dimension profiles rows selected
    go constraints dimension profiles rows selected = do
        matrix <- Internal.candidateMatrix limits dimension constraints selected
        (eliminated, pivots) <- Internal.eliminate limits dimension profiles matrix
        Internal.reserveBlock limits Internal.CorrelationElimination dimension
        case Internal.classify profiles pivots eliminated of
            Internal.Inconsistent -> do
                countInconsistent
                advance constraints dimension profiles rows selected
            Internal.RankDeficient -> do
                countRankDeficient
                advance constraints dimension profiles rows selected
            Internal.FullRank masses -> do
                admitted <- Internal.verifyCandidate limits dimension constraints masses
                if not admitted
                    then do
                        countInequalityRejected
                        advance constraints dimension profiles rows selected
                    else do
                        let entries = zip canonical masses
                        Internal.reserveBlock limits Internal.CorrelationVerification dimension
                        shadow <- case mode of
                            Internal.CorrelatedMode -> Internal.correlatedShadow limits game entries labels
                            Internal.CoarseMode -> Internal.coarseShadow limits game entries labels
                        Internal.reserveBlock limits Internal.CorrelationVerification dimension
                        case exactCorrelationDevice embedded (normalGameProduct game) entries of
                            Left problem -> pure (Left (CorrelationSolveDeviceError problem))
                            Right device -> do
                                Internal.reserveBlock limits Internal.CorrelationVerification dimension
                                case verify shadow device of
                                    Left problem -> pure (Left problem)
                                    Right report -> do
                                        Internal.reserveBlock limits Internal.CorrelationPublication dimension
                                        recordSelected selected
                                        pure (Right (device, report))
    advance constraints dimension profiles rows selected = do
        Internal.reserveBlock limits Internal.CorrelationCombination dimension
        case Internal.successorTuple rows selected of
            Nothing ->
                pure (Left (CorrelationSolveInvariantFailure CorrelationCompletedSearchWithoutWitness))
            Just next -> do
                Internal.creditCandidate limits
                go constraints dimension profiles rows next
    countInconsistent = do
        account <- Internal.readAccount
        Internal.writeAccount
            account
                { Internal.correlationSolveInconsistentCandidates' =
                    Internal.correlationSolveInconsistentCandidates' account + 1
                }
    countRankDeficient = do
        account <- Internal.readAccount
        Internal.writeAccount
            account
                { Internal.correlationSolveRankDeficientCandidates' =
                    Internal.correlationSolveRankDeficientCandidates' account + 1
                }
    countInequalityRejected = do
        account <- Internal.readAccount
        Internal.writeAccount
            account
                { Internal.correlationSolveInequalityRejectedCandidates' =
                    Internal.correlationSolveInequalityRejectedCandidates' account + 1
                }
    recordSelected selected = do
        account <- Internal.readAccount
        Internal.writeAccount account{Internal.correlationSolveSelectedInequalities' = selected}

{- | Compare the actual CE report against the shadow, the expected counts, and
the checker's existing @4qn@ work formula, and require its satisfaction flag.
-}
agreesWithCorrelatedShadow ::
    (Eq owner, Eq action) =>
    GameLimits ->
    ExactNormalGame owner action ->
    ExactCorrelationDevice owner action ->
    [Internal.ShadowRow owner action] ->
    CorrelatedEquilibriumReport owner action ->
    Maybe (CorrelationSolveError owner action)
agreesWithCorrelatedShadow limits game device shadow report
    | not (correlatedEquilibriumSatisfied report) = disagreement
    | correlatedProfileCount report /= profiles = disagreement
    | correlatedObedienceCount report /= Internal.naturalCount checks = disagreement
    | Internal.naturalCount shadow /= Internal.naturalCount checks = disagreement
    | correlatedArithmeticWork report /= expectedWork = disagreement
    | not (and (zipWith agrees shadow checks)) = disagreement
    | otherwise = Nothing
  where
    checks = correlatedObedienceChecks report
    profiles = Internal.naturalCount (Internal.carrierValues (ownedProfiles (normalGameProduct game)))
    rowCount = Internal.naturalCount checks
    expectedWork =
        cappedGameProduct
            (maximumGameWork limits)
            rowCount
            (cappedGameProduct (maximumGameWork limits) (Internal.naturalCount (correlationEntries device)) 4)
    agrees row check =
        Internal.shadowRecommendation row == recommendationMass check
            && Internal.shadowSlack row == obedienceSlack check
            && recommendationStatus check == (if Internal.shadowRecommendation row == 0 then NullRecommendation else PositiveRecommendation)
    disagreement = Just (CorrelationSolveInvariantFailure CorrelationCheckerDisagreement)

-- | Compare the actual CCE report against the shadow and the expected counts.
agreesWithCoarseShadow ::
    (Eq owner, Eq action) =>
    GameLimits ->
    ExactCorrelationDevice owner action ->
    [Internal.ShadowRow owner action] ->
    CoarseCorrelatedEquilibriumReport owner action ->
    Maybe (CorrelationSolveError owner action)
agreesWithCoarseShadow limits device shadow report
    | not (coarseCorrelatedEquilibriumSatisfied report) = disagreement
    | coarseCorrelatedProfileCount report /= profiles = disagreement
    | coarseDeviationCount report /= Internal.naturalCount checks = disagreement
    | Internal.naturalCount shadow /= Internal.naturalCount checks = disagreement
    | coarseArithmeticWork report /= expectedWork = disagreement
    | not (and (zipWith agrees shadow checks)) = disagreement
    | otherwise = Nothing
  where
    checks = coarseDeviationChecks report
    profiles = Internal.naturalCount (correlationEntries device)
    rowCount = Internal.naturalCount checks
    expectedWork =
        cappedGameProduct
            (maximumGameWork limits)
            rowCount
            (cappedGameProduct (maximumGameWork limits) profiles 4)
    agrees row check = Internal.shadowSlack row == coarseDeviationSlack check
    disagreement = Just (CorrelationSolveInvariantFailure CorrelationCheckerDisagreement)

-- | Map a private fault onto the frozen public error type.
mapFault :: Internal.SolveFault owner -> CorrelationSolveError owner action
mapFault fault = case fault of
    Internal.SolveProductFault problem -> CorrelationSolveProductError problem
    Internal.SolveLimitFault phase resource cap required ->
        CorrelationSolveLimitExceeded (mapPhase phase) (mapResource resource) cap required
    Internal.SolveRepresentationFault representation ->
        CorrelationSolveRepresentationExceeded (mapRepresentation representation)
    Internal.SolveInvariantFault invariant ->
        CorrelationSolveInvariantFailure (mapInvariant invariant)

mapPhase :: Internal.CorrelationSolvePhase -> CorrelationSolvePhase
mapPhase phase = case phase of
    Internal.CorrelationAdmission -> CorrelationAdmission
    Internal.CorrelationConstraints -> CorrelationConstraints
    Internal.CorrelationCombination -> CorrelationCombination
    Internal.CorrelationElimination -> CorrelationElimination
    Internal.CorrelationInequalities -> CorrelationInequalities
    Internal.CorrelationVerification -> CorrelationVerification
    Internal.CorrelationPublication -> CorrelationPublication

mapResource :: Internal.CorrelationSolveResource -> CorrelationSolveResource
mapResource resource = case resource of
    Internal.CorrelationInequalityCount -> CorrelationInequalityCount
    Internal.CorrelationCandidateCount -> CorrelationCandidateCount
    Internal.CorrelationMaterialization -> CorrelationMaterialization
    Internal.CorrelationWork -> CorrelationWork
    Internal.CorrelationRationalBits -> CorrelationRationalBits

mapRepresentation :: Internal.CorrelationRepresentation -> CorrelationRepresentation
mapRepresentation representation = case representation of
    Internal.CorrelationOwnerLength -> CorrelationOwnerLength
    Internal.CorrelationChoiceLength -> CorrelationChoiceLength
    Internal.CorrelationProfileLength -> CorrelationProfileLength
    Internal.CorrelationReportLength -> CorrelationReportLength
    Internal.CorrelationRationalLength -> CorrelationRationalLength

mapInvariant :: Internal.CorrelationSolveInvariant -> CorrelationSolveInvariant
mapInvariant invariant = case invariant of
    Internal.CorrelationInputLayoutInvariant -> CorrelationInputLayoutInvariant
    Internal.CorrelationConstraintLayoutInvariant -> CorrelationConstraintLayoutInvariant
    Internal.CorrelationCandidateShapeInvariant -> CorrelationCandidateShapeInvariant
    Internal.CorrelationShadowVerificationInvariant -> CorrelationShadowVerificationInvariant
    Internal.CorrelationCheckerDisagreement -> CorrelationCheckerDisagreement
    Internal.CorrelationCompletedSearchWithoutWitness -> CorrelationCompletedSearchWithoutWitness
