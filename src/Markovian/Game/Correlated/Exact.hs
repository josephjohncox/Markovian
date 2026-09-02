{-# LANGUAGE RoleAnnotations #-}

{- | Exact correlated- and coarse-correlated-equilibrium candidate checks.

Correlated obedience uses unconditional slacks, so a recommendation of zero
probability needs no invented conditional belief.  CE and CCE remain distinct
reports and are not interchangeable with independent mixed profiles.
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
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

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
