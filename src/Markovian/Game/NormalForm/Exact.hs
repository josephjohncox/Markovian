{-# LANGUAGE RoleAnnotations #-}

{- | Exact finite normal-form games and independently mixed candidate checks.

The checker considers every pure unilateral deviation.  This also covers all
real mixed unilateral deviations because expected utility is affine in the
deviator's simplex.  It verifies a supplied candidate; it does not find an
equilibrium or claim that a rational equilibrium representation exists.
-}
module Markovian.Game.NormalForm.Exact (
    ExactPlayerValues,
    PlayerValuesError (..),
    exactPlayerValues,
    playerValuesCarrier,
    playerValue,
    playerValueEntries,
    zeroPlayerValues,
    addPlayerValues,
    scalePlayerValues,
    ExactNormalGame,
    NormalGameError (..),
    exactNormalGame,
    normalGameProduct,
    normalGamePayoffs,
    normalPayoff,
    ExactMixedProfile,
    MixedProfileError (..),
    exactMixedProfile,
    mixedProfileProduct,
    mixedProfileRows,
    mixedStrategyFor,
    mixedProfileProbability,
    diracMixedProfile,
    ExactEvaluationError (..),
    expectedUtility,
    pureDeviationUtility,
    MixedDeviation (..),
    MixedNashReport (..),
    checkMixedNash,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

-- | Complete exact values in player-layout order.
data ExactPlayerValues owner = ExactPlayerValues !(FiniteObject owner) ![(owner, Rational)]
    deriving (Eq, Show)

type role ExactPlayerValues nominal

-- | Player-value construction failures.
data PlayerValuesError owner
    = ExcessPlayerValueEntries
    | DuplicatePlayerValueOwner !owner
    | MissingPlayerValueOwner !owner
    | PlayerValueOwnerOutsideCarrier !owner
    | PlayerValueRationalLimitExceeded !owner !Natural !Natural
    | PlayerValueOwnerLimitExceeded !Natural !Natural
    | PlayerValueWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Validate a complete signed rational player vector.
exactPlayerValues :: (Eq owner) => GameLimits -> FiniteObject owner -> [(owner, Rational)] -> Either (PlayerValuesError owner) (ExactPlayerValues owner)
exactPlayerValues limits owners supplied = do
    let ownerValues = NonEmpty.toList (finiteObjectValues owners)
        ownerCount = fromIntegral (length ownerValues)
        bounded = take (length ownerValues + 1) supplied
    if ownerCount > maximumGameOwners limits then Left (PlayerValueOwnerLimitExceeded ownerCount (maximumGameOwners limits)) else pure ()
    if ownerCount > maximumGameWork limits then Left (PlayerValueWorkLimitExceeded ownerCount (maximumGameWork limits)) else pure ()
    if length bounded > length ownerValues then Left ExcessPlayerValueEntries else pure ()
    case firstDuplicate (map fst bounded) of
        Just duplicate -> Left (DuplicatePlayerValueOwner duplicate)
        Nothing -> pure ()
    case firstOutside ownerValues (map fst bounded) of
        Just outside -> Left (PlayerValueOwnerOutsideCarrier outside)
        Nothing -> pure ()
    entries <- traverse canonical ownerValues
    Right (ExactPlayerValues owners entries)
  where
    canonical owner = case lookup owner supplied of
        Nothing -> Left (MissingPlayerValueOwner owner)
        Just value
            | rationalSizeBits value > maximumGameRationalBits limits -> Left (PlayerValueRationalLimitExceeded owner (rationalSizeBits value) (maximumGameRationalBits limits))
            | otherwise -> Right (owner, value)

-- | Read the player carrier.
playerValuesCarrier :: ExactPlayerValues owner -> FiniteObject owner
playerValuesCarrier (ExactPlayerValues owners _) = owners

-- | Query one player value.
playerValue :: (Eq owner) => ExactPlayerValues owner -> owner -> Maybe Rational
playerValue (ExactPlayerValues _ entries) owner = lookup owner entries

-- | Read canonical entries.
playerValueEntries :: ExactPlayerValues owner -> [(owner, Rational)]
playerValueEntries (ExactPlayerValues _ entries) = entries

-- | The zero vector over a player carrier.
zeroPlayerValues :: FiniteObject owner -> ExactPlayerValues owner
zeroPlayerValues owners = ExactPlayerValues owners [(owner, 0) | owner <- NonEmpty.toList (finiteObjectValues owners)]

-- | Checked pointwise addition.  Carrier layouts must be equal.
addPlayerValues :: (Eq owner) => GameLimits -> ExactPlayerValues owner -> ExactPlayerValues owner -> Either ExactEvaluationError (ExactPlayerValues owner)
addPlayerValues limits (ExactPlayerValues owners left) (ExactPlayerValues other right)
    | owners /= other = Left EvaluationPlayerLayoutMismatch
    | otherwise = do
        traverse_ (validateValue "left player value" . snd) left
        traverse_ (validateValue "right player value" . snd) right
        ExactPlayerValues owners <$> traverse add left
  where
    validateValue label value = checkedRational limits label value >> Right ()
    add (owner, value) = do
        otherValue <- maybe (Left EvaluationPlayerLayoutMismatch) Right (lookup owner right)
        checked <- checkedRational limits "player-value addition" (value + otherValue)
        Right (owner, checked)

-- | Checked scalar multiplication.
scalePlayerValues :: GameLimits -> Rational -> ExactPlayerValues owner -> Either ExactEvaluationError (ExactPlayerValues owner)
scalePlayerValues limits scalar (ExactPlayerValues owners entries) = do
    _ <- checkedRational limits "player-value scalar" scalar
    traverse_ (\(_, value) -> checkedRational limits "player value" value >> Right ()) entries
    ExactPlayerValues owners <$> traverse scale entries
  where
    scale (owner, value) = (owner,) <$> checkedRational limits "player-value scaling" (scalar * value)

-- | A complete exact payoff table.
data ExactNormalGame owner action
    = ExactNormalGame
        !(OwnedProduct owner action)
        ![(OwnedProfile owner action, ExactPlayerValues owner)]
    deriving (Eq, Show)

type role ExactNormalGame nominal nominal

-- | Normal-game table failures.
data NormalGameError owner action
    = ExcessNormalPayoffEntries
    | DuplicateNormalPayoffProfile !(OwnedProfile owner action)
    | MissingNormalPayoffProfile !(OwnedProfile owner action)
    | NormalPayoffProfileOutsideProduct !(OwnedProfile owner action)
    | NormalPayoffPlayerLayoutMismatch !(OwnedProfile owner action)
    | NormalGameCellLimitExceeded !Natural !Natural
    | NormalGameWorkLimitExceeded !Natural !Natural
    | NormalGameProductError !(OwnedProductError owner)
    | NormalPayoffRationalLimitExceeded !(OwnedProfile owner action) !owner !Natural !Natural
    deriving (Eq, Show)

-- | Validate and canonicalize a complete payoff table.
exactNormalGame :: (Eq owner, Eq action) => GameLimits -> OwnedProduct owner action -> [(OwnedProfile owner action, ExactPlayerValues owner)] -> Either (NormalGameError owner action) (ExactNormalGame owner action)
exactNormalGame limits product_ supplied = do
    case validateOwnedProduct limits product_ of
        Left problem -> Left (NormalGameProductError problem)
        Right () -> pure ()
    let profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
        players = ownedOwners product_
        bounded = take (length profiles + 1) supplied
        profileCount = fromIntegral (finiteObjectCardinality (ownedProfiles product_))
        cells = cappedGameProduct (maximumGameCells limits) profileCount (fromIntegral (finiteObjectCardinality players))
        work = cappedGameAdd (maximumGameWork limits) cells profileCount
    if length bounded > length profiles then Left ExcessNormalPayoffEntries else pure ()
    if cells > maximumGameCells limits then Left (NormalGameCellLimitExceeded cells (maximumGameCells limits)) else pure ()
    if work > maximumGameWork limits then Left (NormalGameWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    case firstDuplicate (map fst bounded) of
        Just duplicate -> Left (DuplicateNormalPayoffProfile duplicate)
        Nothing -> pure ()
    case firstOutside profiles (map fst bounded) of
        Just outside -> Left (NormalPayoffProfileOutsideProduct outside)
        Nothing -> pure ()
    entries <- traverse (canonical players bounded) profiles
    Right (ExactNormalGame product_ entries)
  where
    canonical players entries profile = case lookup profile entries of
        Nothing -> Left (MissingNormalPayoffProfile profile)
        Just values
            | playerValuesCarrier values /= players -> Left (NormalPayoffPlayerLayoutMismatch profile)
            | otherwise -> do
                traverse_ (validatePayoff profile) (playerValueEntries values)
                Right (profile, values)
    validatePayoff profile (owner, value)
        | rationalSizeBits value > maximumGameRationalBits limits = Left (NormalPayoffRationalLimitExceeded profile owner (rationalSizeBits value) (maximumGameRationalBits limits))
        | otherwise = Right ()

-- | Read the action product.
normalGameProduct :: ExactNormalGame owner action -> OwnedProduct owner action
normalGameProduct (ExactNormalGame product_ _) = product_

-- | Read the canonical payoff table.
normalGamePayoffs :: ExactNormalGame owner action -> [(OwnedProfile owner action, ExactPlayerValues owner)]
normalGamePayoffs (ExactNormalGame _ payoffs) = payoffs

-- | Query one payoff coordinate.
normalPayoff :: (Eq owner, Eq action) => ExactNormalGame owner action -> owner -> OwnedProfile owner action -> Maybe Rational
normalPayoff (ExactNormalGame _ entries) owner profile = lookup profile entries >>= (`playerValue` owner)

-- | One exact simplex per owner, interpreted independently.
data ExactMixedProfile owner action
    = ExactMixedProfile
        !(OwnedProduct owner action)
        ![(owner, ExactSimplex action)]
    deriving (Eq, Show)

type role ExactMixedProfile nominal nominal

-- | Mixed-profile construction failures.
data MixedProfileError owner
    = ExcessMixedProfileRows
    | DuplicateMixedProfileOwner !owner
    | MissingMixedProfileOwner !owner
    | MixedProfileOwnerOutsideProduct !owner
    | MixedProfileActionLayoutMismatch !owner
    | MixedProfileProductError !(OwnedProductError owner)
    | MixedProfileSimplexInvalid !owner
    deriving (Eq, Show)

-- | Validate one independently randomized row per owner under active limits.
exactMixedProfile :: (Eq owner) => GameLimits -> OwnedProduct owner action -> [(owner, ExactSimplex action)] -> Either (MixedProfileError owner) (ExactMixedProfile owner action)
exactMixedProfile limits product_ supplied = do
    case validateOwnedProduct limits product_ of
        Left problem -> Left (MixedProfileProductError problem)
        Right () -> pure ()
    let owners = NonEmpty.toList (finiteObjectValues (ownedOwners product_))
        bounded = take (length owners + 1) supplied
    if length bounded > length owners then Left ExcessMixedProfileRows else pure ()
    case firstDuplicate (map fst bounded) of
        Just duplicate -> Left (DuplicateMixedProfileOwner duplicate)
        Nothing -> pure ()
    case firstOutside owners (map fst bounded) of
        Just outside -> Left (MixedProfileOwnerOutsideProduct outside)
        Nothing -> pure ()
    rows <- traverse canonical owners
    Right (ExactMixedProfile product_ rows)
  where
    canonical owner = case lookup owner supplied of
        Nothing -> Left (MissingMixedProfileOwner owner)
        Just simplex -> case ownedChoices product_ owner of
            Nothing -> Left (MixedProfileOwnerOutsideProduct owner)
            Just choices
                | simplexCarrier simplex /= choices -> Left (MixedProfileActionLayoutMismatch owner)
                | otherwise -> case validateExactSimplex limits simplex of
                    Left _ -> Left (MixedProfileSimplexInvalid owner)
                    Right () -> Right (owner, simplex)

-- | Read the owned action product.
mixedProfileProduct :: ExactMixedProfile owner action -> OwnedProduct owner action
mixedProfileProduct (ExactMixedProfile product_ _) = product_

-- | Read canonical owner/simplex rows.
mixedProfileRows :: ExactMixedProfile owner action -> [(owner, ExactSimplex action)]
mixedProfileRows (ExactMixedProfile _ rows) = rows

-- | Query one owner's simplex.
mixedStrategyFor :: (Eq owner) => ExactMixedProfile owner action -> owner -> Maybe (ExactSimplex action)
mixedStrategyFor (ExactMixedProfile _ rows) owner = lookup owner rows

{- | Product probability of a pure profile.  A profile outside the represented
product returns 'Nothing'.
-}
mixedProfileProbability :: (Eq owner, Eq action) => GameLimits -> ExactMixedProfile owner action -> OwnedProfile owner action -> Either ExactEvaluationError (Maybe Rational)
mixedProfileProbability limits (ExactMixedProfile product_ rows) profile
    | profile `notElem` NonEmpty.toList (finiteObjectValues (ownedProfiles product_)) = Right Nothing
    | work > maximumGameWork limits = Left (EvaluationWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        traverse_ validateRow rows
        Just <$> foldlM multiply 1 (ownedProfileEntries profile)
  where
    work = fromIntegral (length (ownedProfileEntries profile))
    validateRow (_, simplex) = do
        total <- foldlM addMass 0 (simplexEntries simplex)
        if total == 1 then Right () else Left EvaluationGameProfileMismatch
    addMass accumulator (_, mass) = do
        _ <- checkedRational limits "mixed simplex mass" mass
        checkedRational limits "mixed simplex total" (accumulator + mass)
    multiply accumulator (owner, action) = do
        mass <- maybe (Left EvaluationGameProfileMismatch) Right (lookup owner rows >>= (`simplexMass` action))
        _ <- checkedRational limits "mixed row mass" mass
        checkedRational limits "mixed profile probability" (accumulator * mass)

-- | Embed one pure profile as independent Dirac rows.
diracMixedProfile :: (Eq owner, Eq action) => GameLimits -> OwnedProduct owner action -> OwnedProfile owner action -> Either (MixedProfileError owner) (ExactMixedProfile owner action)
diracMixedProfile limits product_ profile = do
    rows <- traverse row (ownedProfileEntries profile)
    exactMixedProfile limits product_ rows
  where
    row (owner, selected) = case ownedChoices product_ owner of
        Nothing -> Left (MixedProfileOwnerOutsideProduct owner)
        Just carrier -> case exactSimplex limits carrier [(choice, if choice == selected then 1 else 0) | choice <- NonEmpty.toList (finiteObjectValues carrier)] of
            Left _ -> error "diracMixedProfile: validated profile/simplex invariant"
            Right simplex -> Right (owner, simplex)

-- | Bounded exact evaluation failures.
data ExactEvaluationError
    = EvaluationGameProfileMismatch
    | EvaluationPlayerLayoutMismatch
    | EvaluationWorkLimitExceeded !Natural !Natural
    | EvaluationRationalLimitExceeded !String !Natural !Natural
    deriving (Eq, Show)

-- | Exact expected utility under independent randomization.
expectedUtility :: (Eq owner, Eq action) => GameLimits -> ExactNormalGame owner action -> ExactMixedProfile owner action -> Either ExactEvaluationError (ExactPlayerValues owner)
expectedUtility limits game@(ExactNormalGame product_ _) mixed@(ExactMixedProfile mixedProduct _)
    | product_ /= mixedProduct = Left EvaluationGameProfileMismatch
    | work > maximumGameWork limits = Left (EvaluationWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        validateEvaluationInputs limits game mixed
        foldlM addWeighted (zeroPlayerValues (ownedOwners product_)) profiles
  where
    profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
    profileCount = fromIntegral (finiteObjectCardinality (ownedProfiles product_))
    playerSteps = cappedGameAdd (maximumGameWork limits) (fromIntegral (finiteObjectCardinality (ownedOwners product_))) 1
    work = cappedGameProduct (maximumGameWork limits) profileCount playerSteps
    addWeighted accumulator profile = do
        probability <- mixedProfileProbability limits mixed profile >>= maybe (Left EvaluationGameProfileMismatch) Right
        values <- maybe (Left EvaluationGameProfileMismatch) Right (lookup profile (normalGamePayoffs game))
        weighted <- scalePlayerValues limits probability values
        addPlayerValues limits accumulator weighted

{- | Exact expected utility from replacing one owner by one pure action while
all opponents retain their incumbent rows.
-}
pureDeviationUtility :: (Eq owner, Eq action) => GameLimits -> ExactNormalGame owner action -> ExactMixedProfile owner action -> owner -> action -> Either ExactEvaluationError Rational
pureDeviationUtility limits game@(ExactNormalGame product_ _) mixed@(ExactMixedProfile mixedProduct _) owner replacement
    | product_ /= mixedProduct = Left EvaluationGameProfileMismatch
    | work > maximumGameWork limits = Left (EvaluationWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        validateEvaluationInputs limits game mixed
        foldlM contribution 0 profiles
  where
    profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
    profileCount = fromIntegral (finiteObjectCardinality (ownedProfiles product_))
    playerSteps = cappedGameAdd (maximumGameWork limits) (fromIntegral (finiteObjectCardinality (ownedOwners product_))) 2
    work = cappedGameProduct (maximumGameWork limits) profileCount playerSteps
    contribution accumulator profile = do
        replaced <- case replaceChoice product_ owner replacement profile of
            Left _ -> Left EvaluationGameProfileMismatch
            Right value -> Right value
        -- Summing over complete incumbent profiles with their full product mass
        -- integrates the deviator's incumbent row to one.  Omitting that row
        -- would count each opponents' profile once per own action.
        incumbentMass <- foldlM multiply 1 (ownedProfileEntries profile)
        payoff <- maybe (Left EvaluationGameProfileMismatch) Right (normalPayoff game owner replaced)
        term <- checkedRational limits "deviation multiplication" (incumbentMass * payoff)
        checkedRational limits "deviation accumulation" (accumulator + term)
    multiply accumulator (other, action) = do
        mass <- maybe (Left EvaluationGameProfileMismatch) Right (mixedStrategyFor mixed other >>= (`simplexMass` action))
        _ <- checkedRational limits "incumbent row mass" mass
        checkedRational limits "incumbent profile mass" (accumulator * mass)

-- | One exact unilateral pure-deviation comparison.
data MixedDeviation owner action = MixedDeviation
    { deviationOwner :: !owner
    , deviationAction :: !action
    , incumbentExpectedPayoff :: !Rational
    , deviationExpectedPayoff :: !Rational
    , deviationGain :: !Rational
    }
    deriving (Eq, Show)

-- | Deterministic candidate-check report.
data MixedNashReport owner action = MixedNashReport
    { mixedNashSatisfied :: !Bool
    , mixedNashProfileCount :: !Natural
    , mixedNashDeviationCount :: !Natural
    , mixedNashArithmeticWork :: !Natural
    , mixedNashDeviations :: ![MixedDeviation owner action]
    }
    deriving (Eq, Show)

{- | Check every pure unilateral deviation.  No candidate list is returned on
any budget or rational-size failure.
-}
checkMixedNash :: (Eq owner, Eq action) => GameLimits -> ExactNormalGame owner action -> ExactMixedProfile owner action -> Either ExactEvaluationError (MixedNashReport owner action)
checkMixedNash limits game@(ExactNormalGame product_ _) mixed@(ExactMixedProfile mixedProduct _)
    | product_ /= mixedProduct = Left EvaluationGameProfileMismatch
    | totalWork > maximumGameWork limits = Left (EvaluationWorkLimitExceeded totalWork (maximumGameWork limits))
    | otherwise = do
        incumbent <- expectedUtility limits game mixed
        deviations <- traverse (check incumbent) deviationPairs
        Right
            MixedNashReport
                { mixedNashSatisfied = all ((<= 0) . deviationGain) deviations
                , mixedNashProfileCount = profileCount
                , mixedNashDeviationCount = deviationCount
                , mixedNashArithmeticWork = totalWork
                , mixedNashDeviations = deviations
                }
  where
    profileCount = fromIntegral (finiteObjectCardinality (ownedProfiles product_))
    deviationCount = foldl (cappedGameAdd (maximumGameWork limits)) 0 [fromIntegral (finiteObjectCardinality choices) | (_, choices) <- ownedProductRows product_]
    deviationPairs = [(owner, action) | (owner, choices) <- ownedProductRows product_, action <- NonEmpty.toList (finiteObjectValues choices)]
    playerCount = fromIntegral (finiteObjectCardinality (ownedOwners product_))
    expectedSteps = cappedGameProduct (maximumGameWork limits) profileCount (cappedGameAdd (maximumGameWork limits) playerCount 1)
    deviationSteps = cappedGameProduct (maximumGameWork limits) deviationCount (cappedGameProduct (maximumGameWork limits) profileCount (cappedGameAdd (maximumGameWork limits) playerCount 2))
    totalWork = cappedGameAdd (maximumGameWork limits) expectedSteps deviationSteps
    check incumbent (owner, action) = do
        incumbentValue <- maybe (Left EvaluationPlayerLayoutMismatch) Right (playerValue incumbent owner)
        alternative <- pureDeviationUtility limits game mixed owner action
        gain <- checkedRational limits "deviation gain" (alternative - incumbentValue)
        Right (MixedDeviation owner action incumbentValue alternative gain)

validateEvaluationInputs :: (Eq owner, Eq action) => GameLimits -> ExactNormalGame owner action -> ExactMixedProfile owner action -> Either ExactEvaluationError ()
validateEvaluationInputs limits (ExactNormalGame product_ payoffs) (ExactMixedProfile mixedProduct rows)
    | product_ /= mixedProduct = Left EvaluationGameProfileMismatch
    | otherwise = do
        case validateOwnedProduct limits product_ of
            Left _ -> Left EvaluationGameProfileMismatch
            Right () -> pure ()
        traverse_ validatePayoff payoffs
        traverse_ validateRow rows
  where
    validatePayoff (_, values) = traverse_ (\(_, value) -> checkedRational limits "normal payoff" value >> Right ()) (playerValueEntries values)
    validateRow (_, simplex) = do
        total <- foldlM addMass 0 (simplexEntries simplex)
        if total == 1 then Right () else Left EvaluationGameProfileMismatch
    addMass accumulator (_, mass) = do
        _ <- checkedRational limits "mixed simplex mass" mass
        checkedRational limits "mixed simplex total" (accumulator + mass)

checkedRational :: GameLimits -> String -> Rational -> Either ExactEvaluationError Rational
checkedRational limits label value = case checkRationalSize limits value of
    Left (actual, maximum_) -> Left (EvaluationRationalLimitExceeded label actual maximum_)
    Right checked -> Right checked

traverse_ :: (a -> Either error ()) -> [a] -> Either error ()
traverse_ function = foldlM (\() value -> function value) ()

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
