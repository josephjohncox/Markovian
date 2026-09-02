{-# LANGUAGE RoleAnnotations #-}

{- | Exact one-shot finite Harsanyi games with a correlated common prior.

Each player observes only its own type and randomizes privately.  The first
bounded fragment uses one action carrier per player (not a type-dependent
carrier).  Interim checks use unnormalized exact sums.  Null types receive no
invented posterior and are reported explicitly.
-}
module Markovian.Game.Harsanyi.Exact (
    ExactTypePrior,
    TypePriorError (..),
    exactTypePrior,
    typePriorEntries,
    typePriorMass,
    ExactBehaviorProfile,
    BehaviorProfileError (..),
    exactBehaviorProfile,
    behaviorStrategyAt,
    ExactHarsanyiGame,
    HarsanyiGameError (..),
    exactHarsanyiGame,
    harsanyiTypes,
    harsanyiActions,
    BayesTypeStatus (..),
    BayesDeviation (..),
    PositiveTypeBayesNashReport (..),
    BayesCheckError (..),
    checkPositiveTypeInterimBayesNash,
    ExAnteBayesNashReport (..),
    checkExAnteBayesNash,
    ContingentPlan,
    contingentPlanEntries,
    StrategicNormalError (..),
    toStrategicNormalForm,
) where

import Control.Monad (replicateM)
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

-- | Common prior over complete type profiles.  It may correlate types.
data ExactTypePrior owner typ
    = ExactTypePrior
        !(OwnedProduct owner typ)
        ![(OwnedProfile owner typ, Rational)]
    deriving (Eq, Show)

type role ExactTypePrior nominal nominal

-- | Common-prior construction failures.
data TypePriorError owner typ
    = ExcessTypePriorEntries
    | DuplicateTypePriorProfile !(OwnedProfile owner typ)
    | MissingTypePriorProfile !(OwnedProfile owner typ)
    | TypePriorProfileOutsideProduct !(OwnedProfile owner typ)
    | NegativeTypePriorMass !(OwnedProfile owner typ) !Rational
    | TypePriorMassNotOne !Rational
    | TypePriorRationalLimitExceeded !(OwnedProfile owner typ) !Natural !Natural
    | TypePriorTotalRationalLimitExceeded !Natural !Natural
    | TypePriorWorkLimitExceeded !Natural !Natural
    | TypePriorProductError !(OwnedProductError owner)
    deriving (Eq, Show)

-- | Validate a literal common prior without normalization.
exactTypePrior :: (Eq owner, Eq typ) => GameLimits -> OwnedProduct owner typ -> [(OwnedProfile owner typ, Rational)] -> Either (TypePriorError owner typ) (ExactTypePrior owner typ)
exactTypePrior limits product_ supplied = do
    case validateOwnedProduct limits product_ of
        Left problem -> Left (TypePriorProductError problem)
        Right () -> pure ()
    let profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
        bounded = take (length profiles + 1) supplied
        work = fromIntegral (length profiles)
    if work > maximumGameWork limits then Left (TypePriorWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    if length bounded > length profiles then Left ExcessTypePriorEntries else pure ()
    case firstDuplicate (map fst bounded) of
        Just duplicate -> Left (DuplicateTypePriorProfile duplicate)
        Nothing -> pure ()
    case firstOutside profiles (map fst bounded) of
        Just outside -> Left (TypePriorProfileOutsideProduct outside)
        Nothing -> pure ()
    entries <- traverse canonical profiles
    total <- foldlM addMass 0 entries
    if total == 1 then Right (ExactTypePrior product_ entries) else Left (TypePriorMassNotOne total)
  where
    canonical profile = case lookup profile supplied of
        Nothing -> Left (MissingTypePriorProfile profile)
        Just mass
            | mass < 0 -> Left (NegativeTypePriorMass profile mass)
            | rationalSizeBits mass > maximumGameRationalBits limits -> Left (TypePriorRationalLimitExceeded profile (rationalSizeBits mass) (maximumGameRationalBits limits))
            | otherwise -> Right (profile, mass)
    addMass accumulator (_, mass) =
        let total = accumulator + mass
            actual = rationalSizeBits total
         in if actual > maximumGameRationalBits limits
                then Left (TypePriorTotalRationalLimitExceeded actual (maximumGameRationalBits limits))
                else Right total

-- | Read the complete common-prior table.
typePriorEntries :: ExactTypePrior owner typ -> [(OwnedProfile owner typ, Rational)]
typePriorEntries (ExactTypePrior _ entries) = entries

-- | Query one type-profile mass.
typePriorMass :: (Eq owner, Eq typ) => ExactTypePrior owner typ -> OwnedProfile owner typ -> Maybe Rational
typePriorMass (ExactTypePrior product_ entries) profile
    | profile `elem` NonEmpty.toList (finiteObjectValues (ownedProfiles product_)) = lookup profile entries
    | otherwise = Nothing

-- | One action simplex for every owner/own-type pair.
data ExactBehaviorProfile owner typ action
    = ExactBehaviorProfile
        !(OwnedProduct owner typ)
        !(OwnedProduct owner action)
        ![((owner, typ), ExactSimplex action)]
    deriving (Eq, Show)

type role ExactBehaviorProfile nominal nominal nominal

-- | Behavior-profile construction failures.
data BehaviorProfileError owner typ
    = ExcessBehaviorRows
    | DuplicateBehaviorRow !owner !typ
    | MissingBehaviorRow !owner !typ
    | BehaviorRowOutsideDomain !owner !typ
    | BehaviorActionLayoutMismatch !owner !typ
    | BehaviorWorkLimitExceeded !Natural !Natural
    | BehaviorOwnerLayoutMismatch
    | BehaviorTypeProductInvalid
    | BehaviorActionProductInvalid
    | BehaviorSimplexInvalid !owner !typ
    deriving (Eq, Show)

-- | Validate all owner/own-type action rows.
exactBehaviorProfile :: (Eq owner, Eq typ) => GameLimits -> OwnedProduct owner typ -> OwnedProduct owner action -> [((owner, typ), ExactSimplex action)] -> Either (BehaviorProfileError owner typ) (ExactBehaviorProfile owner typ action)
exactBehaviorProfile limits types actions supplied = do
    if ownedOwners types /= ownedOwners actions then Left BehaviorOwnerLayoutMismatch else pure ()
    case validateOwnedProduct limits types of
        Left _ -> Left BehaviorTypeProductInvalid
        Right () -> pure ()
    case validateOwnedProduct limits actions of
        Left _ -> Left BehaviorActionProductInvalid
        Right () -> pure ()
    let work = foldl (cappedGameAdd (maximumGameWork limits)) 0 [fromIntegral (finiteObjectCardinality typeCarrier) | (_, typeCarrier) <- ownedProductRows types]
    if work > maximumGameWork limits then Left (BehaviorWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    let expected = [(owner, typ) | (owner, typeCarrier) <- ownedProductRows types, typ <- NonEmpty.toList (finiteObjectValues typeCarrier)]
        bounded = take (length expected + 1) supplied
    if length bounded > length expected then Left ExcessBehaviorRows else pure ()
    case firstDuplicate (map fst bounded) of
        Just (owner, typ) -> Left (DuplicateBehaviorRow owner typ)
        Nothing -> pure ()
    case firstOutside expected (map fst bounded) of
        Just (owner, typ) -> Left (BehaviorRowOutsideDomain owner typ)
        Nothing -> pure ()
    rows <- traverse canonical expected
    Right (ExactBehaviorProfile types actions rows)
  where
    canonical key@(owner, typ) = case lookup key supplied of
        Nothing -> Left (MissingBehaviorRow owner typ)
        Just simplex -> case ownedChoices actions owner of
            Nothing -> Left (BehaviorRowOutsideDomain owner typ)
            Just carrier
                | simplexCarrier simplex /= carrier -> Left (BehaviorActionLayoutMismatch owner typ)
                | otherwise -> case validateExactSimplex limits simplex of
                    Left _ -> Left (BehaviorSimplexInvalid owner typ)
                    Right () -> Right (key, simplex)

-- | Query one behavioral row.
behaviorStrategyAt :: (Eq owner, Eq typ) => ExactBehaviorProfile owner typ action -> owner -> typ -> Maybe (ExactSimplex action)
behaviorStrategyAt (ExactBehaviorProfile _ _ rows) owner typ = lookup (owner, typ) rows

-- | A complete rational payoff table over type and action profiles.
data ExactHarsanyiGame owner typ action
    = ExactHarsanyiGame
        !(OwnedProduct owner typ)
        !(OwnedProduct owner action)
        !(ExactTypePrior owner typ)
        ![((OwnedProfile owner typ, OwnedProfile owner action), ExactPlayerValues owner)]
    deriving (Eq, Show)

type role ExactHarsanyiGame nominal nominal nominal

-- | Harsanyi-game construction failures.
data HarsanyiGameError owner typ action
    = HarsanyiOwnerLayoutMismatch
    | HarsanyiPriorLayoutMismatch
    | HarsanyiPriorInvalid
    | ExcessHarsanyiPayoffEntries
    | DuplicateHarsanyiPayoff !(OwnedProfile owner typ) !(OwnedProfile owner action)
    | MissingHarsanyiPayoff !(OwnedProfile owner typ) !(OwnedProfile owner action)
    | HarsanyiPayoffOutsideDomain !(OwnedProfile owner typ) !(OwnedProfile owner action)
    | HarsanyiPayoffPlayerLayoutMismatch !(OwnedProfile owner typ) !(OwnedProfile owner action)
    | HarsanyiCellLimitExceeded !Natural !Natural
    | HarsanyiWorkLimitExceeded !Natural !Natural
    | HarsanyiTypeProductInvalid
    | HarsanyiActionProductInvalid
    | HarsanyiPayoffRationalLimitExceeded !(OwnedProfile owner typ) !(OwnedProfile owner action) !owner !Natural !Natural
    deriving (Eq, Show)

-- | Validate the complete type/action payoff table.
exactHarsanyiGame :: (Eq owner, Eq typ, Eq action) => GameLimits -> OwnedProduct owner typ -> OwnedProduct owner action -> ExactTypePrior owner typ -> [((OwnedProfile owner typ, OwnedProfile owner action), ExactPlayerValues owner)] -> Either (HarsanyiGameError owner typ action) (ExactHarsanyiGame owner typ action)
exactHarsanyiGame limits types actions prior@(ExactTypePrior priorTypes _) supplied = do
    if ownedOwners types /= ownedOwners actions then Left HarsanyiOwnerLayoutMismatch else pure ()
    if priorTypes /= types then Left HarsanyiPriorLayoutMismatch else pure ()
    case validatePriorForGame limits prior of
        Left () -> Left HarsanyiPriorInvalid
        Right () -> pure ()
    case validateOwnedProduct limits types of
        Left _ -> Left HarsanyiTypeProductInvalid
        Right () -> pure ()
    case validateOwnedProduct limits actions of
        Left _ -> Left HarsanyiActionProductInvalid
        Right () -> pure ()
    let typeCount = fromIntegral (finiteObjectCardinality (ownedProfiles types))
        actionCount = fromIntegral (finiteObjectCardinality (ownedProfiles actions))
        expectedCount = cappedGameProduct (maximumGameCells limits) typeCount actionCount
        cells = cappedGameProduct (maximumGameCells limits) expectedCount (fromIntegral (finiteObjectCardinality (ownedOwners types)))
        work = cappedGameAdd (maximumGameWork limits) cells expectedCount
    if cells > maximumGameCells limits then Left (HarsanyiCellLimitExceeded cells (maximumGameCells limits)) else pure ()
    if work > maximumGameWork limits then Left (HarsanyiWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    let typeProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles types))
        actionProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles actions))
        expected = [(typ, action) | typ <- typeProfiles, action <- actionProfiles]
        bounded = take (length expected + 1) supplied
    if length bounded > length expected then Left ExcessHarsanyiPayoffEntries else pure ()
    case firstDuplicate (map fst bounded) of
        Just (typ, action) -> Left (DuplicateHarsanyiPayoff typ action)
        Nothing -> pure ()
    case firstOutside expected (map fst bounded) of
        Just (typ, action) -> Left (HarsanyiPayoffOutsideDomain typ action)
        Nothing -> pure ()
    entries <- traverse canonical expected
    Right (ExactHarsanyiGame types actions prior entries)
  where
    canonical key@(typ, action) = case lookup key supplied of
        Nothing -> Left (MissingHarsanyiPayoff typ action)
        Just values
            | playerValuesCarrier values /= ownedOwners types -> Left (HarsanyiPayoffPlayerLayoutMismatch typ action)
            | otherwise -> do
                traverse_ (validatePayoff typ action) (playerValueEntries values)
                Right (key, values)
    validatePayoff typ action (owner, value)
        | rationalSizeBits value > maximumGameRationalBits limits = Left (HarsanyiPayoffRationalLimitExceeded typ action owner (rationalSizeBits value) (maximumGameRationalBits limits))
        | otherwise = Right ()

validatePriorForGame :: GameLimits -> ExactTypePrior owner typ -> Either () ()
validatePriorForGame limits (ExactTypePrior _ entries) = do
    total <- foldlM add 0 entries
    if total == 1 then Right () else Left ()
  where
    add accumulator (_, mass)
        | mass < 0 || rationalSizeBits mass > maximumGameRationalBits limits = Left ()
        | otherwise =
            let total = accumulator + mass
             in if rationalSizeBits total > maximumGameRationalBits limits then Left () else Right total

-- | Read type ownership/layout.
harsanyiTypes :: ExactHarsanyiGame owner typ action -> OwnedProduct owner typ
harsanyiTypes (ExactHarsanyiGame types _ _ _) = types

-- | Read action ownership/layout.
harsanyiActions :: ExactHarsanyiGame owner typ action -> OwnedProduct owner action
harsanyiActions (ExactHarsanyiGame _ actions _ _) = actions

-- | Whether a type has a positive common-prior marginal.
data BayesTypeStatus = PositivePriorType | NullPriorType
    deriving (Eq, Show)

{- | One exact own-type/pure-action deviation comparison.  Conditional values
are absent for null types.
-}
data BayesDeviation owner typ action = BayesDeviation
    { bayesOwner :: !owner
    , observedOwnType :: !typ
    , bayesDeviationAction :: !action
    , ownTypePriorMass :: !Rational
    , bayesTypeStatus :: !BayesTypeStatus
    , weightedIncumbentPayoff :: !Rational
    , weightedDeviationPayoff :: !Rational
    , conditionalIncumbentPayoff :: !(Maybe Rational)
    , conditionalDeviationPayoff :: !(Maybe Rational)
    , bayesDeviationGain :: !Rational
    }
    deriving (Eq, Show)

{- | Positive-prior-type interim candidate report.  Null rows are retained for
diagnostics but do not assert conditional optimality.
-}
data PositiveTypeBayesNashReport owner typ action = PositiveTypeBayesNashReport
    { positiveTypeBayesNashSatisfied :: !Bool
    , bayesTypeActionProfileCount :: !Natural
    , bayesDeviationCount :: !Natural
    , bayesArithmeticWork :: !Natural
    , bayesDeviations :: ![BayesDeviation owner typ action]
    }
    deriving (Eq, Show)

-- | Bounded Bayes-check failures.
data BayesCheckError
    = BayesGameBehaviorMismatch
    | BayesWorkLimitExceeded !Natural !Natural
    | BayesRationalLimitExceeded !String !Natural !Natural
    | BayesInternalLayoutMismatch
    deriving (Eq, Show)

-- | Check every pure action deviation at every positive-marginal own type.
checkPositiveTypeInterimBayesNash :: (Eq owner, Eq typ, Eq action) => GameLimits -> ExactHarsanyiGame owner typ action -> ExactBehaviorProfile owner typ action -> Either BayesCheckError (PositiveTypeBayesNashReport owner typ action)
checkPositiveTypeInterimBayesNash limits game@(ExactHarsanyiGame types actions prior _) behavior@(ExactBehaviorProfile behaviorTypes behaviorActions _)
    | types /= behaviorTypes || actions /= behaviorActions = Left BayesGameBehaviorMismatch
    | work > maximumGameWork limits = Left (BayesWorkLimitExceeded work (maximumGameWork limits))
    | otherwise = do
        validateBayesInputs limits game behavior
        checks <- traverse check deviationRows
        Right
            PositiveTypeBayesNashReport
                { positiveTypeBayesNashSatisfied = all acceptable checks
                , bayesTypeActionProfileCount = typeActionCount
                , bayesDeviationCount = fromIntegral (length checks)
                , bayesArithmeticWork = work
                , bayesDeviations = checks
                }
  where
    typeProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles types))
    actionProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles actions))
    typeActionCount = cappedGameProduct (maximumGameWork limits) (fromIntegral (length typeProfiles)) (fromIntegral (length actionProfiles))
    deviationCount = foldl (cappedGameAdd (maximumGameWork limits)) 0 [cappedGameProduct (maximumGameWork limits) (fromIntegral (finiteObjectCardinality typeCarrier)) (maybe 0 (fromIntegral . finiteObjectCardinality) (ownedChoices actions owner)) | (owner, typeCarrier) <- ownedProductRows types]
    ownerCount = fromIntegral (finiteObjectCardinality (ownedOwners types))
    work = cappedGameProduct (maximumGameWork limits) deviationCount (cappedGameProduct (maximumGameWork limits) typeActionCount (cappedGameAdd (maximumGameWork limits) ownerCount 6))
    deviationRows =
        [ (owner, typ, alternative)
        | (owner, typeCarrier) <- ownedProductRows types
        , typ <- NonEmpty.toList (finiteObjectValues typeCarrier)
        , alternative <- maybe [] (NonEmpty.toList . finiteObjectValues) (ownedChoices actions owner)
        ]
    acceptable row = bayesTypeStatus row == NullPriorType || bayesDeviationGain row <= 0
    check (owner, ownType, alternative) = do
        let matchingTypes = [(typeProfile, mass) | (typeProfile, mass) <- typePriorEntries prior, profileChoice typeProfile owner == Just ownType]
        marginal <- foldlM addMarginal 0 matchingTypes
        (incumbent, deviating) <- foldlM (typeContribution owner alternative) (0, 0) matchingTypes
        gain <- checked limits "Bayes deviation gain" (deviating - incumbent)
        conditionalIncumbent <- if marginal == 0 then Right Nothing else Just <$> checked limits "Bayes conditional incumbent" (incumbent / marginal)
        conditionalDeviation <- if marginal == 0 then Right Nothing else Just <$> checked limits "Bayes conditional deviation" (deviating / marginal)
        let status = if marginal == 0 then NullPriorType else PositivePriorType
        Right
            BayesDeviation
                { bayesOwner = owner
                , observedOwnType = ownType
                , bayesDeviationAction = alternative
                , ownTypePriorMass = marginal
                , bayesTypeStatus = status
                , weightedIncumbentPayoff = incumbent
                , weightedDeviationPayoff = deviating
                , conditionalIncumbentPayoff = conditionalIncumbent
                , conditionalDeviationPayoff = conditionalDeviation
                , bayesDeviationGain = gain
                }
    addMarginal accumulator (_, mass) = checked limits "Bayes type marginal" (accumulator + mass)
    typeContribution owner alternative (incumbentAccumulator, deviationAccumulator) (typeProfile, priorMass) =
        foldlM (actionContribution owner alternative typeProfile priorMass) (incumbentAccumulator, deviationAccumulator) actionProfiles
    actionContribution owner alternative typeProfile priorMass (incumbentAccumulator, deviationAccumulator) actionProfile = do
        actionMass <- behaviorActionMass limits behavior typeProfile actionProfile
        jointMass <- checked limits "Bayes joint mass" (priorMass * actionMass)
        replacement <- either (const (Left BayesInternalLayoutMismatch)) Right (replaceChoice actions owner alternative actionProfile)
        incumbentPayoff <- gamePayoff game owner typeProfile actionProfile
        deviatingPayoff <- gamePayoff game owner typeProfile replacement
        incumbentTerm <- checked limits "Bayes incumbent term" (jointMass * incumbentPayoff)
        deviationTerm <- checked limits "Bayes deviation term" (jointMass * deviatingPayoff)
        nextIncumbent <- checked limits "Bayes incumbent accumulation" (incumbentAccumulator + incumbentTerm)
        nextDeviation <- checked limits "Bayes deviation accumulation" (deviationAccumulator + deviationTerm)
        Right (nextIncumbent, nextDeviation)

{- | Ex-ante report.  In this one-shot perfect-recall fragment a contingent-plan
deviation separates by own type, so the exact positive-type rows are also a
complete ex-ante candidate check; null types contribute zero.
-}
data ExAnteBayesNashReport owner typ action = ExAnteBayesNashReport
    { exAnteBayesNashSatisfied :: !Bool
    , exAnteInterimEvidence :: !(PositiveTypeBayesNashReport owner typ action)
    }
    deriving (Eq, Show)

-- | Check ex-ante deviations through the independently computed own-type rows.
checkExAnteBayesNash :: (Eq owner, Eq typ, Eq action) => GameLimits -> ExactHarsanyiGame owner typ action -> ExactBehaviorProfile owner typ action -> Either BayesCheckError (ExAnteBayesNashReport owner typ action)
checkExAnteBayesNash limits game behavior = do
    interim <- checkPositiveTypeInterimBayesNash limits game behavior
    Right (ExAnteBayesNashReport (positiveTypeBayesNashSatisfied interim) interim)

-- | One pure contingent plan in own-type carrier order.
newtype ContingentPlan typ action = ContingentPlan [(typ, action)]
    deriving (Eq, Show)

type role ContingentPlan nominal nominal

-- | Read a plan's complete own-type table.
contingentPlanEntries :: ContingentPlan typ action -> [(typ, action)]
contingentPlanEntries (ContingentPlan entries) = entries

-- | Bounded strategic-normal (contingent-plan) conversion failures.
data StrategicNormalError owner typ action
    = StrategicNormalOwnerLayoutMismatch
    | StrategicNormalProductError !(OwnedProductError owner)
    | StrategicNormalProfileError !(OwnedProfileError owner action)
    | StrategicNormalGameError !(NormalGameError owner (ContingentPlan typ action))
    | StrategicNormalPlanCountLimitExceeded !owner !Natural !Natural
    | StrategicNormalProfileCountLimitExceeded !Natural !Natural
    | StrategicNormalCellLimitExceeded !Natural !Natural
    | StrategicNormalWorkLimitExceeded !Natural !Natural
    | StrategicNormalRationalLimitExceeded !Natural !Natural
    | StrategicNormalInternalLayoutMismatch
    | StrategicNormalInputInvalid
    deriving (Eq, Show)

{- | Convert to bounded strategic normal form.  Each owner's pure actions are
complete own-type contingent plans.  This is not agent normal form: owner types
are not split into independent player-agents.
-}
toStrategicNormalForm :: (Eq owner, Eq typ, Eq action) => GameLimits -> ExactHarsanyiGame owner typ action -> Either (StrategicNormalError owner typ action) (ExactNormalGame owner (ContingentPlan typ action))
toStrategicNormalForm limits game@(ExactHarsanyiGame types actions prior _) = do
    if ownedOwners types /= ownedOwners actions then Left StrategicNormalOwnerLayoutMismatch else pure ()
    case (validateOwnedProduct limits types, validateOwnedProduct limits actions, validatePriorForGame limits prior) of
        (Right (), Right (), Right ()) -> pure ()
        _ -> Left StrategicNormalInputInvalid
    let ownerRows = ownedProductRows types
    planCounts <- traverse preflightOwner ownerRows
    let profileCount = foldl (cappedGameProduct (maximumGameProfiles limits)) 1 (map snd planCounts)
        ownerCount = fromIntegral (finiteObjectCardinality (ownedOwners types))
        typeProfileCount = fromIntegral (finiteObjectCardinality (ownedProfiles types))
        cells = cappedGameProduct (maximumGameCells limits) profileCount ownerCount
        planEnumerationWork = foldl (cappedGameAdd (maximumGameWork limits)) 0 [cappedGameProduct (maximumGameWork limits) count (fromIntegral (finiteObjectCardinality typeCarrier)) | ((_, typeCarrier), (_, count)) <- zip ownerRows planCounts]
        planTotal = foldl (cappedGameAdd (maximumGameWork limits)) 0 (map snd planCounts)
        productWork = foldl (cappedGameAdd (maximumGameWork limits)) 0 [ownerCount, planTotal, cells]
        perPlanWork = cappedGameProduct (maximumGameWork limits) typeProfileCount (cappedGameAdd (maximumGameWork limits) (cappedGameProduct (maximumGameWork limits) ownerCount 3) 1)
        evaluationWork = cappedGameProduct (maximumGameWork limits) profileCount perPlanWork
        normalWork = cappedGameAdd (maximumGameWork limits) cells profileCount
        work = foldl (cappedGameAdd (maximumGameWork limits)) 0 [planEnumerationWork, productWork, evaluationWork, normalWork]
    if profileCount > maximumGameProfiles limits then Left (StrategicNormalProfileCountLimitExceeded profileCount (maximumGameProfiles limits)) else pure ()
    if cells > maximumGameCells limits then Left (StrategicNormalCellLimitExceeded cells (maximumGameCells limits)) else pure ()
    if work > maximumGameWork limits then Left (StrategicNormalWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    planRows <- traverse plansFor ownerRows
    planProduct <- mapLeft StrategicNormalProductError (ownedProduct limits (ownedOwners types) planRows)
    let planProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles planProduct))
    payoffs <- traverse payoffFor planProfiles
    mapLeft StrategicNormalGameError (exactNormalGame limits planProduct payoffs)
  where
    preflightOwner (owner, typeCarrier) = case ownedChoices actions owner of
        Nothing -> Left StrategicNormalOwnerLayoutMismatch
        Just actionCarrier ->
            let typeCount = fromIntegral (finiteObjectCardinality typeCarrier)
                actionCount = fromIntegral (finiteObjectCardinality actionCarrier)
                planCount = cappedGamePower (maximumGameLocalChoices limits) actionCount typeCount
             in if planCount > maximumGameLocalChoices limits
                    then Left (StrategicNormalPlanCountLimitExceeded owner planCount (maximumGameLocalChoices limits))
                    else Right (owner, planCount)
    plansFor (owner, typeCarrier) = case ownedChoices actions owner of
        Nothing -> Left StrategicNormalOwnerLayoutMismatch
        Just actionCarrier -> do
            let typeValues = NonEmpty.toList (finiteObjectValues typeCarrier)
                actionValues = NonEmpty.toList (finiteObjectValues actionCarrier)
                plans = [ContingentPlan (zip typeValues selected) | selected <- replicateM (length typeValues) actionValues]
            carrier <- case finiteObject plans of
                Left _ -> error "toStrategicNormalForm: nonempty duplicate-free plans invariant"
                Right carrier_ -> Right carrier_
            Right (owner, carrier)
    payoffFor planProfile = do
        values <- foldlM (typeValue planProfile) (zeroPlayerValues (ownedOwners types)) (typePriorEntries prior)
        Right (planProfile, values)
    typeValue planProfile accumulator (typeProfile, mass) = do
        actionEntries <- traverse (selectedAction planProfile typeProfile) (NonEmpty.toList (finiteObjectValues (ownedOwners types)))
        actionProfile <- mapLeft StrategicNormalProfileError (ownedProfile actions actionEntries)
        values <- maybe (Left StrategicNormalInternalLayoutMismatch) Right (lookup (typeProfile, actionProfile) (harsanyiPayoffs game))
        weighted <- mapEvaluation (scalePlayerValues limits mass values)
        mapEvaluation (addPlayerValues limits accumulator weighted)
    selectedAction planProfile typeProfile owner = do
        plan <- maybe (Left StrategicNormalInternalLayoutMismatch) Right (profileChoice planProfile owner)
        typ <- maybe (Left StrategicNormalInternalLayoutMismatch) Right (profileChoice typeProfile owner)
        action <- maybe (Left StrategicNormalInternalLayoutMismatch) Right (lookup typ (contingentPlanEntries plan))
        Right (owner, action)

harsanyiPayoffs :: ExactHarsanyiGame owner typ action -> [((OwnedProfile owner typ, OwnedProfile owner action), ExactPlayerValues owner)]
harsanyiPayoffs (ExactHarsanyiGame _ _ _ payoffs) = payoffs

gamePayoff :: (Eq owner, Eq typ, Eq action) => ExactHarsanyiGame owner typ action -> owner -> OwnedProfile owner typ -> OwnedProfile owner action -> Either BayesCheckError Rational
gamePayoff game owner types actions =
    maybe (Left BayesInternalLayoutMismatch) Right (lookup (types, actions) (harsanyiPayoffs game) >>= (`playerValue` owner))

behaviorActionMass :: (Eq owner, Eq typ, Eq action) => GameLimits -> ExactBehaviorProfile owner typ action -> OwnedProfile owner typ -> OwnedProfile owner action -> Either BayesCheckError Rational
behaviorActionMass limits behavior typeProfile actionProfile = foldlM multiply 1 (ownedProfileEntries actionProfile)
  where
    multiply accumulator (owner, action) = do
        typ <- maybe (Left BayesInternalLayoutMismatch) Right (profileChoice typeProfile owner)
        simplex <- maybe (Left BayesInternalLayoutMismatch) Right (behaviorStrategyAt behavior owner typ)
        mass <- maybe (Left BayesInternalLayoutMismatch) Right (simplexMass simplex action)
        _ <- checked limits "behavior mass" mass
        checked limits "behavior profile mass" (accumulator * mass)

validateBayesInputs :: (Eq owner, Eq typ, Eq action) => GameLimits -> ExactHarsanyiGame owner typ action -> ExactBehaviorProfile owner typ action -> Either BayesCheckError ()
validateBayesInputs limits (ExactHarsanyiGame types actions (ExactTypePrior priorTypes priorEntries) payoffs) (ExactBehaviorProfile behaviorTypes behaviorActions rows)
    | types /= behaviorTypes || actions /= behaviorActions || priorTypes /= types = Left BayesGameBehaviorMismatch
    | otherwise = do
        case (validateOwnedProduct limits types, validateOwnedProduct limits actions) of
            (Right (), Right ()) -> pure ()
            _ -> Left BayesGameBehaviorMismatch
        priorTotal <- foldlM addPrior 0 priorEntries
        if priorTotal == 1 then pure () else Left BayesInternalLayoutMismatch
        traverse_ validatePayoff payoffs
        traverse_ validateBehavior rows
  where
    addPrior accumulator (_, mass)
        | mass < 0 = Left BayesInternalLayoutMismatch
        | otherwise = do
            _ <- checked limits "prior mass" mass
            checked limits "prior total" (accumulator + mass)
    validatePayoff (_, values) = traverse_ (\(_, value) -> checked limits "Harsanyi payoff" value >> Right ()) (playerValueEntries values)
    validateBehavior (_, simplex) = case validateExactSimplex limits simplex of
        Left _ -> Left BayesInternalLayoutMismatch
        Right () -> Right ()

checked :: GameLimits -> String -> Rational -> Either BayesCheckError Rational
checked limits label value = case checkRationalSize limits value of
    Left (actual, maximum_) -> Left (BayesRationalLimitExceeded label actual maximum_)
    Right valid -> Right valid

mapEvaluation :: Either ExactEvaluationError value -> Either (StrategicNormalError owner typ action) value
mapEvaluation result = case result of
    Left (EvaluationRationalLimitExceeded _ actual maximum_) -> Left (StrategicNormalRationalLimitExceeded actual maximum_)
    Left _ -> Left StrategicNormalInternalLayoutMismatch
    Right value -> Right value

mapLeft :: (left -> right) -> Either left value -> Either right value
mapLeft function result = case result of
    Left problem -> Left (function problem)
    Right value -> Right value

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
