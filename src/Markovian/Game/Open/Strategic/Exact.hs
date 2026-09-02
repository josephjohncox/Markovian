{-# LANGUAGE RoleAnnotations #-}

{- | Checked extraction of one closed finite open-game context as an exact
normal-form game.

This is not a probabilistic lifting of open games.  The caller supplies an
explicit owner-local product and a bijection to the open game's whole-profile
carrier.  The adapter exhaustively checks the existing pure best-response
callback against exact unilateral payoff maximization before returning the
normal form.
-}
module Markovian.Game.Open.Strategic.Exact (
    OwnedDeviationLayout,
    DeviationLayoutError (..),
    ownedDeviationLayout,
    deviationLocalProduct,
    deviationGlobalProfile,
    OpenStrategicError (..),
    normalFormFromOpenContext,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set (finiteSetValues)
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Open.Finite
import Markovian.Game.Optic.Finite (applyFiniteFunction)
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

-- | Explicit owner-local projection of each represented global strategy.
data OwnedDeviationLayout owner global local
    = OwnedDeviationLayout
        !(OwnedProduct owner local)
        ![(OwnedProfile owner local, global)]
    deriving (Eq, Show)

type role OwnedDeviationLayout nominal nominal nominal

-- | Layout construction failures.
data DeviationLayoutError owner global local
    = DeviationOwnerLayoutMismatch
    | ExcessDeviationLayoutEntries
    | DuplicateDeviationLocalProfile !(OwnedProfile owner local)
    | MissingDeviationLocalProfile !(OwnedProfile owner local)
    | DeviationLocalProfileOutsideProduct !(OwnedProfile owner local)
    | DuplicateDeviationGlobalProfile !global
    | MissingDeviationGlobalProfile !global
    | DeviationGlobalProfileOutsideSchema !global
    | DeviationLayoutWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Validate a complete bijection between owner-local and global profiles.
ownedDeviationLayout :: (Eq owner, Eq global, Eq local) => GameLimits -> OwnedProduct owner local -> StrategySchema owner global -> [(OwnedProfile owner local, global)] -> Either (DeviationLayoutError owner global local) (OwnedDeviationLayout owner global local)
ownedDeviationLayout limits product_ schema supplied = do
    let owners = NonEmpty.toList (finiteObjectValues (ownedOwners product_))
        schemaOwners = strategySchemaOwners schema
        localProfiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
        globals = finiteSetValues (strategySchemaProfiles schema)
        bounded = take (length localProfiles + 1) supplied
        work = fromIntegral (length localProfiles + length globals)
    if owners /= schemaOwners then Left DeviationOwnerLayoutMismatch else pure ()
    if work > maximumGameWork limits then Left (DeviationLayoutWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    if length bounded > length localProfiles then Left ExcessDeviationLayoutEntries else pure ()
    case firstDuplicate (map fst bounded) of
        Just duplicate -> Left (DuplicateDeviationLocalProfile duplicate)
        Nothing -> pure ()
    case firstDuplicate (map snd bounded) of
        Just duplicate -> Left (DuplicateDeviationGlobalProfile duplicate)
        Nothing -> pure ()
    case firstOutside localProfiles (map fst bounded) of
        Just outside -> Left (DeviationLocalProfileOutsideProduct outside)
        Nothing -> pure ()
    case firstOutside globals (map snd bounded) of
        Just outside -> Left (DeviationGlobalProfileOutsideSchema outside)
        Nothing -> pure ()
    case firstMissing localProfiles (map fst bounded) of
        Just missing -> Left (MissingDeviationLocalProfile missing)
        Nothing -> pure ()
    case firstMissing globals (map snd bounded) of
        Just missing -> Left (MissingDeviationGlobalProfile missing)
        Nothing -> pure ()
    let canonical = [(local, required "deviation global profile" (lookup local bounded)) | local <- localProfiles]
    Right (OwnedDeviationLayout product_ canonical)

-- | Read the owner-local product.
deviationLocalProduct :: OwnedDeviationLayout owner global local -> OwnedProduct owner local
deviationLocalProduct (OwnedDeviationLayout product_ _) = product_

-- | Map one represented local profile to the open game's global profile.
deviationGlobalProfile :: (Eq owner, Eq local) => OwnedDeviationLayout owner global local -> OwnedProfile owner local -> Maybe global
deviationGlobalProfile (OwnedDeviationLayout _ entries) profile = lookup profile entries

-- | Checked extraction failures.
data OpenStrategicError owner global local y
    = OpenStrategicContextError !(OpenGameQueryError global)
    | OpenStrategicPlayOutsideTarget !global
    | OpenStrategicContinuationOutsideTable !y
    | OpenStrategicLayoutMismatch
    | OpenStrategicBestResponseMismatch !owner !(OwnedProfile owner local) !local !Bool !Bool
    | OpenStrategicValueError !(PlayerValuesError owner)
    | OpenStrategicNormalGameError !(NormalGameError owner local)
    | OpenStrategicWorkLimitExceeded !Natural !Natural
    | OpenStrategicRationalLimitExceeded !owner !Natural !Natural
    deriving (Eq, Show)

{- | Extract and verify one closed context.  The utility projection is evaluated
only on the context's represented continuation values.
-}
normalFormFromOpenContext :: (Eq owner, Eq global, Eq local, Eq x, Eq y, Eq r) => GameLimits -> FiniteOpenGame owner global x s y r -> OpenGameContext x y r -> OwnedDeviationLayout owner global local -> (owner -> r -> Rational) -> Either (OpenStrategicError owner global local y) (ExactNormalGame owner local)
normalFormFromOpenContext limits game context layout utility = do
    if not (layoutMatches layout) then Left OpenStrategicLayoutMismatch else pure ()
    let product_ = deviationLocalProduct layout
        profiles = NonEmpty.toList (finiteObjectValues (ownedProfiles product_))
        deviationCount = sum [finiteObjectCardinality choices | (_, choices) <- ownedProductRows product_]
        profileCount = length profiles
        playerCount = finiteObjectCardinality (ownedOwners product_)
        payoffWork = profileCount * (playerCount + 2)
        callbackWork = profileCount * deviationCount * 2
        normalBuildWork = profileCount * (playerCount + 1)
        work = fromIntegral (payoffWork + callbackWork + normalBuildWork)
    if work > maximumGameWork limits then Left (OpenStrategicWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    payoffs <- traverse payoffEntry profiles
    verifyCallbacks product_ payoffs profiles
    mapLeft OpenStrategicNormalGameError (exactNormalGame limits product_ payoffs)
  where
    layoutMatches checkedLayout =
        strategySchemaOwners (openGameStrategySchema game) == NonEmpty.toList (finiteObjectValues (ownedOwners (deviationLocalProduct checkedLayout)))
            && sameMembers (finiteSetValues (strategySchemaProfiles (openGameStrategySchema game))) (map snd (layoutEntries checkedLayout))
    payoffEntry local = do
        global <- maybe (Left OpenStrategicLayoutMismatch) Right (deviationGlobalProfile layout local)
        output <- maybe (Left (OpenStrategicPlayOutsideTarget global)) Right (playOpenGame game global (contextInput context))
        result <- maybe (Left (OpenStrategicContinuationOutsideTable output)) Right (applyFiniteFunction (contextContinuation context) output)
        values <- traverse (coordinate result) (NonEmpty.toList (finiteObjectValues (ownedOwners (deviationLocalProduct layout))))
        checkedValues <- mapLeft OpenStrategicValueError (exactPlayerValues limits (ownedOwners (deviationLocalProduct layout)) values)
        Right (local, checkedValues)
    coordinate result owner =
        let value = utility owner result
         in case checkRationalSize limits value of
                Left (actual, maximum_) -> Left (OpenStrategicRationalLimitExceeded owner actual maximum_)
                Right valid -> Right (owner, valid)
    verifyCallbacks product_ payoffs = traverse_ (verifyIncumbent product_ payoffs)
    verifyIncumbent product_ payoffs incumbent = traverse_ (verifyOwner product_ payoffs incumbent) (ownedProductRows product_)
    verifyOwner product_ payoffs incumbent (owner, choices) = do
        candidates <- traverse (candidate product_ incumbent owner) (NonEmpty.toList (finiteObjectValues choices))
        let candidateValues = [(choice, required "candidate payoff" (lookup profile payoffs >>= (`playerValue` owner))) | (choice, profile) <- candidates]
            maximumValue = maximum (map snd candidateValues)
        traverse_ (compareCallback payoffs owner incumbent maximumValue) candidates
    candidate product_ incumbent owner choice = case replaceChoice product_ owner choice incumbent of
        Left _ -> Left OpenStrategicLayoutMismatch
        Right profile -> Right (choice, profile)
    compareCallback payoffs owner incumbent maximumValue (choice, candidateProfile) = do
        incumbentGlobal <- maybe (Left OpenStrategicLayoutMismatch) Right (deviationGlobalProfile layout incumbent)
        candidateGlobal <- maybe (Left OpenStrategicLayoutMismatch) Right (deviationGlobalProfile layout candidateProfile)
        observed <- mapLeft OpenStrategicContextError (bestResponse game context incumbentGlobal candidateGlobal)
        let numeric = required "candidate payoff" (lookup candidateProfile payoffs >>= (`playerValue` owner)) == maximumValue
        if observed == numeric
            then Right ()
            else Left (OpenStrategicBestResponseMismatch owner incumbent choice observed numeric)

layoutEntries :: OwnedDeviationLayout owner global local -> [(OwnedProfile owner local, global)]
layoutEntries (OwnedDeviationLayout _ entries) = entries

mapLeft :: (left -> right) -> Either left value -> Either right value
mapLeft function result = case result of
    Left problem -> Left (function problem)
    Right value -> Right value

traverse_ :: (a -> Either error ()) -> [a] -> Either error ()
traverse_ _ [] = Right ()
traverse_ function (value : remaining) = case function value of
    Left problem -> Left problem
    Right () -> traverse_ function remaining

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

sameMembers :: (Eq value) => [value] -> [value] -> Bool
sameMembers left right = length left == length right && all (`elem` right) left && all (`elem` left) right

firstMissing :: (Eq value) => [value] -> [value] -> Maybe value
firstMissing [] _ = Nothing
firstMissing (value : remaining) supplied
    | value `notElem` supplied = Just value
    | otherwise = firstMissing remaining supplied

required :: String -> Maybe value -> value
required _ (Just value) = value
required label Nothing = error ("internal checked invariant failed: " ++ label)
