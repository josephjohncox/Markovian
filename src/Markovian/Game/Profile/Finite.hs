{-# LANGUAGE RoleAnnotations #-}

{- | Checked owner-indexed finite products and exact rational simplexes.

The representations are opaque.  A simplex is a complete extensional table:
entries are nonnegative and sum literally to one; construction never
normalizes input.  Products are enumerated only after their represented size
has passed all configured limits.
-}
module Markovian.Game.Profile.Finite (
    GameLimits,
    gameLimits,
    maximumGameOwners,
    maximumGameLocalChoices,
    maximumGameProfiles,
    maximumGameCells,
    maximumGameWork,
    maximumGameRationalBits,
    maximumGameHorizon,
    OwnedProduct,
    OwnedProductError (..),
    ownedProduct,
    ownedOwners,
    ownedChoices,
    ownedProfiles,
    ownedProductRows,
    validateOwnedProduct,
    OwnedProfile,
    OwnedProfileError (..),
    ownedProfile,
    ownedProfileEntries,
    profileChoice,
    replaceChoice,
    ExactSimplex,
    ExactSimplexError (..),
    exactSimplex,
    simplexCarrier,
    simplexEntries,
    simplexMass,
    simplexSupport,
    validateExactSimplex,
    rationalSizeBits,
    checkRationalSize,
    cappedGameAdd,
    cappedGameProduct,
    cappedGamePower,
) where

import Data.List (foldl')
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Markovian.Category.Finite.Object
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

{- | Operation-wide limits.  Work counts represented arithmetic/table steps;
it is not a heap or constant-time arithmetic claim.
-}
data GameLimits = GameLimits
    { maximumGameOwners :: !Natural
    -- ^ Maximum represented owners.
    , maximumGameLocalChoices :: !Natural
    -- ^ Maximum choices in any one owner's carrier.
    , maximumGameProfiles :: !Natural
    -- ^ Maximum pure profiles in an owned product.
    , maximumGameCells :: !Natural
    -- ^ Maximum represented owner/profile or payoff cells.
    , maximumGameWork :: !Natural
    -- ^ Maximum operation-wide represented work.
    , maximumGameRationalBits :: !Natural
    -- ^ Maximum combined numerator/denominator bit size after arithmetic.
    , maximumGameHorizon :: !Natural
    -- ^ Maximum dynamic transition horizon.
    }
    deriving (Eq, Show)

-- | Construct explicit limits.
gameLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> GameLimits
gameLimits = GameLimits

-- | A checked heterogeneous product, using one common choice label type.
data OwnedProduct owner choice
    = UnsafeOwnedProduct
        !(FiniteObject owner)
        ![(owner, FiniteObject choice)]
        !(FiniteObject (OwnedProfile owner choice))
    deriving (Eq, Show)

type role OwnedProduct nominal nominal

-- | One point of an 'OwnedProduct'.
newtype OwnedProfile owner choice = OwnedProfile [(owner, choice)]
    deriving (Eq, Show)

type role OwnedProfile nominal nominal

-- | Product construction failures.
data OwnedProductError owner
    = TooManyOwners !Natural !Natural
    | ExcessOwnerRows
    | DuplicateOwnerRow !owner
    | MissingOwnerRow !owner
    | OwnerRowOutsideProduct !owner
    | TooManyLocalChoices !owner !Natural !Natural
    | ProductCardinalityLimitExceeded !Natural !Natural
    | ProductCellLimitExceeded !Natural !Natural
    | ProductWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Validate owner rows and enumerate the canonical owner-major product.
ownedProduct :: (Eq owner, Eq choice) => GameLimits -> FiniteObject owner -> [(owner, FiniteObject choice)] -> Either (OwnedProductError owner) (OwnedProduct owner choice)
ownedProduct limits owners suppliedRows = do
    let ownerValues = NonEmpty.toList (finiteObjectValues owners)
        ownerCount = naturalLength ownerValues
        boundedRows = take (length ownerValues + 1) suppliedRows
    requireAtMost (maximumGameOwners limits) ownerCount (TooManyOwners ownerCount (maximumGameOwners limits))
    if length boundedRows > length ownerValues then Left ExcessOwnerRows else pure ()
    case firstDuplicateBy fst boundedRows of
        Just duplicate -> Left (DuplicateOwnerRow duplicate)
        Nothing -> pure ()
    case firstOutside ownerValues (map fst boundedRows) of
        Just outside -> Left (OwnerRowOutsideProduct outside)
        Nothing -> pure ()
    rows <- traverse (canonicalRow boundedRows) ownerValues
    traverse_ (checkLocal limits) rows
    let localCounts = [naturalLength (NonEmpty.toList (finiteObjectValues choices)) | (_, choices) <- rows]
        cardinality = foldl' (cappedGameProduct (maximumGameProfiles limits)) 1 localCounts
    if cardinality > maximumGameProfiles limits
        then Left (ProductCardinalityLimitExceeded cardinality (maximumGameProfiles limits))
        else pure ()
    let cells = cappedGameProduct (maximumGameCells limits) cardinality ownerCount
        localWork = foldl' (cappedGameAdd (maximumGameWork limits)) 0 localCounts
        work = foldl' (cappedGameAdd (maximumGameWork limits)) 0 [ownerCount, localWork, cells]
    if cells > maximumGameCells limits
        then Left (ProductCellLimitExceeded cells (maximumGameCells limits))
        else pure ()
    if work > maximumGameWork limits
        then Left (ProductWorkLimitExceeded work (maximumGameWork limits))
        else pure ()
    let represented = map OwnedProfile (sequence [[(owner, choice) | choice <- NonEmpty.toList (finiteObjectValues choices)] | (owner, choices) <- rows])
    profiles <- case finiteObject represented of
        Left _ -> error "ownedProduct: nonempty duplicate-free product invariant"
        Right checked -> Right checked
    Right (UnsafeOwnedProduct owners rows profiles)
  where
    canonicalRow rows owner = case lookup owner rows of
        Nothing -> Left (MissingOwnerRow owner)
        Just choices -> Right (owner, choices)

{- | Revalidate a previously constructed product under the active operation limits.
No profiles are reconstructed.
-}
validateOwnedProduct :: GameLimits -> OwnedProduct owner choice -> Either (OwnedProductError owner) ()
validateOwnedProduct limits product_ = do
    let ownerCount = fromIntegral (finiteObjectCardinality (ownedOwners product_))
        localCounts = [fromIntegral (finiteObjectCardinality choices) | (_, choices) <- ownedProductRows product_]
        cardinality = foldl' (cappedGameProduct (maximumGameProfiles limits)) 1 localCounts
        cells = cappedGameProduct (maximumGameCells limits) cardinality ownerCount
        localWork = foldl' (cappedGameAdd (maximumGameWork limits)) 0 localCounts
        work = foldl' (cappedGameAdd (maximumGameWork limits)) 0 [ownerCount, localWork, cells]
    requireAtMost (maximumGameOwners limits) ownerCount (TooManyOwners ownerCount (maximumGameOwners limits))
    traverse_ (checkLocal limits) (ownedProductRows product_)
    requireAtMost (maximumGameProfiles limits) cardinality (ProductCardinalityLimitExceeded cardinality (maximumGameProfiles limits))
    requireAtMost (maximumGameCells limits) cells (ProductCellLimitExceeded cells (maximumGameCells limits))
    requireAtMost (maximumGameWork limits) work (ProductWorkLimitExceeded work (maximumGameWork limits))

-- | Read the owner layout.
ownedOwners :: OwnedProduct owner choice -> FiniteObject owner
ownedOwners (UnsafeOwnedProduct owners _ _) = owners

-- | Read one owner's local carrier.
ownedChoices :: (Eq owner) => OwnedProduct owner choice -> owner -> Maybe (FiniteObject choice)
ownedChoices (UnsafeOwnedProduct _ rows _) owner = lookup owner rows

-- | Read all canonical profiles.
ownedProfiles :: OwnedProduct owner choice -> FiniteObject (OwnedProfile owner choice)
ownedProfiles (UnsafeOwnedProduct _ _ profiles) = profiles

-- | Read owner/carrier rows in owner layout order.
ownedProductRows :: OwnedProduct owner choice -> [(owner, FiniteObject choice)]
ownedProductRows (UnsafeOwnedProduct _ rows _) = rows

-- | Profile construction failures.
data OwnedProfileError owner choice
    = ExcessProfileEntries
    | DuplicateProfileOwner !owner
    | MissingProfileOwner !owner
    | ProfileOwnerOutsideProduct !owner
    | ProfileChoiceOutsideCarrier !owner !choice
    | ReplacementOwnerOutsideProduct !owner
    | ReplacementChoiceOutsideCarrier !owner !choice
    deriving (Eq, Show)

-- | Validate and canonicalize one complete profile.
ownedProfile :: (Eq owner, Eq choice) => OwnedProduct owner choice -> [(owner, choice)] -> Either (OwnedProfileError owner choice) (OwnedProfile owner choice)
ownedProfile product_ supplied = do
    let owners = NonEmpty.toList (finiteObjectValues (ownedOwners product_))
        bounded = take (length owners + 1) supplied
    if length bounded > length owners then Left ExcessProfileEntries else pure ()
    case firstDuplicateBy fst bounded of
        Just duplicate -> Left (DuplicateProfileOwner duplicate)
        Nothing -> pure ()
    case firstOutside owners (map fst bounded) of
        Just outside -> Left (ProfileOwnerOutsideProduct outside)
        Nothing -> pure ()
    entries <- traverse (canonicalEntry bounded) owners
    Right (OwnedProfile entries)
  where
    canonicalEntry entries owner = case lookup owner entries of
        Nothing -> Left (MissingProfileOwner owner)
        Just choice -> case ownedChoices product_ owner of
            Nothing -> Left (ProfileOwnerOutsideProduct owner)
            Just carrier
                | choice `elem` NonEmpty.toList (finiteObjectValues carrier) -> Right (owner, choice)
                | otherwise -> Left (ProfileChoiceOutsideCarrier owner choice)

-- | Read canonical owner/choice entries.
ownedProfileEntries :: OwnedProfile owner choice -> [(owner, choice)]
ownedProfileEntries (OwnedProfile entries) = entries

-- | Query one choice.
profileChoice :: (Eq owner) => OwnedProfile owner choice -> owner -> Maybe choice
profileChoice (OwnedProfile entries) owner = lookup owner entries

-- | Replace exactly one owner's choice, retaining canonical layout.
replaceChoice :: (Eq owner, Eq choice) => OwnedProduct owner choice -> owner -> choice -> OwnedProfile owner choice -> Either (OwnedProfileError owner choice) (OwnedProfile owner choice)
replaceChoice product_ owner replacement (OwnedProfile entries) = case ownedChoices product_ owner of
    Nothing -> Left (ReplacementOwnerOutsideProduct owner)
    Just carrier
        | replacement `notElem` NonEmpty.toList (finiteObjectValues carrier) -> Left (ReplacementChoiceOutsideCarrier owner replacement)
        | otherwise -> Right (OwnedProfile [(current, if current == owner then replacement else choice) | (current, choice) <- entries])

-- | An exact complete probability table over a finite carrier.
data ExactSimplex choice = ExactSimplex !(FiniteObject choice) ![(choice, Rational)]
    deriving (Eq, Show)

type role ExactSimplex nominal

-- | Exact simplex construction failures.
data ExactSimplexError choice
    = ExcessSimplexEntries
    | DuplicateSimplexChoice !choice
    | MissingSimplexChoice !choice
    | SimplexChoiceOutsideCarrier !choice
    | NegativeSimplexMass !choice !Rational
    | SimplexMassNotOne !Rational
    | SimplexRationalLimitExceeded !choice !Natural !Natural
    | SimplexTotalRationalLimitExceeded !Natural !Natural
    | SimplexWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Validate a literal exact simplex.  Input is never normalized.
exactSimplex :: (Eq choice) => GameLimits -> FiniteObject choice -> [(choice, Rational)] -> Either (ExactSimplexError choice) (ExactSimplex choice)
exactSimplex limits carrier supplied = do
    let choices = NonEmpty.toList (finiteObjectValues carrier)
        bounded = take (length choices + 1) supplied
        work = fromIntegral (length choices)
    if work > maximumGameWork limits then Left (SimplexWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    if length bounded > length choices then Left ExcessSimplexEntries else pure ()
    case firstDuplicateBy fst bounded of
        Just duplicate -> Left (DuplicateSimplexChoice duplicate)
        Nothing -> pure ()
    case firstOutside choices (map fst bounded) of
        Just outside -> Left (SimplexChoiceOutsideCarrier outside)
        Nothing -> pure ()
    entries <- traverse (canonicalMass bounded) choices
    total <- checkedTotal entries
    if total == 1 then Right (ExactSimplex carrier entries) else Left (SimplexMassNotOne total)
  where
    canonicalMass entries choice = case lookup choice entries of
        Nothing -> Left (MissingSimplexChoice choice)
        Just mass
            | mass < 0 -> Left (NegativeSimplexMass choice mass)
            | rationalSizeBits mass > maximumGameRationalBits limits -> Left (SimplexRationalLimitExceeded choice (rationalSizeBits mass) (maximumGameRationalBits limits))
            | otherwise -> Right (choice, mass)
    checkedTotal = foldlM add 0
    add accumulator (_, mass) =
        let total = accumulator + mass
            actual = rationalSizeBits total
         in if actual > maximumGameRationalBits limits
                then Left (SimplexTotalRationalLimitExceeded actual (maximumGameRationalBits limits))
                else Right total

-- | Read the represented carrier.
simplexCarrier :: ExactSimplex choice -> FiniteObject choice
simplexCarrier (ExactSimplex carrier _) = carrier

-- | Read the complete canonical mass table.
simplexEntries :: ExactSimplex choice -> [(choice, Rational)]
simplexEntries (ExactSimplex _ entries) = entries

-- | Query a represented mass.
simplexMass :: (Eq choice) => ExactSimplex choice -> choice -> Maybe Rational
simplexMass (ExactSimplex _ entries) choice = lookup choice entries

-- | Read positive-mass support in carrier order.
simplexSupport :: ExactSimplex choice -> [choice]
simplexSupport (ExactSimplex _ entries) = [choice | (choice, mass) <- entries, mass > 0]

-- | Revalidate a simplex under the active operation limits.
validateExactSimplex :: GameLimits -> ExactSimplex choice -> Either (ExactSimplexError choice) ()
validateExactSimplex limits (ExactSimplex _ entries) = do
    let work = fromIntegral (length entries)
    if work > maximumGameWork limits then Left (SimplexWorkLimitExceeded work (maximumGameWork limits)) else pure ()
    total <- foldlM add 0 entries
    if total == 1 then Right () else Left (SimplexMassNotOne total)
  where
    add accumulator (choice, mass)
        | mass < 0 = Left (NegativeSimplexMass choice mass)
        | rationalSizeBits mass > maximumGameRationalBits limits = Left (SimplexRationalLimitExceeded choice (rationalSizeBits mass) (maximumGameRationalBits limits))
        | otherwise =
            let total = accumulator + mass
                actual = rationalSizeBits total
             in if actual > maximumGameRationalBits limits
                    then Left (SimplexTotalRationalLimitExceeded actual (maximumGameRationalBits limits))
                    else Right total

{- | Saturating addition for operation preflight.  A result above the limit is
represented by exactly @limit + 1@.
-}
cappedGameAdd :: Natural -> Natural -> Natural -> Natural
cappedGameAdd limit left right
    | left > limit || right > limit = limit + 1
    | left > limit - right = limit + 1
    | otherwise = left + right

-- | Saturating multiplication for operation preflight.
cappedGameProduct :: Natural -> Natural -> Natural -> Natural
cappedGameProduct limit left right
    | left == 0 || right == 0 = 0
    | left > limit || right > limit = limit + 1
    | left > limit `quot` right = limit + 1
    | otherwise = left * right

-- | Saturating natural exponentiation for operation preflight.
cappedGamePower :: Natural -> Natural -> Natural -> Natural
cappedGamePower limit = go 1
  where
    go accumulator _ 0 = accumulator
    go accumulator factor remaining
        | odd remaining =
            let next = cappedGameProduct limit accumulator factor
             in if next > limit then limit + 1 else go next (cappedGameProduct limit factor factor) (remaining `quot` 2)
        | otherwise = go accumulator (cappedGameProduct limit factor factor) (remaining `quot` 2)

-- | Combined numerator/denominator binary size (zero has one numerator bit).
rationalSizeBits :: Rational -> Natural
rationalSizeBits value = integerBits (numerator value) + integerBits (denominator value)

-- | Check an intermediate rational against the operation limit.
checkRationalSize :: GameLimits -> Rational -> Either (Natural, Natural) Rational
checkRationalSize limits value
    | actual > maximumGameRationalBits limits = Left (actual, maximumGameRationalBits limits)
    | otherwise = Right value
  where
    actual = rationalSizeBits value

integerBits :: Integer -> Natural
integerBits integer = fromIntegral (go (abs integer))
  where
    go number
        | number < 2 = 1 :: Int
        | otherwise = 1 + go (number `quot` 2)

naturalLength :: [a] -> Natural
naturalLength = fromIntegral . length

checkLocal :: GameLimits -> (owner, FiniteObject choice) -> Either (OwnedProductError owner) ()
checkLocal limits (owner, choices) =
    let actual = naturalLength (NonEmpty.toList (finiteObjectValues choices))
     in if actual > maximumGameLocalChoices limits
            then Left (TooManyLocalChoices owner actual (maximumGameLocalChoices limits))
            else Right ()

requireAtMost :: Natural -> Natural -> error -> Either error ()
requireAtMost limit actual problem = if actual > limit then Left problem else Right ()

firstDuplicateBy :: (Eq key) => (value -> key) -> [value] -> Maybe key
firstDuplicateBy _ [] = Nothing
firstDuplicateBy project (value : remaining)
    | project value `elem` map project remaining = Just (project value)
    | otherwise = firstDuplicateBy project remaining

firstOutside :: (Eq value) => [value] -> [value] -> Maybe value
firstOutside allowed = firstWhere (`notElem` allowed)

firstWhere :: (a -> Bool) -> [a] -> Maybe a
firstWhere _ [] = Nothing
firstWhere predicate (value : remaining)
    | predicate value = Just value
    | otherwise = firstWhere predicate remaining

foldlM :: (accumulator -> value -> Either error accumulator) -> accumulator -> [value] -> Either error accumulator
foldlM function = go
  where
    go accumulator [] = Right accumulator
    go accumulator (value : remaining) = case function accumulator value of
        Left problem -> Left problem
        Right next -> go next remaining

traverse_ :: (a -> Either error ()) -> [a] -> Either error ()
traverse_ function = foldl' step (Right ())
  where
    step (Left problem) _ = Left problem
    step (Right ()) value = function value
