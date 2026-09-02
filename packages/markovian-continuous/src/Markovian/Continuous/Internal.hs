{-# LANGUAGE RoleAnnotations #-}

module Markovian.Continuous.Internal where

import Control.Monad (unless, when)
import Data.List (foldl', sortOn)
import Data.Ratio (denominator, numerator)
import Numeric.Natural (Natural)

-- Space witnesses are closed descriptions, not run-time sigma algebras.

-- | The real line with its Borel sigma algebra.
data RealBorel

-- | A represented finite discrete space.
newtype Discrete value = Discrete value

-- | A finite product of represented spaces.
newtype Product left right = Product (left, right)

-- | An opaque witness for one supported standard-Borel space.
newtype StandardBorel space = StandardBorel String

type role StandardBorel nominal

-- | A closed executable Borel map.
data BorelMap source target = AffineMap Rational Rational

type role BorelMap nominal nominal

-- | A nominal identity for one independent uniform noise source.
newtype NoiseOwner owner = NoiseOwner Natural
    deriving stock (Eq, Ord, Show)

type role NoiseOwner nominal

-- | A nonempty compact interval with rational endpoints.
data RationalInterval = RationalInterval Rational Rational
    deriving stock (Eq, Show)

{- | Limits for degree, public input terms, symbolic expansion terms, owners,
cumulative work, and rational bit size. Raw expansion pairs and live
canonical monomials have separate limits because cancellation can make the
latter much smaller than the former.
-}
data ExactLimits = ExactLimits
    { limitDegree :: Natural
    , limitTerms :: Natural
    , limitRawExpansionTerms :: Natural
    , limitCanonicalExpansionTerms :: Natural
    , limitNoiseOwners :: Natural
    , limitWork :: Natural
    , limitRationalBits :: Natural
    }
    deriving stock (Eq, Show)

-- | A construction, ownership, evidence, or budget failure.
data ExactError
    = InvalidLimit String
    | InvalidInterval Rational Rational
    | DuplicateNoiseOwner
    | DuplicateNoiseMapping
    | NonInjectiveNoiseRenaming
    | NoiseOwnerCollision
    | InconsistentSharedNoise
    | DegreeLimitExceeded Natural Natural
    | TermLimitExceeded Natural Natural
    | RawExpansionTermLimitExceeded Natural Natural
    | CanonicalExpansionTermLimitExceeded Natural Natural
    | NoiseOwnerLimitExceeded Natural Natural
    | WorkLimitExceeded Natural Natural
    | RationalBitLimitExceeded Natural Natural
    | DuplicatePolynomialDegree Natural
    | DuplicateObservation
    | IncompleteObservationLayout
    | InvalidLikelihood String
    | PriorOutsideLikelihoodDomain
    | ZeroEvidence
    | UnknownObservation
    deriving stock (Eq, Show)

-- | A bounded sparse rational polynomial.
newtype RationalPolynomial = RationalPolynomial [(Natural, Rational)]
    deriving stock (Eq, Show)

-- | A bounded sparse rational polynomial in two variables.
newtype RationalBivariatePolynomial = RationalBivariatePolynomial [(Natural, Natural, Rational)]
    deriving stock (Eq, Show)

-- | A compact rational affine-uniform law on a supported space.
data ExactLaw space = ExactLaw Rational [(Natural, Rational, RationalInterval)]

type role ExactLaw nominal

-- | Two affine laws with explicit shared or independent noise owners.
data ExactJointLaw left right = ExactJointLaw (ExactLaw left) (ExactLaw right)

type role ExactJointLaw nominal nominal

-- | A deterministic exact value and its semantic work counts.
data ExactIntegralReport = ExactIntegralReport
    { exactIntegralValueInternal :: Rational
    , exactIntegralDegreeInternal :: Natural
    , exactIntegralTermsInternal :: Natural
    , exactIntegralNoiseOwnersInternal :: Natural
    , exactIntegralWorkInternal :: Natural
    , exactIntegralRawExpansionPairsInternal :: Natural
    , exactIntegralCanonicalTermsInternal :: Natural
    , exactIntegralMaximumCanonicalTermsInternal :: Natural
    , exactIntegralMonomialPowerMergesInternal :: Natural
    , exactIntegralMonomialPowerComparisonsInternal :: Natural
    , exactIntegralCoefficientMultiplicationsInternal :: Natural
    , exactIntegralCoefficientAdditionsInternal :: Natural
    , exactIntegralCoefficientCancellationsInternal :: Natural
    , exactIntegralCanonicalCombinationsInternal :: Natural
    , exactIntegralInputArithmeticOperationsInternal :: Natural
    , exactIntegralMomentPowerOperationsInternal :: Natural
    , exactIntegralMomentArithmeticOperationsInternal :: Natural
    , exactIntegralMomentProductsInternal :: Natural
    , exactIntegralFinalSummationsInternal :: Natural
    , exactIntegralMaximumRationalBitsInternal :: Natural
    }
    deriving stock (Eq, Show)

-- | A rational affine additive-uniform kernel.
data ExactContinuousKernel source target = ExactContinuousKernel Rational Rational [(Natural, Rational, RationalInterval)]

type role ExactContinuousKernel nominal nominal

-- | A complete finite affine likelihood on a compact interval.
data ExactFiniteLikelihood observation = ExactFiniteLikelihood [observation] RationalInterval [(observation, Rational, Rational)]

type role ExactFiniteLikelihood nominal

-- | A positive-evidence posterior row for one finite observation.
data ExactPosterior observation = ExactPosterior observation Rational (ExactLaw RealBorel) Rational Rational

type role ExactPosterior nominal

-- | The positive-evidence rows of a finite observation disintegration.
newtype FiniteDisintegration observation = FiniteDisintegration [(observation, ExactPosterior observation)]

type role FiniteDisintegration nominal

validateLimits :: ExactLimits -> Either ExactError ()
validateLimits limits
    | limitTerms limits == 0 = Left (InvalidLimit "term limit must be positive")
    | limitRawExpansionTerms limits == 0 = Left (InvalidLimit "raw expansion-term limit must be positive")
    | limitCanonicalExpansionTerms limits == 0 = Left (InvalidLimit "canonical expansion-term limit must be positive")
    | limitNoiseOwners limits == 0 = Left (InvalidLimit "noise-owner limit must be positive")
    | limitWork limits == 0 = Left (InvalidLimit "work limit must be positive")
    | limitRationalBits limits == 0 = Left (InvalidLimit "rational-bit limit must be positive")
    | any (> machineMaximum) [limitDegree limits, limitTerms limits, limitRawExpansionTerms limits, limitCanonicalExpansionTerms limits, limitNoiseOwners limits, limitWork limits] = Left (InvalidLimit "execution limits exceed machine indexing")
    | otherwise = Right ()
  where
    machineMaximum = fromIntegral (maxBound :: Int)

integerBits :: Integer -> Natural
integerBits value = go 0 (abs value)
  where
    go count 0 = max 1 count
    go count n = go (count + 1) (n `div` 2)

rationalBits :: Rational -> Natural
rationalBits value = max (integerBits (numerator value)) (integerBits (denominator value))

checkRational :: ExactLimits -> Rational -> Either ExactError ()
checkRational limits value
    | actual > limitRationalBits limits = Left (RationalBitLimitExceeded (limitRationalBits limits) actual)
    | otherwise = Right ()
  where
    actual = rationalBits value

checkCount :: (ExactLimits -> Natural) -> (Natural -> Natural -> ExactError) -> ExactLimits -> Natural -> Either ExactError ()
checkCount project makeError limits actual
    | actual > project limits = Left (makeError (project limits) actual)
    | otherwise = Right ()

-- Consume no more than @limit + 1@ list cells. This is used before filtering,
-- sorting, duplicate detection, or expansion at every public list boundary.
boundedList :: Natural -> (Natural -> Natural -> ExactError) -> [value] -> Either ExactError [value]
boundedList limit makeError = go 0 []
  where
    go _ values [] = Right (reverse values)
    go count _ (_ : _) | count >= limit = Left (makeError limit (limit + 1))
    go count values (value : rest) = go (count + 1) (value : values) rest

checkWork :: ExactLimits -> Natural -> Either ExactError ()
checkWork = checkCount limitWork WorkLimitExceeded

checkedRational :: ExactLimits -> (Rational -> Rational -> Rational) -> Rational -> Rational -> Either ExactError Rational
checkedRational limits operation left right = do
    let result = operation left right
    checkRational limits result
    pure result

checkedSum :: ExactLimits -> [Rational] -> Either ExactError Rational
checkedSum limits = foldl' step (Right 0)
  where
    step failure@(Left _) _ = failure
    step (Right total) value = checkedRational limits (+) total value

canonicalNoise :: ExactLimits -> [(Natural, Rational, RationalInterval)] -> Either ExactError [(Natural, Rational, RationalInterval)]
canonicalNoise limits input = do
    validateLimits limits
    raw <- boundedList (limitTerms limits) TermLimitExceeded input
    let rawCount = fromIntegral (length raw)
    -- The quadratic charge is a conservative bound for the stable sort and
    -- duplicate pass. Zero coefficients are charged before being discarded.
    checkWork limits (rawCount + rawCount * rawCount)
    mapM_ validateRaw raw
    let nonzero = filter (\(_, coefficient, _) -> coefficient /= 0) raw
        sorted = sortOn (\(owner, _, _) -> owner) nonzero
    checkCount limitNoiseOwners NoiseOwnerLimitExceeded limits (fromIntegral (length sorted))
    when (or (zipWith sameOwner sorted (drop 1 sorted))) (Left DuplicateNoiseOwner)
    _ <- supportBoundsChecked limits 0 sorted
    pure sorted
  where
    validateRaw (_, coefficient, RationalInterval lower upper) = do
        checkRational limits coefficient
        checkRational limits lower
        checkRational limits upper
        when (lower >= upper) (Left (InvalidInterval lower upper))
    sameOwner (left, _, _) (right, _, _) = left == right

supportBoundsChecked :: ExactLimits -> Rational -> [(Natural, Rational, RationalInterval)] -> Either ExactError (Rational, Rational)
supportBoundsChecked limits constant terms = do
    checkRational limits constant
    contributions <- traverse bounds terms
    lower <- checkedSum limits (constant : map fst contributions)
    upper <- checkedSum limits (constant : map snd contributions)
    pure (lower, upper)
  where
    bounds (_, coefficient, RationalInterval lower upper)
        | coefficient >= 0 = (,) <$> checkedRational limits (*) coefficient lower <*> checkedRational limits (*) coefficient upper
        | otherwise = (,) <$> checkedRational limits (*) coefficient upper <*> checkedRational limits (*) coefficient lower

validateLawInput :: ExactLimits -> ExactLaw RealBorel -> Either ExactError ()
validateLawInput limits law@(ExactLaw constant terms) = do
    validateLawStructure limits law
    _ <- supportBoundsChecked limits constant terms
    pure ()

validateLawStructure :: ExactLimits -> ExactLaw RealBorel -> Either ExactError ()
validateLawStructure limits (ExactLaw constant terms) = do
    validateLimits limits
    checkRational limits constant
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length terms))
    checkCount limitNoiseOwners NoiseOwnerLimitExceeded limits (fromIntegral (length terms))
    mapM_ validateTerm terms
  where
    validateTerm (_, coefficient, RationalInterval lower upper) = do
        mapM_ (checkRational limits) [coefficient, lower, upper]
        when (lower >= upper) (Left (InvalidInterval lower upper))

supportOf :: ExactLaw RealBorel -> RationalInterval
supportOf (ExactLaw constant terms) = RationalInterval (constant + sum lowers) (constant + sum uppers)
  where
    bounds (_, coefficient, RationalInterval lower upper)
        | coefficient >= 0 = (coefficient * lower, coefficient * upper)
        | otherwise = (coefficient * upper, coefficient * lower)
    (lowers, uppers) = unzip (map bounds terms)

binomialChecked :: ExactLimits -> Natural -> Natural -> Either ExactError Rational
binomialChecked _ n k | k > n = Right 0
binomialChecked limits n k = go 1 1
  where
    reduced = min k (n - k)
    go index value
        | index > reduced = Right value
        | otherwise = do
            numeratorValue <- checkedRational limits (*) value (fromIntegral (n - reduced + index))
            next <- checkedRational limits (/) numeratorValue (fromIntegral index)
            go (index + 1) next

momentsOf :: ExactLimits -> ExactLaw RealBorel -> Natural -> Either ExactError ([Rational], Natural)
momentsOf limits law@(ExactLaw constant noise) degree = do
    validateLawInput limits law
    checkCount limitDegree DegreeLimitExceeded limits degree
    let required = (degree + 1) * (fromIntegral (length noise) + 1) * (degree + 1)
    checkWork limits required
    initial <- traverse (checkedPowerRational limits constant) [0 .. degree]
    result <- foldMExact (addVariable limits degree) initial noise
    pure (result, required)

addVariable :: ExactLimits -> Natural -> [Rational] -> (Natural, Rational, RationalInterval) -> Either ExactError [Rational]
addVariable limits degree current (_, coefficient, interval) = traverse moment [0 .. degree]
  where
    moment k = traverse term [0 .. k] >>= checkedSum limits
      where
        term j = do
            coefficientPower <- checkedPowerRational limits coefficient j
            momentValue <- uniformMomentChecked limits interval j
            binomialValue <- binomialChecked limits k j
            first <- checkedRational limits (*) binomialValue (current !! fromIntegral (k - j))
            second <- checkedRational limits (*) first coefficientPower
            checkedRational limits (*) second momentValue

foldMExact :: (value -> element -> Either ExactError value) -> value -> [element] -> Either ExactError value
foldMExact _ initial [] = Right initial
foldMExact step initial (value : rest) = step initial value >>= \next -> foldMExact step next rest

checkedPowerRational :: ExactLimits -> Rational -> Natural -> Either ExactError Rational
checkedPowerRational limits base = go 1
  where
    go value 0 = Right value
    go value remaining = checkedRational limits (*) value base >>= \next -> go next (remaining - 1)

uniformMomentChecked :: ExactLimits -> RationalInterval -> Natural -> Either ExactError Rational
uniformMomentChecked _ _ 0 = Right 1
uniformMomentChecked limits (RationalInterval lower upper) degree = do
    upperPower <- checkedPowerRational limits upper (degree + 1)
    lowerPower <- checkedPowerRational limits lower (degree + 1)
    difference <- checkedRational limits (-) upperPower lowerPower
    width <- checkedRational limits (-) upper lower
    denominatorValue <- checkedRational limits (*) (fromIntegral (degree + 1)) width
    checkedRational limits (/) difference denominatorValue

expectPolynomialInternal :: ExactLimits -> ExactLaw RealBorel -> RationalPolynomial -> Either ExactError ExactIntegralReport
expectPolynomialInternal limits law@(ExactLaw _ noise) (RationalPolynomial terms) = do
    validateLawInput limits law
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length terms))
    mapM_ (\(degreeValue, coefficient) -> checkCount limitDegree DegreeLimitExceeded limits degreeValue >> checkRational limits coefficient) terms
    let degree = foldr (max . fst) 0 terms
        momentWork = (degree + 1) * (fromIntegral (length noise) + 1) * (degree + 1)
        totalWork = momentWork + fromIntegral (length terms)
    checkWork limits totalWork
    (moments, work) <- momentsOf limits law degree
    weighted <- traverse (\(power, coefficient) -> checkedRational limits (*) coefficient (moments !! fromIntegral power)) terms
    value <- checkedSum limits weighted
    pure
        ExactIntegralReport
            { exactIntegralValueInternal = value
            , exactIntegralDegreeInternal = degree
            , exactIntegralTermsInternal = fromIntegral (length terms)
            , exactIntegralNoiseOwnersInternal = fromIntegral (length noise)
            , exactIntegralWorkInternal = work + fromIntegral (length terms)
            , exactIntegralRawExpansionPairsInternal = 0
            , exactIntegralCanonicalTermsInternal = 0
            , exactIntegralMaximumCanonicalTermsInternal = 0
            , exactIntegralMonomialPowerMergesInternal = 0
            , exactIntegralMonomialPowerComparisonsInternal = 0
            , exactIntegralCoefficientMultiplicationsInternal = 0
            , exactIntegralCoefficientAdditionsInternal = 0
            , exactIntegralCoefficientCancellationsInternal = 0
            , exactIntegralCanonicalCombinationsInternal = 0
            , exactIntegralInputArithmeticOperationsInternal = 0
            , exactIntegralMomentPowerOperationsInternal = 0
            , exactIntegralMomentArithmeticOperationsInternal = 0
            , exactIntegralMomentProductsInternal = 0
            , exactIntegralFinalSummationsInternal = 0
            , exactIntegralMaximumRationalBitsInternal = maximum (map rationalBits (value : moments ++ weighted))
            }

-- Expand powers of two affine forms into owner monomials. Every generated
-- Cartesian pair is inserted immediately into a sorted canonical expansion.
-- Thus an exhausted raw-pair or canonical-term budget stops at limit + 1 and
-- never constructs the complete oversized expansion.
type Monomial = [(Natural, Natural)]
type Expansion = [(Monomial, Rational)]

data RationalOperation
    = CoefficientMultiplication
    | CoefficientAddition
    | InputArithmeticOperation
    | MomentPowerOperation
    | MomentArithmeticOperation
    | MomentProduct
    | FinalSummation

data ExactMeter = ExactMeter
    { meterWork :: Natural
    , meterRawExpansionPairs :: Natural
    , meterMaximumCanonicalTerms :: Natural
    , meterMonomialPowerMerges :: Natural
    , meterMonomialPowerComparisons :: Natural
    , meterCoefficientMultiplications :: Natural
    , meterCoefficientAdditions :: Natural
    , meterCoefficientCancellations :: Natural
    , meterCanonicalCombinations :: Natural
    , meterInputArithmeticOperations :: Natural
    , meterMomentPowerOperations :: Natural
    , meterMomentArithmeticOperations :: Natural
    , meterMomentProducts :: Natural
    , meterFinalSummations :: Natural
    , meterMaximumRationalBits :: Natural
    }

emptyMeter :: ExactMeter
emptyMeter =
    ExactMeter
        { meterWork = 0
        , meterRawExpansionPairs = 0
        , meterMaximumCanonicalTerms = 0
        , meterMonomialPowerMerges = 0
        , meterMonomialPowerComparisons = 0
        , meterCoefficientMultiplications = 0
        , meterCoefficientAdditions = 0
        , meterCoefficientCancellations = 0
        , meterCanonicalCombinations = 0
        , meterInputArithmeticOperations = 0
        , meterMomentPowerOperations = 0
        , meterMomentArithmeticOperations = 0
        , meterMomentProducts = 0
        , meterFinalSummations = 0
        , meterMaximumRationalBits = 0
        }

chargeWorkUnit :: ExactLimits -> ExactMeter -> Either ExactError ExactMeter
chargeWorkUnit limits meter = do
    let actual = meterWork meter + 1
    checkWork limits actual
    pure meter{meterWork = actual}

chargeRawPair :: ExactLimits -> ExactMeter -> Either ExactError ExactMeter
chargeRawPair limits meter = do
    let actual = meterRawExpansionPairs meter + 1
    checkCount limitRawExpansionTerms RawExpansionTermLimitExceeded limits actual
    charged <- chargeWorkUnit limits meter
    pure charged{meterRawExpansionPairs = actual}

chargePowerMerge :: ExactLimits -> ExactMeter -> Either ExactError ExactMeter
chargePowerMerge limits meter = do
    charged <- chargeWorkUnit limits meter
    pure charged{meterMonomialPowerMerges = meterMonomialPowerMerges charged + 1}

chargePowerComparison :: ExactLimits -> ExactMeter -> Either ExactError ExactMeter
chargePowerComparison limits meter = do
    charged <- chargeWorkUnit limits meter
    pure charged{meterMonomialPowerComparisons = meterMonomialPowerComparisons charged + 1}

chargeCancellation :: ExactLimits -> ExactMeter -> Either ExactError ExactMeter
chargeCancellation limits meter = do
    charged <- chargeWorkUnit limits meter
    pure charged{meterCoefficientCancellations = meterCoefficientCancellations charged + 1}

chargeCanonicalCombination :: ExactLimits -> ExactMeter -> Either ExactError ExactMeter
chargeCanonicalCombination limits meter = do
    charged <- chargeWorkUnit limits meter
    pure charged{meterCanonicalCombinations = meterCanonicalCombinations charged + 1}

chargeRationalOperation :: ExactLimits -> RationalOperation -> ExactMeter -> Either ExactError ExactMeter
chargeRationalOperation limits operation meter = do
    charged <- chargeWorkUnit limits meter
    pure $ case operation of
        CoefficientMultiplication -> charged{meterCoefficientMultiplications = meterCoefficientMultiplications charged + 1}
        CoefficientAddition -> charged{meterCoefficientAdditions = meterCoefficientAdditions charged + 1}
        InputArithmeticOperation -> charged{meterInputArithmeticOperations = meterInputArithmeticOperations charged + 1}
        MomentPowerOperation -> charged{meterMomentPowerOperations = meterMomentPowerOperations charged + 1}
        MomentArithmeticOperation -> charged{meterMomentArithmeticOperations = meterMomentArithmeticOperations charged + 1}
        MomentProduct -> charged{meterMomentProducts = meterMomentProducts charged + 1}
        FinalSummation -> charged{meterFinalSummations = meterFinalSummations charged + 1}

observeRational :: ExactLimits -> Rational -> ExactMeter -> Either ExactError ExactMeter
observeRational limits value meter = do
    checkRational limits value
    pure meter{meterMaximumRationalBits = max (meterMaximumRationalBits meter) (rationalBits value)}

checkedMeterRational :: ExactLimits -> RationalOperation -> (Rational -> Rational -> Rational) -> Rational -> Rational -> ExactMeter -> Either ExactError (Rational, ExactMeter)
checkedMeterRational limits kind operation left right meter = do
    observedLeft <- observeRational limits left meter
    observedRight <- observeRational limits right observedLeft
    let result = operation left right
    observedResult <- observeRational limits result observedRight
    charged <- chargeRationalOperation limits kind observedResult
    pure (result, charged)

observeCanonicalCount :: ExactLimits -> Expansion -> ExactMeter -> Either ExactError ExactMeter
observeCanonicalCount limits expansion meter = do
    actual <- boundedCanonicalCount (limitCanonicalExpansionTerms limits) expansion
    pure meter{meterMaximumCanonicalTerms = max (meterMaximumCanonicalTerms meter) actual}
  where
    boundedCanonicalCount maximumAllowed = go 0
      where
        go seen [] = Right seen
        go seen (_ : _)
            | seen >= maximumAllowed = Left (CanonicalExpansionTermLimitExceeded maximumAllowed (seen + 1))
        go seen (_ : rest) = go (seen + 1) rest

compareMonomials :: ExactLimits -> Monomial -> Monomial -> ExactMeter -> Either ExactError (Ordering, ExactMeter)
compareMonomials _ [] [] meter = Right (EQ, meter)
compareMonomials _ [] (_ : _) meter = Right (LT, meter)
compareMonomials _ (_ : _) [] meter = Right (GT, meter)
compareMonomials limits ((leftOwner, leftPower) : leftRest) ((rightOwner, rightPower) : rightRest) meter = do
    ownerMeter <- chargePowerComparison limits meter
    case compare leftOwner rightOwner of
        LT -> Right (LT, ownerMeter)
        GT -> Right (GT, ownerMeter)
        EQ -> do
            powerMeter <- chargePowerComparison limits ownerMeter
            case compare leftPower rightPower of
                LT -> Right (LT, powerMeter)
                GT -> Right (GT, powerMeter)
                EQ -> compareMonomials limits leftRest rightRest powerMeter

mergePowers :: ExactLimits -> Monomial -> Monomial -> ExactMeter -> Either ExactError (Monomial, ExactMeter)
mergePowers _ [] right meter = Right (right, meter)
mergePowers _ left [] meter = Right (left, meter)
mergePowers limits left@((leftOwner, leftPower) : leftRest) right@((rightOwner, rightPower) : rightRest) meter = do
    compared <- chargePowerComparison limits meter
    case compare leftOwner rightOwner of
        LT -> do
            (suffix, next) <- mergePowers limits leftRest right compared
            pure ((leftOwner, leftPower) : suffix, next)
        GT -> do
            (suffix, next) <- mergePowers limits left rightRest compared
            pure ((rightOwner, rightPower) : suffix, next)
        EQ -> do
            merged <- chargePowerMerge limits compared
            (suffix, next) <- mergePowers limits leftRest rightRest merged
            pure ((leftOwner, leftPower + rightPower) : suffix, next)

insertCanonical :: ExactLimits -> (Monomial, Rational) -> Expansion -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
insertCanonical limits candidate@(_, coefficient) expansion meter = do
    combined <- chargeCanonicalCombination limits meter
    if coefficient == 0
        then do
            cancelled <- chargeCancellation limits combined
            next <- observeCanonicalCount limits expansion cancelled
            pure (expansion, next)
        else go expansion combined
  where
    go [] current = do
        let result = [candidate]
        next <- observeCanonicalCount limits result current
        pure (result, next)
    go allTerms@(term@(powers, value) : rest) current = do
        (ordering, compared) <- compareMonomials limits (fst candidate) powers current
        case ordering of
            LT -> do
                let result = candidate : allTerms
                next <- observeCanonicalCount limits result compared
                pure (result, next)
            GT -> do
                (suffix, next) <- go rest compared
                let result = term : suffix
                observed <- observeCanonicalCount limits result next
                pure (result, observed)
            EQ -> do
                (total, added) <- checkedMeterRational limits CoefficientAddition (+) coefficient value compared
                if total == 0
                    then do
                        cancelled <- chargeCancellation limits added
                        next <- observeCanonicalCount limits rest cancelled
                        pure (rest, next)
                    else do
                        let result = (powers, total) : rest
                        next <- observeCanonicalCount limits result added
                        pure (result, next)

canonicalForm :: ExactLimits -> ExactLaw RealBorel -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
canonicalForm limits (ExactLaw constant terms) meter = do
    let expansion = filter ((/= 0) . snd) (([], constant) : [([(owner, 1)], coefficient) | (owner, coefficient, _) <- terms])
    next <- observeCanonicalCount limits expansion meter
    pure (expansion, next)

multiplyExpansion :: ExactLimits -> Expansion -> Expansion -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
multiplyExpansion limits left right meter = goLeft left meter []
  where
    goLeft [] current result = Right (result, current)
    goLeft (leftTerm : leftRest) current result = do
        (nextResult, next) <- goRight leftTerm right current result
        goLeft leftRest next nextResult
    goRight _ [] current result = Right (result, current)
    goRight (leftPowers, leftValue) ((rightPowers, rightValue) : rightRest) current result = do
        raw <- chargeRawPair limits current
        (coefficient, multiplied) <- checkedMeterRational limits CoefficientMultiplication (*) leftValue rightValue raw
        (powers, merged) <- mergePowers limits leftPowers rightPowers multiplied
        (nextResult, combined) <- insertCanonical limits (powers, coefficient) result merged
        goRight (leftPowers, leftValue) rightRest combined nextResult

powerExpansion :: ExactLimits -> ExactLaw RealBorel -> Natural -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
powerExpansion limits law degree meter = do
    identityMeter <- observeCanonicalCount limits [([], 1)] meter
    (form, formMeter) <- canonicalForm limits law identityMeter
    go degree [([], 1)] form formMeter
  where
    go 0 result _ current = Right (result, current)
    go remaining result form current = do
        (nextResult, next) <- multiplyExpansion limits result form current
        go (remaining - 1) nextResult form next

scaleExpansion :: ExactLimits -> Rational -> Expansion -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
scaleExpansion limits scale = go []
  where
    go result [] meter = Right (result, meter)
    go result ((powers, value) : rest) meter = do
        (scaled, multiplied) <- checkedMeterRational limits CoefficientMultiplication (*) scale value meter
        (nextResult, combined) <- insertCanonical limits (powers, scaled) result multiplied
        go nextResult rest combined

combineExpansions :: ExactLimits -> Expansion -> Expansion -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
combineExpansions limits = foldMInsert
  where
    foldMInsert result [] meter = Right (result, meter)
    foldMInsert result (term : rest) meter = do
        (nextResult, next) <- insertCanonical limits term result meter
        foldMInsert nextResult rest next

expandTerm :: ExactLimits -> ExactLaw RealBorel -> ExactLaw RealBorel -> (Natural, Natural, Rational) -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
expandTerm limits left right (leftPower, rightPower, coefficient) meter = do
    (leftTerms, leftMeter) <- powerExpansion limits left leftPower meter
    (rightTerms, rightMeter) <- powerExpansion limits right rightPower leftMeter
    (productTerms, productMeter) <- multiplyExpansion limits leftTerms rightTerms rightMeter
    scaleExpansion limits coefficient productTerms productMeter

expandPolynomial :: ExactLimits -> ExactLaw RealBorel -> ExactLaw RealBorel -> [(Natural, Natural, Rational)] -> ExactMeter -> Either ExactError (Expansion, ExactMeter)
expandPolynomial limits left right = go []
  where
    go result [] meter = Right (result, meter)
    go result (term : rest) meter = do
        (expanded, expandedMeter) <- expandTerm limits left right term meter
        (nextResult, combined) <- combineExpansions limits result expanded expandedMeter
        go nextResult rest combined

lookupIntervals :: ExactLaw RealBorel -> [(Natural, RationalInterval)]
lookupIntervals (ExactLaw _ terms) = [(owner, interval) | (owner, _, interval) <- terms]

checkedPowerMeter :: ExactLimits -> Rational -> Natural -> ExactMeter -> Either ExactError (Rational, ExactMeter)
checkedPowerMeter limits base = go 1
  where
    go value 0 meter = Right (value, meter)
    go value remaining meter = do
        (next, charged) <- checkedMeterRational limits MomentPowerOperation (*) value base meter
        go next (remaining - 1) charged

uniformMomentMeter :: ExactLimits -> RationalInterval -> Natural -> ExactMeter -> Either ExactError (Rational, ExactMeter)
uniformMomentMeter _ _ 0 meter = Right (1, meter)
uniformMomentMeter limits (RationalInterval lower upper) degree meter = do
    (upperPower, upperMeter) <- checkedPowerMeter limits upper (degree + 1) meter
    (lowerPower, lowerMeter) <- checkedPowerMeter limits lower (degree + 1) upperMeter
    (difference, differenceMeter) <- checkedMeterRational limits MomentArithmeticOperation (-) upperPower lowerPower lowerMeter
    (width, widthMeter) <- checkedMeterRational limits MomentArithmeticOperation (-) upper lower differenceMeter
    (denominatorValue, denominatorMeter) <- checkedMeterRational limits MomentArithmeticOperation (*) (fromIntegral (degree + 1)) width widthMeter
    checkedMeterRational limits MomentArithmeticOperation (/) difference denominatorValue denominatorMeter

evaluateMonomial :: ExactLimits -> [(Natural, RationalInterval)] -> (Monomial, Rational) -> ExactMeter -> Either ExactError (Rational, ExactMeter)
evaluateMonomial limits intervals (powers, coefficient) = go coefficient powers
  where
    go value [] meter = Right (value, meter)
    go value ((owner, power) : rest) meter = case lookup owner intervals of
        Nothing -> Left InconsistentSharedNoise
        Just interval -> do
            (moment, momentMeter) <- uniformMomentMeter limits interval power meter
            (next, productMeter) <- checkedMeterRational limits MomentProduct (*) value moment momentMeter
            go next rest productMeter

evaluateExpansion :: ExactLimits -> [(Natural, RationalInterval)] -> Expansion -> ExactMeter -> Either ExactError (Rational, ExactMeter)
evaluateExpansion limits intervals = go 0
  where
    go total [] meter = Right (total, meter)
    go total (term : rest) meter = do
        (value, valueMeter) <- evaluateMonomial limits intervals term meter
        (nextTotal, sumMeter) <- checkedMeterRational limits FinalSummation (+) total value valueMeter
        go nextTotal rest sumMeter

observeLawAndPolynomial :: ExactLimits -> ExactLaw RealBorel -> ExactLaw RealBorel -> [(Natural, Natural, Rational)] -> ExactMeter -> Either ExactError ExactMeter
observeLawAndPolynomial limits (ExactLaw leftConstant leftTerms) (ExactLaw rightConstant rightTerms) terms meter =
    foldMExact observe meter values
  where
    values = leftConstant : rightConstant : map (\(_, coefficient, _) -> coefficient) (leftTerms ++ rightTerms) ++ concatMap intervalValues (leftTerms ++ rightTerms) ++ map (\(_, _, coefficient) -> coefficient) terms
    intervalValues (_, _, RationalInterval lower upper) = [lower, upper]
    observe current value = observeRational limits value current

accountSupportBounds :: ExactLimits -> ExactLaw RealBorel -> ExactMeter -> Either ExactError ExactMeter
accountSupportBounds limits (ExactLaw constant terms) = go constant constant terms
  where
    go _ _ [] meter = Right meter
    go lowerTotal upperTotal ((_, coefficient, RationalInterval lower upper) : rest) meter = do
        let (lowerEndpoint, upperEndpoint)
                | coefficient >= 0 = (lower, upper)
                | otherwise = (upper, lower)
        (lowerContribution, lowerProductMeter) <- checkedMeterRational limits InputArithmeticOperation (*) coefficient lowerEndpoint meter
        (upperContribution, upperProductMeter) <- checkedMeterRational limits InputArithmeticOperation (*) coefficient upperEndpoint lowerProductMeter
        (nextLower, lowerSumMeter) <- checkedMeterRational limits InputArithmeticOperation (+) lowerTotal lowerContribution upperProductMeter
        (nextUpper, upperSumMeter) <- checkedMeterRational limits InputArithmeticOperation (+) upperTotal upperContribution lowerSumMeter
        go nextLower nextUpper rest upperSumMeter

expectBivariateInternal :: ExactLimits -> ExactJointLaw RealBorel RealBorel -> RationalBivariatePolynomial -> Either ExactError ExactIntegralReport
expectBivariateInternal limits (ExactJointLaw left right) (RationalBivariatePolynomial terms) = do
    validateLawStructure limits left
    validateLawStructure limits right
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length terms))
    mapM_ validateTerm terms
    let degree = foldr max 0 [x + y | (x, y, _) <- terms]
    checkCount limitDegree DegreeLimitExceeded limits degree
    let leftIntervals = lookupIntervals left
        rightIntervals = lookupIntervals right
        conflicts = [() | (owner, interval) <- leftIntervals, Just other <- [lookup owner rightIntervals], interval /= other]
    unless (null conflicts) (Left InconsistentSharedNoise)
    let intervals = foldl' addInterval leftIntervals rightIntervals
    checkCount limitNoiseOwners NoiseOwnerLimitExceeded limits (fromIntegral (length intervals))
    seeded <- observeLawAndPolynomial limits left right terms emptyMeter
    leftAccounted <- accountSupportBounds limits left seeded
    rightAccounted <- accountSupportBounds limits right leftAccounted
    (expansion, expandedMeter) <- expandPolynomial limits left right terms rightAccounted
    (value, finalMeter) <- evaluateExpansion limits intervals expansion expandedMeter
    pure
        ExactIntegralReport
            { exactIntegralValueInternal = value
            , exactIntegralDegreeInternal = degree
            , exactIntegralTermsInternal = fromIntegral (length terms)
            , exactIntegralNoiseOwnersInternal = fromIntegral (length intervals)
            , exactIntegralWorkInternal = meterWork finalMeter
            , exactIntegralRawExpansionPairsInternal = meterRawExpansionPairs finalMeter
            , exactIntegralCanonicalTermsInternal = fromIntegral (length expansion)
            , exactIntegralMaximumCanonicalTermsInternal = meterMaximumCanonicalTerms finalMeter
            , exactIntegralMonomialPowerMergesInternal = meterMonomialPowerMerges finalMeter
            , exactIntegralMonomialPowerComparisonsInternal = meterMonomialPowerComparisons finalMeter
            , exactIntegralCoefficientMultiplicationsInternal = meterCoefficientMultiplications finalMeter
            , exactIntegralCoefficientAdditionsInternal = meterCoefficientAdditions finalMeter
            , exactIntegralCoefficientCancellationsInternal = meterCoefficientCancellations finalMeter
            , exactIntegralCanonicalCombinationsInternal = meterCanonicalCombinations finalMeter
            , exactIntegralInputArithmeticOperationsInternal = meterInputArithmeticOperations finalMeter
            , exactIntegralMomentPowerOperationsInternal = meterMomentPowerOperations finalMeter
            , exactIntegralMomentArithmeticOperationsInternal = meterMomentArithmeticOperations finalMeter
            , exactIntegralMomentProductsInternal = meterMomentProducts finalMeter
            , exactIntegralFinalSummationsInternal = meterFinalSummations finalMeter
            , exactIntegralMaximumRationalBitsInternal = meterMaximumRationalBits finalMeter
            }
  where
    validateTerm (leftPower, rightPower, coefficient) = do
        checkCount limitDegree DegreeLimitExceeded limits (leftPower + rightPower)
        checkRational limits coefficient
    addInterval acc pair@(owner, _)
        | any ((== owner) . fst) acc = acc
        | otherwise = acc ++ [pair]
