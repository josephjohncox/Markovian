{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TupleSections #-}

{- | Exact conditioning for a continuous latent affine-uniform law and a
finite observation with affine likelihoods on a checked compact interval.
-}
module Markovian.Continuous.Condition.Exact (
    ExactFiniteLikelihood,
    ExactPosterior,
    FiniteDisintegration,
    affineFiniteLikelihood,
    conditionFiniteObservation,
    finiteObservationDisintegration,
    posteriorObservation,
    posteriorEvidence,
    posteriorExpectation,
    positivePosteriorRows,
    ExactLimits (..),
    ExactError (..),
) where

import Data.List (foldl')
import Data.Maybe (catMaybes)
import Markovian.Continuous.Internal
import Numeric.Natural (Natural)

{- | Build a complete affine likelihood table. The layout and rows must contain
the same observations exactly once. Endpoint checks suffice because every
represented likelihood is affine.
-}
affineFiniteLikelihood :: (Eq observation) => ExactLimits -> [observation] -> RationalInterval -> [(observation, Rational, Rational)] -> Either ExactError (ExactFiniteLikelihood observation)
affineFiniteLikelihood limits inputLayout domain@(RationalInterval lower upper) inputRows = do
    validateLimits limits
    layout <- boundedList (limitTerms limits) TermLimitExceeded inputLayout
    rows <- boundedList (limitTerms limits) TermLimitExceeded inputRows
    let rowCount = fromIntegral (length rows)
        layoutCount = fromIntegral (length layout)
    checkWork limits (rowCount * rowCount + layoutCount * layoutCount + rowCount + layoutCount)
    if null layout then Left IncompleteObservationLayout else Right ()
    if lower < upper then Right () else Left (InvalidInterval lower upper)
    if hasDuplicate layout || hasDuplicate (map first rows) then Left DuplicateObservation else Right ()
    if length layout /= length rows || any (\observation -> not (any ((== observation) . first) rows)) layout
        then Left IncompleteObservationLayout
        else Right ()
    mapM_ validateRow rows
    slopeTotal <- checkedSum limits [slope | (_, slope, _) <- rows]
    offsetTotal <- checkedSum limits [offset | (_, _, offset) <- rows]
    if slopeTotal == 0 then Right () else Left (InvalidLikelihood "likelihood slopes must sum to zero")
    if offsetTotal == 1 then Right () else Left (InvalidLikelihood "likelihood offsets must sum to one")
    endpoints <- traverse endpointValues rows
    if any (any (\value -> value < 0 || value > 1)) endpoints
        then Left (InvalidLikelihood "likelihood must be in [0,1] over the domain")
        else Right (ExactFiniteLikelihood layout domain rows)
  where
    first (value, _, _) = value
    validateRow (_, slope, offset) = checkRational limits slope >> checkRational limits offset
    endpointValues (_, slope, offset) = do
        lowerProduct <- checkedRational limits (*) slope lower
        upperProduct <- checkedRational limits (*) slope upper
        lowerValue <- checkedRational limits (+) lowerProduct offset
        upperValue <- checkedRational limits (+) upperProduct offset
        pure [lowerValue, upperValue]

-- | Construct one posterior row and reject zero evidence.
conditionFiniteObservation :: (Eq observation) => ExactLimits -> ExactLaw RealBorel -> ExactFiniteLikelihood observation -> observation -> Either ExactError (ExactPosterior observation)
conditionFiniteObservation limits law likelihood observation = do
    (candidate, _) <- posteriorCandidateWithWork limits law likelihood observation
    maybe (Left ZeroEvidence) Right candidate

-- | Construct all positive-evidence posterior rows.
finiteObservationDisintegration :: (Eq observation) => ExactLimits -> ExactLaw RealBorel -> ExactFiniteLikelihood observation -> Either ExactError (FiniteDisintegration observation)
finiteObservationDisintegration limits law likelihood@(ExactFiniteLikelihood layout domain rowsInput) = do
    validateLawInput limits law
    ensureSupport domain (supportOf law)
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length layout))
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length rowsInput))
    let baseWork = fromIntegral (length layout)
    checkWork limits baseWork
    (rows, _) <- foldl' attempt (Right ([], baseWork)) layout
    pure (FiniteDisintegration (reverse (catMaybes rows)))
  where
    attempt failure@(Left _) _ = failure
    attempt (Right (rows, work)) observation =
        case posteriorCandidateWithWork limits law likelihood observation of
            Left err -> Left err
            Right (candidate, rowWork) -> addRow (fmap (observation,) candidate) rowWork
      where
        addRow row rowWork = do
            let total = work + rowWork
            checkWork limits total
            pure (row : rows, total)

-- | Get the observation for this posterior row.
posteriorObservation :: ExactPosterior observation -> observation
posteriorObservation (ExactPosterior observation _ _ _ _) = observation

-- | Get the exact positive evidence.
posteriorEvidence :: ExactPosterior observation -> Rational
posteriorEvidence (ExactPosterior _ evidence _ _ _) = evidence

-- | Integrate a polynomial under the posterior row.
posteriorExpectation :: ExactLimits -> ExactPosterior observation -> RationalPolynomial -> Either ExactError Rational
posteriorExpectation limits (ExactPosterior _ evidence law slope offset) (RationalPolynomial terms) = do
    checkRational limits evidence
    checkRational limits slope
    checkRational limits offset
    let weightedCount = 2 * fromIntegral (length terms)
    checkCount limitTerms TermLimitExceeded limits weightedCount
    checkWork limits weightedCount
    weightedTerms <- fmap concat . traverse multiply $ terms
    report <- expectPolynomialInternal limits law (RationalPolynomial weightedTerms)
    checkWork limits (weightedCount + exactIntegralWorkInternal report)
    let numeratorValue = exactIntegralValueInternal report
    checkedRational limits (/) numeratorValue evidence
  where
    multiply (degree, coefficient) = do
        withSlope <- checkedRational limits (*) coefficient slope
        withOffset <- checkedRational limits (*) coefficient offset
        pure (filter ((/= 0) . snd) [(degree + 1, withSlope), (degree, withOffset)])

-- | Get the positive-evidence rows in layout order.
positivePosteriorRows :: FiniteDisintegration observation -> [(observation, ExactPosterior observation)]
positivePosteriorRows (FiniteDisintegration rows) = rows

posteriorCandidateWithWork :: (Eq observation) => ExactLimits -> ExactLaw RealBorel -> ExactFiniteLikelihood observation -> observation -> Either ExactError (Maybe (ExactPosterior observation), Natural)
posteriorCandidateWithWork limits law (ExactFiniteLikelihood layout domain rows) observation = do
    validateLawInput limits law
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length layout))
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length rows))
    ensureSupport domain (supportOf law)
    case [(slope, offset) | (candidate, slope, offset) <- rows, candidate == observation] of
        [] -> Left UnknownObservation
        (slope, offset) : _ -> do
            polynomial <- likelihoodPolynomial limits slope offset
            report <- expectPolynomialInternal limits law polynomial
            let evidence = exactIntegralValueInternal report
            checkRational limits evidence
            if evidence == 0
                then pure (Nothing, exactIntegralWorkInternal report)
                else pure (Just (ExactPosterior observation evidence law slope offset), exactIntegralWorkInternal report)

likelihoodPolynomial :: ExactLimits -> Rational -> Rational -> Either ExactError RationalPolynomial
likelihoodPolynomial limits slope offset = do
    checkRational limits slope
    checkRational limits offset
    let terms = filter ((/= 0) . snd) [(1, slope), (0, offset)]
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length terms))
    checkCount limitDegree DegreeLimitExceeded limits 1
    checkWork limits 2
    pure (RationalPolynomial terms)

ensureSupport :: RationalInterval -> RationalInterval -> Either ExactError ()
ensureSupport (RationalInterval domainLower domainUpper) (RationalInterval priorLower priorUpper)
    | priorLower < domainLower || priorUpper > domainUpper = Left PriorOutsideLikelihoodDomain
    | otherwise = Right ()

hasDuplicate :: (Eq value) => [value] -> Bool
hasDuplicate [] = False
hasDuplicate (value : rest) = value `elem` rest || hasDuplicate rest
