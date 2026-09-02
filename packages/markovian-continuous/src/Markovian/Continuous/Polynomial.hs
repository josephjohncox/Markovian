-- | Bounded rational polynomials accepted by exact integration.
module Markovian.Continuous.Polynomial (
    RationalPolynomial,
    RationalBivariatePolynomial,
    ExactLimits (..),
    ExactError (..),
    rationalPolynomial,
    rationalBivariatePolynomial,
    polynomialTerms,
    bivariatePolynomialTerms,
) where

import Control.Monad (when)
import Data.Maybe (isJust)
import Markovian.Continuous.Internal
import Numeric.Natural (Natural)

-- | Validate a sparse univariate polynomial.
rationalPolynomial :: ExactLimits -> [(Natural, Rational)] -> Either ExactError RationalPolynomial
rationalPolynomial limits input = do
    validateLimits limits
    raw <- boundedList (limitTerms limits) TermLimitExceeded input
    checkWork limits (fromIntegral (length raw))
    let terms = filter ((/= 0) . snd) raw
        degrees = map fst terms
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length terms))
    mapM_ (checkCount limitDegree DegreeLimitExceeded limits) degrees
    case firstDuplicate degrees of
        Just degree -> Left (DuplicatePolynomialDegree degree)
        Nothing -> pure ()
    mapM_ (checkRational limits . snd) terms
    pure (RationalPolynomial terms)

-- | Validate a sparse bivariate polynomial.
rationalBivariatePolynomial :: ExactLimits -> [(Natural, Natural, Rational)] -> Either ExactError RationalBivariatePolynomial
rationalBivariatePolynomial limits input = do
    validateLimits limits
    raw <- boundedList (limitTerms limits) TermLimitExceeded input
    checkWork limits (fromIntegral (length raw))
    let terms = filter (\(_, _, coefficient) -> coefficient /= 0) raw
        degrees = [(left, right) | (left, right, _) <- terms]
    checkCount limitTerms TermLimitExceeded limits (fromIntegral (length terms))
    mapM_ (\(left, right) -> checkCount limitDegree DegreeLimitExceeded limits (left + right)) degrees
    when (hasDuplicate degrees) (Left (DuplicatePolynomialDegree 0))
    mapM_ (\(_, _, coefficient) -> checkRational limits coefficient) terms
    pure (RationalBivariatePolynomial terms)

-- | Get canonical nonzero univariate terms.
polynomialTerms :: RationalPolynomial -> [(Natural, Rational)]
polynomialTerms (RationalPolynomial terms) = terms

-- | Get canonical nonzero bivariate terms.
bivariatePolynomialTerms :: RationalBivariatePolynomial -> [(Natural, Natural, Rational)]
bivariatePolynomialTerms (RationalBivariatePolynomial terms) = terms

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : rest)
    | value `elem` rest = Just value
    | otherwise = firstDuplicate rest

hasDuplicate :: (Eq value) => [value] -> Bool
hasDuplicate = isJust . firstDuplicate
