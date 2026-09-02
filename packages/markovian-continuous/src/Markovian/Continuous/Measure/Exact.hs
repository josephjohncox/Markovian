{-# LANGUAGE RoleAnnotations #-}

{- | Compact affine combinations of independent rational uniform noises.
"Exact" means that supported polynomial expectations use literal 'Rational'
arithmetic. It is not an oracle for arbitrary measurable events.
-}
module Markovian.Continuous.Measure.Exact (
    NoiseOwner,
    noiseOwner,
    RationalInterval,
    rationalInterval,
    intervalBounds,
    ExactLaw,
    ExactJointLaw,
    ExactIntegralReport,
    exactIntegralValue,
    exactIntegralDegree,
    exactIntegralTerms,
    exactIntegralNoiseOwners,
    exactIntegralWork,
    exactIntegralRawExpansionPairs,
    exactIntegralCanonicalTerms,
    exactIntegralMaximumCanonicalTerms,
    exactIntegralMonomialPowerMerges,
    exactIntegralMonomialPowerComparisons,
    exactIntegralCoefficientMultiplications,
    exactIntegralCoefficientAdditions,
    exactIntegralCoefficientCancellations,
    exactIntegralCanonicalCombinations,
    exactIntegralInputArithmeticOperations,
    exactIntegralMomentPowerOperations,
    exactIntegralMomentArithmeticOperations,
    exactIntegralMomentProducts,
    exactIntegralFinalSummations,
    exactIntegralMaximumRationalBits,
    diracReal,
    uniformReal,
    affineUniformLaw,
    affinePushforward,
    lawSupport,
    expectPolynomial,
    shareAffineSource,
    independentPair,
    firstMarginal,
    secondMarginal,
    expectBivariatePolynomial,
    ExactLimits (..),
    ExactError (..),
) where

import Markovian.Continuous.Internal
import Numeric.Natural (Natural)

-- | Make a nominal noise owner from a stable identifier.
noiseOwner :: Natural -> NoiseOwner owner
noiseOwner = NoiseOwner

-- | Validate strictly ordered rational endpoints.
rationalInterval :: Rational -> Rational -> Either ExactError RationalInterval
rationalInterval lower upper
    | lower < upper = Right (RationalInterval lower upper)
    | otherwise = Left (InvalidInterval lower upper)

-- | Get the lower and upper endpoints.
intervalBounds :: RationalInterval -> (Rational, Rational)
intervalBounds (RationalInterval lower upper) = (lower, upper)

-- | Make a deterministic real law.
diracReal :: ExactLimits -> Rational -> Either ExactError (ExactLaw RealBorel)
diracReal limits value = do
    validateLimits limits
    checkRational limits value
    pure (ExactLaw value [])

-- | Make one compact rational uniform law.
uniformReal :: ExactLimits -> NoiseOwner owner -> RationalInterval -> Either ExactError (ExactLaw RealBorel)
uniformReal limits (NoiseOwner owner) interval = affineUniformLaw limits 0 [(NoiseOwner owner, 1, interval)]

-- | Make a rational affine combination of independent uniform noises.
affineUniformLaw :: ExactLimits -> Rational -> [(NoiseOwner owner, Rational, RationalInterval)] -> Either ExactError (ExactLaw RealBorel)
affineUniformLaw limits constant input = do
    checkRational limits constant
    terms <- canonicalNoise limits [(owner, coefficient, interval) | (NoiseOwner owner, coefficient, interval) <- input]
    pure (ExactLaw constant terms)

-- | Push a law through a rational affine map.
affinePushforward :: ExactLimits -> Rational -> Rational -> ExactLaw RealBorel -> Either ExactError (ExactLaw RealBorel)
affinePushforward limits scale offset law@(ExactLaw constant terms) = do
    validateLawInput limits law
    mapM_ (checkRational limits) [scale, offset, constant]
    scaledConstant <- checkedRational limits (*) scale constant
    pushedConstant <- checkedRational limits (+) scaledConstant offset
    pushedTerms <- traverse scaleTerm terms
    affineUniformLaw limits pushedConstant pushedTerms
  where
    scaleTerm (owner, coefficient, interval) = do
        scaled <- checkedRational limits (*) scale coefficient
        pure (NoiseOwner owner, scaled, interval)

-- | Get the exact compact support interval.
lawSupport :: ExactLaw RealBorel -> RationalInterval
lawSupport = supportOf

-- | Integrate a represented polynomial exactly.
expectPolynomial :: ExactLimits -> ExactLaw RealBorel -> RationalPolynomial -> Either ExactError ExactIntegralReport
expectPolynomial = expectPolynomialInternal

-- | Use one law twice with the same noise owners.
shareAffineSource :: ExactLaw RealBorel -> ExactJointLaw RealBorel RealBorel
shareAffineSource law = ExactJointLaw law law

-- | Pair two laws after a disjoint-owner check.
independentPair :: ExactLaw RealBorel -> ExactLaw RealBorel -> Either ExactError (ExactJointLaw RealBorel RealBorel)
independentPair left@(ExactLaw _ leftTerms) right@(ExactLaw _ rightTerms)
    | any (`elem` rightOwners) leftOwners = Left NoiseOwnerCollision
    | otherwise = Right (ExactJointLaw left right)
  where
    leftOwners = [owner | (owner, _, _) <- leftTerms]
    rightOwners = [owner | (owner, _, _) <- rightTerms]

-- | Get the first represented marginal.
firstMarginal :: ExactJointLaw left right -> ExactLaw left
firstMarginal (ExactJointLaw left _) = left

-- | Get the second represented marginal.
secondMarginal :: ExactJointLaw left right -> ExactLaw right
secondMarginal (ExactJointLaw _ right) = right

-- | Integrate a represented bivariate polynomial exactly.
expectBivariatePolynomial :: ExactLimits -> ExactJointLaw RealBorel RealBorel -> RationalBivariatePolynomial -> Either ExactError ExactIntegralReport
expectBivariatePolynomial = expectBivariateInternal

-- | Get the exact rational result.
exactIntegralValue :: ExactIntegralReport -> Rational
exactIntegralValue = exactIntegralValueInternal

-- | Get the largest integrated total degree.
exactIntegralDegree :: ExactIntegralReport -> Natural
exactIntegralDegree = exactIntegralDegreeInternal

-- | Get the input term count.
exactIntegralTerms :: ExactIntegralReport -> Natural
exactIntegralTerms = exactIntegralTermsInternal

-- | Get the distinct noise-owner count.
exactIntegralNoiseOwners :: ExactIntegralReport -> Natural
exactIntegralNoiseOwners = exactIntegralNoiseOwnersInternal

-- | Get the deterministic cumulative work count.
exactIntegralWork :: ExactIntegralReport -> Natural
exactIntegralWork = exactIntegralWorkInternal

-- | Get the cumulative number of generated Cartesian expansion pairs.
exactIntegralRawExpansionPairs :: ExactIntegralReport -> Natural
exactIntegralRawExpansionPairs = exactIntegralRawExpansionPairsInternal

-- | Get the number of nonzero canonical monomials evaluated.
exactIntegralCanonicalTerms :: ExactIntegralReport -> Natural
exactIntegralCanonicalTerms = exactIntegralCanonicalTermsInternal

-- | Get the largest live canonical expansion during the operation.
exactIntegralMaximumCanonicalTerms :: ExactIntegralReport -> Natural
exactIntegralMaximumCanonicalTerms = exactIntegralMaximumCanonicalTermsInternal

-- | Get the number of equal-owner exponent merges.
exactIntegralMonomialPowerMerges :: ExactIntegralReport -> Natural
exactIntegralMonomialPowerMerges = exactIntegralMonomialPowerMergesInternal

-- | Get the number of owner or exponent comparisons between monomials.
exactIntegralMonomialPowerComparisons :: ExactIntegralReport -> Natural
exactIntegralMonomialPowerComparisons = exactIntegralMonomialPowerComparisonsInternal

-- | Get the number of symbolic coefficient multiplications.
exactIntegralCoefficientMultiplications :: ExactIntegralReport -> Natural
exactIntegralCoefficientMultiplications = exactIntegralCoefficientMultiplicationsInternal

-- | Get the number of additions used to combine equal monomials.
exactIntegralCoefficientAdditions :: ExactIntegralReport -> Natural
exactIntegralCoefficientAdditions = exactIntegralCoefficientAdditionsInternal

-- | Get the number of zero products or zero sums discarded canonically.
exactIntegralCoefficientCancellations :: ExactIntegralReport -> Natural
exactIntegralCoefficientCancellations = exactIntegralCoefficientCancellationsInternal

-- | Get the number of candidate insertions into canonical expansions.
exactIntegralCanonicalCombinations :: ExactIntegralReport -> Natural
exactIntegralCanonicalCombinations = exactIntegralCanonicalCombinationsInternal

-- | Get the number of support-bound products and sums checked at admission.
exactIntegralInputArithmeticOperations :: ExactIntegralReport -> Natural
exactIntegralInputArithmeticOperations = exactIntegralInputArithmeticOperationsInternal

-- | Get the number of multiplications used to form uniform powers.
exactIntegralMomentPowerOperations :: ExactIntegralReport -> Natural
exactIntegralMomentPowerOperations = exactIntegralMomentPowerOperationsInternal

-- | Get the number of subtractions, products, and quotients used to form moments.
exactIntegralMomentArithmeticOperations :: ExactIntegralReport -> Natural
exactIntegralMomentArithmeticOperations = exactIntegralMomentArithmeticOperationsInternal

-- | Get the number of monomial-by-moment products.
exactIntegralMomentProducts :: ExactIntegralReport -> Natural
exactIntegralMomentProducts = exactIntegralMomentProductsInternal

-- | Get the number of additions into the final expectation.
exactIntegralFinalSummations :: ExactIntegralReport -> Natural
exactIntegralFinalSummations = exactIntegralFinalSummationsInternal

{- | Get the maximum observed numerator or denominator bit size. This is a
complete intermediate maximum for bivariate reports. Univariate reports
retain their earlier, narrower accounting and report only retained values.
-}
exactIntegralMaximumRationalBits :: ExactIntegralReport -> Natural
exactIntegralMaximumRationalBits = exactIntegralMaximumRationalBitsInternal
