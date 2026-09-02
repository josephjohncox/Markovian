{-# LANGUAGE RoleAnnotations #-}

{- | Rational affine additive-uniform kernels. Composition is checked and
fallible, so this module intentionally supplies no unrestricted category instance.
-}
module Markovian.Continuous.Kernel.Exact (
    ExactContinuousKernel,
    deterministicAffineKernel,
    affineUniformKernel,
    composeContinuousKernel,
    alphaRenameKernel,
    kernelLawAt,
    kernelNoiseOwners,
    ExactLimits (..),
    ExactError (..),
) where

import Markovian.Continuous.Internal
import Numeric.Natural (Natural)

-- | The deterministic kernel @x -> scale*x + offset@.
deterministicAffineKernel :: Rational -> Rational -> ExactContinuousKernel RealBorel RealBorel
deterministicAffineKernel scale offset = ExactContinuousKernel scale offset []

-- | Construct @x -> scale*x + offset + sum coefficient*U@.
affineUniformKernel :: ExactLimits -> Rational -> Rational -> [(NoiseOwner owner, Rational, RationalInterval)] -> Either ExactError (ExactContinuousKernel RealBorel RealBorel)
affineUniformKernel limits scale offset input = do
    checkRational limits scale
    checkRational limits offset
    terms <- canonicalNoise limits [(owner, coefficient, interval) | (NoiseOwner owner, coefficient, interval) <- input]
    pure (ExactContinuousKernel scale offset terms)

-- | Compose two kernels after an owner-collision and budget check.
composeContinuousKernel :: ExactLimits -> ExactContinuousKernel RealBorel RealBorel -> ExactContinuousKernel RealBorel RealBorel -> Either ExactError (ExactContinuousKernel RealBorel RealBorel)
composeContinuousKernel limits (ExactContinuousKernel firstScale firstOffset firstNoise) (ExactContinuousKernel secondScale secondOffset secondNoise)
    | any (`elem` secondOwners) firstOwners = Left NoiseOwnerCollision
    | otherwise = do
        mapM_ (checkRational limits) [firstScale, firstOffset, secondScale, secondOffset]
        _ <- canonicalNoise limits firstNoise
        _ <- canonicalNoise limits secondNoise
        scale <- checkedRational limits (*) secondScale firstScale
        scaledOffset <- checkedRational limits (*) secondScale firstOffset
        offset <- checkedRational limits (+) scaledOffset secondOffset
        scaledNoise <- traverse scaleTerm firstNoise
        canonical <- canonicalNoise limits (scaledNoise ++ secondNoise)
        pure (ExactContinuousKernel scale offset canonical)
  where
    scaleTerm (owner, coefficient, interval) = do
        scaled <- checkedRational limits (*) secondScale coefficient
        pure (owner, scaled, interval)
    firstOwners = [owner | (owner, _, _) <- firstNoise]
    secondOwners = [owner | (owner, _, _) <- secondNoise]

-- | Rename noise owners and reject duplicate results.
alphaRenameKernel :: ExactLimits -> [(NoiseOwner old, NoiseOwner new)] -> ExactContinuousKernel source target -> Either ExactError (ExactContinuousKernel source target)
alphaRenameKernel limits inputRenaming (ExactContinuousKernel scale offset terms) = do
    mapM_ (checkRational limits) [scale, offset]
    renaming <- boundedList (limitTerms limits) TermLimitExceeded inputRenaming
    let sources = [old | (NoiseOwner old, _) <- renaming]
        targets = [new | (_, NoiseOwner new) <- renaming]
    checkWork limits (fromIntegral (length renaming) * fromIntegral (length renaming))
    if hasDuplicate sources then Left DuplicateNoiseMapping else Right ()
    if hasDuplicate targets then Left NonInjectiveNoiseRenaming else Right ()
    let rename owner = case [new | (NoiseOwner old, NoiseOwner new) <- renaming, old == owner] of
            [] -> owner
            new : _ -> new
        renamed = [(rename owner, coefficient, interval) | (owner, coefficient, interval) <- terms]
    if hasDuplicate [owner | (owner, _, _) <- renamed] then Left NonInjectiveNoiseRenaming else Right ()
    canonical <- canonicalNoise limits renamed
    pure (ExactContinuousKernel scale offset canonical)
  where
    hasDuplicate [] = False
    hasDuplicate (value : rest) = value `elem` rest || hasDuplicate rest

-- | Apply a kernel to a rational input.
kernelLawAt :: ExactLimits -> ExactContinuousKernel RealBorel RealBorel -> Rational -> Either ExactError (ExactLaw RealBorel)
kernelLawAt limits (ExactContinuousKernel scale offset terms) input = do
    mapM_ (checkRational limits) [scale, offset, input]
    productValue <- checkedRational limits (*) scale input
    constant <- checkedRational limits (+) productValue offset
    affineUniformLawInternal limits constant terms

-- | Get stable noise-owner identifiers.
kernelNoiseOwners :: ExactContinuousKernel source target -> [Natural]
kernelNoiseOwners (ExactContinuousKernel _ _ terms) = [owner | (owner, _, _) <- terms]

affineUniformLawInternal :: ExactLimits -> Rational -> [(Natural, Rational, RationalInterval)] -> Either ExactError (ExactLaw RealBorel)
affineUniformLawInternal limits constant terms = do
    checkRational limits constant
    canonical <- canonicalNoise limits terms
    pure (ExactLaw constant canonical)
