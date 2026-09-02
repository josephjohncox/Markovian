-- | Executable affine maps on the real Borel space.
module Markovian.Continuous.Map (
    BorelMap,
    identityRealMap,
    affineRealMap,
    composeRealMap,
    applyRealMap,
) where

import Markovian.Continuous.Internal

-- | The identity affine map.
identityRealMap :: BorelMap RealBorel RealBorel
identityRealMap = AffineMap 1 0

-- | Construct the measurable map @x -> scale*x + offset@.
affineRealMap :: Rational -> Rational -> BorelMap RealBorel RealBorel
affineRealMap = AffineMap

-- | Compose maps in application order: @composeRealMap f g@ means @g . f@.
composeRealMap :: BorelMap RealBorel RealBorel -> BorelMap RealBorel RealBorel -> BorelMap RealBorel RealBorel
composeRealMap (AffineMap firstScale firstOffset) (AffineMap secondScale secondOffset) =
    AffineMap (secondScale * firstScale) (secondScale * firstOffset + secondOffset)

-- | Apply a represented affine map to a rational value.
applyRealMap :: BorelMap RealBorel RealBorel -> Rational -> Rational
applyRealMap (AffineMap scale offset) value = scale * value + offset
