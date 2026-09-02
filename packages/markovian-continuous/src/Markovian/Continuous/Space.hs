{-# LANGUAGE RoleAnnotations #-}

{- | Closed witnesses for the standard-Borel spaces implemented by this package.
A witness is not an enumerable sigma algebra and does not make arbitrary
Haskell functions measurable.
-}
module Markovian.Continuous.Space (
    RealBorel,
    Discrete,
    Product,
    StandardBorel,
    SpaceError (..),
    realBorel,
    finiteDiscrete,
    productBorel,
    spaceDescription,
) where

import Markovian.Continuous.Internal
import Numeric.Natural (Natural)

-- | A finite-space layout failure.
data SpaceError
    = EmptyDiscreteLayout
    | DuplicateDiscreteValue
    | DiscreteLayoutLimitExceeded Natural Natural
    deriving stock (Eq, Show)

-- | Witness the real Borel space.
realBorel :: StandardBorel RealBorel
realBorel = StandardBorel "real Borel"

-- | Validate and witness a nonempty finite discrete space.
finiteDiscrete :: (Eq value) => Natural -> [value] -> Either SpaceError (StandardBorel (Discrete value))
finiteDiscrete maximumValues input = do
    values <- bounded input
    let count = naturalLength values
    if null values then Left EmptyDiscreteLayout else Right ()
    if hasDuplicate values then Left DuplicateDiscreteValue else Right ()
    Right (StandardBorel ("finite discrete (" ++ show count ++ ")"))
  where
    -- Inspect at most maximumValues + 1 cells, including for an infinite list.
    bounded = go 0 []
    go _ values [] = Right (reverse values)
    go count _ (_ : _) | count >= maximumValues = Left (DiscreteLayoutLimitExceeded maximumValues (maximumValues + 1))
    go count values (value : rest) = go (count + 1) (value : values) rest
    naturalLength :: [value] -> Natural
    naturalLength = foldr (const (+ 1)) 0
    hasDuplicate [] = False
    hasDuplicate (value : rest) = value `elem` rest || hasDuplicate rest

-- | Form a witness for a finite product.
productBorel :: StandardBorel left -> StandardBorel right -> StandardBorel (Product left right)
productBorel (StandardBorel left) (StandardBorel right) = StandardBorel ("product(" ++ left ++ "," ++ right ++ ")")

-- | Get a deterministic witness description.
spaceDescription :: StandardBorel space -> String
spaceDescription (StandardBorel description) = description
