{- | Sized structural action masks for neural output heads.

A mask records the complete output width and a nonempty, duplicate-free list of
active indices. The list order is semantic evidence for deterministic tie
breaking. Boolean flags describe membership in global output order; no numeric
mask is constructed.
-}
module Markovian.Backend.Neural.Mask (
    ActionMaskError (..),
    ActionMask,
    mkActionMask,
    actionMaskWidth,
    actionMaskIndices,
    actionMaskFlags,
    actionMaskContains,
    gatherActionMask,
    scatterActionMask,
) where

import Numeric.Natural (Natural)

-- | Construction and checked gather/scatter failures.
data ActionMaskError
    = InvalidActionMaskWidth !Int
    | EmptyActionMask
    | NegativeActionIndex !Int !Int
    | ActionMaskIndexOutOfBounds !Int !Int !Int
    | DuplicateActionIndex !Int
    | ActionMaskGatherWidthMismatch !Int !Natural
    | ActionMaskScatterCountMismatch !Int !Natural
    | ActionMaskScatterNonFiniteValue !Int
    deriving (Eq, Show)

-- | A positive width, active count, and nonempty ordered active output indices.
data ActionMask = ActionMask !Int !Int ![Int]
    deriving (Eq, Show)

-- | Construct a mask for one complete neural output width.
mkActionMask :: Int -> [Int] -> Either ActionMaskError ActionMask
mkActionMask width indices
    | width <= 0 = Left (InvalidActionMaskWidth width)
    | null indices = Left EmptyActionMask
    | otherwise = do
        count <- validateIndices 0 [] indices
        Right (ActionMask width count indices)
  where
    validateIndices position _ [] = Right position
    validateIndices position seen (index : remaining)
        | index < 0 = Left (NegativeActionIndex position index)
        | index >= width = Left (ActionMaskIndexOutOfBounds position index width)
        | index `elem` seen = Left (DuplicateActionIndex index)
        | otherwise = validateIndices (position + 1) (index : seen) remaining

-- | Complete neural output width.
actionMaskWidth :: ActionMask -> Int
actionMaskWidth (ActionMask width _ _) = width

-- | Active indices in caller-defined availability and tie-breaking order.
actionMaskIndices :: ActionMask -> [Int]
actionMaskIndices (ActionMask _ _ indices) = indices

-- | Membership flags in complete global output order.
actionMaskFlags :: ActionMask -> [Bool]
actionMaskFlags mask =
    [ actionMaskContains index mask
    | index <- [0 .. actionMaskWidth mask - 1]
    ]

-- | Test whether one global output index is active.
actionMaskContains :: Int -> ActionMask -> Bool
actionMaskContains index = elem index . actionMaskIndices

{- | Gather active values in availability order.

The complete input width must match the mask width. This operation is the
forward masking primitive used before softmax or argmax.
-}
gatherActionMask :: ActionMask -> [value] -> Either ActionMaskError [value]
gatherActionMask mask values
    | actual /= fromIntegral expected = Left (ActionMaskGatherWidthMismatch expected actual)
    | otherwise = traverse valueAt (actionMaskIndices mask)
  where
    expected = actionMaskWidth mask
    actual = boundedLength expected values
    valueAt index =
        case valueAtIndex index values of
            Just value -> Right value
            Nothing -> Left (ActionMaskIndexOutOfBounds 0 index expected)

{- | Scatter active 'Double' values into global output order.

Every active value must be finite. Every unavailable position is the literal
positive IEEE-754 zero @0.0@. The mask is structural and is not differentiated.
-}
scatterActionMask :: ActionMask -> [Double] -> Either ActionMaskError [Double]
scatterActionMask mask values
    | actual /= fromIntegral expected = Left (ActionMaskScatterCountMismatch expected actual)
    | Just position <- firstNonFinite 0 values = Left (ActionMaskScatterNonFiniteValue position)
    | otherwise = Right (go 0)
  where
    expected = actionMaskActiveCount mask
    actual = boundedLength expected values
    indexed = zip (actionMaskIndices mask) values
    go index
        | index >= actionMaskWidth mask = []
        | otherwise =
            case lookup index indexed of
                Nothing -> 0.0 : go (index + 1)
                Just value -> value : go (index + 1)

actionMaskActiveCount :: ActionMask -> Int
actionMaskActiveCount (ActionMask _ count _) = count

-- Consume no more than the expected prefix and one witness of overrun.
boundedLength :: Int -> [value] -> Natural
boundedLength expected = go 0
  where
    naturalExpected = fromIntegral expected
    go count [] = count
    go count (_ : remaining)
        | count >= naturalExpected = naturalExpected + 1
        | otherwise = go (count + 1) remaining

firstNonFinite :: Int -> [Double] -> Maybe Int
firstNonFinite _ [] = Nothing
firstNonFinite position (value : remaining)
    | isNaN value || isInfinite value = Just position
    | otherwise = firstNonFinite (position + 1) remaining

valueAtIndex :: Int -> [value] -> Maybe value
valueAtIndex requested
    | requested < 0 = const Nothing
    | otherwise = go requested
  where
    go _ [] = Nothing
    go 0 (value : _) = Just value
    go remaining (_ : values) = go (remaining - 1) values
