{-# LANGUAGE DeriveFunctor #-}

-- | Private bounded scalar SSA used only for conservative exact rewrites.
module Markovian.Autodiff.Internal.SSA (
    Operand (..),
    Instruction (..),
    ScalarSSA,
    SSAError (..),
    scalarSSA,
    ssaInstructions,
    rewriteExactSSA,
    interpretSSA,
) where

import Numeric.Natural (Natural)

-- | An input, earlier register, or literal.
data Operand scalar
    = Input !Natural
    | Register !Natural
    | Literal !scalar
    deriving (Eq, Show, Functor)

-- | Closed scalar instructions. There is no branch or callback instruction.
data Instruction scalar
    = Copy !(Operand scalar)
    | Negate !(Operand scalar)
    | Add !(Operand scalar) !(Operand scalar)
    | Multiply !(Operand scalar) !(Operand scalar)
    deriving (Eq, Show, Functor)

-- | Validated input count, instructions, and result operand.
data ScalarSSA scalar = ScalarSSA !Natural ![Instruction scalar] !(Operand scalar)
    deriving (Eq, Show)

-- | Bounded construction or interpretation failure.
data SSAError
    = SSAInstructionLimitExceeded !Natural
    | SSAInputOutOfBounds !Natural
    | SSARegisterNotEarlier !Natural !Natural
    | SSAInputLengthMismatch !Natural !Natural
    | SSAInternalReferenceFailure !Natural
    deriving (Eq, Show)

-- | Admit at most @instructionLimit + 1@ list cells and check all references.
scalarSSA :: Natural -> Natural -> [Instruction scalar] -> Operand scalar -> Either SSAError (ScalarSSA scalar)
scalarSSA instructionLimit inputCount rawInstructions output = do
    instructions <- consume 0 rawInstructions
    mapM_ (uncurry (validateInstruction inputCount)) (zip [0 ..] instructions)
    validateOperand inputCount (fromIntegral (length instructions)) output
    Right (ScalarSSA inputCount instructions output)
  where
    consume _ [] = Right []
    consume seen (_ : _) | seen >= instructionLimit = Left (SSAInstructionLimitExceeded instructionLimit)
    consume seen (instruction : rest) = (instruction :) <$> consume (seen + 1) rest

validateInstruction :: Natural -> Natural -> Instruction scalar -> Either SSAError ()
validateInstruction inputs register instruction = case instruction of
    Copy operand -> validateOperand inputs register operand
    Negate operand -> validateOperand inputs register operand
    Add left right -> validateOperand inputs register left >> validateOperand inputs register right
    Multiply left right -> validateOperand inputs register left >> validateOperand inputs register right

validateOperand :: Natural -> Natural -> Operand scalar -> Either SSAError ()
validateOperand inputs _ (Input index)
    | index < inputs = Right ()
    | otherwise = Left (SSAInputOutOfBounds index)
validateOperand _ earlier (Register index)
    | index < earlier = Right ()
    | otherwise = Left (SSARegisterNotEarlier index earlier)
validateOperand _ _ (Literal _) = Right ()

{- | Exact identities only. This pass does not reassociate, distribute, or
reorder operations, so it is not reused for floating execution.
-}
rewriteExactSSA :: ScalarSSA Rational -> ScalarSSA Rational
rewriteExactSSA (ScalarSSA inputs instructions output) = ScalarSSA inputs (map rewrite instructions) output
  where
    rewrite (Add operand (Literal 0)) = Copy operand
    rewrite (Add (Literal 0) operand) = Copy operand
    rewrite (Multiply operand (Literal 1)) = Copy operand
    rewrite (Multiply (Literal 1) operand) = Copy operand
    rewrite instruction = instruction

-- | Independent direct execution of validated SSA.
interpretSSA :: (Num scalar) => ScalarSSA scalar -> [scalar] -> Either SSAError scalar
interpretSSA (ScalarSSA inputCount instructions output) inputs
    | fromIntegral (length inputs) /= inputCount = Left (SSAInputLengthMismatch inputCount (fromIntegral (length inputs)))
    | otherwise = do
        registers <- build [] instructions
        resolve registers output
  where
    build values [] = Right values
    build values (instruction : rest) = do
        value <- evaluateInstruction values instruction
        build (values ++ [value]) rest
    evaluateInstruction values instruction = case instruction of
        Copy operand -> resolve values operand
        Negate operand -> negate <$> resolve values operand
        Add left right -> (+) <$> resolve values left <*> resolve values right
        Multiply left right -> (*) <$> resolve values left <*> resolve values right
    resolve values operand = case operand of
        Input index -> lookupNatural index inputs
        Register index -> lookupNatural index values
        Literal value -> Right value
    lookupNatural target = go 0
      where
        go _ [] = Left (SSAInternalReferenceFailure target)
        go current (value : values)
            | current == target = Right value
            | otherwise = go (current + 1) values

-- | Read the validated instruction sequence for exact rewrite evidence.
ssaInstructions :: ScalarSSA scalar -> [Instruction scalar]
ssaInstructions (ScalarSSA _ instructions _) = instructions
