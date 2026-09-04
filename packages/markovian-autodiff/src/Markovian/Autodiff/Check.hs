{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{- | Independent bounded primal/JVP reference for the exact polynomial syntax.

This interpreter is syntax-recursive. It does not import
"Markovian.Autodiff.Compile" or call compiler backend primitives. It exists for
cross-checking reverse rules; it is not a second lowering backend.
-}
module Markovian.Autodiff.Check (
    interpretExactPolynomialJVP,
    checkExactSSAIdentities,
) where

import Control.Monad (when)
import Markovian.Autodiff.Internal.SSA
import Markovian.Autodiff.Internal.Shape
import Markovian.Autodiff.Internal.Syntax

-- | Evaluate one exact primal and one directional derivative.
interpretExactPolynomialJVP ::
    Program Rational 'Polynomial parameters input output ->
    ParameterValue Rational parameters ->
    ParameterValue Rational parameters ->
    Value Rational input ->
    Value Rational input ->
    Either String (Value Rational output, Value Rational output)
interpretExactPolynomialJVP = go
  where
    go ::
        Program Rational 'Polynomial p x y ->
        ParameterValue Rational p ->
        ParameterValue Rational p ->
        Value Rational x ->
        Value Rational x ->
        Either String (Value Rational y, Value Rational y)
    go (PrimitiveNode primitive) p dp x dx = primitiveJVP primitive p dp x dx
    go (IdentityNode _) NoParameterValue NoParameterValue x dx = Right (x, dx)
    go (ComposeNode left right) p dp x dx = do
        let (leftP, rightP) = splitParameterProduct p
            (leftDP, rightDP) = splitParameterProduct dp
        (middle, dMiddle) <- go left leftP leftDP x dx
        go right rightP rightDP middle dMiddle
    go (ParallelNode left right) p dp x dx = do
        let (leftP, rightP) = splitParameterProduct p
            (leftDP, rightDP) = splitParameterProduct dp
            (leftX, rightX) = splitProductValue x
            (leftDX, rightDX) = splitProductValue dx
        (leftY, leftDY) <- go left leftP leftDP leftX leftDX
        (rightY, rightDY) <- go right rightP rightDP rightX rightDX
        Right (ProductValue leftY rightY, ProductValue leftDY rightDY)
    go (FanoutNode left right) p dp x dx = do
        let (leftP, rightP) = splitParameterProduct p
            (leftDP, rightDP) = splitParameterProduct dp
        (leftY, leftDY) <- go left leftP leftDP x dx
        (rightY, rightDY) <- go right rightP rightDP x dx
        Right (ProductValue leftY rightY, ProductValue leftDY rightDY)
    go (ShareParametersNode left right) p dp x dx = do
        let (leftX, rightX) = splitProductValue x
            (leftDX, rightDX) = splitProductValue dx
        (leftY, leftDY) <- go left p dp leftX leftDX
        (rightY, rightDY) <- go right p dp rightX rightDX
        Right (ProductValue leftY rightY, ProductValue leftDY rightDY)

primitiveJVP ::
    Primitive Rational 'Polynomial parameters input output ->
    ParameterValue Rational parameters ->
    ParameterValue Rational parameters ->
    Value Rational input ->
    Value Rational input ->
    Either String (Value Rational output, Value Rational output)
primitiveJVP primitive parameters parameterDirection input inputDirection = case primitive of
    ConstantScalar _ value -> Right (ScalarValue value, ScalarValue 0)
    ConstantVector _ _ value -> Right (value, VectorValue (replicate (length (vectorFromValue value)) 0))
    Parameter{} -> Right (ownedParameterValue parameters, ownedParameterValue parameterDirection)
    NegateScalar -> Right (ScalarValue (negate (scalarFromValue input)), ScalarValue (negate (scalarFromValue inputDirection)))
    AddScalar ->
        let (left, right) = scalarPair input
            (dLeft, dRight) = scalarPair inputDirection
         in Right (ScalarValue (left + right), ScalarValue (dLeft + dRight))
    MultiplyScalar ->
        let (left, right) = scalarPair input
            (dLeft, dRight) = scalarPair inputDirection
         in Right (ScalarValue (left * right), ScalarValue (dLeft * right + left * dRight))
    AddVector _ ->
        let (left, right) = vectorPair input
            (dLeft, dRight) = vectorPair inputDirection
         in Right (VectorValue (zipWith (+) left right), VectorValue (zipWith (+) dLeft dRight))
    Hadamard _ ->
        let (left, right) = vectorPair input
            (dLeft, dRight) = vectorPair inputDirection
         in Right (VectorValue (zipWith (*) left right), VectorValue (zipWith (+) (zipWith (*) dLeft right) (zipWith (*) left dRight)))
    Dot _ ->
        let (left, right) = vectorPair input
            (dLeft, dRight) = vectorPair inputDirection
         in Right (ScalarValue (sum (zipWith (*) left right)), ScalarValue (sum (zipWith (*) dLeft right) + sum (zipWith (*) left dRight)))
    SumVector _ -> Right (ScalarValue (sum (vectorFromValue input)), ScalarValue (sum (vectorFromValue inputDirection)))
    First _ _ ->
        let (left, _) = splitProductValue input
            (dLeft, _) = splitProductValue inputDirection
         in Right (left, dLeft)
    Second _ _ ->
        let (_, right) = splitProductValue input
            (_, dRight) = splitProductValue inputDirection
         in Right (right, dRight)
    ProjectValue projection -> Right (followProjection projection input, followProjection projection inputDirection)

{- | Check exact identity rewrites, semantics, and the one-below SSA limit.

This executable evidence stays behind a result value so private SSA
constructors do not become release API.
-}
checkExactSSAIdentities :: Either String ()
checkExactSSAIdentities = do
    ssa <- mapProblem (scalarSSA 2 1 [Add (Input 0) (Literal 0), Multiply (Register 0) (Literal 1)] (Register 1))
    let rewritten = rewriteExactSSA ssa
    when (ssaInstructions rewritten /= [Copy (Input 0), Copy (Register 0)]) $
        Left "exact SSA identity rewrite changed"
    value <- mapProblem (interpretSSA rewritten [7])
    when (value /= (7 :: Rational)) $
        Left "exact SSA rewrite changed semantics"
    case scalarSSA 1 1 [Copy (Input 0), Copy (Register 0)] (Register 1 :: Operand Rational) of
        Left (SSAInstructionLimitExceeded 1) -> Right ()
        other -> Left ("exact SSA one-below result changed: " ++ show other)
  where
    mapProblem result = case result of
        Left problem -> Left (show problem)
        Right value -> Right value

followProjection :: Projection environment selected -> Value scalar environment -> Value scalar selected
followProjection projection value = case projection of
    ProjectionHere _ -> value
    ProjectionLeft inner _ -> let (left, _) = splitProductValue value in followProjection inner left
    ProjectionRight _ inner -> let (_, right) = splitProductValue value in followProjection inner right

scalarPair :: Value Rational ('Product 'Scalar 'Scalar) -> (Rational, Rational)
scalarPair value = let (left, right) = splitProductValue value in (scalarFromValue left, scalarFromValue right)

vectorPair :: Value Rational ('Product ('Vector n) ('Vector n)) -> ([Rational], [Rational])
vectorPair value = let (left, right) = splitProductValue value in (vectorFromValue left, vectorFromValue right)
