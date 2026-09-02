{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Private constructors for the closed typed source language.
module Markovian.Autodiff.Internal.Syntax (
    Fragment (..),
    Primitive (..),
    Program (..),
    identity,
    compose,
    parallel,
    fanout,
    shareParameters,
    constantScalar,
    constantVector,
    parameter,
    negateScalar,
    addScalar,
    multiplyScalar,
    addVector,
    hadamard,
    dot,
    sumVector,
    tanhScalar,
    tanhVector,
    first,
    second,
    liftPolynomial,
) where

import GHC.TypeLits (KnownSymbol)
import Markovian.Autodiff.Internal.Shape

-- | Closed differentiability fragments.
data Fragment = Polynomial | Smooth

-- | Compiler-owned primitive signature. There is no callback constructor.
data Primitive scalar (fragment :: Fragment) parameters input output where
    ConstantScalar :: SShape input -> !scalar -> Primitive scalar fragment 'NoParameters input 'Scalar
    ConstantVector :: SShape input -> SShape ('Vector n) -> !(Value scalar ('Vector n)) -> Primitive scalar fragment 'NoParameters input ('Vector n)
    Parameter :: String -> SShape input -> SShape shape -> Primitive scalar fragment (Owner owner shape) input shape
    NegateScalar :: Primitive scalar fragment 'NoParameters 'Scalar 'Scalar
    AddScalar :: Primitive scalar fragment 'NoParameters ('Product 'Scalar 'Scalar) 'Scalar
    MultiplyScalar :: Primitive scalar fragment 'NoParameters ('Product 'Scalar 'Scalar) 'Scalar
    AddVector :: SShape ('Vector n) -> Primitive scalar fragment 'NoParameters ('Product ('Vector n) ('Vector n)) ('Vector n)
    Hadamard :: SShape ('Vector n) -> Primitive scalar fragment 'NoParameters ('Product ('Vector n) ('Vector n)) ('Vector n)
    Dot :: SShape ('Vector n) -> Primitive scalar fragment 'NoParameters ('Product ('Vector n) ('Vector n)) 'Scalar
    SumVector :: SShape ('Vector n) -> Primitive scalar fragment 'NoParameters ('Vector n) 'Scalar
    TanhScalar :: Primitive scalar 'Smooth 'NoParameters 'Scalar 'Scalar
    TanhVector :: SShape ('Vector n) -> Primitive scalar 'Smooth 'NoParameters ('Vector n) ('Vector n)
    First :: SShape left -> SShape right -> Primitive scalar fragment 'NoParameters ('Product left right) left
    Second :: SShape left -> SShape right -> Primitive scalar fragment 'NoParameters ('Product left right) right

-- | Finite first-order syntax. Constructors are hidden by the public module.
data Program scalar (fragment :: Fragment) parameters input output where
    PrimitiveNode :: Primitive scalar fragment parameters input output -> Program scalar fragment parameters input output
    IdentityNode :: SShape shape -> Program scalar fragment 'NoParameters shape shape
    ComposeNode ::
        Program scalar fragment leftParameters input middle ->
        Program scalar fragment rightParameters middle output ->
        Program scalar fragment ('ParameterProduct leftParameters rightParameters) input output
    ParallelNode ::
        Program scalar fragment leftParameters leftInput leftOutput ->
        Program scalar fragment rightParameters rightInput rightOutput ->
        Program scalar fragment ('ParameterProduct leftParameters rightParameters) ('Product leftInput rightInput) ('Product leftOutput rightOutput)
    FanoutNode ::
        Program scalar fragment leftParameters input leftOutput ->
        Program scalar fragment rightParameters input rightOutput ->
        Program scalar fragment ('ParameterProduct leftParameters rightParameters) input ('Product leftOutput rightOutput)
    ShareParametersNode ::
        Program scalar fragment parameters leftInput leftOutput ->
        Program scalar fragment parameters rightInput rightOutput ->
        Program scalar fragment parameters ('Product leftInput rightInput) ('Product leftOutput rightOutput)

type role Program representational nominal nominal nominal nominal

-- | Identity with an explicit endpoint shape.
identity :: SShape shape -> Program scalar fragment 'NoParameters shape shape
identity = IdentityNode

-- | Sequential composition with an associated independent parameter product.
compose ::
    Program scalar fragment p input middle ->
    Program scalar fragment q middle output ->
    Program scalar fragment ('ParameterProduct p q) input output
compose = ComposeNode

-- | Independent parallel composition on product inputs and outputs.
parallel ::
    Program scalar fragment p leftInput leftOutput ->
    Program scalar fragment q rightInput rightOutput ->
    Program scalar fragment ('ParameterProduct p q) ('Product leftInput rightInput) ('Product leftOutput rightOutput)
parallel = ParallelNode

-- | Send one input to two branches. Reverse execution adds input cotangents.
fanout ::
    Program scalar fragment p input leftOutput ->
    Program scalar fragment q input rightOutput ->
    Program scalar fragment ('ParameterProduct p q) input ('Product leftOutput rightOutput)
fanout = FanoutNode

-- | Use one parameter tree in two branches. Reverse execution adds parameter cotangents.
shareParameters ::
    Program scalar fragment p leftInput leftOutput ->
    Program scalar fragment p rightInput rightOutput ->
    Program scalar fragment p ('Product leftInput rightInput) ('Product leftOutput rightOutput)
shareParameters = ShareParametersNode

-- | Constant scalar that ignores an input with the given shape.
constantScalar :: SShape input -> scalar -> Program scalar fragment 'NoParameters input 'Scalar
constantScalar inputShape = PrimitiveNode . ConstantScalar inputShape

-- | Constant checked-length vector that ignores its input.
constantVector :: SShape input -> SShape ('Vector n) -> Value scalar ('Vector n) -> Program scalar fragment 'NoParameters input ('Vector n)
constantVector inputShape vectorShape = PrimitiveNode . ConstantVector inputShape vectorShape

-- | Read one statically named parameter and ignore the ordinary input.
parameter :: forall owner shape input scalar fragment. (KnownSymbol owner) => SShape input -> SShape shape -> Program scalar fragment (Owner owner shape) input shape
parameter inputShape shape = PrimitiveNode (Parameter owner inputShape shape)
  where
    SOwner owner _ = ownerParameterShape @owner shape

-- | Scalar negation.
negateScalar :: Program scalar fragment 'NoParameters 'Scalar 'Scalar
negateScalar = PrimitiveNode NegateScalar

-- | Add a pair of scalars.
addScalar :: Program scalar fragment 'NoParameters ('Product 'Scalar 'Scalar) 'Scalar
addScalar = PrimitiveNode AddScalar

-- | Multiply a pair of scalars.
multiplyScalar :: Program scalar fragment 'NoParameters ('Product 'Scalar 'Scalar) 'Scalar
multiplyScalar = PrimitiveNode MultiplyScalar

-- | Add two vectors coordinate by coordinate.
addVector :: SShape ('Vector n) -> Program scalar fragment 'NoParameters ('Product ('Vector n) ('Vector n)) ('Vector n)
addVector = PrimitiveNode . AddVector

-- | Multiply two vectors coordinate by coordinate.
hadamard :: SShape ('Vector n) -> Program scalar fragment 'NoParameters ('Product ('Vector n) ('Vector n)) ('Vector n)
hadamard = PrimitiveNode . Hadamard

-- | Compute a fixed-vector dot product in left-to-right order.
dot :: SShape ('Vector n) -> Program scalar fragment 'NoParameters ('Product ('Vector n) ('Vector n)) 'Scalar
dot = PrimitiveNode . Dot

-- | Sum vector coordinates in left-to-right order.
sumVector :: SShape ('Vector n) -> Program scalar fragment 'NoParameters ('Vector n) 'Scalar
sumVector = PrimitiveNode . SumVector

-- | Checked-Double scalar hyperbolic tangent.
tanhScalar :: Program Double 'Smooth 'NoParameters 'Scalar 'Scalar
tanhScalar = PrimitiveNode TanhScalar

-- | Checked-Double coordinate-wise hyperbolic tangent.
tanhVector :: SShape ('Vector n) -> Program Double 'Smooth 'NoParameters ('Vector n) ('Vector n)
tanhVector = PrimitiveNode . TanhVector

-- | Project the left value from an associated product.
first :: SShape left -> SShape right -> Program scalar fragment 'NoParameters ('Product left right) left
first left = PrimitiveNode . First left

-- | Project the right value from an associated product.
second :: SShape left -> SShape right -> Program scalar fragment 'NoParameters ('Product left right) right
second left = PrimitiveNode . Second left

-- | Embed polynomial syntax in the smooth fragment without changing order.
liftPolynomial :: Program scalar 'Polynomial parameters input output -> Program scalar 'Smooth parameters input output
liftPolynomial (PrimitiveNode primitive) = PrimitiveNode (liftPrimitive primitive)
liftPolynomial (IdentityNode shape) = IdentityNode shape
liftPolynomial (ComposeNode left right) = ComposeNode (liftPolynomial left) (liftPolynomial right)
liftPolynomial (ParallelNode left right) = ParallelNode (liftPolynomial left) (liftPolynomial right)
liftPolynomial (FanoutNode left right) = FanoutNode (liftPolynomial left) (liftPolynomial right)
liftPolynomial (ShareParametersNode left right) = ShareParametersNode (liftPolynomial left) (liftPolynomial right)

liftPrimitive :: Primitive scalar 'Polynomial parameters input output -> Primitive scalar 'Smooth parameters input output
liftPrimitive (ConstantScalar shape value) = ConstantScalar shape value
liftPrimitive (ConstantVector input shape value) = ConstantVector input shape value
liftPrimitive (Parameter owner input shape) = Parameter owner input shape
liftPrimitive NegateScalar = NegateScalar
liftPrimitive AddScalar = AddScalar
liftPrimitive MultiplyScalar = MultiplyScalar
liftPrimitive (AddVector shape) = AddVector shape
liftPrimitive (Hadamard shape) = Hadamard shape
liftPrimitive (Dot shape) = Dot shape
liftPrimitive (SumVector shape) = SumVector shape
liftPrimitive (First left right) = First left right
liftPrimitive (Second left right) = Second left right
