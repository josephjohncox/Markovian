{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{- | Closed finite shapes and parameter-owner trees for autodiff programs.

'Value' constructors are private.  In particular, a scalar is not
representationally interchangeable with a one-element vector, and a parameter
owner cannot be changed with @coerce@.
-}
module Markovian.Autodiff.Internal.Shape (
    Shape (..),
    SShape (..),
    Parameters (..),
    OwnerToken,
    Owner,
    SParameters (..),
    noParameterShape,
    ownerParameterShape,
    productParameterShape,
    Value (..),
    unitValue,
    scalarValue,
    vectorValue,
    productValue,
    scalarFromValue,
    vectorFromValue,
    splitProductValue,
    valueScalars,
    ParameterValue (..),
    noParameters,
    ownedParameters,
    parameterProduct,
    ownedParameterValue,
    splitParameterProduct,
    parameterScalars,
    ValueError (..),
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import Numeric.Natural (Natural)

-- | Closed source-language shapes.
data Shape
    = Unit
    | Scalar
    | Vector Nat
    | Product Shape Shape

-- | Singleton witness for one shape. Products retain their association.
data SShape (shape :: Shape) where
    SUnit :: SShape 'Unit
    SScalar :: SShape 'Scalar
    SVector :: (KnownNat n) => SShape ('Vector n)
    SProduct :: SShape left -> SShape right -> SShape ('Product left right)

type role SShape nominal

-- | Injective nominal seal used by 'Owner'. It has no public equations.
type family OwnerToken (owner :: Symbol) = (token :: Type) | token -> owner

-- | Closed parameter tree. Every non-unit leaf has a nominal owner token.
data Parameters
    = NoParameters
    | ParameterOwner Type Shape
    | ParameterProduct Parameters Parameters

-- | A parameter leaf with a nominal type-level owner.
type Owner owner shape = 'ParameterOwner (OwnerToken owner) shape

-- | Runtime witness for a parameter tree. Constructors stay private.
data SParameters (parameters :: Parameters) where
    SNoParameters :: SParameters 'NoParameters
    SOwner :: String -> SShape shape -> SParameters (Owner owner shape)
    SParameterProduct :: SParameters left -> SParameters right -> SParameters ('ParameterProduct left right)

type role SParameters nominal

-- | Witness for no parameters.
noParameterShape :: SParameters 'NoParameters
noParameterShape = SNoParameters

-- | Witness for one statically named owner.
ownerParameterShape :: forall owner shape. (KnownSymbol owner) => SShape shape -> SParameters (Owner owner shape)
ownerParameterShape = SOwner (symbolVal (Proxy @owner))

-- | Ordered parameter product witness.
productParameterShape :: SParameters left -> SParameters right -> SParameters ('ParameterProduct left right)
productParameterShape = SParameterProduct

-- | A finite value with a statically known shape.
data Value scalar (shape :: Shape) where
    UnitValue :: Value scalar 'Unit
    ScalarValue :: !scalar -> Value scalar 'Scalar
    VectorValue :: ![scalar] -> Value scalar ('Vector n)
    ProductValue :: !(Value scalar left) -> !(Value scalar right) -> Value scalar ('Product left right)

type role Value representational nominal

instance (Eq scalar) => Eq (Value scalar shape) where
    UnitValue == UnitValue = True
    ScalarValue left == ScalarValue right = left == right
    VectorValue left == VectorValue right = left == right
    ProductValue leftA rightA == ProductValue leftB rightB = leftA == leftB && rightA == rightB

instance (Show scalar) => Show (Value scalar shape) where
    showsPrec _ UnitValue = showString "unitValue"
    showsPrec precedence (ScalarValue value) = showParen (precedence > 10) (showString "scalarValue " . showsPrec 11 value)
    showsPrec precedence (VectorValue values) = showParen (precedence > 10) (showString "vectorValue " . shows values)
    showsPrec precedence (ProductValue left right) =
        showParen (precedence > 10) (showString "productValue " . showsPrec 11 left . showChar ' ' . showsPrec 11 right)

-- | Unit value.
unitValue :: Value scalar 'Unit
unitValue = UnitValue

-- | Scalar value. Checked-Double execution rejects nonfinite values.
scalarValue :: scalar -> Value scalar 'Scalar
scalarValue = ScalarValue

-- | Value-construction failures.
data ValueError
    = VectorDimensionExceedsLimit !Natural !Natural
    | VectorTooShort !Natural !Natural
    | VectorTooLong !Natural
    deriving (Eq, Show)

{- | Construct a vector without an unrestricted length pass.

The dimension is checked before the list is consumed, and at most @n + 1@
list cells are inspected.
-}
vectorValue :: forall n scalar. (KnownNat n) => Natural -> [scalar] -> Either ValueError (Value scalar ('Vector n))
vectorValue limit values
    | dimension > limit = Left (VectorDimensionExceedsLimit limit dimension)
    | otherwise = VectorValue <$> consume 0 dimension values
  where
    dimension = fromInteger (natVal (Proxy @n))
    consume :: Natural -> Natural -> [scalar] -> Either ValueError [scalar]
    consume _ 0 [] = Right []
    consume _ 0 (_ : _) = Left (VectorTooLong dimension)
    consume seen _ [] = Left (VectorTooShort dimension seen)
    consume seen remaining (value : rest) = (value :) <$> consume (seen + 1) (remaining - 1) rest

-- | Ordered product value.
productValue :: Value scalar left -> Value scalar right -> Value scalar ('Product left right)
productValue = ProductValue

-- | Read a scalar coordinate.
scalarFromValue :: Value scalar 'Scalar -> scalar
scalarFromValue (ScalarValue value) = value

-- | Read vector coordinates. The returned list has the statically declared length.
vectorFromValue :: Value scalar ('Vector n) -> [scalar]
vectorFromValue (VectorValue values) = values

-- | Split an ordered product.
splitProductValue :: Value scalar ('Product left right) -> (Value scalar left, Value scalar right)
splitProductValue (ProductValue left right) = (left, right)

-- | Flatten a value in deterministic left-to-right coordinate order.
valueScalars :: Value scalar shape -> [scalar]
valueScalars UnitValue = []
valueScalars (ScalarValue value) = [value]
valueScalars (VectorValue values) = values
valueScalars (ProductValue left right) = valueScalars left ++ valueScalars right

-- | Runtime parameter values matching an owner tree.
data ParameterValue scalar (parameters :: Parameters) where
    NoParameterValue :: ParameterValue scalar 'NoParameters
    OwnedParameterValue :: !(Value scalar shape) -> ParameterValue scalar (Owner owner shape)
    ParameterProductValue ::
        !(ParameterValue scalar left) ->
        !(ParameterValue scalar right) ->
        ParameterValue scalar ('ParameterProduct left right)

type role ParameterValue representational nominal

instance (Eq scalar) => Eq (ParameterValue scalar parameters) where
    NoParameterValue == NoParameterValue = True
    OwnedParameterValue left == OwnedParameterValue right = left == right
    ParameterProductValue leftA rightA == ParameterProductValue leftB rightB = leftA == leftB && rightA == rightB

instance (Show scalar) => Show (ParameterValue scalar parameters) where
    showsPrec _ NoParameterValue = showString "noParameters"
    showsPrec precedence (OwnedParameterValue value) = showParen (precedence > 10) (showString "ownedParameters " . showsPrec 11 value)
    showsPrec precedence (ParameterProductValue left right) =
        showParen (precedence > 10) (showString "parameterProduct " . showsPrec 11 left . showChar ' ' . showsPrec 11 right)

-- | Runtime unit parameter.
noParameters :: ParameterValue scalar 'NoParameters
noParameters = NoParameterValue

-- | Runtime value for one statically named owner.
ownedParameters :: Value scalar shape -> ParameterValue scalar (Owner owner shape)
ownedParameters = OwnedParameterValue

-- | Ordered runtime parameter product.
parameterProduct :: ParameterValue scalar left -> ParameterValue scalar right -> ParameterValue scalar ('ParameterProduct left right)
parameterProduct = ParameterProductValue

-- | Read the value held by one owner leaf.
ownedParameterValue :: ParameterValue scalar (Owner owner shape) -> Value scalar shape
ownedParameterValue (OwnedParameterValue value) = value

-- | Split an ordered parameter product.
splitParameterProduct ::
    ParameterValue scalar ('ParameterProduct left right) ->
    (ParameterValue scalar left, ParameterValue scalar right)
splitParameterProduct (ParameterProductValue left right) = (left, right)

-- | Flatten parameters in owner-tree and coordinate order.
parameterScalars :: ParameterValue scalar parameters -> [scalar]
parameterScalars NoParameterValue = []
parameterScalars (OwnedParameterValue value) = valueScalars value
parameterScalars (ParameterProductValue left right) = parameterScalars left ++ parameterScalars right
