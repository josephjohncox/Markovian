{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Public closed shapes and nominal parameter-owner trees.
module Markovian.Autodiff.Shape (
    Shape (..),
    SShape (..),
    Parameters (..),
    OwnerToken,
    Owner,
    SParameters,
    noParameterShape,
    ownerParameterShape,
    productParameterShape,
    Value,
    unitValue,
    scalarValue,
    vectorValue,
    productValue,
    scalarFromValue,
    vectorFromValue,
    splitProductValue,
    valueScalars,
    ParameterValue,
    noParameters,
    ownedParameters,
    parameterProduct,
    ownedParameterValue,
    splitParameterProduct,
    parameterScalars,
    ValueError (..),
) where

import Markovian.Autodiff.Internal.Shape
