{-# LANGUAGE DataKinds #-}

module AutodiffOwnerCoercion where

import Data.Coerce (coerce)
import Markovian.Autodiff.Shape

badOwnerCoercion ::
    ParameterValue Rational (Owner "left" 'Scalar) ->
    ParameterValue Rational (Owner "right" 'Scalar)
badOwnerCoercion = coerce
