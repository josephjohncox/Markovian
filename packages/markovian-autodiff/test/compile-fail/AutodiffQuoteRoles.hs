{-# LANGUAGE DataKinds #-}

module AutodiffQuoteRoles where

import Data.Coerce (coerce)
import Markovian.Autodiff

badPathEnvironmentRole :: Path (RootEnvironment 'Scalar) 'Scalar -> Path (RootEnvironment ('Vector 1)) 'Scalar
badPathEnvironmentRole = coerce

badPathSelectedRole :: Path (RootEnvironment 'Scalar) 'Scalar -> Path (RootEnvironment 'Scalar) ('Vector 1)
badPathSelectedRole = coerce

badQuoteEnvironmentRole ::
    Quote Rational 'Polynomial (RootEnvironment 'Scalar) 'NoParameters 'Scalar ->
    Quote Rational 'Polynomial (RootEnvironment ('Vector 1)) 'NoParameters 'Scalar
badQuoteEnvironmentRole = coerce

badQuoteParameterRole ::
    Quote Rational 'Polynomial (RootEnvironment 'Scalar) 'NoParameters 'Scalar ->
    Quote Rational 'Polynomial (RootEnvironment 'Scalar) (Owner "forged" 'Scalar) 'Scalar
badQuoteParameterRole = coerce
