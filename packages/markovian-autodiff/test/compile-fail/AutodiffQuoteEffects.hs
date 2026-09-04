{-# LANGUAGE DataKinds #-}

module AutodiffQuoteEffects where

import Markovian.Autodiff

badHaskellFunction :: Quote Rational 'Polynomial (RootEnvironment 'Scalar) 'NoParameters 'Scalar
badHaskellFunction = quoteProgram (+ 1)

badEffect :: Quote Rational 'Polynomial (RootEnvironment 'Scalar) 'NoParameters 'Scalar
badEffect = quoteProgram (pure (scalarValue 1) :: IO (Value Rational 'Scalar))
