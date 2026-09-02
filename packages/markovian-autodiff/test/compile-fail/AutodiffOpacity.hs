{-# LANGUAGE DataKinds #-}

module AutodiffOpacity where

import Markovian.Autodiff.Compile (ExactTape (..))
import Markovian.Autodiff.Language (Program (..))
import Markovian.Autodiff.Shape (Value (..))

badProgramConstructor = PrimitiveNode
badTapeConstructor = ExactTape
badValueConstructor = ScalarValue
