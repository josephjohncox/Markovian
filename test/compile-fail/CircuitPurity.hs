{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module CircuitPurity where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Markovian.Circuit

data Primitive (purity :: Purity) (source :: Type) (target :: Type)

badCopyRewrite ::
    Circuit Primitive 'Stochastic source target ->
    Circuit Primitive 'Stochastic source (target, target)
badCopyRewrite = copyNaturalDeterministic

badStrengthen ::
    Circuit Primitive 'Stochastic source target ->
    Circuit Primitive 'Deterministic source target
badStrengthen = weakenPurity

badPurityCoerce ::
    Circuit Primitive 'Stochastic source target ->
    Circuit Primitive 'Deterministic source target
badPurityCoerce = coerce
