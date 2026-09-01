{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module CircuitPurity where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Markovian.Circuit
import Markovian.Circuit.Rewrite.Deterministic
import Markovian.Circuit.Rewrite.Deterministic.Exact

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

badDeduplicationCertificate ::
    Circuit Primitive 'Stochastic source target ->
    DeterministicRewrite Primitive source (target, target)
badDeduplicationCertificate = deduplicateDeterministicRewrite

badDiracStochasticCertificate ::
    Circuit Primitive 'Stochastic Bool Bool ->
    DeterministicRewrite Primitive Bool (Bool, Bool)
badDiracStochasticCertificate = deduplicateDeterministicRewrite

badEndpointCoerce ::
    Circuit Primitive 'Deterministic Bool Bool ->
    Circuit Primitive 'Deterministic () Bool
badEndpointCoerce = coerce

badCertificateConstructor ::
    Circuit Primitive 'Deterministic source target ->
    DeterministicRewrite Primitive source target
badCertificateConstructor circuit =
    UnsafeDeterministicRewrite RemoveLeftIdentity circuit circuit

badCheckedConstructor ::
    DeterministicRewrite Primitive source target ->
    CheckedDeterministicRewrite Primitive source target
badCheckedConstructor = UnsafeCheckedDeterministicRewrite
