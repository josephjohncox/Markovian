{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Opaque deterministic circuit-rewrite candidates.

Construction records a proposed equation. It does not establish denotational
soundness; use the exact checker in
"Markovian.Circuit.Rewrite.Deterministic.Exact" before treating a candidate as
semantic evidence.
-}
module Markovian.Circuit.Rewrite.Deterministic (
    DeterministicRewriteRule (..),
    DeterministicRewrite,
    leftIdentityRewrite,
    rightIdentityRewrite,
    reassociateCompositionRewrite,
    deduplicateDeterministicRewrite,
    deterministicRewriteRule,
    deterministicRewriteOriginal,
    deterministicRewriteResult,
) where

import Markovian.Circuit

-- | The four supported rewrite schemas.
data DeterministicRewriteRule
    = RemoveLeftIdentity
    | RemoveRightIdentity
    | ReassociateComposition
    | DeduplicateDeterministicFanout
    deriving (Eq, Show)

-- | Opaque proposed rewrite with fixed Haskell endpoints.
type role DeterministicRewrite nominal nominal nominal

data DeterministicRewrite primitive source target
    = UnsafeDeterministicRewrite
        !DeterministicRewriteRule
        !(Circuit primitive 'Deterministic source target)
        !(Circuit primitive 'Deterministic source target)

-- | Propose removal of an identity before a deterministic circuit.
leftIdentityRewrite ::
    Circuit primitive 'Deterministic source target ->
    Either CircuitConstructionError (DeterministicRewrite primitive source target)
leftIdentityRewrite circuit = do
    original <- composeCircuit (identityCircuit (circuitSource circuit)) circuit
    Right (UnsafeDeterministicRewrite RemoveLeftIdentity original circuit)

-- | Propose removal of an identity after a deterministic circuit.
rightIdentityRewrite ::
    Circuit primitive 'Deterministic source target ->
    Either CircuitConstructionError (DeterministicRewrite primitive source target)
rightIdentityRewrite circuit = do
    original <- composeCircuit circuit (identityCircuit (circuitTarget circuit))
    Right (UnsafeDeterministicRewrite RemoveRightIdentity original circuit)

-- | Propose changing only the parentheses of three sequential circuits.
reassociateCompositionRewrite ::
    Circuit primitive 'Deterministic source firstMiddle ->
    Circuit primitive 'Deterministic firstMiddle secondMiddle ->
    Circuit primitive 'Deterministic secondMiddle target ->
    Either CircuitConstructionError (DeterministicRewrite primitive source target)
reassociateCompositionRewrite first second third = do
    firstSecond <- composeCircuit first second
    original <- composeCircuit firstSecond third
    secondThird <- composeCircuit second third
    result <- composeCircuit first secondThird
    Right (UnsafeDeterministicRewrite ReassociateComposition original result)

{- | Propose replacing two executions of deterministic syntax after input copy
with one execution followed by output copy. There is intentionally no
stochastic constructor.
-}
deduplicateDeterministicRewrite ::
    Circuit primitive 'Deterministic source target ->
    DeterministicRewrite primitive source (target, target)
deduplicateDeterministicRewrite circuit =
    UnsafeDeterministicRewrite
        DeduplicateDeterministicFanout
        (copyNaturalDeterministic circuit)
        (shareCircuit circuit)

-- | Read the proposed rule name.
deterministicRewriteRule :: DeterministicRewrite primitive source target -> DeterministicRewriteRule
deterministicRewriteRule (UnsafeDeterministicRewrite rule _ _) = rule

-- | Read the original circuit. This does not itself certify the rewrite.
deterministicRewriteOriginal ::
    DeterministicRewrite primitive source target ->
    Circuit primitive 'Deterministic source target
deterministicRewriteOriginal (UnsafeDeterministicRewrite _ original _) = original

-- | Read the proposed replacement. This does not itself certify the rewrite.
deterministicRewriteResult ::
    DeterministicRewrite primitive source target ->
    Circuit primitive 'Deterministic source target
deterministicRewriteResult (UnsafeDeterministicRewrite _ _ result) = result
