{- | Finite owned syntax for interpreting caller-supplied primitive VJPs.

This module is the pure, 'Either'-based interpreter. It does not differentiate
arbitrary Haskell and does not implement loops, recursion, stochastic
differentiation, tensors, or checkpoint scheduling. Every program is a finite
acyclic tree. Preparation checks structural ownership, declared primal and
cotangent layouts, and caller-supplied limits before evaluation.
-}
module Markovian.Reverse.Program (
    FinitePrimalSpace,
    finitePrimalSpace,
    primalFiniteLayout,
    primalEqualityMode,
    validatePrimal,
    primalsEquivalent,
    samePrimalLayout,
    ParameterOwnership,
    noParameterOwnership,
    parameterOwner,
    parameterOwnershipProduct,
    parameterOwnershipDescription,
    parameterOwnerKeys,
    PrimitiveTapePolicy (..),
    PrimitiveRecomputation,
    primitiveRecomputation,
    OwnedReversePrimitive,
    ownedReversePrimitive,
    ownedReversePrimitiveWithRecomputation,
    ReversePrimitiveResolver,
    ReverseProgram,
    primitiveProgram,
    identityProgram,
    composeProgram,
    tensorProgram,
    pairInputProgram,
    shareParameterProgram,
    ReverseLimits,
    reverseLimits,
    reverseLimitsWithStructure,
    ReverseDefinitionError (..),
    ReversePathStep (..),
    ReverseStage (..),
    ReverseProgramError (..),
    PreparedReverseProgram,
    prepareReverseProgram,
    ReverseProgramReport,
    preparedReverseProgramReport,
    renderReverseProgramReport,
    ReverseRun,
    reverseRunOutput,
    reverseRunTape,
    ReverseTape,
    ReverseTapeReport (..),
    reverseTapeReport,
    runPreparedReverse,
    applyReverseTape,
) where

import Data.Functor.Identity qualified as Identity
import Markovian.Reverse.Program.Effect qualified as Effect
import Markovian.Reverse.Program.Internal hiding (
    ReverseRun,
    ReverseTape,
    applyReverseTape,
    reverseRunOutput,
    reverseRunTape,
    reverseTapeReport,
    runPreparedReverse,
 )

-- The public pure run and tape are the Identity specialization of the effect
-- execution core. Preparation remains the single pure structural checker.

-- | Completed pure forward execution and its closed reverse tape.
type ReverseRun error scalar p pc x xc y yc = Effect.EffectReverseRun Identity.Identity error scalar p pc x xc y yc

-- | Closed pure tape captured by one completed forward execution.
type ReverseTape error scalar p pc x xc y yc = Effect.EffectReverseTape Identity.Identity error scalar p pc x xc y yc

-- | Read the primal output without applying the tape.
reverseRunOutput :: ReverseRun error scalar p pc x xc y yc -> y
reverseRunOutput = Effect.effectReverseRunOutput

-- | Read the closed tape retained by a completed run.
reverseRunTape :: ReverseRun error scalar p pc x xc y yc -> ReverseTape error scalar p pc x xc y yc
reverseRunTape = Effect.effectReverseRunTape

-- | Read deterministic structure and resource accounting for a pure tape.
reverseTapeReport :: ReverseTape error scalar p pc x xc y yc -> ReverseTapeReport
reverseTapeReport = Effect.effectReverseTapeReport

-- | Execute one prepared finite program through the effect core specialized to @Identity@.
runPreparedReverse ::
    PreparedReverseProgram primitive error scalar p pc x xc y yc ->
    p ->
    x ->
    Either (ReverseProgramError error) (ReverseRun error scalar p pc x xc y yc)
runPreparedReverse prepared parameter input =
    case Identity.runIdentity (Effect.runPreparedReverseM (Effect.specializeIdentityProgram prepared) parameter input) of
        Left problem -> Left (Effect.effectReverseFailure problem)
        Right result -> Right result

-- | Apply a closed pure tape to one validated output cotangent.
applyReverseTape ::
    ReverseTape error scalar p pc x xc y yc ->
    yc ->
    Either (ReverseProgramError error) (pc, xc)
applyReverseTape tape outputCotangent =
    case Identity.runIdentity (Effect.applyReverseTapeM tape outputCotangent) of
        Left problem -> Left (Effect.effectReverseFailure problem)
        Right (parameterCotangent, inputCotangent, _) -> Right (parameterCotangent, inputCotangent)
