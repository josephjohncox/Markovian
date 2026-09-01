{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Exact bounded checking for deterministic circuit rewrites.

A checked witness is issued only after represented endpoint layouts, the
selected interpreter's exact one-hot matrix denotation, and literal row-major
matrix layout all agree. The witness certifies only that checked exact matrix
interpretation; it is not evidence for other interpreters. Cost comparison is
subsequent accounting and cannot authorize a rewrite.
-}
module Markovian.Circuit.Rewrite.Deterministic.Exact (
    RewriteSide (..),
    RewriteCheckLimits (..),
    DeterministicRewriteCheckError (..),
    CheckedDeterministicRewrite,
    checkDeterministicRewrite,
    checkedRewriteRule,
    checkedRewriteOriginal,
    checkedRewriteResult,
    RewriteCostComparisonError (..),
    RewriteCostReport (..),
    compareCheckedRewriteCosts,
    renderRewriteCostReport,
) where

import Data.Void (Void)
import Markovian.Category.Finite.Set (sameFiniteSetLayout)
import Markovian.Category.Matrix (sameMatrixLayout)
import Markovian.Category.Matrix.Deterministic (
    deterministicEquivalent,
    forgetDeterministic,
 )
import Markovian.Circuit
import Markovian.Circuit.Interpret.Cost
import Markovian.Circuit.Interpret.Exact
import Markovian.Circuit.Rewrite.Deterministic
import Numeric.Natural (Natural)

-- | Original or replacement side of a checked operation.
data RewriteSide = RewriteOriginalSide | RewriteResultSide
    deriving (Eq, Show)

-- | Explicit finite analysis and denotation limits.
data RewriteCheckLimits = RewriteCheckLimits
    { rewriteMaximumSyntaxNodes :: !Natural
    , rewriteMaximumLayoutCardinality :: !Natural
    , rewriteMaximumMatrixCells :: !Natural
    }
    deriving (Eq, Show)

{- | Exact-checking failure. Primitive interpretation errors retain their
original payload under 'ExactCircuitPrimitiveError'.
-}
data DeterministicRewriteCheckError primitiveError
    = RewriteAnalysisError !RewriteSide !(CircuitCostError Void)
    | RewriteSourceLayoutChanged
    | RewriteTargetLayoutChanged
    | RewriteInterpretationError
        !RewriteSide
        !(BoundedCircuitFoldError (ExactCircuitInterpretationError primitiveError))
    | RewriteDeterministicDenotationChanged
    | {- | Redundant representation assertion after endpoint-layout and exact
      denotation checks. It is retained to detect checker representation drift.
      -}
      RewriteMatrixLayoutChanged
    deriving (Eq, Show)

-- | Opaque evidence that one candidate passed the bounded exact checker.
type role CheckedDeterministicRewrite nominal nominal nominal

newtype CheckedDeterministicRewrite primitive source target
    = UnsafeCheckedDeterministicRewrite (DeterministicRewrite primitive source target)

-- | Check endpoints and exact denotation before issuing a witness.
checkDeterministicRewrite ::
    RewriteCheckLimits ->
    ExactPrimitiveInterpreter primitive primitiveError ->
    DeterministicRewrite primitive source target ->
    Either
        (DeterministicRewriteCheckError primitiveError)
        (CheckedDeterministicRewrite primitive source target)
checkDeterministicRewrite limits interpreter candidate = do
    let original = deterministicRewriteOriginal candidate
        result = deterministicRewriteResult candidate
    analyze RewriteOriginalSide original
    analyze RewriteResultSide result
    if sameFiniteSetLayout (circuitSource original) (circuitSource result)
        then Right ()
        else Left RewriteSourceLayoutChanged
    if sameFiniteSetLayout (circuitTarget original) (circuitTarget result)
        then Right ()
        else Left RewriteTargetLayoutChanged
    originalMatrix <- interpret RewriteOriginalSide original
    resultMatrix <- interpret RewriteResultSide result
    if deterministicEquivalent originalMatrix resultMatrix
        then Right ()
        else Left RewriteDeterministicDenotationChanged
    if sameMatrixLayout (forgetDeterministic originalMatrix) (forgetDeterministic resultMatrix)
        then Right (UnsafeCheckedDeterministicRewrite candidate)
        else Left RewriteMatrixLayoutChanged
  where
    analyze side circuit =
        case interpretCircuitCost analysisLimits analysisInterpreter circuit of
            Left problem -> Left (RewriteAnalysisError side problem)
            Right _ -> Right ()
    interpret side circuit =
        case interpretDeterministicCircuitWithNodeLimit (rewriteMaximumSyntaxNodes limits) interpreter circuit of
            Left problem -> Left (RewriteInterpretationError side problem)
            Right matrix -> Right matrix
    analysisLimits =
        CircuitCostLimits
            { maximumSyntaxNodes = rewriteMaximumSyntaxNodes limits
            , maximumDeclaredWork = 0
            , maximumLayoutCardinality = rewriteMaximumLayoutCardinality limits
            , maximumMatrixCells = rewriteMaximumMatrixCells limits
            , maximumOwnerEntries = 1
            }
    analysisInterpreter =
        CircuitCostInterpreter
            { costDeterministicPrimitive = \_ _ _ -> Right (primitiveCharge () 0)
            , costStochasticPrimitive = \_ _ _ -> Right (primitiveCharge () 0)
            , costDeterministicTable = \_ -> Right (primitiveCharge () 0)
            }

-- | Read the checked rule name.
checkedRewriteRule :: CheckedDeterministicRewrite primitive source target -> DeterministicRewriteRule
checkedRewriteRule (UnsafeCheckedDeterministicRewrite candidate) = deterministicRewriteRule candidate

-- | Read the checked original circuit.
checkedRewriteOriginal ::
    CheckedDeterministicRewrite primitive source target ->
    Circuit primitive 'Deterministic source target
checkedRewriteOriginal (UnsafeCheckedDeterministicRewrite candidate) = deterministicRewriteOriginal candidate

-- | Read the checked replacement circuit.
checkedRewriteResult ::
    CheckedDeterministicRewrite primitive source target ->
    Circuit primitive 'Deterministic source target
checkedRewriteResult (UnsafeCheckedDeterministicRewrite candidate) = deterministicRewriteResult candidate

-- | Side-specific cost-analysis failure after semantic checking.
data RewriteCostComparisonError primitiveError
    = RewriteOriginalCostError !(CircuitCostError primitiveError)
    | RewriteResultCostError !(CircuitCostError primitiveError)
    deriving (Eq, Show)

{- | Caller-relative costs for an already checked rewrite. Deltas are original
minus replacement and may be zero or negative.
-}
data RewriteCostReport owner = RewriteCostReport
    { originalRewriteCost :: !(CircuitCostReport owner)
    , resultRewriteCost :: !(CircuitCostReport owner)
    , declaredWorkReduction :: !Integer
    , chargedLeafOccurrenceReduction :: !Integer
    -- ^ Reduction in primitive nodes plus quoted deterministic-table nodes.
    }
    deriving (Eq, Show)

-- | Compare caller-declared costs only after exact semantic checking.
compareCheckedRewriteCosts ::
    (Eq owner) =>
    CircuitCostLimits ->
    CircuitCostInterpreter primitive owner primitiveError ->
    CheckedDeterministicRewrite primitive source target ->
    Either (RewriteCostComparisonError primitiveError) (RewriteCostReport owner)
compareCheckedRewriteCosts limits interpreter (UnsafeCheckedDeterministicRewrite candidate) = do
    original <-
        case interpretCircuitCost limits interpreter (deterministicRewriteOriginal candidate) of
            Left problem -> Left (RewriteOriginalCostError problem)
            Right report -> Right report
    result <-
        case interpretCircuitCost limits interpreter (deterministicRewriteResult candidate) of
            Left problem -> Left (RewriteResultCostError problem)
            Right report -> Right report
    Right
        RewriteCostReport
            { originalRewriteCost = original
            , resultRewriteCost = result
            , declaredWorkReduction = toInteger (declaredPrimitiveWork original) - toInteger (declaredPrimitiveWork result)
            , chargedLeafOccurrenceReduction = toInteger (chargedLeafOccurrences original) - toInteger (chargedLeafOccurrences result)
            }
  where
    chargedLeafOccurrences report =
        deterministicPrimitiveCount report
            + stochasticPrimitiveCount report
            + deterministicTableCount report

-- | Stable deterministic rendering. No timing field is semantic data.
renderRewriteCostReport :: (Show owner) => RewriteCostReport owner -> String
renderRewriteCostReport report =
    unlines
        [ "original:"
        , indent (renderCircuitCostReport (originalRewriteCost report))
        , "replacement:"
        , indent (renderCircuitCostReport (resultRewriteCost report))
        , "declared-work-reduction: " ++ show (declaredWorkReduction report)
        , "charged-leaf-occurrence-reduction: " ++ show (chargedLeafOccurrenceReduction report)
        ]
  where
    indent = unlines . fmap ("  " ++) . lines
