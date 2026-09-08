{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Bounded explicit quotation for the closed exact polynomial language.

A 'Quote' is first-order syntax.  It contains no Haskell callback.  A 'Path'
selects a value from the associated product that is the current quotation
environment.  Path and quotation constructors are private, and both indexes
have nominal roles.
-}
module Markovian.Autodiff.Quote (
    Environment,
    RootEnvironment,
    BindEnvironment,
    EnvironmentShape,
    QuoteEnvironment,
    rootEnvironment,
    extendEnvironment,
    QuoteScope,
    withQuoteScope,
    Path,
    pathHere,
    pathLeft,
    pathRight,
    Quote,
    quoteProgram,
    quoteProgramAt,
    project,
    composeQuote,
    fanoutQuote,
    letQuote,
    QuotationLimits,
    quotationLimits,
    QuoteReport,
    quoteNodeCount,
    quoteSourceDepth,
    quoteMaximumPathDepth,
    quotePredictedTargetNodes,
    quotePredictedTargetDepth,
    quoteTransformedNodes,
    quoteMaximumCoordinateExtent,
    quoteAllocationCount,
    quoteRuntimeWork,
    quoteTraversalWork,
    quoteTotalWork,
    quoteMaximumRationalBits,
    QuoteCompilationReport,
    quoteCompilationPreflight,
    quoteCompilationTarget,
    QuoteError (..),
    QuoteCompileError (..),
    preflightQuote,
    preflightExactQuoteExecution,
    preflightExactQuoteJVPExecution,
    lowerQuote,
    compileExactQuote,
    interpretExactQuote,
    interpretExactQuoteJVP,
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import GHC.TypeLits (natVal)
import Markovian.Autodiff.Compile (
    CompileError,
    CompileReport,
    CompilerLimits,
    ExactExecutable,
    TapePolicy,
    compileExactPolynomial,
    compilerLimits,
    exactCompileReport,
 )
import Markovian.Autodiff.Internal.Shape
import Markovian.Autodiff.Internal.Syntax
import Numeric.Natural (Natural)

-- Object-like prefixes disappear completely in normal builds. Defaults precede
-- private overrides so the unpreprocessed source is also formatter-readable.
#define D080_BUILDER_CALL
#define D080_COMPILER_CALL
#define D080_PLAN_UNIT
#define D080_PLAN_SCALAR
#define D080_PLAN_VECTOR
#define D080_PLAN_PRODUCT
#define D080_RESERVE_TRAVERSAL
#define D080_RESERVE_ALLOCATION
#define D080_RESERVE_RUNTIME
#define D080_ADMISSION_WITNESS
#define D080_SYNTAX_START
#define D080_SYNTAX_SUCCESS
#define D080_PLANNER_START
#define D080_PLANNER_SUCCESS
#define D080_BUILD_QUOTE
#define D080_GENERATED_PROGRAM
#define D080_BUILD_PATH
#define D080_GENERATED_PROJECTION_PRIMITIVE
#define D080_PATH_PROJECTION
#define D080_PROJECTION_WITNESS
#define D080_ENVIRONMENT_WITNESS
#define D080_SHAPE_WITNESS
#define D080_LET_BOUND_START
#define D080_LET_BOUND_COMPLETE
#define D080_LET_IDENTITY_COMPLETE
#define D080_LET_FANOUT_COMPLETE
#define D080_LET_BODY_START
#define D080_LET_BODY_COMPLETE
#ifdef D080_PRIVATE_PROBE
import D080Probe (probeEvent)
#undef D080_BUILDER_CALL
#define D080_BUILDER_CALL probeEvent "builder-call" (pure ())
#undef D080_COMPILER_CALL
#define D080_COMPILER_CALL probeEvent "compiler-call" (pure ())
#undef D080_PLAN_UNIT
#define D080_PLAN_UNIT probeEvent "plan-shape/unit" $
#undef D080_PLAN_SCALAR
#define D080_PLAN_SCALAR probeEvent "plan-shape/scalar" $
#undef D080_PLAN_VECTOR
#define D080_PLAN_VECTOR probeEvent "plan-shape/vector" $
#undef D080_PLAN_PRODUCT
#define D080_PLAN_PRODUCT probeEvent "plan-shape/product" (pure ())
#undef D080_RESERVE_TRAVERSAL
#define D080_RESERVE_TRAVERSAL probeEvent "reserve-traversal" (pure ())
#undef D080_RESERVE_ALLOCATION
#define D080_RESERVE_ALLOCATION probeEvent "reserve-allocation" (pure ())
#undef D080_RESERVE_RUNTIME
#define D080_RESERVE_RUNTIME probeEvent "reserve-runtime" (pure ())
#undef D080_ADMISSION_WITNESS
#define D080_ADMISSION_WITNESS probeEvent "admission-witness" $
#undef D080_SYNTAX_START
#define D080_SYNTAX_START probeEvent "syntax-start" (pure ())
#undef D080_SYNTAX_SUCCESS
#define D080_SYNTAX_SUCCESS probeEvent "syntax-success" (pure ())
#undef D080_PLANNER_START
#define D080_PLANNER_START probeEvent "planner-start" (pure ())
#undef D080_PLANNER_SUCCESS
#define D080_PLANNER_SUCCESS probeEvent "planner-success" (pure ())
#undef D080_BUILD_QUOTE
#define D080_BUILD_QUOTE probeEvent "buildQuote" $
#undef D080_GENERATED_PROGRAM
#define D080_GENERATED_PROGRAM probeEvent "generated-program" $
#undef D080_BUILD_PATH
#define D080_BUILD_PATH probeEvent "buildPath" $
#undef D080_GENERATED_PROJECTION_PRIMITIVE
#define D080_GENERATED_PROJECTION_PRIMITIVE probeEvent "generated-projection-primitive" $
#undef D080_PATH_PROJECTION
#define D080_PATH_PROJECTION probeEvent "pathProjection" $
#undef D080_PROJECTION_WITNESS
#define D080_PROJECTION_WITNESS probeEvent "projection-witness" $
#undef D080_ENVIRONMENT_WITNESS
#define D080_ENVIRONMENT_WITNESS probeEvent "environment-witness" $
#undef D080_SHAPE_WITNESS
#define D080_SHAPE_WITNESS probeEvent "shape-witness" $
#undef D080_LET_BOUND_START
#define D080_LET_BOUND_START probeEvent "let-bound-start" (pure ())
#undef D080_LET_BOUND_COMPLETE
#define D080_LET_BOUND_COMPLETE probeEvent "let-bound-complete" (pure ())
#undef D080_LET_IDENTITY_COMPLETE
#define D080_LET_IDENTITY_COMPLETE probeEvent "let-identity-complete" (pure ())
#undef D080_LET_FANOUT_COMPLETE
#define D080_LET_FANOUT_COMPLETE probeEvent "let-fanout-complete" (pure ())
#undef D080_LET_BODY_START
#define D080_LET_BODY_START probeEvent "let-body-start" (pure ())
#undef D080_LET_BODY_COMPLETE
#define D080_LET_BODY_COMPLETE probeEvent "let-body-complete" (pure ())
#endif

-- | Type-level lexical environments. Constructors are not exported.
data Environment
    = RootEnvironmentConstructor Shape
    | BindEnvironmentConstructor Type Environment Shape

-- | A top-level quotation environment.
type RootEnvironment shape = 'RootEnvironmentConstructor shape

-- | Extend an environment with one lexically scoped binding.
type BindEnvironment scope environment bound = 'BindEnvironmentConstructor scope environment bound

-- | Runtime shape represented by a lexical environment.
type family EnvironmentShape (environment :: Environment) :: Shape where
    EnvironmentShape ('RootEnvironmentConstructor shape) = shape
    EnvironmentShape ('BindEnvironmentConstructor scope environment bound) = 'Product (EnvironmentShape environment) bound

-- | Opaque witness for one lexical environment.
data QuoteEnvironment (environment :: Environment) where
    RootEnvironmentWitness :: SShape shape -> QuoteEnvironment (RootEnvironment shape)
    BindEnvironmentWitness :: QuoteEnvironment environment -> SShape bound -> QuoteEnvironment (BindEnvironment scope environment bound)

type role QuoteEnvironment nominal

-- | Construct a top-level environment witness.
rootEnvironment :: SShape shape -> QuoteEnvironment (RootEnvironment shape)
rootEnvironment = RootEnvironmentWitness

-- | Extend an environment witness with one fresh scope and bound shape.
extendEnvironment :: QuoteScope scope -> QuoteEnvironment environment -> SShape bound -> QuoteEnvironment (BindEnvironment scope environment bound)
extendEnvironment _ = BindEnvironmentWitness

-- | Opaque lexical scope token.
data QuoteScope (scope :: Type) = QuoteScope

type role QuoteScope nominal

{- | Introduce one generative lexical scope. The continuation constructs syntax,
but no continuation or Haskell function is stored in a 'Quote'.
-}
withQuoteScope :: (forall scope. QuoteScope scope -> result) -> result
withQuoteScope continuation = continuation QuoteScope

-- | A selection from one lexically scoped associated-product environment.
data Path (environment :: Environment) (selected :: Shape) where
    PathHere :: SShape shape -> Path (RootEnvironment shape) shape
    PathLeft :: Path environment selected -> SShape bound -> Path (BindEnvironment scope environment bound) selected
    PathRight :: QuoteEnvironment environment -> SShape bound -> Path (BindEnvironment scope environment bound) bound

type role Path nominal nominal

-- | Select the complete top-level environment.
pathHere :: SShape shape -> Path (RootEnvironment shape) shape
pathHere = PathHere

-- | Retain a path when a binding extends its lexical environment.
pathLeft :: QuoteScope scope -> Path environment selected -> SShape bound -> Path (BindEnvironment scope environment bound) selected
pathLeft _ = PathLeft

-- | Select the newest binding in one lexical environment.
pathRight :: QuoteScope scope -> QuoteEnvironment environment -> SShape bound -> Path (BindEnvironment scope environment bound) bound
pathRight _ = PathRight

{- | Explicit first-order quotation syntax.

The environment carries generative lexical scope identity. The parameter index
records the literal associated tree and is not normalized.
-}
data Quote scalar (fragment :: Fragment) (environment :: Environment) (parameters :: Parameters) (output :: Shape) where
    ProgramQuote :: QuoteEnvironment environment -> Program scalar fragment parameters (EnvironmentShape environment) output -> Quote scalar fragment environment parameters output
    ProjectQuote :: Path environment output -> Quote scalar fragment environment 'NoParameters output
    ComposeQuote ::
        Quote scalar fragment environment leftParameters middle ->
        Program scalar fragment rightParameters middle output ->
        Quote scalar fragment environment ('ParameterProduct leftParameters rightParameters) output
    FanoutQuote ::
        Quote scalar fragment environment leftParameters leftOutput ->
        Quote scalar fragment environment rightParameters rightOutput ->
        Quote scalar fragment environment ('ParameterProduct leftParameters rightParameters) ('Product leftOutput rightOutput)
    LetQuote ::
        QuoteScope scope ->
        Quote scalar fragment environment boundParameters bound ->
        Quote scalar fragment (BindEnvironment scope environment bound) bodyParameters output ->
        Quote scalar fragment environment ('ParameterProduct ('ParameterProduct 'NoParameters boundParameters) bodyParameters) output

type role Quote nominal nominal nominal nominal nominal

-- | Embed a top-level closed-language program as a quotation leaf.
quoteProgram :: Program scalar fragment parameters input output -> Quote scalar fragment (RootEnvironment input) parameters output
quoteProgram program = ProgramQuote (RootEnvironmentWitness (programInputShape program)) program

-- | Embed a program at an existing lexical environment.
quoteProgramAt :: QuoteEnvironment environment -> Program scalar fragment parameters (EnvironmentShape environment) output -> Quote scalar fragment environment parameters output
quoteProgramAt = ProgramQuote

-- | Project a path. Projection has no parameters.
project :: Path environment output -> Quote scalar fragment environment 'NoParameters output
project = ProjectQuote

-- | Feed a quoted value to an existing closed-language program.
composeQuote ::
    Quote scalar fragment environment p middle ->
    Program scalar fragment q middle output ->
    Quote scalar fragment environment ('ParameterProduct p q) output
composeQuote = ComposeQuote

-- | Evaluate two quoted terms from the same environment, from left to right.
fanoutQuote ::
    Quote scalar fragment environment p leftOutput ->
    Quote scalar fragment environment q rightOutput ->
    Quote scalar fragment environment ('ParameterProduct p q) ('Product leftOutput rightOutput)
fanoutQuote = FanoutQuote

{- | Evaluate a bound term and then its body.

The body syntax uses the fresh scope token and is already constructed when
'letQuote' stores it. Its parameter tree is literally
@ParameterProduct (ParameterProduct NoParameters p) q@. Lowering uses
@compose (fanout identity e) body@.
-}
letQuote ::
    QuoteScope scope ->
    Quote scalar fragment environment p bound ->
    Quote scalar fragment (BindEnvironment scope environment bound) q output ->
    Quote scalar fragment environment ('ParameterProduct ('ParameterProduct 'NoParameters p) q) output
letQuote = LetQuote

{- | One limit record governs quotation traversal, lowering, direct exact
execution, and target compilation. Arguments are traversal, quotation nodes,
source depth, path depth, target nodes, target depth, coordinate extent,
transformed nodes, allocation units, runtime arithmetic, total work, and
rational bits. All limits are inclusive.
-}
data QuotationLimits
    = QuotationLimitsValue
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
    deriving (Eq, Show)

-- | Construct the single cumulative quotation-operation limit record.
quotationLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> QuotationLimits
quotationLimits = QuotationLimitsValue

-- | Successful timing-free cumulative preflight account.
data QuoteReport = QuoteReport
    { quoteNodeCount :: !Natural
    -- ^ Explicit quotation constructors.
    , quoteSourceDepth :: !Natural
    -- ^ Maximum quotation and embedded source depth.
    , quoteMaximumPathDepth :: !Natural
    -- ^ Maximum lexical path depth.
    , quotePredictedTargetNodes :: !Natural
    -- ^ Nodes in the lowered program.
    , quotePredictedTargetDepth :: !Natural
    -- ^ Depth of the lowered program.
    , quoteMaximumCoordinateExtent :: !Natural
    -- ^ Largest represented value or parameter extent.
    , quoteTransformedNodes :: !Natural
    -- ^ New program nodes made by lowering.
    , quoteAllocationCount :: !Natural
    -- ^ Syntax allocation units, plus logical compiler reconstruction coupons when compiling.
    , quoteRuntimeWork :: !Natural
    -- ^ Syntax arithmetic allowance, plus compiler forward/reverse allowances when compiling.
    , quoteTraversalWork :: !Natural
    -- ^ Syntax visits, plus charged metadata planning and reconstruction coupons when compiling.
    , quoteTotalWork :: !Natural
    -- ^ Cumulative traversal, target, transformation, allocation, and runtime work.
    , quoteMaximumRationalBits :: !Natural
    -- ^ Largest admitted literal numerator or denominator bit count.
    }
    deriving (Eq, Show)

-- | Combined quotation and exact target compiler report.
data QuoteCompilationReport = QuoteCompilationReport
    { quoteCompilationPreflight :: !QuoteReport
    -- ^ The cumulative quotation preflight report.
    , quoteCompilationTarget :: !CompileReport
    -- ^ The exact target compiler report.
    }
    deriving (Eq, Show)

-- | Frozen preflight and direct-execution failures.
data QuoteError
    = QuoteTraversalLimitExceeded !Natural !Natural
    | QuoteNodeLimitExceeded !Natural !Natural
    | QuoteSourceDepthLimitExceeded !Natural !Natural
    | QuotePathDepthLimitExceeded !Natural !Natural
    | QuoteTargetNodeLimitExceeded !Natural !Natural
    | QuoteTargetDepthLimitExceeded !Natural !Natural
    | QuoteCoordinateExtentLimitExceeded !Natural !Natural
    | QuoteMachineExtentExceeded !Natural
    | QuoteTransformedNodeLimitExceeded !Natural !Natural
    | QuoteAllocationLimitExceeded !Natural !Natural
    | QuoteRuntimeWorkLimitExceeded !Natural !Natural
    | QuoteTotalWorkLimitExceeded !Natural !Natural
    | QuoteRationalMagnitudeLimitExceeded !String !Natural !Natural
    | QuoteInternalVectorLengthMismatch !String
    deriving (Eq, Show)

-- | Distinguish quotation preflight from target compilation failure.
data QuoteCompileError
    = QuoteCompilePreflightFailure !QuoteError
    | QuoteCompileTargetFailure !CompileError
    deriving (Eq, Show)

data Ledger = Ledger
    { ledgerQuoteNodes :: !Natural
    , ledgerSourceDepth :: !Natural
    , ledgerPathDepth :: !Natural
    , ledgerTargetNodes :: !Natural
    , ledgerTargetDepth :: !Natural
    , ledgerCoordinateExtent :: !Natural
    , ledgerTransformedNodes :: !Natural
    , ledgerAllocation :: !Natural
    , ledgerRuntimeWork :: !Natural
    , ledgerTraversal :: !Natural
    , ledgerTotalWork :: !Natural
    , ledgerRationalBits :: !Natural
    }

emptyLedger :: Ledger
emptyLedger = Ledger 0 0 0 0 0 0 0 0 0 0 0 0

{- | Preflight uses charge-before-descent DFS. Each cumulative charge checks
its dimension and then total work. A quotation entry checks traversal, total
work, quotation nodes, and source depth in that order. Constructor-specific
charges then follow source order. Children are visited left to right. A failed
charge stops without visiting later children.
-}
preflightQuote :: QuotationLimits -> Quote Rational 'Polynomial environment parameters output -> Either QuoteError QuoteReport
preflightQuote limits quotation = do
    (_, ledger) <- walkQuote limits 1 quotation emptyLedger
    pure (ledgerReport ledger)

-- | Preflight and only then construct the closed target program.
lowerQuote :: QuotationLimits -> Quote Rational 'Polynomial environment parameters output -> Either QuoteError (Program Rational 'Polynomial parameters (EnvironmentShape environment) output)
lowerQuote limits quotation = do
    _ <- preflightQuote limits quotation
    pure (buildQuote quotation)

{- | Admit syntax, metadata planning, and compiler reservations in one ledger
before building a target. The compiler receives derived structural capacities
and the caller's rational-bit allowance. Return the complete quotation account
and the actual target report; standalone compilation is unchanged.
-}
compileExactQuote :: QuotationLimits -> TapePolicy -> Quote Rational 'Polynomial environment parameters output -> Either QuoteCompileError (ExactExecutable parameters (EnvironmentShape environment) output, QuoteCompilationReport)
compileExactQuote limits policy quotation = do
    AdmittedQuoteCompilation preflight reservation <- mapLeft QuoteCompilePreflightFailure (preflightQuoteCompilation limits quotation)
    -- Call-edge events are forced independently of demand for the lazy result.
    D080_BUILDER_CALL
    let program = buildQuote quotation
    D080_COMPILER_CALL
    executable <- mapLeft QuoteCompileTargetFailure (compileExactPolynomial (reservationCapacities reservation) policy program)
    pure (executable, QuoteCompilationReport preflight (exactCompileReport executable))

-- | Independent bounded exact primal recursion.
interpretExactQuote ::
    QuotationLimits ->
    Quote Rational 'Polynomial environment parameters output ->
    ParameterValue Rational parameters ->
    Value Rational (EnvironmentShape environment) ->
    Either QuoteError (Value Rational output)
interpretExactQuote limits quotation parameters input = do
    _ <- preflightExactQuoteExecution limits quotation parameters input
    evalQuotePrimal (rationalBitLimit limits) quotation parameters input

-- | Independent bounded exact primal/JVP recursion.
interpretExactQuoteJVP ::
    QuotationLimits ->
    Quote Rational 'Polynomial environment parameters output ->
    ParameterValue Rational parameters ->
    ParameterValue Rational parameters ->
    Value Rational (EnvironmentShape environment) ->
    Value Rational (EnvironmentShape environment) ->
    Either QuoteError (Value Rational output, Value Rational output)
interpretExactQuoteJVP limits quotation parameters parameterDirection input inputDirection = do
    _ <- preflightExactQuoteJVPExecution limits quotation parameters parameterDirection input inputDirection
    evalQuoteJVP (rationalBitLimit limits) quotation parameters parameterDirection input inputDirection

-- | Preflight one direct exact primal execution and return its cumulative input account.
preflightExactQuoteExecution :: QuotationLimits -> Quote Rational 'Polynomial environment parameters output -> ParameterValue Rational parameters -> Value Rational (EnvironmentShape environment) -> Either QuoteError QuoteReport
preflightExactQuoteExecution limits quotation parameters input =
    preflightRuntimeValues limits quotation parameters Nothing input Nothing

-- | Preflight one direct exact primal/JVP execution and return its cumulative input account.
preflightExactQuoteJVPExecution :: QuotationLimits -> Quote Rational 'Polynomial environment parameters output -> ParameterValue Rational parameters -> ParameterValue Rational parameters -> Value Rational (EnvironmentShape environment) -> Value Rational (EnvironmentShape environment) -> Either QuoteError QuoteReport
preflightExactQuoteJVPExecution limits quotation parameters parameterDirection input inputDirection =
    preflightRuntimeValues limits quotation parameters (Just parameterDirection) input (Just inputDirection)

preflightRuntimeValues :: QuotationLimits -> Quote Rational 'Polynomial environment parameters output -> ParameterValue Rational parameters -> Maybe (ParameterValue Rational parameters) -> Value Rational (EnvironmentShape environment) -> Maybe (Value Rational (EnvironmentShape environment)) -> Either QuoteError QuoteReport
preflightRuntimeValues limits quotation parameters parameterDirection input inputDirection = do
    (_, syntaxLedger) <- walkQuote limits 1 quotation emptyLedger
    ledger1 <- scanRationals limits "parameter" (parameterScalars parameters) syntaxLedger
    ledger2 <- maybe (pure ledger1) (\value -> scanRationals limits "parameter-direction" (parameterScalars value) ledger1) parameterDirection
    ledger3 <- scanRationals limits "input" (valueScalars input) ledger2
    ledger4 <- maybe (pure ledger3) (\value -> scanRationals limits "input-direction" (valueScalars value) ledger3) inputDirection
    pure (ledgerReport ledger4)

-- The compiler-stage inventory is numeric metadata, never a virtual target tree.
-- These are cumulative logical reconstruction coupons, not heap measurements.
data ShapeSummary = ShapeSummary
    { summaryExtent :: !Natural
    , summaryLayoutNodes :: !Natural
    , summaryLayoutDepth :: !Natural
    }

data ParameterSummary = ParameterSummary
    { summaryParameterLayout :: !ShapeSummary
    , summaryOwnershipNodes :: !Natural
    , summaryOwnershipDepth :: !Natural
    , summaryOwnerLeaves :: !Natural
    , summaryKeyEnumerationCells :: !Natural
    }

data TargetSummary = TargetSummary
    { summaryParameters :: !ParameterSummary
    , summaryInput :: !ShapeSummary
    , summaryOutput :: !ShapeSummary
    , summaryNodes :: !Natural
    , summaryPrimitives :: !Natural
    , summaryDepth :: !Natural
    , summaryMaximumExtent :: !Natural
    , summaryMaximumStructureNodes :: !Natural
    , summaryMaximumStructureDepth :: !Natural
    , summaryForwardWork :: !Natural
    , summaryReverseWork :: !Natural
    , summaryMetadataTraversalSum :: !Natural
    , summaryMetadataAllocationSum :: !Natural
    , summaryOwnershipTraversalSum :: !Natural
    , summaryOwnershipAllocationSum :: !Natural
    , summaryJoinComparisonSum :: !Natural
    , summaryPrimitiveLayoutComparisonSum :: !Natural
    , summaryPathCellSum :: !Natural
    , summaryDepthSum :: !Natural
    , summaryQuoteEntries :: !Natural
    , summaryBuilderReservation :: !Natural
    }

data CompilerReservation = CompilerReservation
    { reservationTraversal :: !Natural
    , reservationAllocation :: !Natural
    , reservationRuntime :: !Natural
    , reservationCapacities :: !CompilerLimits
    }

data AdmittedQuoteCompilation = AdmittedQuoteCompilation !QuoteReport !CompilerReservation

unitSummary, scalarSummary :: ShapeSummary
unitSummary = ShapeSummary 0 1 1
scalarSummary = ShapeSummary 1 1 1

productSummary :: ShapeSummary -> ShapeSummary -> ShapeSummary
productSummary (ShapeSummary e l h) (ShapeSummary e' l' h') =
    ShapeSummary (e + e') (1 + l + l') (1 + max h h')

noParameterSummary :: ParameterSummary
noParameterSummary = ParameterSummary unitSummary 1 1 0 0

ownerSummary :: ShapeSummary -> ParameterSummary
ownerSummary shape = ParameterSummary shape 1 1 1 1

parameterProductSummary :: ParameterSummary -> ParameterSummary -> ParameterSummary
parameterProductSummary left right =
    ParameterSummary
        (productSummary (summaryParameterLayout left) (summaryParameterLayout right))
        (1 + summaryOwnershipNodes left + summaryOwnershipNodes right)
        (1 + max (summaryOwnershipDepth left) (summaryOwnershipDepth right))
        (summaryOwnerLeaves left + summaryOwnerLeaves right)
        (summaryKeyEnumerationCells left + summaryKeyEnumerationCells right + summaryOwnerLeaves left)

checkSummary :: QuotationLimits -> ShapeSummary -> Ledger -> Either QuoteError Ledger
checkSummary limits shape ledger
    | extent > fromIntegral (maxBound :: Int) = Left (QuoteMachineExtentExceeded extent)
    | otherwise = checkCoordinateExtent limits extent ledger
  where
    extent = summaryExtent shape

planShape :: QuotationLimits -> SShape shape -> Ledger -> Either QuoteError (ShapeSummary, Ledger)
planShape limits shape initial = do
    ledger <- chargeTraversal limits initial
    (summary, next) <- case shape of
        SUnit -> D080_PLAN_UNIT pure (unitSummary, ledger)
        SScalar -> D080_PLAN_SCALAR pure (scalarSummary, ledger)
        vector@SVector -> D080_PLAN_VECTOR pure (ShapeSummary (vectorExtent vector) 1 1, ledger)
        SProduct left right -> do
            D080_PLAN_PRODUCT
            (l, ledger1) <- planShape limits left ledger
            (r, ledger2) <- planShape limits right ledger1
            pure (productSummary l r, ledger2)
    checkedLedger <- checkSummary limits summary next
    pure (summary, checkedLedger)

planEnvironment :: QuotationLimits -> QuoteEnvironment environment -> Ledger -> Either QuoteError (ShapeSummary, Ledger)
planEnvironment limits environment initial = do
    ledger <- chargeTraversal limits initial
    case environment of
        RootEnvironmentWitness shape -> planShape limits shape ledger
        BindEnvironmentWitness parent bound -> do
            (p, ledger1) <- planEnvironment limits parent ledger
            (b, ledger2) <- planShape limits bound ledger1
            let summary = productSummary p b
            ledger3 <- checkSummary limits summary ledger2
            pure (summary, ledger3)

finishSelection :: QuotationLimits -> ShapeSummary -> ShapeSummary -> Ledger -> Either QuoteError (ShapeSummary, ShapeSummary, Ledger)
finishSelection limits input output ledger = do
    ledger1 <- checkSummary limits input ledger
    ledger2 <- checkSummary limits output ledger1
    pure (input, output, ledger2)

planPath :: QuotationLimits -> Path environment selected -> Ledger -> Either QuoteError (ShapeSummary, ShapeSummary, Ledger)
planPath limits path initial = do
    ledger <- chargeTraversal limits initial
    case path of
        PathHere shape -> do
            (s, ledger1) <- planShape limits shape ledger
            finishSelection limits s s ledger1
        PathLeft inner bound -> do
            (i, o, ledger1) <- planPath limits inner ledger
            (b, ledger2) <- planShape limits bound ledger1
            finishSelection limits (productSummary i b) o ledger2
        PathRight environment bound -> do
            (e, ledger1) <- planEnvironment limits environment ledger
            (b, ledger2) <- planShape limits bound ledger1
            finishSelection limits (productSummary e b) b ledger2

planProjection :: QuotationLimits -> Projection input output -> Ledger -> Either QuoteError (ShapeSummary, ShapeSummary, Ledger)
planProjection limits projection initial = do
    ledger <- chargeTraversal limits initial
    case projection of
        ProjectionHere shape -> do
            (s, ledger1) <- planShape limits shape ledger
            finishSelection limits s s ledger1
        ProjectionLeft inner right -> do
            (i, o, ledger1) <- planProjection limits inner ledger
            (r, ledger2) <- planShape limits right ledger1
            finishSelection limits (productSummary i r) o ledger2
        ProjectionRight left inner -> do
            (l, ledger1) <- planShape limits left ledger
            (i, o, ledger2) <- planProjection limits inner ledger1
            finishSelection limits (productSummary l i) o ledger2

-- Divide the even factor before multiplying; Natural subtraction at zero is guarded.
triangular :: Natural -> Natural
triangular 0 = 0
triangular k
    | even k = (k `quot` 2) * (k - 1)
    | otherwise = k * ((k - 1) `quot` 2)

endpointInventory :: ParameterSummary -> ShapeSummary -> ShapeSummary -> Natural
endpointInventory p i o = summaryOwnershipNodes p + summaryLayoutNodes (summaryParameterLayout p) + 2 * summaryLayoutNodes i + summaryLayoutNodes o

targetInventory :: TargetSummary -> Natural
targetInventory s = endpointInventory (summaryParameters s) (summaryInput s) (summaryOutput s)

-- Each named row in contract section 5.1 receives its own full reconstruction
-- coupon. In particular represented zero coordinates are not atomic layouts.
leafSummary :: ParameterSummary -> ShapeSummary -> ShapeSummary -> Natural -> Natural -> Natural -> TargetSummary
leafSummary p i o primitives forward reverseWork =
    TargetSummary
        p
        i
        o
        1
        primitives
        1
        extent
        structures
        height
        forward
        reverseWork
        mt
        (mt - 12)
        (2 * (on + c + k + triangular k))
        (2 * (on + c + k))
        0
        (primitives * lp)
        0
        1
        0
        0
  where
    pl = summaryParameterLayout p
    lp = summaryLayoutNodes pl
    on = summaryOwnershipNodes p
    k = summaryOwnerLeaves p
    c = summaryKeyEnumerationCells p
    b = lp + summaryLayoutNodes i + summaryLayoutNodes o
    u = endpointInventory p i o
    z = on + b + summaryExtent pl + summaryExtent i + summaryExtent o
    mt = 7 * u + 2 * b + on + lp + 2 * (on + b) + 24 * (on + b) + 3 * b + 2 * z + 18 + 2 * b + 2 * lp + 42
    extent = max (summaryExtent pl) (max (summaryExtent i) (summaryExtent o))
    structures = max on (max lp (max (summaryLayoutNodes i) (summaryLayoutNodes o)))
    height = max (summaryOwnershipDepth p) (max (summaryLayoutDepth pl) (max (summaryLayoutDepth i) (summaryLayoutDepth o)))

binarySummary :: ParameterSummary -> ShapeSummary -> ShapeSummary -> TargetSummary -> TargetSummary -> TargetSummary
binarySummary p i o left right =
    local
        { summaryNodes = n
        , summaryPrimitives = summaryPrimitives left + summaryPrimitives right
        , summaryDepth = 1 + max (summaryDepth left) (summaryDepth right)
        , summaryMaximumExtent = maximumOf summaryMaximumExtent
        , summaryMaximumStructureNodes = maximumOf summaryMaximumStructureNodes
        , summaryMaximumStructureDepth = maximumOf summaryMaximumStructureDepth
        , summaryMetadataTraversalSum = sumOf summaryMetadataTraversalSum
        , summaryMetadataAllocationSum = sumOf summaryMetadataAllocationSum
        , summaryOwnershipTraversalSum = sumOf summaryOwnershipTraversalSum
        , summaryOwnershipAllocationSum = sumOf summaryOwnershipAllocationSum
        , summaryJoinComparisonSum = summaryJoinComparisonSum left + summaryJoinComparisonSum right + targetInventory left + targetInventory right
        , summaryPrimitiveLayoutComparisonSum = summaryPrimitiveLayoutComparisonSum left + summaryPrimitiveLayoutComparisonSum right
        , summaryPathCellSum = depths - n
        , summaryDepthSum = depths
        , summaryQuoteEntries = summaryQuoteEntries left + summaryQuoteEntries right
        , summaryBuilderReservation = summaryBuilderReservation left + summaryBuilderReservation right
        }
  where
    local = leafSummary p i o 0 (1 + summaryForwardWork left + summaryForwardWork right) (1 + summaryReverseWork left + summaryReverseWork right)
    n = 1 + summaryNodes left + summaryNodes right
    depths = 1 + summaryDepthSum left + summaryNodes left + summaryDepthSum right + summaryNodes right
    sumOf field = field local + field left + field right
    maximumOf field = max (field local) (max (field left) (field right))

composeSummary, parallelSummary, fanoutSummary, shareSummary :: TargetSummary -> TargetSummary -> TargetSummary
composeSummary l r = binarySummary (parameterProductSummary (summaryParameters l) (summaryParameters r)) (summaryInput l) (summaryOutput r) l r
parallelSummary l r = binarySummary (parameterProductSummary (summaryParameters l) (summaryParameters r)) (productSummary (summaryInput l) (summaryInput r)) (productSummary (summaryOutput l) (summaryOutput r)) l r
fanoutSummary l r = binarySummary (parameterProductSummary (summaryParameters l) (summaryParameters r)) (summaryInput l) (productSummary (summaryOutput l) (summaryOutput r)) l r
shareSummary l r = binarySummary (summaryParameters l) (productSummary (summaryInput l) (summaryInput r)) (productSummary (summaryOutput l) (summaryOutput r)) l r

finishSummary :: QuotationLimits -> TargetSummary -> Ledger -> Either QuoteError (TargetSummary, Ledger)
finishSummary limits summary ledger = do
    ledger1 <- checkSummary limits (summaryParameterLayout (summaryParameters summary)) ledger
    ledger2 <- checkSummary limits (summaryInput summary) ledger1
    ledger3 <- checkSummary limits (summaryOutput summary) ledger2
    pure (summary, ledger3)

primitiveSummary :: ParameterSummary -> ShapeSummary -> ShapeSummary -> Natural -> TargetSummary
primitiveSummary p i o arithmetic = leafSummary p i o 1 (base + arithmetic) (base + 3 * arithmetic)
  where
    base = 1 + summaryExtent (summaryParameterLayout p) + summaryExtent i + summaryExtent o

identitySummary :: ShapeSummary -> TargetSummary
identitySummary s = leafSummary noParameterSummary s s 0 (1 + summaryExtent s) (1 + summaryExtent s)

planPrimitiveCompilation :: QuotationLimits -> Primitive Rational 'Polynomial parameters input output -> Ledger -> Either QuoteError (TargetSummary, Ledger)
planPrimitiveCompilation limits primitive initial = do
    ledger <- chargeTraversal limits initial
    case primitive of
        ConstantScalar input _ -> do
            (i, next) <- planShape limits input ledger
            finish noParameterSummary i scalarSummary 0 next
        ConstantVector input output _ -> do
            (i, next) <- planShape limits input ledger
            (o, final) <- planShape limits output next
            finish noParameterSummary i o (summaryExtent o) final
        Parameter _ input output -> do
            (i, next) <- planShape limits input ledger
            (o, final) <- planShape limits output next
            finish (ownerSummary o) i o (summaryExtent o) final
        NegateScalar -> finish noParameterSummary scalarSummary scalarSummary 1 ledger
        AddScalar -> scalarBinary ledger
        MultiplyScalar -> scalarBinary ledger
        AddVector shape -> vectorBinary shape False 1 ledger
        Hadamard shape -> vectorBinary shape False 1 ledger
        Dot shape -> vectorBinary shape True 2 ledger
        SumVector shape -> do
            (s, next) <- planShape limits shape ledger
            finish noParameterSummary s scalarSummary (summaryExtent s) next
        First left right -> selection left right True ledger
        Second left right -> selection left right False ledger
        ProjectValue projection -> do
            (i, o, next) <- planProjection limits projection ledger
            finish noParameterSummary i o 0 next
  where
    finish p i o a = finishSummary limits (primitiveSummary p i o a)
    scalarBinary = finish noParameterSummary (productSummary scalarSummary scalarSummary) scalarSummary 1
    vectorBinary :: SShape ('Vector n) -> Bool -> Natural -> Ledger -> Either QuoteError (TargetSummary, Ledger)
    vectorBinary shape scalarOutput factor ledger = do
        (s, next) <- planShape limits shape ledger
        finish noParameterSummary (productSummary s s) (if scalarOutput then scalarSummary else s) (factor * summaryExtent s) next
    selection :: SShape l -> SShape r -> Bool -> Ledger -> Either QuoteError (TargetSummary, Ledger)
    selection left right selectLeft ledger = do
        (l, next) <- planShape limits left ledger
        (r, final) <- planShape limits right next
        finish noParameterSummary (productSummary l r) (if selectLeft then l else r) 0 final

planProgramCompilation :: QuotationLimits -> Program Rational 'Polynomial parameters input output -> Ledger -> Either QuoteError (TargetSummary, Ledger)
planProgramCompilation limits program initial = do
    ledger <- chargeTraversal limits initial
    case program of
        PrimitiveNode primitive -> planPrimitiveCompilation limits primitive ledger
        IdentityNode shape -> do
            (s, next) <- planShape limits shape ledger
            finishSummary limits (identitySummary s) next
        ComposeNode l r -> pair composeSummary l r ledger
        ParallelNode l r -> pair parallelSummary l r ledger
        FanoutNode l r -> pair fanoutSummary l r ledger
        ShareParametersNode l r -> pair shareSummary l r ledger
  where
    pair :: (TargetSummary -> TargetSummary -> TargetSummary) -> Program Rational 'Polynomial p x y -> Program Rational 'Polynomial q u v -> Ledger -> Either QuoteError (TargetSummary, Ledger)
    pair combine l r ledger = do
        (left, next) <- planProgramCompilation limits l ledger
        (right, final) <- planProgramCompilation limits r next
        finishSummary limits (combine left right) final

planQuoteCompilation :: QuotationLimits -> Quote Rational 'Polynomial environment parameters output -> Ledger -> Either QuoteError (TargetSummary, Ledger)
planQuoteCompilation limits quotation initial = do
    ledger <- chargeTraversal limits initial
    (summary, final) <- case quotation of
        ProgramQuote _ program -> planProgramCompilation limits program ledger
        ProjectQuote path -> do
            (i, o, next) <- planPath limits path ledger
            let s = primitiveSummary noParameterSummary i o 0
            finishSummary limits s{summaryBuilderReservation = 1 + 2 * summaryLayoutNodes i} next
        ComposeQuote quoted program -> do
            (l, next) <- planQuoteCompilation limits quoted ledger
            (r, end) <- planProgramCompilation limits program next
            finishSummary limits (composeSummary l r) end
        FanoutQuote left right -> do
            (l, next) <- planQuoteCompilation limits left ledger
            (r, end) <- planQuoteCompilation limits right next
            finishSummary limits (fanoutSummary l r) end
        LetQuote _ bound body -> do
            D080_LET_BOUND_START
            (b, next) <- planQuoteCompilation limits bound ledger
            D080_LET_BOUND_COMPLETE
            (i, next1) <- finishSummary limits (identitySummary (summaryInput b)) next
            D080_LET_IDENTITY_COMPLETE
            (f, next2) <- finishSummary limits (fanoutSummary i b) next1
            D080_LET_FANOUT_COMPLETE
            D080_LET_BODY_START
            (r, next3) <- planQuoteCompilation limits body next2
            D080_LET_BODY_COMPLETE
            let s = composeSummary f r
                g = summaryBuilderReservation s + summaryQuoteEntries b + 2 * summaryLayoutNodes (summaryInput b)
            finishSummary limits s{summaryBuilderReservation = g} next3
    pure (summary{summaryQuoteEntries = summaryQuoteEntries summary + 1, summaryBuilderReservation = summaryBuilderReservation summary + 1}, final)

-- Final dimension expressions saturate independently at remaining+1. Shared
-- summaries stay exact: a smaller later allocation/total cap must not truncate them.
cappedAdd, cappedMultiply :: Natural -> Natural -> Natural -> Natural
cappedAdd cap a b
    | a >= cap || b >= cap - a = cap
    | otherwise = a + b
cappedMultiply cap a b
    | a == 0 || b == 0 = 0
    | a > cap `quot` b = cap
    | otherwise = min cap (a * b)

compilerTraversalAmount, compilerAllocationAmount :: Natural -> TargetSummary -> Natural
compilerTraversalAmount cap s =
    mul 6 n
        `add` g
        `add` summaryMetadataTraversalSum s
        `add` summaryOwnershipTraversalSum s
        `add` mul (mul 2 p) (add p 1)
        `add` p
        `add` summaryPathCellSum s
        `add` mul 3 (summaryJoinComparisonSum s)
        `add` mul 4 n
        `add` summaryPrimitiveLayoutComparisonSum s
  where
    add = cappedAdd cap
    mul = cappedMultiply cap
    n = summaryNodes s
    p = summaryPrimitives s
    g = summaryBuilderReservation s
compilerAllocationAmount cap s =
    mul 3 n
        `add` p
        `add` 5
        `add` summaryBuilderReservation s
        `add` summaryMetadataAllocationSum s
        `add` summaryOwnershipAllocationSum s
        `add` mul p (add p 1)
        `add` p
        `add` summaryPathCellSum s
        `add` mul 3 (summaryJoinComparisonSum s)
        `add` mul 4 n
        `add` summaryPrimitiveLayoutComparisonSum s
  where
    add = cappedAdd cap
    mul = cappedMultiply cap
    n = summaryNodes s
    p = summaryPrimitives s

reserveQuoteCompilation :: QuotationLimits -> TargetSummary -> Ledger -> Either QuoteError AdmittedQuoteCompilation
reserveQuoteCompilation limits s ledger = do
    let tc = compilerTraversalAmount (traversalLimit limits - ledgerTraversal ledger + 1) s
    traversal <- addBounded QuoteTraversalLimitExceeded (traversalLimit limits) (ledgerTraversal ledger) tc
    ledger1 <- chargeTotal limits tc ledger{ledgerTraversal = traversal}
    D080_RESERVE_TRAVERSAL
    let ac = compilerAllocationAmount (allocationLimit limits - ledgerAllocation ledger1 + 1) s
    ledger2 <- chargeAllocation limits ac ledger1
    D080_RESERVE_ALLOCATION
    let rc = cappedAdd (runtimeLimit limits - ledgerRuntimeWork ledger2 + 1) (summaryForwardWork s) (summaryReverseWork s)
    ledger3 <- chargeRuntime limits rc ledger2
    D080_RESERVE_RUNTIME
    let capacities =
            compilerLimits
                (summaryNodes s)
                (summaryPrimitives s)
                (summaryDepth s)
                (summaryPrimitives s)
                (summaryMaximumExtent s)
                (summaryMaximumExtent s)
                (summaryMaximumStructureNodes s)
                (summaryMaximumStructureDepth s)
                (max (summaryForwardWork s) (summaryReverseWork s))
                (rationalBitLimit limits)
        reservation = CompilerReservation tc ac rc capacities
        report = ledgerReport ledger3
        admitted = D080_ADMISSION_WITNESS AdmittedQuoteCompilation report reservation
    -- Force the witness and all strict fields before the successful builder branch.
    reservationTraversal reservation `seq` reservationAllocation reservation `seq` reservationRuntime reservation `seq` admitted `seq` pure admitted

preflightQuoteCompilation :: QuotationLimits -> Quote Rational 'Polynomial environment parameters output -> Either QuoteError AdmittedQuoteCompilation
preflightQuoteCompilation limits quotation = do
    D080_SYNTAX_START
    (_, syntaxLedger) <- walkQuote limits 1 quotation emptyLedger
    D080_SYNTAX_SUCCESS
    D080_PLANNER_START
    (summary, plannedLedger) <- planQuoteCompilation limits quotation syntaxLedger
    D080_PLANNER_SUCCESS
    reserveQuoteCompilation limits summary plannedLedger

rationalBitLimit :: QuotationLimits -> Natural
rationalBitLimit (QuotationLimitsValue _ _ _ _ _ _ _ _ _ _ _ bits) = bits

mapLeft :: (left -> right) -> Either left value -> Either right value
mapLeft function result = case result of
    Left problem -> Left (function problem)
    Right value -> Right value

walkQuote :: QuotationLimits -> Natural -> Quote Rational 'Polynomial environment parameters output -> Ledger -> Either QuoteError (Natural, Ledger)
walkQuote limits sourceDepth quotation initial = do
    ledger0 <- chargeTraversal limits initial
    ledger1 <- chargeQuoteNode limits ledger0
    ledger2 <- checkSourceDepth limits sourceDepth ledger1
    case quotation of
        ProgramQuote _ program -> walkProgram limits (sourceDepth + 1) program ledger2
        ProjectQuote path -> do
            ledger3 <- inspectShape limits (environmentShape (pathEnvironment path)) ledger2
            ledger4 <- inspectShape limits (pathSelectedShape path) ledger3
            ledger5 <- chargeTargetNodes limits 1 ledger4
            ledger6 <- chargeAllocation limits (shapeExtent (pathSelectedShape path)) ledger5
            ledger7 <- chargeRuntime limits (1 + shapeExtent (environmentShape (pathEnvironment path)) + shapeExtent (pathSelectedShape path)) ledger6
            ledger8 <- chargeTransformed limits 1 ledger7
            ledger9 <- walkPath limits 1 path ledger8
            finishTargetDepth limits 1 ledger9
        ComposeQuote quoted program -> do
            ledger3 <- inspectShape limits (programOutputShape program) ledger2
            ledger4 <- chargeTargetNodes limits 1 ledger3
            ledger5 <- chargeAllocation limits (shapeExtent (programOutputShape program)) ledger4
            ledger6 <- chargeRuntime limits 1 ledger5
            ledger7 <- chargeTransformed limits 1 ledger6
            (quotedDepth, ledger8) <- walkQuote limits (sourceDepth + 1) quoted ledger7
            (programDepth, ledger9) <- walkProgram limits (sourceDepth + 1) program ledger8
            finishTargetDepth limits (1 + max quotedDepth programDepth) ledger9
        FanoutQuote left right -> do
            let outputShape = SProduct (quoteOutputShape left) (quoteOutputShape right)
            ledger3 <- inspectShape limits outputShape ledger2
            ledger4 <- chargeTargetNodes limits 1 ledger3
            ledger5 <- chargeAllocation limits (shapeExtent outputShape) ledger4
            ledger6 <- chargeRuntime limits 1 ledger5
            ledger7 <- chargeTransformed limits 1 ledger6
            (leftDepth, ledger8) <- walkQuote limits (sourceDepth + 1) left ledger7
            (rightDepth, ledger9) <- walkQuote limits (sourceDepth + 1) right ledger8
            finishTargetDepth limits (1 + max leftDepth rightDepth) ledger9
        LetQuote _ bound body -> do
            let inputShape = quoteEnvironmentShape bound
                boundShape = quoteOutputShape bound
                bodyShape = quoteOutputShape body
                generatedExtent = shapeExtent inputShape + shapeExtent (SProduct inputShape boundShape) + shapeExtent bodyShape
            ledger3 <- inspectShape limits inputShape ledger2
            ledger4 <- inspectShape limits (SProduct inputShape boundShape) ledger3
            ledger5 <- inspectShape limits bodyShape ledger4
            ledger6 <- chargeTargetNodes limits 3 ledger5
            ledger7 <- chargeAllocation limits generatedExtent ledger6
            ledger8 <- chargeRuntime limits (4 + shapeExtent inputShape) ledger7
            ledger9 <- chargeTransformed limits 3 ledger8
            (boundDepth, ledger10) <- walkQuote limits (sourceDepth + 1) bound ledger9
            (bodyDepth, ledger11) <- walkQuote limits (sourceDepth + 1) body ledger10
            let fanoutDepth = 1 + max 1 boundDepth
            finishTargetDepth limits (1 + max fanoutDepth bodyDepth) ledger11

walkPath :: QuotationLimits -> Natural -> Path environment selected -> Ledger -> Either QuoteError Ledger
walkPath limits depth path initial = do
    ledger0 <- chargeTraversal limits initial
    ledger1 <- checkPathDepth limits depth ledger0
    case path of
        PathHere _ -> pure ledger1
        PathLeft inner _ -> walkPath limits (depth + 1) inner ledger1
        PathRight _ _ -> pure ledger1

walkProgram :: QuotationLimits -> Natural -> Program Rational 'Polynomial parameters input output -> Ledger -> Either QuoteError (Natural, Ledger)
walkProgram limits sourceDepth program initial = do
    ledger0 <- chargeTraversal limits initial
    ledger1 <- checkSourceDepth limits sourceDepth ledger0
    ledger2 <- inspectShape limits (programOutputShape program) ledger1
    ledger3 <- chargeTargetNodes limits 1 ledger2
    ledger4 <- chargeAllocation limits (shapeExtent (programOutputShape program)) ledger3
    case program of
        PrimitiveNode primitive -> do
            ledger5 <- inspectPrimitive limits primitive ledger4
            finishTargetDepth limits 1 ledger5
        IdentityNode shape -> do
            ledger5 <- inspectShape limits shape ledger4
            ledger6 <- chargeRuntime limits (1 + shapeExtent shape) ledger5
            finishTargetDepth limits 1 ledger6
        ComposeNode left right -> chargeRuntime limits 1 ledger4 >>= walkProgramPair limits sourceDepth left right
        ParallelNode left right -> chargeRuntime limits 1 ledger4 >>= walkProgramPair limits sourceDepth left right
        FanoutNode left right -> chargeRuntime limits 1 ledger4 >>= walkProgramPair limits sourceDepth left right
        ShareParametersNode left right -> chargeRuntime limits 1 ledger4 >>= walkProgramPair limits sourceDepth left right

walkProgramPair :: QuotationLimits -> Natural -> Program Rational 'Polynomial p x y -> Program Rational 'Polynomial q u v -> Ledger -> Either QuoteError (Natural, Ledger)
walkProgramPair limits sourceDepth left right ledger = do
    (leftDepth, ledger1) <- walkProgram limits (sourceDepth + 1) left ledger
    (rightDepth, ledger2) <- walkProgram limits (sourceDepth + 1) right ledger1
    finishTargetDepth limits (1 + max leftDepth rightDepth) ledger2

inspectPrimitive :: QuotationLimits -> Primitive Rational 'Polynomial parameters input output -> Ledger -> Either QuoteError Ledger
inspectPrimitive limits primitive initial = do
    ledger0 <- inspectShape limits (primitiveInputShape primitive) initial
    ledger1 <- inspectShape limits (primitiveOutputShape primitive) ledger0
    ledger2 <- inspectParameterShape limits (primitiveParameterShape primitive) ledger1
    let outputExtent = shapeExtent (primitiveOutputShape primitive)
    ledger3 <- chargeAllocation limits (2 * outputExtent) ledger2
    ledger4 <- chargeRuntime limits (primitiveJVPWorkUnbounded primitive) ledger3
    ledger5 <- chargeRuntime limits (primitiveCompilerWork primitive) ledger4
    case primitive of
        ConstantScalar _ value -> scanRationals limits "source/constant-scalar" [value] ledger5
        ConstantVector _ _ value -> scanRationals limits "source/constant-vector" (valueScalars value) ledger5
        _ -> pure ledger5

inspectShape :: QuotationLimits -> SShape shape -> Ledger -> Either QuoteError Ledger
inspectShape limits shape ledger = do
    let extent = shapeExtent shape
    if extent > fromIntegral (maxBound :: Int)
        then Left (QuoteMachineExtentExceeded extent)
        else checkCoordinateExtent limits extent ledger

inspectParameterShape :: QuotationLimits -> SParameters parameters -> Ledger -> Either QuoteError Ledger
inspectParameterShape _ SNoParameters ledger = pure ledger
inspectParameterShape limits (SOwner _ shape) ledger = inspectShape limits shape ledger
inspectParameterShape limits (SParameterProduct left right) ledger = do
    ledger1 <- inspectParameterShape limits left ledger
    inspectParameterShape limits right ledger1

primitiveParameterShape :: Primitive scalar fragment parameters input output -> SParameters parameters
primitiveParameterShape primitive = case primitive of
    ConstantScalar{} -> SNoParameters
    ConstantVector{} -> SNoParameters
    Parameter owner _ shape -> SOwner owner shape
    NegateScalar -> SNoParameters
    AddScalar -> SNoParameters
    MultiplyScalar -> SNoParameters
    AddVector{} -> SNoParameters
    Hadamard{} -> SNoParameters
    Dot{} -> SNoParameters
    SumVector{} -> SNoParameters
    TanhScalar -> SNoParameters
    TanhVector{} -> SNoParameters
    First{} -> SNoParameters
    Second{} -> SNoParameters
    ProjectValue{} -> SNoParameters

primitiveOutputShape :: Primitive scalar fragment parameters input output -> SShape output
primitiveOutputShape primitive = case primitive of
    ConstantScalar{} -> SScalar
    ConstantVector _ shape _ -> shape
    Parameter _ _ shape -> shape
    NegateScalar -> SScalar
    AddScalar -> SScalar
    MultiplyScalar -> SScalar
    AddVector shape -> shape
    Hadamard shape -> shape
    Dot{} -> SScalar
    SumVector{} -> SScalar
    TanhScalar -> SScalar
    TanhVector shape -> shape
    First left _ -> left
    Second _ right -> right
    ProjectValue projection -> projectionOutputShape projection

projectionOutputShape :: Projection environment selected -> SShape selected
projectionOutputShape projection = case projection of
    ProjectionHere shape -> shape
    ProjectionLeft inner _ -> projectionOutputShape inner
    ProjectionRight _ inner -> projectionOutputShape inner

primitiveCompilerWork :: Primitive scalar fragment parameters input output -> Natural
primitiveCompilerWork primitive =
    1
        + parameterShapeExtent (primitiveParameterShape primitive)
        + shapeExtent (primitiveInputShape primitive)
        + shapeExtent (primitiveOutputShape primitive)
        + 3 * primitiveArithmetic primitive

parameterShapeExtent :: SParameters parameters -> Natural
parameterShapeExtent parameters = case parameters of
    SNoParameters -> 0
    SOwner _ shape -> shapeExtent shape
    SParameterProduct left right -> parameterShapeExtent left + parameterShapeExtent right

primitiveArithmetic :: Primitive scalar fragment parameters input output -> Natural
primitiveArithmetic primitive = case primitive of
    ConstantScalar{} -> 0
    ConstantVector _ shape _ -> shapeExtent shape
    Parameter _ _ shape -> shapeExtent shape
    NegateScalar -> 1
    AddScalar -> 1
    MultiplyScalar -> 1
    AddVector shape -> shapeExtent shape
    Hadamard shape -> shapeExtent shape
    Dot shape -> 2 * shapeExtent shape
    SumVector shape -> shapeExtent shape
    TanhScalar -> 1
    TanhVector shape -> shapeExtent shape
    First{} -> 0
    Second{} -> 0
    ProjectValue{} -> 0

primitiveJVPWorkUnbounded :: Primitive scalar fragment parameters input output -> Natural
primitiveJVPWorkUnbounded primitive = case primitive of
    ConstantScalar{} -> 0
    ConstantVector{} -> 0
    Parameter{} -> 0
    NegateScalar -> 2
    AddScalar -> 2
    MultiplyScalar -> 4
    AddVector shape -> 2 * shapeExtent shape
    Hadamard shape -> 4 * shapeExtent shape
    Dot shape -> 1 + 6 * shapeExtent shape
    SumVector shape -> 2 * shapeExtent shape
    TanhScalar -> 0
    TanhVector{} -> 0
    First{} -> 0
    Second{} -> 0
    ProjectValue{} -> 0

scanRationals :: QuotationLimits -> String -> [Rational] -> Ledger -> Either QuoteError Ledger
scanRationals limits context = go 0
  where
    go _ [] ledger = pure ledger
    go coordinate (value : rest) initial = do
        ledger0 <- chargeTraversal limits initial
        let bits = max (integerBits (numerator value)) (integerBits (denominator value))
            bitLimit = rationalBitLimit limits
        if bits > bitLimit
            then Left (QuoteRationalMagnitudeLimitExceeded context coordinate bitLimit)
            else go (coordinate + 1) rest ledger0{ledgerRationalBits = max bits (ledgerRationalBits ledger0)}

chargeTraversal :: QuotationLimits -> Ledger -> Either QuoteError Ledger
chargeTraversal limits ledger = do
    count <- incrementBounded QuoteTraversalLimitExceeded (traversalLimit limits) (ledgerTraversal ledger)
    chargeTotal limits 1 ledger{ledgerTraversal = count}

chargeQuoteNode :: QuotationLimits -> Ledger -> Either QuoteError Ledger
chargeQuoteNode limits ledger = do
    count <- incrementBounded QuoteNodeLimitExceeded (quoteNodeLimit limits) (ledgerQuoteNodes ledger)
    pure ledger{ledgerQuoteNodes = count}

checkSourceDepth :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
checkSourceDepth limits depth ledger
    | depth > sourceDepthLimit limits = Left (QuoteSourceDepthLimitExceeded (sourceDepthLimit limits) depth)
    | otherwise = pure ledger{ledgerSourceDepth = max depth (ledgerSourceDepth ledger)}

checkPathDepth :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
checkPathDepth limits depth ledger
    | depth > pathDepthLimit limits = Left (QuotePathDepthLimitExceeded (pathDepthLimit limits) depth)
    | otherwise = pure ledger{ledgerPathDepth = max depth (ledgerPathDepth ledger)}

finishTargetDepth :: QuotationLimits -> Natural -> Ledger -> Either QuoteError (Natural, Ledger)
finishTargetDepth limits depth ledger
    | depth > targetDepthLimit limits = Left (QuoteTargetDepthLimitExceeded (targetDepthLimit limits) depth)
    | otherwise = pure (depth, ledger{ledgerTargetDepth = max depth (ledgerTargetDepth ledger)})

checkCoordinateExtent :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
checkCoordinateExtent limits extent ledger
    | extent > coordinateLimit limits = Left (QuoteCoordinateExtentLimitExceeded (coordinateLimit limits) extent)
    | otherwise = pure ledger{ledgerCoordinateExtent = max extent (ledgerCoordinateExtent ledger)}

chargeTargetNodes :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
chargeTargetNodes limits amount ledger = do
    count <- addBounded QuoteTargetNodeLimitExceeded (targetNodeLimit limits) (ledgerTargetNodes ledger) amount
    ledger1 <- chargeAllocation limits amount ledger{ledgerTargetNodes = count}
    chargeTotal limits amount ledger1

chargeTransformed :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
chargeTransformed limits amount ledger = do
    count <- addBounded QuoteTransformedNodeLimitExceeded (transformedLimit limits) (ledgerTransformedNodes ledger) amount
    chargeTotal limits amount ledger{ledgerTransformedNodes = count}

chargeAllocation :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
chargeAllocation limits amount ledger = do
    count <- addBounded QuoteAllocationLimitExceeded (allocationLimit limits) (ledgerAllocation ledger) amount
    chargeTotal limits amount ledger{ledgerAllocation = count}

chargeRuntime :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
chargeRuntime limits amount ledger = do
    count <- addBounded QuoteRuntimeWorkLimitExceeded (runtimeLimit limits) (ledgerRuntimeWork ledger) amount
    chargeTotal limits amount ledger{ledgerRuntimeWork = count}

chargeTotal :: QuotationLimits -> Natural -> Ledger -> Either QuoteError Ledger
chargeTotal limits amount ledger = do
    count <- addBounded QuoteTotalWorkLimitExceeded (totalLimit limits) (ledgerTotalWork ledger) amount
    pure ledger{ledgerTotalWork = count}

incrementBounded :: (Natural -> Natural -> QuoteError) -> Natural -> Natural -> Either QuoteError Natural
incrementBounded constructor limit current
    | current >= limit = Left (constructor limit (limit + 1))
    | otherwise = pure (current + 1)

addBounded :: (Natural -> Natural -> QuoteError) -> Natural -> Natural -> Natural -> Either QuoteError Natural
addBounded constructor limit current amount
    | amount > limit - min limit current = Left (constructor limit (limit + 1))
    | otherwise = pure (current + amount)

ledgerReport :: Ledger -> QuoteReport
ledgerReport ledger =
    QuoteReport
        (ledgerQuoteNodes ledger)
        (ledgerSourceDepth ledger)
        (ledgerPathDepth ledger)
        (ledgerTargetNodes ledger)
        (ledgerTargetDepth ledger)
        (ledgerCoordinateExtent ledger)
        (ledgerTransformedNodes ledger)
        (ledgerAllocation ledger)
        (ledgerRuntimeWork ledger)
        (ledgerTraversal ledger)
        (ledgerTotalWork ledger)
        (ledgerRationalBits ledger)

traversalLimit, quoteNodeLimit, sourceDepthLimit, pathDepthLimit, targetNodeLimit, targetDepthLimit, coordinateLimit, transformedLimit, allocationLimit, runtimeLimit, totalLimit :: QuotationLimits -> Natural
traversalLimit (QuotationLimitsValue value _ _ _ _ _ _ _ _ _ _ _) = value
quoteNodeLimit (QuotationLimitsValue _ value _ _ _ _ _ _ _ _ _ _) = value
sourceDepthLimit (QuotationLimitsValue _ _ value _ _ _ _ _ _ _ _ _) = value
pathDepthLimit (QuotationLimitsValue _ _ _ value _ _ _ _ _ _ _ _) = value
targetNodeLimit (QuotationLimitsValue _ _ _ _ value _ _ _ _ _ _ _) = value
targetDepthLimit (QuotationLimitsValue _ _ _ _ _ value _ _ _ _ _ _) = value
coordinateLimit (QuotationLimitsValue _ _ _ _ _ _ value _ _ _ _ _) = value
transformedLimit (QuotationLimitsValue _ _ _ _ _ _ _ value _ _ _ _) = value
allocationLimit (QuotationLimitsValue _ _ _ _ _ _ _ _ value _ _ _) = value
runtimeLimit (QuotationLimitsValue _ _ _ _ _ _ _ _ _ value _ _) = value
totalLimit (QuotationLimitsValue _ _ _ _ _ _ _ _ _ _ value _) = value

checkRational :: Natural -> String -> Natural -> Rational -> Maybe (String, Natural)
checkRational limit context coordinate value
    | integerBits (numerator value) <= limit && integerBits (denominator value) <= limit = Nothing
    | otherwise = Just (context, coordinate)

integerBits :: Integer -> Natural
integerBits = go 0 . abs
  where
    go bits 0 = bits
    go bits value = go (bits + 1) (value `quot` 2)

shapeExtent :: SShape shape -> Natural
shapeExtent SUnit = 0
shapeExtent SScalar = 1
shapeExtent vector@SVector = vectorExtent vector
shapeExtent (SProduct left right) = shapeExtent left + shapeExtent right

vectorExtent :: forall n. SShape ('Vector n) -> Natural
vectorExtent SVector = fromInteger (natVal (Proxy @n))

-- Build only after successful preflight.
buildQuote :: Quote scalar fragment environment parameters output -> Program scalar fragment parameters (EnvironmentShape environment) output
buildQuote quotation = D080_BUILD_QUOTE case quotation of
    ProgramQuote _ program -> program
    ProjectQuote path -> buildPath path
    ComposeQuote quoted program -> D080_GENERATED_PROGRAM compose (buildQuote quoted) program
    FanoutQuote left right -> D080_GENERATED_PROGRAM fanout (buildQuote left) (buildQuote right)
    LetQuote _ bound body -> D080_GENERATED_PROGRAM compose (D080_GENERATED_PROGRAM fanout (D080_GENERATED_PROGRAM identity (quoteEnvironmentShape bound)) (buildQuote bound)) (buildQuote body)

buildPath :: Path environment selected -> Program scalar fragment 'NoParameters (EnvironmentShape environment) selected
buildPath path = D080_BUILD_PATH D080_GENERATED_PROGRAM PrimitiveNode (D080_GENERATED_PROJECTION_PRIMITIVE ProjectValue (pathProjection path))

pathProjection :: Path environment selected -> Projection (EnvironmentShape environment) selected
pathProjection path = D080_PATH_PROJECTION case path of
    PathHere shape -> D080_PROJECTION_WITNESS ProjectionHere shape
    PathLeft inner right -> D080_PROJECTION_WITNESS ProjectionLeft (pathProjection inner) right
    PathRight environment right -> D080_PROJECTION_WITNESS ProjectionRight (environmentShape environment) (D080_PROJECTION_WITNESS ProjectionHere right)

pathEnvironment :: Path environment selected -> QuoteEnvironment environment
pathEnvironment path = case path of
    PathHere shape -> D080_ENVIRONMENT_WITNESS RootEnvironmentWitness shape
    PathLeft inner right -> D080_ENVIRONMENT_WITNESS BindEnvironmentWitness (pathEnvironment inner) right
    PathRight environment right -> D080_ENVIRONMENT_WITNESS BindEnvironmentWitness environment right

pathSelectedShape :: Path environment selected -> SShape selected
pathSelectedShape path = case path of
    PathHere shape -> shape
    PathLeft inner _ -> pathSelectedShape inner
    PathRight _ bound -> bound

environmentShape :: QuoteEnvironment environment -> SShape (EnvironmentShape environment)
environmentShape environment = case environment of
    RootEnvironmentWitness shape -> shape
    BindEnvironmentWitness parent bound -> D080_SHAPE_WITNESS SProduct (environmentShape parent) bound

quoteEnvironment :: Quote scalar fragment environment parameters output -> QuoteEnvironment environment
quoteEnvironment quotation = case quotation of
    ProgramQuote environment _ -> environment
    ProjectQuote path -> pathEnvironment path
    ComposeQuote quoted _ -> quoteEnvironment quoted
    FanoutQuote left _ -> quoteEnvironment left
    LetQuote _ bound _ -> quoteEnvironment bound

quoteEnvironmentShape :: Quote scalar fragment environment parameters output -> SShape (EnvironmentShape environment)
quoteEnvironmentShape = environmentShape . quoteEnvironment

quoteOutputShape :: Quote scalar fragment environment parameters output -> SShape output
quoteOutputShape quotation = case quotation of
    ProgramQuote _ program -> programOutputShape program
    ProjectQuote path -> pathSelectedShape path
    ComposeQuote _ program -> programOutputShape program
    FanoutQuote left right -> SProduct (quoteOutputShape left) (quoteOutputShape right)
    LetQuote _ _ body -> quoteOutputShape body

programOutputShape :: Program scalar fragment parameters input output -> SShape output
programOutputShape program = case program of
    PrimitiveNode primitive -> primitiveOutputShape primitive
    IdentityNode shape -> shape
    ComposeNode _ right -> programOutputShape right
    ParallelNode left right -> SProduct (programOutputShape left) (programOutputShape right)
    FanoutNode left right -> SProduct (programOutputShape left) (programOutputShape right)
    ShareParametersNode left right -> SProduct (programOutputShape left) (programOutputShape right)

programInputShape :: Program scalar fragment parameters input output -> SShape input
programInputShape program = case program of
    PrimitiveNode primitive -> primitiveInputShape primitive
    IdentityNode shape -> shape
    ComposeNode left _ -> programInputShape left
    ParallelNode left right -> SProduct (programInputShape left) (programInputShape right)
    FanoutNode left _ -> programInputShape left
    ShareParametersNode left right -> SProduct (programInputShape left) (programInputShape right)

primitiveInputShape :: Primitive scalar fragment parameters input output -> SShape input
primitiveInputShape primitive = case primitive of
    ConstantScalar shape _ -> shape
    ConstantVector shape _ _ -> shape
    Parameter _ shape _ -> shape
    NegateScalar -> SScalar
    AddScalar -> SProduct SScalar SScalar
    MultiplyScalar -> SProduct SScalar SScalar
    AddVector shape -> SProduct shape shape
    Hadamard shape -> SProduct shape shape
    Dot shape -> SProduct shape shape
    SumVector shape -> shape
    TanhScalar -> SScalar
    TanhVector shape -> shape
    First left right -> SProduct left right
    Second left right -> SProduct left right
    ProjectValue projection -> projectionInputShape projection

projectionInputShape :: Projection environment selected -> SShape environment
projectionInputShape projection = case projection of
    ProjectionHere shape -> shape
    ProjectionLeft inner right -> SProduct (projectionInputShape inner) right
    ProjectionRight left inner -> SProduct left (projectionInputShape inner)

followProjection :: Projection environment selected -> Value scalar environment -> Value scalar selected
followProjection projection value = case projection of
    ProjectionHere _ -> value
    ProjectionLeft inner _ -> let (left, _) = splitProductValue value in followProjection inner left
    ProjectionRight _ inner -> let (_, right) = splitProductValue value in followProjection inner right

-- Direct exact quotation evaluators.  They deliberately recurse over source
-- syntax instead of using Compile.interpretExactPolynomial.
evalQuotePrimal :: Natural -> Quote Rational 'Polynomial environment parameters output -> ParameterValue Rational parameters -> Value Rational (EnvironmentShape environment) -> Either QuoteError (Value Rational output)
evalQuotePrimal bits quotation parameters input = case quotation of
    ProgramQuote _ program -> evalProgramPrimal bits program parameters input
    ProjectQuote path -> validateValueBits bits "project-input" input >> Right (followPath path input)
    ComposeQuote quoted program -> do
        let (quotedParameters, programParameters) = splitParameterProduct parameters
        middle <- evalQuotePrimal bits quoted quotedParameters input
        evalProgramPrimal bits program programParameters middle
    FanoutQuote left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
        leftOutput <- evalQuotePrimal bits left leftParameters input
        rightOutput <- evalQuotePrimal bits right rightParameters input
        Right (ProductValue leftOutput rightOutput)
    LetQuote _ bound body -> do
        let (identityAndBound, bodyParameters) = splitParameterProduct parameters
            (_, boundParameters) = splitParameterProduct identityAndBound
        boundValue <- evalQuotePrimal bits bound boundParameters input
        evalQuotePrimal bits body bodyParameters (ProductValue input boundValue)

evalQuoteJVP :: Natural -> Quote Rational 'Polynomial environment parameters output -> ParameterValue Rational parameters -> ParameterValue Rational parameters -> Value Rational (EnvironmentShape environment) -> Value Rational (EnvironmentShape environment) -> Either QuoteError (Value Rational output, Value Rational output)
evalQuoteJVP bits quotation parameters parameterDirection input inputDirection = case quotation of
    ProgramQuote _ program -> evalProgramJVP bits program parameters parameterDirection input inputDirection
    ProjectQuote path -> do
        validateValueBits bits "project-input" input
        validateValueBits bits "project-input-direction" inputDirection
        Right (followPath path input, followPath path inputDirection)
    ComposeQuote quoted program -> do
        let (quotedParameters, programParameters) = splitParameterProduct parameters
            (quotedDirection, programDirection) = splitParameterProduct parameterDirection
        (middle, middleDirection) <- evalQuoteJVP bits quoted quotedParameters quotedDirection input inputDirection
        evalProgramJVP bits program programParameters programDirection middle middleDirection
    FanoutQuote left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
            (leftDirection, rightDirection) = splitParameterProduct parameterDirection
        (leftOutput, leftOutputDirection) <- evalQuoteJVP bits left leftParameters leftDirection input inputDirection
        (rightOutput, rightOutputDirection) <- evalQuoteJVP bits right rightParameters rightDirection input inputDirection
        Right (ProductValue leftOutput rightOutput, ProductValue leftOutputDirection rightOutputDirection)
    LetQuote _ bound body -> do
        let (identityAndBound, bodyParameters) = splitParameterProduct parameters
            (_, boundParameters) = splitParameterProduct identityAndBound
            (identityAndBoundDirection, bodyDirection) = splitParameterProduct parameterDirection
            (_, boundDirection) = splitParameterProduct identityAndBoundDirection
        (boundValue, boundValueDirection) <- evalQuoteJVP bits bound boundParameters boundDirection input inputDirection
        evalQuoteJVP bits body bodyParameters bodyDirection (ProductValue input boundValue) (ProductValue inputDirection boundValueDirection)

followPath :: Path environment selected -> Value scalar (EnvironmentShape environment) -> Value scalar selected
followPath path value = case path of
    PathHere _ -> value
    PathLeft inner _ -> let (left, _) = splitProductValue value in followPath inner left
    PathRight _ _ -> let (_, right) = splitProductValue value in right

evalProgramPrimal :: Natural -> Program Rational 'Polynomial parameters input output -> ParameterValue Rational parameters -> Value Rational input -> Either QuoteError (Value Rational output)
evalProgramPrimal bits program parameters input = case program of
    PrimitiveNode primitive -> evalPrimitivePrimal bits primitive parameters input
    IdentityNode _ -> validateValueBits bits "identity-input" input >> Right input
    ComposeNode left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
        middle <- evalProgramPrimal bits left leftParameters input
        evalProgramPrimal bits right rightParameters middle
    ParallelNode left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
            (leftInput, rightInput) = splitProductValue input
        leftOutput <- evalProgramPrimal bits left leftParameters leftInput
        rightOutput <- evalProgramPrimal bits right rightParameters rightInput
        Right (ProductValue leftOutput rightOutput)
    FanoutNode left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
        leftOutput <- evalProgramPrimal bits left leftParameters input
        rightOutput <- evalProgramPrimal bits right rightParameters input
        Right (ProductValue leftOutput rightOutput)
    ShareParametersNode left right -> do
        let (leftInput, rightInput) = splitProductValue input
        leftOutput <- evalProgramPrimal bits left parameters leftInput
        rightOutput <- evalProgramPrimal bits right parameters rightInput
        Right (ProductValue leftOutput rightOutput)

evalProgramJVP :: Natural -> Program Rational 'Polynomial parameters input output -> ParameterValue Rational parameters -> ParameterValue Rational parameters -> Value Rational input -> Value Rational input -> Either QuoteError (Value Rational output, Value Rational output)
evalProgramJVP bits program parameters parameterDirection input inputDirection = case program of
    PrimitiveNode primitive -> evalPrimitiveJVP bits primitive parameters parameterDirection input inputDirection
    IdentityNode _ -> do
        validateValueBits bits "identity-input" input
        validateValueBits bits "identity-direction" inputDirection
        Right (input, inputDirection)
    ComposeNode left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
            (leftDirection, rightDirection) = splitParameterProduct parameterDirection
        (middle, middleDirection) <- evalProgramJVP bits left leftParameters leftDirection input inputDirection
        evalProgramJVP bits right rightParameters rightDirection middle middleDirection
    ParallelNode left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
            (leftDirection, rightDirection) = splitParameterProduct parameterDirection
            (leftInput, rightInput) = splitProductValue input
            (leftInputDirection, rightInputDirection) = splitProductValue inputDirection
        (leftOutput, leftOutputDirection) <- evalProgramJVP bits left leftParameters leftDirection leftInput leftInputDirection
        (rightOutput, rightOutputDirection) <- evalProgramJVP bits right rightParameters rightDirection rightInput rightInputDirection
        Right (ProductValue leftOutput rightOutput, ProductValue leftOutputDirection rightOutputDirection)
    FanoutNode left right -> do
        let (leftParameters, rightParameters) = splitParameterProduct parameters
            (leftDirection, rightDirection) = splitParameterProduct parameterDirection
        (leftOutput, leftOutputDirection) <- evalProgramJVP bits left leftParameters leftDirection input inputDirection
        (rightOutput, rightOutputDirection) <- evalProgramJVP bits right rightParameters rightDirection input inputDirection
        Right (ProductValue leftOutput rightOutput, ProductValue leftOutputDirection rightOutputDirection)
    ShareParametersNode left right -> do
        let (leftInput, rightInput) = splitProductValue input
            (leftInputDirection, rightInputDirection) = splitProductValue inputDirection
        (leftOutput, leftOutputDirection) <- evalProgramJVP bits left parameters parameterDirection leftInput leftInputDirection
        (rightOutput, rightOutputDirection) <- evalProgramJVP bits right parameters parameterDirection rightInput rightInputDirection
        Right (ProductValue leftOutput rightOutput, ProductValue leftOutputDirection rightOutputDirection)

evalPrimitivePrimal :: Natural -> Primitive Rational 'Polynomial parameters input output -> ParameterValue Rational parameters -> Value Rational input -> Either QuoteError (Value Rational output)
evalPrimitivePrimal bits primitive parameters input = do
    validateParametersBits bits "parameter" parameters
    validateValueBits bits "primitive-input" input
    output <- case primitive of
        ConstantScalar _ value -> ScalarValue <$> checked bits "constant-scalar" 0 value
        ConstantVector _ _ value -> validateValueBits bits "constant-vector" value >> Right value
        Parameter{} -> Right (ownedParameterValue parameters)
        NegateScalar -> ScalarValue <$> checked bits "negate" 0 (negate (scalarFromValue input))
        AddScalar ->
            let (left, right) = scalarPair input
             in ScalarValue <$> checked bits "add" 0 (left + right)
        MultiplyScalar ->
            let (left, right) = scalarPair input
             in ScalarValue <$> checked bits "multiply" 0 (left * right)
        AddVector _ -> do
            let (left, right) = vectorPair input
            VectorValue <$> checkedZip2 bits "vector-add" (+) left right
        Hadamard _ -> do
            let (left, right) = vectorPair input
            VectorValue <$> checkedZip2 bits "hadamard" (*) left right
        Dot _ ->
            let (left, right) = vectorPair input
             in ScalarValue <$> checkedDot bits "dot" left right
        SumVector _ -> ScalarValue <$> checkedSum bits "sum" (vectorFromValue input)
        First _ _ -> let (left, _) = splitProductValue input in Right left
        Second _ _ -> let (_, right) = splitProductValue input in Right right
        ProjectValue projection -> Right (followProjection projection input)
    validateValueBits bits "primitive-output" output
    Right output

evalPrimitiveJVP :: Natural -> Primitive Rational 'Polynomial parameters input output -> ParameterValue Rational parameters -> ParameterValue Rational parameters -> Value Rational input -> Value Rational input -> Either QuoteError (Value Rational output, Value Rational output)
evalPrimitiveJVP bits primitive parameters parameterDirection input inputDirection = do
    validateParametersBits bits "parameter" parameters
    validateParametersBits bits "parameter-direction" parameterDirection
    validateValueBits bits "primitive-input" input
    validateValueBits bits "primitive-input-direction" inputDirection
    result <- case primitive of
        ConstantScalar _ value -> do
            _ <- checked bits "constant-scalar" 0 value
            Right (ScalarValue value, ScalarValue 0)
        ConstantVector _ _ value -> do
            validateValueBits bits "constant-vector" value
            Right (value, zeroValueLike value)
        Parameter{} -> Right (ownedParameterValue parameters, ownedParameterValue parameterDirection)
        NegateScalar -> do
            primal <- checked bits "negate" 0 (negate (scalarFromValue input))
            tangent <- checked bits "jvp/negate" 0 (negate (scalarFromValue inputDirection))
            Right (ScalarValue primal, ScalarValue tangent)
        AddScalar -> do
            let (left, right) = scalarPair input
                (dLeft, dRight) = scalarPair inputDirection
            primal <- checked bits "add" 0 (left + right)
            tangent <- checked bits "jvp/add" 0 (dLeft + dRight)
            Right (ScalarValue primal, ScalarValue tangent)
        MultiplyScalar -> do
            let (left, right) = scalarPair input
                (dLeft, dRight) = scalarPair inputDirection
            primal <- checked bits "multiply" 0 (left * right)
            leftTerm <- checked bits "jvp/multiply-left" 0 (dLeft * right)
            rightTerm <- checked bits "jvp/multiply-right" 0 (left * dRight)
            tangent <- checked bits "jvp/multiply-add" 0 (leftTerm + rightTerm)
            Right (ScalarValue primal, ScalarValue tangent)
        AddVector _ -> binaryVectorJVP bits "vector-add" (+) (\_ _ dLeft dRight -> dLeft + dRight) input inputDirection
        Hadamard _ -> hadamardVectorJVP bits input inputDirection
        Dot _ -> do
            let (left, right) = vectorPair input
                (dLeft, dRight) = vectorPair inputDirection
            primal <- checkedDot bits "dot" left right
            leftTangent <- checkedDot bits "jvp/dot-left" dLeft right
            rightTangent <- checkedDot bits "jvp/dot-right" left dRight
            tangent <- checked bits "jvp/dot-add" 0 (leftTangent + rightTangent)
            Right (ScalarValue primal, ScalarValue tangent)
        SumVector _ -> do
            primal <- checkedSum bits "sum" (vectorFromValue input)
            tangent <- checkedSum bits "jvp/sum" (vectorFromValue inputDirection)
            Right (ScalarValue primal, ScalarValue tangent)
        First _ _ ->
            let (left, _) = splitProductValue input
                (dLeft, _) = splitProductValue inputDirection
             in Right (left, dLeft)
        Second _ _ ->
            let (_, right) = splitProductValue input
                (_, dRight) = splitProductValue inputDirection
             in Right (right, dRight)
        ProjectValue projection -> Right (followProjection projection input, followProjection projection inputDirection)
    validateValueBits bits "primitive-output" (fst result)
    validateValueBits bits "primitive-output-direction" (snd result)
    Right result

hadamardVectorJVP :: Natural -> Value Rational ('Product ('Vector n) ('Vector n)) -> Value Rational ('Product ('Vector n) ('Vector n)) -> Either QuoteError (Value Rational ('Vector n), Value Rational ('Vector n))
hadamardVectorJVP bits input inputDirection =
    let (left, right) = vectorPair input
        (dLeft, dRight) = vectorPair inputDirection
     in if not (sameLengths [left, right, dLeft, dRight])
            then Left (QuoteInternalVectorLengthMismatch "hadamard")
            else do
                (primal, tangent) <- go 0 left right dLeft dRight
                Right (VectorValue primal, VectorValue tangent)
  where
    go _ [] [] [] [] = Right ([], [])
    go coordinate (left : leftRest) (right : rightRest) (dLeft : dLeftRest) (dRight : dRightRest) = do
        primal <- checked bits "hadamard" coordinate (left * right)
        leftTerm <- checked bits "jvp/hadamard-left" coordinate (dLeft * right)
        rightTerm <- checked bits "jvp/hadamard-right" coordinate (left * dRight)
        tangent <- checked bits "jvp/hadamard-add" coordinate (leftTerm + rightTerm)
        (primalRest, tangentRest) <- go (coordinate + 1) leftRest rightRest dLeftRest dRightRest
        Right (primal : primalRest, tangent : tangentRest)
    go _ _ _ _ _ = Left (QuoteInternalVectorLengthMismatch "hadamard")

binaryVectorJVP :: Natural -> String -> (Rational -> Rational -> Rational) -> (Rational -> Rational -> Rational -> Rational -> Rational) -> Value Rational ('Product ('Vector n) ('Vector n)) -> Value Rational ('Product ('Vector n) ('Vector n)) -> Either QuoteError (Value Rational ('Vector n), Value Rational ('Vector n))
binaryVectorJVP bits context primalOperation tangentOperation input inputDirection = do
    let (left, right) = vectorPair input
        (dLeft, dRight) = vectorPair inputDirection
    if not (sameLengths [left, right, dLeft, dRight])
        then Left (QuoteInternalVectorLengthMismatch context)
        else do
            primal <- checkedZip4 bits context (\l r _ _ -> primalOperation l r) left right dLeft dRight
            tangent <- checkedZip4 bits ("jvp/" ++ context) tangentOperation left right dLeft dRight
            Right (VectorValue primal, VectorValue tangent)

sameLengths :: [[value]] -> Bool
sameLengths [] = True
sameLengths (firstList : rest) = all ((== length firstList) . length) rest

checkedZip2 :: Natural -> String -> (Rational -> Rational -> Rational) -> [Rational] -> [Rational] -> Either QuoteError [Rational]
checkedZip2 bits context operation = go 0
  where
    go _ [] [] = Right []
    go coordinate (left : leftRest) (right : rightRest) = do
        value <- checked bits context coordinate (operation left right)
        (value :) <$> go (coordinate + 1) leftRest rightRest
    go _ _ _ = Left (QuoteInternalVectorLengthMismatch context)

checkedZip4 :: Natural -> String -> (Rational -> Rational -> Rational -> Rational -> Rational) -> [Rational] -> [Rational] -> [Rational] -> [Rational] -> Either QuoteError [Rational]
checkedZip4 bits context operation = go 0
  where
    go _ [] [] [] [] = Right []
    go coordinate (a : as) (b : bs) (c : cs) (d : ds) = do
        value <- checked bits context coordinate (operation a b c d)
        (value :) <$> go (coordinate + 1) as bs cs ds
    go _ _ _ _ _ = Left (QuoteInternalVectorLengthMismatch context)

checkedDot :: Natural -> String -> [Rational] -> [Rational] -> Either QuoteError Rational
checkedDot bits context left right
    | length left /= length right = Left (QuoteInternalVectorLengthMismatch context)
    | otherwise = go 0 0 left right
  where
    go _ accumulator [] [] = Right accumulator
    go coordinate accumulator (x : xs) (y : ys) = do
        productValue' <- checked bits (context ++ "/multiply") coordinate (x * y)
        next <- checked bits (context ++ "/add") coordinate (accumulator + productValue')
        go (coordinate + 1) next xs ys
    go _ _ _ _ = Left (QuoteInternalVectorLengthMismatch context)

checkedSum :: Natural -> String -> [Rational] -> Either QuoteError Rational
checkedSum bits context = go 0 0
  where
    go _ accumulator [] = Right accumulator
    go coordinate accumulator (value : rest) = do
        next <- checked bits context coordinate (accumulator + value)
        go (coordinate + 1) next rest

checked :: Natural -> String -> Natural -> Rational -> Either QuoteError Rational
checked bits context coordinate value = case checkRational bits context coordinate value of
    Nothing -> Right value
    Just _ -> Left (QuoteRationalMagnitudeLimitExceeded context coordinate bits)

validateValueBits :: Natural -> String -> Value Rational shape -> Either QuoteError ()
validateValueBits bits context = go 0 . valueScalars
  where
    go _ [] = Right ()
    go coordinate (value : rest) = checked bits context coordinate value >> go (coordinate + 1) rest

validateParametersBits :: Natural -> String -> ParameterValue Rational parameters -> Either QuoteError ()
validateParametersBits bits context = go 0 . parameterScalars
  where
    go _ [] = Right ()
    go coordinate (value : rest) = checked bits context coordinate value >> go (coordinate + 1) rest

zeroValueLike :: Value Rational shape -> Value Rational shape
zeroValueLike UnitValue = UnitValue
zeroValueLike (ScalarValue _) = ScalarValue 0
zeroValueLike (VectorValue values) = VectorValue (map (const 0) values)
zeroValueLike (ProductValue left right) = ProductValue (zeroValueLike left) (zeroValueLike right)

scalarPair :: Value Rational ('Product 'Scalar 'Scalar) -> (Rational, Rational)
scalarPair value = let (left, right) = splitProductValue value in (scalarFromValue left, scalarFromValue right)

vectorPair :: Value Rational ('Product ('Vector n) ('Vector n)) -> ([Rational], [Rational])
vectorPair value = let (left, right) = splitProductValue value in (vectorFromValue left, vectorFromValue right)
