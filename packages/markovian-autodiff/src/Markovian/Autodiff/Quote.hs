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
    -- ^ Conservative direct-output and target-node allocation units.
    , quoteRuntimeWork :: !Natural
    -- ^ Scalar arithmetic for one primal-and-JVP run.
    , quoteTraversalWork :: !Natural
    -- ^ Syntax, path, and literal coordinates visited in preflight.
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

{- | Use the same limit record for quotation and exact target compilation.
The result carries both deterministic reports.
-}
compileExactQuote :: QuotationLimits -> TapePolicy -> Quote Rational 'Polynomial environment parameters output -> Either QuoteCompileError (ExactExecutable parameters (EnvironmentShape environment) output, QuoteCompilationReport)
compileExactQuote limits policy quotation = do
    preflight <- mapLeft QuoteCompilePreflightFailure (preflightQuote limits quotation)
    let program = buildQuote quotation
        compiler = limitsCompiler limits
    executable <- mapLeft QuoteCompileTargetFailure (compileExactPolynomial compiler policy program)
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

limitsCompiler :: QuotationLimits -> CompilerLimits
limitsCompiler (QuotationLimitsValue _ _ _ _ targetNodes targetDepth coordinates _ allocation runtime _ bits) =
    compilerLimits targetNodes allocation targetDepth targetNodes coordinates coordinates allocation targetDepth runtime bits

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
buildQuote quotation = case quotation of
    ProgramQuote _ program -> program
    ProjectQuote path -> buildPath path
    ComposeQuote quoted program -> compose (buildQuote quoted) program
    FanoutQuote left right -> fanout (buildQuote left) (buildQuote right)
    LetQuote _ bound body -> compose (fanout (identity (quoteEnvironmentShape bound)) (buildQuote bound)) (buildQuote body)

buildPath :: Path environment selected -> Program scalar fragment 'NoParameters (EnvironmentShape environment) selected
buildPath = PrimitiveNode . ProjectValue . pathProjection

pathProjection :: Path environment selected -> Projection (EnvironmentShape environment) selected
pathProjection path = case path of
    PathHere shape -> ProjectionHere shape
    PathLeft inner right -> ProjectionLeft (pathProjection inner) right
    PathRight environment right -> ProjectionRight (environmentShape environment) (ProjectionHere right)

pathEnvironment :: Path environment selected -> QuoteEnvironment environment
pathEnvironment path = case path of
    PathHere shape -> RootEnvironmentWitness shape
    PathLeft inner right -> BindEnvironmentWitness (pathEnvironment inner) right
    PathRight environment right -> BindEnvironmentWitness environment right

pathSelectedShape :: Path environment selected -> SShape selected
pathSelectedShape path = case path of
    PathHere shape -> shape
    PathLeft inner _ -> pathSelectedShape inner
    PathRight _ bound -> bound

environmentShape :: QuoteEnvironment environment -> SShape (EnvironmentShape environment)
environmentShape environment = case environment of
    RootEnvironmentWitness shape -> shape
    BindEnvironmentWitness parent bound -> SProduct (environmentShape parent) bound

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
