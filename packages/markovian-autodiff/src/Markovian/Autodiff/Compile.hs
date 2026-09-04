{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Bounded lowering to the owned reverse-program interpreter.

Polynomial programs have separate exact 'Rational' and checked 'Double'
executables. Smooth programs add only @tanh@ and have no exact compiler.
Reverse results are transposed Jacobian actions under the declared finite
coordinate pairing; they are not matrix daggers, Bayesian inverses, payoff
pullbacks, feedback, strategic duals, or disintegrations.
-}
module Markovian.Autodiff.Compile (
    TapePolicy (..),
    CompilerLimits,
    compilerLimits,
    DoubleComparisonPolicy (..),
    doubleComparisonPolicy,
    renderDoubleComparisonPolicy,
    AutodiffFailure (..),
    CompileError,
    ExactExecutable,
    DoubleExecutable,
    compileExactPolynomial,
    compileDoublePolynomial,
    compileDoubleSmooth,
    CompileReport,
    exactCompileReport,
    doubleCompileReport,
    renderCompileReport,
    ExactRun,
    DoubleRun,
    exactRunOutput,
    doubleRunOutput,
    ExactTape,
    DoubleTape,
    runExact,
    runDouble,
    exactRunTape,
    doubleRunTape,
    applyExactTape,
    applyDoubleTape,
    applyExactVJP,
    applyDoubleVJP,
    interpretExactPolynomial,
    interpretDoublePolynomial,
    interpretDoubleSmooth,
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import GHC.TypeLits (natVal)
import Markovian.Autodiff.Internal.Shape
import Markovian.Autodiff.Internal.Syntax
import Markovian.Reverse (
    CotangentEqualityMode (..),
    CotangentSpace,
    FiniteLayout,
    ReverseEvaluation,
    applyReverseVJP,
    declaredCotangentSpace,
    finiteLayout,
    finiteLayoutDescription,
    primitiveReverseCircuit,
    productFiniteLayout,
    reverseEvaluation,
    reversePrimalOutput,
    unitFiniteLayout,
 )
import Markovian.Reverse.Program (
    FinitePrimalSpace,
    OwnedReversePrimitive,
    ParameterOwnership,
    PreparedReverseProgram,
    PrimitiveTapePolicy (..),
    ReverseDefinitionError,
    ReverseLimits,
    ReverseProgram,
    ReverseProgramError (..),
    ReverseProgramReport,
    ReverseRun,
    ReverseStage (..),
    ReverseTape,
    applyReverseTape,
    composeProgram,
    finitePrimalSpace,
    identityProgram,
    noParameterOwnership,
    ownedReversePrimitive,
    ownedReversePrimitiveWithRecomputation,
    pairInputProgram,
    parameterOwner,
    parameterOwnershipProduct,
    prepareReverseProgram,
    preparedReverseProgramReport,
    primitiveProgram,
    primitiveRecomputation,
    renderReverseProgramReport,
    reverseLimitsWithStructure,
    reverseRunOutput,
    reverseRunTape,
    runPreparedReverse,
    shareParameterProgram,
    tensorProgram,
 )
import Numeric.Natural (Natural)

{- | Primitive tape policy. This is whole-primitive storage or recomputation,
not general checkpoint scheduling.
-}
data TapePolicy = StorePullbacks | RecomputePullbacks
    deriving (Eq, Show)

-- | Structural, per-run scalar-work, and exact-rational magnitude limits.
data CompilerLimits = CompilerLimits !ReverseLimits !Natural !Natural

{- | Build limits for source nodes, primitive nodes, depth, owners, primal
extent, cotangent extent, structure nodes, structure depth, scalar work for
one forward or reverse execution, and numerator/denominator bits for exact
coordinates. A rational bit limit of zero admits only zero numerators and the
unit denominator.
-}
compilerLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> CompilerLimits
compilerLimits nodes primitives depth owners primalExtent cotangentExtent structureNodes structureDepth =
    CompilerLimits (reverseLimitsWithStructure nodes primitives depth owners primalExtent cotangentExtent structureNodes structureDepth)

-- | Immutable policy used by all checked-Double comparisons.
data DoubleComparisonPolicy = DoubleComparisonPolicy
    { absoluteTolerance :: !Double
    , relativeTolerance :: !Double
    }
    deriving (Eq, Show)

-- | Named comparison policy for recomputation and differential evidence.
doubleComparisonPolicy :: DoubleComparisonPolicy
doubleComparisonPolicy = DoubleComparisonPolicy 2e-10 2e-8

-- | Stable comparison-policy rendering used by reports.
renderDoubleComparisonPolicy :: DoubleComparisonPolicy -> String
renderDoubleComparisonPolicy policy =
    "abs<=" ++ show (absoluteTolerance policy) ++ " or rel<=" ++ show (relativeTolerance policy)

{- | Checked execution failures. No run, tape, or gradient is returned on one
of these failures.
-}
data AutodiffFailure
    = NonFiniteCoordinate !String !Natural
    | RationalMagnitudeLimitExceeded !String !Natural !Natural
    | MachineVectorExtentExceeded !Natural
    | ExecutionWorkLimitExceeded !Natural !Natural
    | InternalVectorLengthMismatch !String
    deriving (Eq, Show)

-- | Opaque structured source, target, execution, and pullback failure.
newtype CompileError = CompileError (ReverseProgramError AutodiffFailure)
    deriving (Eq)

instance Show CompileError where
    showsPrec precedence (CompileError problem) = showsPrec precedence problem

-- Runtime representations deliberately follow the target's structural tuples.
type RuntimeValue :: Type -> Shape -> Type
type family RuntimeValue scalar shape where
    RuntimeValue scalar 'Unit = ()
    RuntimeValue scalar 'Scalar = scalar
    RuntimeValue scalar ('Vector n) = [scalar]
    RuntimeValue scalar ('Product left right) = (RuntimeValue scalar left, RuntimeValue scalar right)

type RuntimeParameters :: Type -> Parameters -> Type
type family RuntimeParameters scalar parameters where
    RuntimeParameters scalar 'NoParameters = ()
    RuntimeParameters scalar ('ParameterOwner owner shape) = RuntimeValue scalar shape
    RuntimeParameters scalar ('ParameterProduct left right) = (RuntimeParameters scalar left, RuntimeParameters scalar right)

-- The target primitive is closed and cannot hold a caller callback.
data TargetPrimitive scalar fragment parameter parameterCotangent input inputCotangent output outputCotangent where
    TargetPrimitive ::
        Primitive scalar fragment parameters input output ->
        TargetPrimitive
            scalar
            fragment
            (RuntimeParameters scalar parameters)
            (RuntimeParameters scalar parameters)
            (RuntimeValue scalar input)
            (RuntimeValue scalar input)
            (RuntimeValue scalar output)
            (RuntimeValue scalar output)

type TargetProgram scalar fragment parameters input output =
    ReverseProgram
        (TargetPrimitive scalar fragment)
        AutodiffFailure
        scalar
        (RuntimeParameters scalar parameters)
        (RuntimeParameters scalar parameters)
        (RuntimeValue scalar input)
        (RuntimeValue scalar input)
        (RuntimeValue scalar output)
        (RuntimeValue scalar output)

type Prepared scalar fragment parameters input output =
    PreparedReverseProgram
        (TargetPrimitive scalar fragment)
        AutodiffFailure
        scalar
        (RuntimeParameters scalar parameters)
        (RuntimeParameters scalar parameters)
        (RuntimeValue scalar input)
        (RuntimeValue scalar input)
        (RuntimeValue scalar output)
        (RuntimeValue scalar output)

-- | Exact formal-polynomial executable.
data ExactExecutable parameters input output = ExactExecutable !(SParameters parameters) !(SShape input) !(SShape output) !(Prepared Rational 'Polynomial parameters input output) !CompileReport

type role ExactExecutable nominal nominal nominal

-- | Checked-Double executable for one closed fragment.
data DoubleExecutable (fragment :: Fragment) parameters input output = DoubleExecutable !(SParameters parameters) !(SShape input) !(SShape output) !(Prepared Double fragment parameters input output) !CompileReport

type role DoubleExecutable nominal nominal nominal nominal

-- | Deterministic timing-free lowering inventory.
data CompileReport = CompileReport !String !TapePolicy !Natural !Natural !ReverseProgramReport
    deriving (Eq, Show)

-- | Exact executable report.
exactCompileReport :: ExactExecutable parameters input output -> CompileReport
exactCompileReport (ExactExecutable _ _ _ _ report) = report

-- | Double executable report.
doubleCompileReport :: DoubleExecutable fragment parameters input output -> CompileReport
doubleCompileReport (DoubleExecutable _ _ _ _ report) = report

-- | Stable report renderer. Counts are structural and are not byte or timing estimates.
renderCompileReport :: CompileReport -> String
renderCompileReport (CompileReport semantics policy forwardWork reverseWork target) =
    unlines
        [ "autodiff-compile-report"
        , "semantics: " ++ semantics
        , "tape-policy: " ++ show policy
        , "forward-scalar-work: " ++ show forwardWork
        , "reverse-scalar-work: " ++ show reverseWork
        , "double-comparison-policy: " ++ renderDoubleComparisonPolicy doubleComparisonPolicy
        , "target: markovian-reverse-owned-program"
        ]
        ++ renderReverseProgramReport target

-- | Compile the polynomial fragment with literal 'Rational' arithmetic.
compileExactPolynomial :: CompilerLimits -> TapePolicy -> Program Rational 'Polynomial parameters input output -> Either CompileError (ExactExecutable parameters input output)
compileExactPolynomial compiler@(CompilerLimits limits _ rationalBits) policy program = do
    let backend = exactBackend rationalBits
    prepared <- mapTargetError (prepareReverseProgram limits (resolveTargetPrimitive backend policy) (lower backend program))
    (forwardWork, reverseWork) <- preflightSource compiler program
    let report = CompileReport "exact-rational-formal-polynomial" policy forwardWork reverseWork (preparedReverseProgramReport prepared)
    Right (ExactExecutable (programParameters program) (programInput program) (programOutput program) prepared report)

-- | Compile polynomial syntax with checked finite 'Double' execution.
compileDoublePolynomial :: CompilerLimits -> TapePolicy -> Program Double 'Polynomial parameters input output -> Either CompileError (DoubleExecutable 'Polynomial parameters input output)
compileDoublePolynomial = compileDoubleWith "checked-double-polynomial"

-- | Compile smooth syntax. The only non-polynomial primitive is @tanh@.
compileDoubleSmooth :: CompilerLimits -> TapePolicy -> Program Double 'Smooth parameters input output -> Either CompileError (DoubleExecutable 'Smooth parameters input output)
compileDoubleSmooth = compileDoubleWith "checked-double-smooth"

compileDoubleWith :: String -> CompilerLimits -> TapePolicy -> Program Double fragment parameters input output -> Either CompileError (DoubleExecutable fragment parameters input output)
compileDoubleWith semantics compiler@(CompilerLimits limits _ _) policy program = do
    prepared <- mapTargetError (prepareReverseProgram limits (resolveTargetPrimitive doubleBackend policy) (lower doubleBackend program))
    (forwardWork, reverseWork) <- preflightSource compiler program
    let report = CompileReport semantics policy forwardWork reverseWork (preparedReverseProgramReport prepared)
    Right (DoubleExecutable (programParameters program) (programInput program) (programOutput program) prepared report)

-- Source work is deterministic because the language has no branches or
-- recursion. Preparation bounds the tree first; this pass then rejects each
-- machine-unrepresentable vector and conservatively charges coordinate
-- validation, primal arithmetic, tape construction, and pullback arithmetic.
preflightSource :: CompilerLimits -> Program scalar fragment parameters input output -> Either CompileError (Natural, Natural)
preflightSource (CompilerLimits _ workLimit _) = go
  where
    go :: Program scalar fragment p x y -> Either CompileError (Natural, Natural)
    go (PrimitiveNode primitive) = do
        checkParameters (primitiveParameters primitive)
        inputExtent <- checkedShapeExtent (primitiveInput primitive)
        outputExtent <- checkedShapeExtent (primitiveOutput primitive)
        parameterExtent <- checkedParameterExtent (primitiveParameters primitive)
        let arithmetic = primitiveArithmetic primitive
        forward <- charge (1 + parameterExtent + inputExtent + outputExtent + arithmetic)
        reverseCost <- charge (1 + parameterExtent + inputExtent + outputExtent + 3 * arithmetic)
        Right (forward, reverseCost)
    go (IdentityNode shape) = do
        extent <- checkedShapeExtent shape
        cost <- charge (1 + extent)
        Right (cost, cost)
    go (ComposeNode left right) = combine (go left) (go right)
    go (ParallelNode left right) = combine (go left) (go right)
    go (FanoutNode left right) = combine (go left) (go right)
    go (ShareParametersNode left right) = combine (go left) (go right)

    combine leftResult rightResult = do
        (leftForward, leftReverse) <- leftResult
        (rightForward, rightReverse) <- rightResult
        forward <- cappedAdd leftForward rightForward >>= charge . (+ 1)
        reverseCost <- cappedAdd leftReverse rightReverse >>= charge . (+ 1)
        Right (forward, reverseCost)

    charge required
        | required > workLimit = Left (sourceFailure (ExecutionWorkLimitExceeded workLimit required))
        | otherwise = Right required

    cappedAdd left right
        | left > workLimit - min workLimit right = Left (sourceFailure (ExecutionWorkLimitExceeded workLimit (workLimit + 1)))
        | otherwise = Right (left + right)

sourceFailure :: AutodiffFailure -> CompileError
sourceFailure = CompileError . ReversePrimalValidationFailure [] InputPrimalStage

checkedShapeExtent :: SShape shape -> Either CompileError Natural
checkedShapeExtent SUnit = Right 0
checkedShapeExtent SScalar = Right 1
checkedShapeExtent shape@SVector = do
    let extent = vectorExtent shape
    if extent > fromIntegral (maxBound :: Int)
        then Left (sourceFailure (MachineVectorExtentExceeded extent))
        else Right extent
checkedShapeExtent (SProduct left right) = (+) <$> checkedShapeExtent left <*> checkedShapeExtent right

checkedParameterExtent :: SParameters parameters -> Either CompileError Natural
checkedParameterExtent SNoParameters = Right 0
checkedParameterExtent (SOwner _ shape) = checkedShapeExtent shape
checkedParameterExtent (SParameterProduct left right) = (+) <$> checkedParameterExtent left <*> checkedParameterExtent right

checkParameters :: SParameters parameters -> Either CompileError ()
checkParameters parameters = checkedParameterExtent parameters >> Right ()

vectorExtent :: forall n. SShape ('Vector n) -> Natural
vectorExtent SVector = fromInteger (natVal (Proxy @n))

primitiveArithmetic :: Primitive scalar fragment parameters input output -> Natural
primitiveArithmetic primitive = case primitive of
    ConstantScalar{} -> 0
    ConstantVector _ shape _ -> vectorExtent shape
    Parameter _ _ shape -> shapeArithmetic shape
    NegateScalar -> 1
    AddScalar -> 1
    MultiplyScalar -> 1
    AddVector shape -> vectorExtent shape
    Hadamard shape -> vectorExtent shape
    Dot shape -> 2 * vectorExtent shape
    SumVector shape -> vectorExtent shape
    TanhScalar -> 1
    TanhVector shape -> vectorExtent shape
    First{} -> 0
    Second{} -> 0
    ProjectValue{} -> 0
  where
    shapeArithmetic :: SShape shape -> Natural
    shapeArithmetic SUnit = 0
    shapeArithmetic SScalar = 1
    shapeArithmetic vector@SVector = vectorExtent vector
    shapeArithmetic (SProduct left right) = shapeArithmetic left + shapeArithmetic right

lower :: Backend scalar -> Program scalar fragment parameters input output -> TargetProgram scalar fragment parameters input output
lower _ (PrimitiveNode primitive) = primitiveProgram (TargetPrimitive primitive)
lower backend (IdentityNode shape) = identityProgram (runtimePrimalSpace backend shape) (runtimeValueCotangentSpace backend shape)
lower backend (ComposeNode left right) = composeProgram (lower backend left) (lower backend right)
lower backend (ParallelNode left right) = tensorProgram (lower backend left) (lower backend right)
lower backend (FanoutNode left right) = pairInputProgram (lower backend left) (lower backend right)
lower backend (ShareParametersNode left right) = shareParameterProgram (lower backend left) (lower backend right)

-- | One exact run. Its tape is self-contained and endpoint-indexed.
data ExactRun parameters input output = ExactRun !(SParameters parameters) !(SShape input) !(SShape output) !(ReverseRun AutodiffFailure Rational (RuntimeParameters Rational parameters) (RuntimeParameters Rational parameters) (RuntimeValue Rational input) (RuntimeValue Rational input) (RuntimeValue Rational output) (RuntimeValue Rational output))

type role ExactRun nominal nominal nominal

-- | One checked-Double run.
data DoubleRun (fragment :: Fragment) parameters input output = DoubleRun !(SParameters parameters) !(SShape input) !(SShape output) !(ReverseRun AutodiffFailure Double (RuntimeParameters Double parameters) (RuntimeParameters Double parameters) (RuntimeValue Double input) (RuntimeValue Double input) (RuntimeValue Double output) (RuntimeValue Double output))

type role DoubleRun nominal nominal nominal nominal

-- | Opaque exact tape.
data ExactTape parameters input output = ExactTape !(SParameters parameters) !(SShape input) !(SShape output) !(ReverseTape AutodiffFailure Rational (RuntimeParameters Rational parameters) (RuntimeParameters Rational parameters) (RuntimeValue Rational input) (RuntimeValue Rational input) (RuntimeValue Rational output) (RuntimeValue Rational output))

type role ExactTape nominal nominal nominal

-- | Opaque checked-Double tape.
data DoubleTape (fragment :: Fragment) parameters input output = DoubleTape !(SParameters parameters) !(SShape input) !(SShape output) !(ReverseTape AutodiffFailure Double (RuntimeParameters Double parameters) (RuntimeParameters Double parameters) (RuntimeValue Double input) (RuntimeValue Double input) (RuntimeValue Double output) (RuntimeValue Double output))

type role DoubleTape nominal nominal nominal nominal

-- | Execute one exact program and retain its opaque reverse tape.
runExact :: ExactExecutable parameters input output -> ParameterValue Rational parameters -> Value Rational input -> Either CompileError (ExactRun parameters input output)
runExact (ExactExecutable parametersShape inputShape outputShape executable _) parameters input =
    ExactRun parametersShape inputShape outputShape <$> mapTargetError (runPreparedReverse executable (unwrapParameters parameters) (unwrapValue input))

-- | Execute one checked-Double program and retain its opaque reverse tape.
runDouble :: DoubleExecutable fragment parameters input output -> ParameterValue Double parameters -> Value Double input -> Either CompileError (DoubleRun fragment parameters input output)
runDouble (DoubleExecutable parametersShape inputShape outputShape executable _) parameters input =
    DoubleRun parametersShape inputShape outputShape <$> mapTargetError (runPreparedReverse executable (unwrapParameters parameters) (unwrapValue input))

-- | Read an exact run output.
exactRunOutput :: ExactRun parameters input output -> Value Rational output
exactRunOutput (ExactRun _ _ outputShape run) = wrapValue outputShape (reverseRunOutput run)

-- | Read a checked-Double run output.
doubleRunOutput :: DoubleRun fragment parameters input output -> Value Double output
doubleRunOutput (DoubleRun _ _ outputShape run) = wrapValue outputShape (reverseRunOutput run)

-- | Read the reusable exact tape from a successful run.
exactRunTape :: ExactRun parameters input output -> ExactTape parameters input output
exactRunTape (ExactRun parametersShape inputShape outputShape run) = ExactTape parametersShape inputShape outputShape (reverseRunTape run)

-- | Read the reusable checked-Double tape from a successful run.
doubleRunTape :: DoubleRun fragment parameters input output -> DoubleTape fragment parameters input output
doubleRunTape (DoubleRun parametersShape inputShape outputShape run) = DoubleTape parametersShape inputShape outputShape (reverseRunTape run)

-- | Apply an exact tape to one output cotangent.
applyExactTape :: ExactTape parameters input output -> Value Rational output -> Either CompileError (ParameterValue Rational parameters, Value Rational input)
applyExactTape (ExactTape parametersShape inputShape _ tape) seed = do
    (parameterCotangent, inputCotangent) <- mapTargetError (applyReverseTape tape (unwrapValue seed))
    Right (wrapParameters parametersShape parameterCotangent, wrapValue inputShape inputCotangent)

-- | Apply a checked-Double tape to one output cotangent.
applyDoubleTape :: DoubleTape fragment parameters input output -> Value Double output -> Either CompileError (ParameterValue Double parameters, Value Double input)
applyDoubleTape (DoubleTape parametersShape inputShape _ tape) seed = do
    (parameterCotangent, inputCotangent) <- mapTargetError (applyReverseTape tape (unwrapValue seed))
    Right (wrapParameters parametersShape parameterCotangent, wrapValue inputShape inputCotangent)

-- | Apply the exact run tape to one output cotangent.
applyExactVJP :: ExactRun parameters input output -> Value Rational output -> Either CompileError (ParameterValue Rational parameters, Value Rational input)
applyExactVJP run = applyExactTape (exactRunTape run)

-- | Apply the checked-Double run tape to one output cotangent.
applyDoubleVJP :: DoubleRun fragment parameters input output -> Value Double output -> Either CompileError (ParameterValue Double parameters, Value Double input)
applyDoubleVJP run = applyDoubleTape (doubleRunTape run)

-- | Direct source interpreter for exact polynomial primal semantics.
interpretExactPolynomial :: CompilerLimits -> Program Rational 'Polynomial parameters input output -> ParameterValue Rational parameters -> Value Rational input -> Either CompileError (Value Rational output)
interpretExactPolynomial limits@(CompilerLimits _ _ rationalBits) program parameters input = do
    _ <- preflightSource limits program
    mapSourceExecution (interpret (exactBackend rationalBits) program parameters input)

-- | Direct bounded source interpreter for checked-Double polynomial primal semantics.
interpretDoublePolynomial :: CompilerLimits -> Program Double 'Polynomial parameters input output -> ParameterValue Double parameters -> Value Double input -> Either CompileError (Value Double output)
interpretDoublePolynomial limits program parameters input = do
    _ <- preflightSource limits program
    mapSourceExecution (interpret doubleBackend program parameters input)

-- | Direct bounded source interpreter for checked-Double smooth primal semantics.
interpretDoubleSmooth :: CompilerLimits -> Program Double 'Smooth parameters input output -> ParameterValue Double parameters -> Value Double input -> Either CompileError (Value Double output)
interpretDoubleSmooth limits program parameters input = do
    _ <- preflightSource limits program
    mapSourceExecution (interpret doubleBackend program parameters input)

mapSourceExecution :: Either AutodiffFailure value -> Either CompileError value
mapSourceExecution result = case result of
    Left problem -> Left (CompileError (ReversePrimitiveForwardFailure [] problem))
    Right value -> Right value

mapTargetError :: Either (ReverseProgramError AutodiffFailure) value -> Either CompileError value
mapTargetError result = case result of
    Left problem -> Left (CompileError problem)
    Right value -> Right value

interpret :: forall scalar fragment parameters input output. Backend scalar -> Program scalar fragment parameters input output -> ParameterValue scalar parameters -> Value scalar input -> Either AutodiffFailure (Value scalar output)
interpret backend program parameters input = do
    validateParameters backend parameters
    validateValue backend "input" input
    go program parameters input
  where
    go :: Program scalar fragment p x y -> ParameterValue scalar p -> Value scalar x -> Either AutodiffFailure (Value scalar y)
    go (PrimitiveNode primitive) p x = primitivePrimal backend primitive p x
    go (IdentityNode _) NoParameterValue x = Right x
    go (ComposeNode left right) p x = do
        let (leftP, rightP) = splitParameterProduct p
        middle <- go left leftP x
        go right rightP middle
    go (ParallelNode left right) p x = do
        let (leftP, rightP) = splitParameterProduct p
            (leftX, rightX) = splitProductValue x
        leftY <- go left leftP leftX
        rightY <- go right rightP rightX
        Right (ProductValue leftY rightY)
    go (FanoutNode left right) p x = do
        let (leftP, rightP) = splitParameterProduct p
        leftY <- go left leftP x
        rightY <- go right rightP x
        Right (ProductValue leftY rightY)
    go (ShareParametersNode left right) p x = do
        let (leftX, rightX) = splitProductValue x
        leftY <- go left p leftX
        rightY <- go right p rightX
        Right (ProductValue leftY rightY)

-- Backend arithmetic. Every operation validates its result for Double.
data Backend scalar = Backend
    { backendValidate :: String -> Natural -> scalar -> Either AutodiffFailure ()
    , backendZero :: !scalar
    , backendOne :: !scalar
    , backendAdd :: scalar -> scalar -> scalar
    , backendMultiply :: scalar -> scalar -> scalar
    , backendNegate :: scalar -> scalar
    , backendTanh :: scalar -> scalar
    , backendEquivalent :: scalar -> scalar -> Bool
    , backendEqualityMode :: !CotangentEqualityMode
    }

exactBackend :: Natural -> Backend Rational
exactBackend bitLimit = Backend validate 0 1 (+) (*) negate id (==) ExactCotangentEquality
  where
    validate context coordinate value
        | fitsBits bitLimit (numerator value) && fitsBits bitLimit (denominator value) = Right ()
        | otherwise = Left (RationalMagnitudeLimitExceeded context coordinate bitLimit)

doubleBackend :: Backend Double
doubleBackend = Backend validate 0 1 (+) (*) negate tanh (approximatelyEqual doubleComparisonPolicy) (ApproximateCotangentEquality (renderDoubleComparisonPolicy doubleComparisonPolicy))
  where
    validate context coordinate value
        | isNaN value || isInfinite value = Left (NonFiniteCoordinate context coordinate)
        | otherwise = Right ()

fitsBits :: Natural -> Integer -> Bool
fitsBits limit = go 0 . abs
  where
    go used value
        | value == 0 = True
        | used >= limit = False
        | otherwise = go (used + 1) (value `quot` 2)

approximatelyEqual :: DoubleComparisonPolicy -> Double -> Double -> Bool
approximatelyEqual policy left right =
    let difference = abs (left - right)
     in difference <= absoluteTolerance policy || difference <= relativeTolerance policy * max (abs left) (abs right)

resolveTargetPrimitive :: Backend scalar -> TapePolicy -> TargetPrimitive scalar fragment parameter parameterCotangent input inputCotangent output outputCotangent -> Either ReverseDefinitionError (OwnedReversePrimitive AutodiffFailure scalar parameter parameterCotangent input inputCotangent output outputCotangent)
resolveTargetPrimitive backend policy (TargetPrimitive primitive) = do
    let parameterWitness = primitiveParameters primitive
        inputWitness = primitiveInput primitive
        outputWitness = primitiveOutput primitive
        parameterPrimal = runtimeParameterPrimalSpace backend parameterWitness
        inputPrimal = runtimePrimalSpace backend inputWitness
        outputPrimal = runtimePrimalSpace backend outputWitness
        evaluation = runtimePrimitiveEvaluation backend primitive
        circuit =
            primitiveReverseCircuit
                (runtimeParameterCotangentSpace backend parameterWitness)
                (runtimeValueCotangentSpace backend inputWitness)
                (runtimeValueCotangentSpace backend outputWitness)
                evaluation
        name = primitiveName primitive
        revision = "1"
    ownership <- ownershipOf parameterWitness
    case policy of
        StorePullbacks -> ownedReversePrimitive name revision ownership parameterPrimal inputPrimal outputPrimal circuit StoreCapturedPullback
        RecomputePullbacks -> ownedReversePrimitiveWithRecomputation name revision ownership parameterPrimal inputPrimal outputPrimal circuit (primitiveRecomputation evaluation)

runtimePrimitiveEvaluation :: Backend scalar -> Primitive scalar fragment parameters input output -> RuntimeParameters scalar parameters -> RuntimeValue scalar input -> Either AutodiffFailure (ReverseEvaluation AutodiffFailure (RuntimeParameters scalar parameters) (RuntimeValue scalar input) (RuntimeValue scalar output) (RuntimeValue scalar output))
runtimePrimitiveEvaluation backend primitive parameters input = do
    evaluation <- primitiveEvaluation backend primitive (wrapParameters parameterWitness parameters) (wrapValue inputWitness input)
    let output = reversePrimalOutput evaluation
    Right
        ( reverseEvaluation
            (unwrapValue output)
            ( \seed -> do
                (parameterCotangent, inputCotangent) <- applyReverseVJP evaluation (wrapValue outputWitness seed)
                Right (unwrapParameters parameterCotangent, unwrapValue inputCotangent)
            )
        )
  where
    parameterWitness = primitiveParameters primitive
    inputWitness = primitiveInput primitive
    outputWitness = primitiveOutput primitive

primitiveEvaluation :: Backend scalar -> Primitive scalar fragment parameters input output -> ParameterValue scalar parameters -> Value scalar input -> Either AutodiffFailure (ReverseEvaluation AutodiffFailure (ParameterValue scalar parameters) (Value scalar input) (Value scalar output) (Value scalar output))
primitiveEvaluation backend primitive parameters input = do
    output <- primitivePrimal backend primitive parameters input
    Right (reverseEvaluation output (primitiveVJP backend primitive parameters input output))

primitivePrimal :: Backend scalar -> Primitive scalar fragment parameters input output -> ParameterValue scalar parameters -> Value scalar input -> Either AutodiffFailure (Value scalar output)
primitivePrimal backend primitive parameters input = do
    validateParameters backend parameters
    validateValue backend "primitive-input" input
    output <- case primitive of
        ConstantScalar _ constant -> Right (ScalarValue constant)
        ConstantVector _ _ constant -> Right constant
        Parameter{} -> Right (ownedParameterValue parameters)
        NegateScalar -> checkedScalar backend "negate" (backendNegate backend (scalarFromValue input))
        AddScalar -> let (left, right) = scalarPair input in checkedScalar backend "add" (backendAdd backend left right)
        MultiplyScalar -> let (left, right) = scalarPair input in checkedScalar backend "multiply" (backendMultiply backend left right)
        AddVector _ -> binaryVector backend "vector-add" (backendAdd backend) input
        Hadamard _ -> binaryVector backend "hadamard" (backendMultiply backend) input
        Dot _ -> let (left, right) = vectorPair input in ScalarValue <$> checkedDot backend "dot" left right
        SumVector _ -> ScalarValue <$> checkedFold backend "sum" (vectorFromValue input)
        TanhScalar -> checkedScalar backend "tanh" (backendTanh backend (scalarFromValue input))
        TanhVector _ -> checkedVector backend "vector-tanh" (map (backendTanh backend) (vectorFromValue input))
        First _ _ -> let (left, _) = splitProductValue input in Right left
        Second _ _ -> let (_, right) = splitProductValue input in Right right
        ProjectValue projection -> Right (projectPrimal projection input)
    validateValue backend "primitive-output" output
    Right output

primitiveVJP :: Backend scalar -> Primitive scalar fragment parameters input output -> ParameterValue scalar parameters -> Value scalar input -> Value scalar output -> Value scalar output -> Either AutodiffFailure (ParameterValue scalar parameters, Value scalar input)
primitiveVJP backend primitive _ input output seed = do
    validateValue backend "output-cotangent" seed
    result <- case primitive of
        ConstantScalar inputShape _ -> Right (NoParameterValue, zeroValue backend inputShape)
        ConstantVector inputShape _ _ -> Right (NoParameterValue, zeroValue backend inputShape)
        Parameter _ inputShape _ -> Right (OwnedParameterValue seed, zeroValue backend inputShape)
        NegateScalar -> Right (NoParameterValue, ScalarValue (backendNegate backend (scalarFromValue seed)))
        AddScalar -> let lambda = scalarFromValue seed in Right (NoParameterValue, ProductValue (ScalarValue lambda) (ScalarValue lambda))
        MultiplyScalar ->
            let (left, right) = scalarPair input
                lambda = scalarFromValue seed
             in Right (NoParameterValue, ProductValue (ScalarValue (backendMultiply backend lambda right)) (ScalarValue (backendMultiply backend lambda left)))
        AddVector _ -> Right (NoParameterValue, ProductValue seed seed)
        Hadamard _ ->
            let (left, right) = vectorPair input
                lambda = vectorFromValue seed
             in Right (NoParameterValue, ProductValue (VectorValue (zipWith (backendMultiply backend) lambda right)) (VectorValue (zipWith (backendMultiply backend) lambda left)))
        Dot _ ->
            let (left, right) = vectorPair input
                lambda = scalarFromValue seed
             in Right (NoParameterValue, ProductValue (VectorValue (map (backendMultiply backend lambda) right)) (VectorValue (map (backendMultiply backend lambda) left)))
        SumVector _ -> let lambda = scalarFromValue seed in Right (NoParameterValue, VectorValue (replicate (length (vectorFromValue input)) lambda))
        TanhScalar -> do
            let lambda = scalarFromValue seed
                y = scalarFromValue output
            square <- checkedRaw backend "vjp/tanh-square" 0 (backendMultiply backend y y)
            derivative <- checkedRaw backend "vjp/tanh-derivative" 0 (backendAdd backend (backendOne backend) (backendNegate backend square))
            value <- checkedRaw backend "vjp/tanh" 0 (backendMultiply backend lambda derivative)
            Right (NoParameterValue, ScalarValue value)
        TanhVector _ -> do
            values <- checkedTanhVJP backend (vectorFromValue seed) (vectorFromValue output)
            Right (NoParameterValue, VectorValue values)
        First _ rightShape -> Right (NoParameterValue, ProductValue seed (zeroValue backend rightShape))
        Second leftShape _ -> Right (NoParameterValue, ProductValue (zeroValue backend leftShape) seed)
        ProjectValue projection -> Right (NoParameterValue, projectCotangent backend projection seed)
    validateParameters backend (fst result)
    validateValue backend "input-cotangent" (snd result)
    Right result

primitiveName :: Primitive scalar fragment parameters input output -> String
primitiveName primitive = case primitive of
    ConstantScalar _ _ -> "constant-scalar"
    ConstantVector{} -> "constant-vector"
    Parameter owner _ _ -> "parameter/" ++ owner
    NegateScalar -> "negate-scalar"
    AddScalar -> "add-scalar"
    MultiplyScalar -> "multiply-scalar"
    AddVector _ -> "add-vector"
    Hadamard _ -> "hadamard"
    Dot _ -> "dot"
    SumVector _ -> "sum-vector"
    TanhScalar -> "tanh-scalar"
    TanhVector _ -> "tanh-vector"
    First _ _ -> "first"
    Second _ _ -> "second"
    ProjectValue{} -> "quote-project"

primitiveParameters :: Primitive scalar fragment parameters input output -> SParameters parameters
primitiveParameters primitive = case primitive of
    ConstantScalar _ _ -> SNoParameters
    ConstantVector{} -> SNoParameters
    Parameter owner _ shape -> SOwner owner shape
    NegateScalar -> SNoParameters
    AddScalar -> SNoParameters
    MultiplyScalar -> SNoParameters
    AddVector _ -> SNoParameters
    Hadamard _ -> SNoParameters
    Dot _ -> SNoParameters
    SumVector _ -> SNoParameters
    TanhScalar -> SNoParameters
    TanhVector _ -> SNoParameters
    First _ _ -> SNoParameters
    Second _ _ -> SNoParameters
    ProjectValue{} -> SNoParameters

primitiveInput :: Primitive scalar fragment parameters input output -> SShape input
primitiveInput primitive = case primitive of
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
    ProjectValue projection -> projectionInput projection

primitiveOutput :: Primitive scalar fragment parameters input output -> SShape output
primitiveOutput primitive = case primitive of
    ConstantScalar _ _ -> SScalar
    ConstantVector _ shape _ -> shape
    Parameter _ _ shape -> shape
    NegateScalar -> SScalar
    AddScalar -> SScalar
    MultiplyScalar -> SScalar
    AddVector shape -> shape
    Hadamard shape -> shape
    Dot _ -> SScalar
    SumVector _ -> SScalar
    TanhScalar -> SScalar
    TanhVector shape -> shape
    First left _ -> left
    Second _ right -> right
    ProjectValue projection -> projectionOutput projection

projectionInput :: Projection environment selected -> SShape environment
projectionInput projection = case projection of
    ProjectionHere shape -> shape
    ProjectionLeft inner right -> SProduct (projectionInput inner) right
    ProjectionRight left inner -> SProduct left (projectionInput inner)

projectionOutput :: Projection environment selected -> SShape selected
projectionOutput projection = case projection of
    ProjectionHere shape -> shape
    ProjectionLeft inner _ -> projectionOutput inner
    ProjectionRight _ inner -> projectionOutput inner

projectPrimal :: Projection environment selected -> Value scalar environment -> Value scalar selected
projectPrimal projection value = case projection of
    ProjectionHere _ -> value
    ProjectionLeft inner _ -> let (left, _) = splitProductValue value in projectPrimal inner left
    ProjectionRight _ inner -> let (_, right) = splitProductValue value in projectPrimal inner right

projectCotangent :: Backend scalar -> Projection environment selected -> Value scalar selected -> Value scalar environment
projectCotangent backend projection seed = case projection of
    ProjectionHere _ -> seed
    ProjectionLeft inner right -> ProductValue (projectCotangent backend inner seed) (zeroValue backend right)
    ProjectionRight left inner -> ProductValue (zeroValue backend left) (projectCotangent backend inner seed)

shapeLayout :: SShape shape -> FiniteLayout
shapeLayout SUnit = unitFiniteLayout
shapeLayout SScalar = atomic "scalar" 1
shapeLayout shape@SVector = vectorLayout shape
shapeLayout (SProduct left right) = productFiniteLayout (shapeLayout left) (shapeLayout right)

vectorLayout :: forall n. SShape ('Vector n) -> FiniteLayout
vectorLayout SVector = atomic "vector" (fromInteger (natVal (Proxy @n)))

atomic :: String -> Natural -> FiniteLayout
atomic name extent = case finiteLayout name extent of
    Just layout -> layout
    Nothing -> error "internal nonempty finite-layout name"

parameterLayout :: SParameters parameters -> FiniteLayout
parameterLayout SNoParameters = unitFiniteLayout
parameterLayout (SOwner _ shape) = shapeLayout shape
parameterLayout (SParameterProduct left right) = productFiniteLayout (parameterLayout left) (parameterLayout right)

ownershipOf :: SParameters parameters -> Either ReverseDefinitionError ParameterOwnership
ownershipOf SNoParameters = Right noParameterOwnership
ownershipOf (SOwner owner shape) = parameterOwner owner (shapeLayout shape)
ownershipOf (SParameterProduct left right) = parameterOwnershipProduct <$> ownershipOf left <*> ownershipOf right

programParameters :: Program scalar fragment parameters input output -> SParameters parameters
programParameters (PrimitiveNode primitive) = primitiveParameters primitive
programParameters (IdentityNode _) = SNoParameters
programParameters (ComposeNode left right) = SParameterProduct (programParameters left) (programParameters right)
programParameters (ParallelNode left right) = SParameterProduct (programParameters left) (programParameters right)
programParameters (FanoutNode left right) = SParameterProduct (programParameters left) (programParameters right)
programParameters (ShareParametersNode left _) = programParameters left

programInput :: Program scalar fragment parameters input output -> SShape input
programInput (PrimitiveNode primitive) = primitiveInput primitive
programInput (IdentityNode shape) = shape
programInput (ComposeNode left _) = programInput left
programInput (ParallelNode left right) = SProduct (programInput left) (programInput right)
programInput (FanoutNode left _) = programInput left
programInput (ShareParametersNode left right) = SProduct (programInput left) (programInput right)

programOutput :: Program scalar fragment parameters input output -> SShape output
programOutput (PrimitiveNode primitive) = primitiveOutput primitive
programOutput (IdentityNode shape) = shape
programOutput (ComposeNode _ right) = programOutput right
programOutput (ParallelNode left right) = SProduct (programOutput left) (programOutput right)
programOutput (FanoutNode left right) = SProduct (programOutput left) (programOutput right)
programOutput (ShareParametersNode left right) = SProduct (programOutput left) (programOutput right)

unwrapValue :: Value scalar shape -> RuntimeValue scalar shape
unwrapValue UnitValue = ()
unwrapValue (ScalarValue value) = value
unwrapValue (VectorValue values) = values
unwrapValue (ProductValue left right) = (unwrapValue left, unwrapValue right)

wrapValue :: SShape shape -> RuntimeValue scalar shape -> Value scalar shape
wrapValue SUnit () = UnitValue
wrapValue SScalar value = ScalarValue value
wrapValue SVector values = VectorValue values
wrapValue (SProduct left right) (leftValue, rightValue) = ProductValue (wrapValue left leftValue) (wrapValue right rightValue)

unwrapParameters :: ParameterValue scalar parameters -> RuntimeParameters scalar parameters
unwrapParameters NoParameterValue = ()
unwrapParameters (OwnedParameterValue value) = unwrapValue value
unwrapParameters (ParameterProductValue left right) = (unwrapParameters left, unwrapParameters right)

wrapParameters :: SParameters parameters -> RuntimeParameters scalar parameters -> ParameterValue scalar parameters
wrapParameters SNoParameters () = NoParameterValue
wrapParameters (SOwner _ shape) value = OwnedParameterValue (wrapValue shape value)
wrapParameters (SParameterProduct left right) (leftValue, rightValue) = ParameterProductValue (wrapParameters left leftValue) (wrapParameters right rightValue)

runtimePrimalSpace :: Backend scalar -> SShape shape -> FinitePrimalSpace AutodiffFailure (RuntimeValue scalar shape)
runtimePrimalSpace backend shape = finitePrimalSpace (shapeLayout shape) validate equivalent (runtimeValueMode backend shape)
  where
    validate = validateValue backend "primal" . wrapValue shape
    equivalent left right = equivalentValue backend (wrapValue shape left) (wrapValue shape right)

runtimeParameterPrimalSpace :: Backend scalar -> SParameters parameters -> FinitePrimalSpace AutodiffFailure (RuntimeParameters scalar parameters)
runtimeParameterPrimalSpace backend parameters = finitePrimalSpace (parameterLayout parameters) validate equivalent (runtimeParameterMode backend parameters)
  where
    validate = validateParameters backend . wrapParameters parameters
    equivalent left right = equivalentParameters backend (wrapParameters parameters left) (wrapParameters parameters right)

runtimeValueCotangentSpace :: Backend scalar -> SShape shape -> CotangentSpace AutodiffFailure scalar (RuntimeValue scalar shape)
runtimeValueCotangentSpace backend shape = declared (runtimeValueOwner shape) (shapeLayout shape) validate zero add scale equivalent (runtimeValueMode backend shape)
  where
    validate = validateValue backend "cotangent" . wrapValue shape
    zero = unwrapValue (zeroValue backend shape)
    add left right = unwrapValue <$> addValue backend (wrapValue shape left) (wrapValue shape right)
    scale scalar value = unwrapValue <$> scaleValue backend scalar (wrapValue shape value)
    equivalent left right = equivalentValue backend (wrapValue shape left) (wrapValue shape right)

runtimeParameterCotangentSpace :: Backend scalar -> SParameters parameters -> CotangentSpace AutodiffFailure scalar (RuntimeParameters scalar parameters)
runtimeParameterCotangentSpace backend parameters = declared (runtimeParameterOwner parameters) (parameterLayout parameters) validate zero add scale equivalent (runtimeParameterMode backend parameters)
  where
    validate = validateParameters backend . wrapParameters parameters
    zero = unwrapParameters (zeroParameters backend parameters)
    add left right = unwrapParameters <$> addParameters backend (wrapParameters parameters left) (wrapParameters parameters right)
    scale scalar value = unwrapParameters <$> scaleParameters backend scalar (wrapParameters parameters value)
    equivalent left right = equivalentParameters backend (wrapParameters parameters left) (wrapParameters parameters right)

runtimeValueOwner :: SShape shape -> String
runtimeValueOwner SUnit = "unit"
runtimeValueOwner shape@SScalar = "autodiff-value/" ++ finiteLayoutDescription (shapeLayout shape)
runtimeValueOwner shape@SVector = "autodiff-value/" ++ finiteLayoutDescription (shapeLayout shape)
runtimeValueOwner (SProduct left right) = "(" ++ runtimeValueOwner left ++ " * " ++ runtimeValueOwner right ++ ")"

runtimeParameterOwner :: SParameters parameters -> String
runtimeParameterOwner SNoParameters = "autodiff-parameters/unit"
runtimeParameterOwner (SOwner _ shape) = "autodiff-parameters/" ++ finiteLayoutDescription (shapeLayout shape)
runtimeParameterOwner (SParameterProduct left right) = "(" ++ runtimeParameterOwner left ++ " * " ++ runtimeParameterOwner right ++ ")"

runtimeValueMode :: Backend scalar -> SShape shape -> CotangentEqualityMode
runtimeValueMode _ SUnit = ExactCotangentEquality
runtimeValueMode backend SScalar = backendEqualityMode backend
runtimeValueMode backend SVector = backendEqualityMode backend
runtimeValueMode backend (SProduct left right) = combineModes (runtimeValueMode backend left) (runtimeValueMode backend right)

runtimeParameterMode :: Backend scalar -> SParameters parameters -> CotangentEqualityMode
runtimeParameterMode _ SNoParameters = ExactCotangentEquality
runtimeParameterMode backend (SOwner _ shape) = runtimeValueMode backend shape
runtimeParameterMode backend (SParameterProduct left right) = combineModes (runtimeParameterMode backend left) (runtimeParameterMode backend right)

combineModes :: CotangentEqualityMode -> CotangentEqualityMode -> CotangentEqualityMode
combineModes ExactCotangentEquality ExactCotangentEquality = ExactCotangentEquality
combineModes left right = ApproximateCotangentEquality ("product of " ++ show left ++ " and " ++ show right)

declared :: String -> FiniteLayout -> (cotangent -> Either error ()) -> cotangent -> (cotangent -> cotangent -> Either error cotangent) -> (scalar -> cotangent -> Either error cotangent) -> (cotangent -> cotangent -> Bool) -> CotangentEqualityMode -> CotangentSpace error scalar cotangent
declared owner layout validate zero add scale equivalent mode = case declaredCotangentSpace owner layout validate zero add scale equivalent mode of
    Just space -> space
    Nothing -> error "internal nonempty cotangent owner"

validateValue :: Backend scalar -> String -> Value scalar shape -> Either AutodiffFailure ()
validateValue backend context value = check 0 (valueScalars value)
  where
    check _ [] = Right ()
    check coordinate (scalar : rest) = do
        backendValidate backend context coordinate scalar
        check (coordinate + 1) rest

validateParameters :: Backend scalar -> ParameterValue scalar parameters -> Either AutodiffFailure ()
validateParameters backend = check 0 . parameterScalars
  where
    check _ [] = Right ()
    check coordinate (scalar : rest) = do
        backendValidate backend "parameter" coordinate scalar
        check (coordinate + 1) rest

replicateNat :: forall n a. SShape ('Vector n) -> a -> [a]
replicateNat SVector = replicate (fromInteger (natVal (Proxy @n)))

zeroValue :: Backend scalar -> SShape shape -> Value scalar shape
zeroValue _ SUnit = UnitValue
zeroValue backend SScalar = ScalarValue (backendZero backend)
zeroValue backend shape@SVector = VectorValue (replicateNat shape (backendZero backend))
zeroValue backend (SProduct left right) = ProductValue (zeroValue backend left) (zeroValue backend right)

addValue :: Backend scalar -> Value scalar shape -> Value scalar shape -> Either AutodiffFailure (Value scalar shape)
addValue _ UnitValue UnitValue = Right UnitValue
addValue backend (ScalarValue left) (ScalarValue right) = checkedScalar backend "cotangent-add" (backendAdd backend left right)
addValue backend (VectorValue left) (VectorValue right)
    | length left /= length right = Left (InternalVectorLengthMismatch "cotangent-add")
    | otherwise = checkedVector backend "cotangent-add" (zipWith (backendAdd backend) left right)
addValue backend (ProductValue leftA rightA) (ProductValue leftB rightB) = ProductValue <$> addValue backend leftA leftB <*> addValue backend rightA rightB

scaleValue :: Backend scalar -> scalar -> Value scalar shape -> Either AutodiffFailure (Value scalar shape)
scaleValue _ _ UnitValue = Right UnitValue
scaleValue backend scalar (ScalarValue value) = checkedScalar backend "cotangent-scale" (backendMultiply backend scalar value)
scaleValue backend scalar (VectorValue values) = checkedVector backend "cotangent-scale" (map (backendMultiply backend scalar) values)
scaleValue backend scalar (ProductValue left right) = ProductValue <$> scaleValue backend scalar left <*> scaleValue backend scalar right

zeroParameters :: Backend scalar -> SParameters parameters -> ParameterValue scalar parameters
zeroParameters _ SNoParameters = NoParameterValue
zeroParameters backend (SOwner _ shape) = OwnedParameterValue (zeroValue backend shape)
zeroParameters backend (SParameterProduct left right) = ParameterProductValue (zeroParameters backend left) (zeroParameters backend right)

addParameters :: Backend scalar -> ParameterValue scalar parameters -> ParameterValue scalar parameters -> Either AutodiffFailure (ParameterValue scalar parameters)
addParameters _ NoParameterValue NoParameterValue = Right NoParameterValue
addParameters backend (OwnedParameterValue left) (OwnedParameterValue right) = OwnedParameterValue <$> addValue backend left right
addParameters backend (ParameterProductValue leftA rightA) (ParameterProductValue leftB rightB) = ParameterProductValue <$> addParameters backend leftA leftB <*> addParameters backend rightA rightB

scaleParameters :: Backend scalar -> scalar -> ParameterValue scalar parameters -> Either AutodiffFailure (ParameterValue scalar parameters)
scaleParameters _ _ NoParameterValue = Right NoParameterValue
scaleParameters backend scalar (OwnedParameterValue value) = OwnedParameterValue <$> scaleValue backend scalar value
scaleParameters backend scalar (ParameterProductValue left right) = ParameterProductValue <$> scaleParameters backend scalar left <*> scaleParameters backend scalar right

equivalentValue :: Backend scalar -> Value scalar shape -> Value scalar shape -> Bool
equivalentValue backend left right = and (zipWith (backendEquivalent backend) (valueScalars left) (valueScalars right)) && length (valueScalars left) == length (valueScalars right)

equivalentParameters :: Backend scalar -> ParameterValue scalar parameters -> ParameterValue scalar parameters -> Bool
equivalentParameters backend left right = and (zipWith (backendEquivalent backend) (parameterScalars left) (parameterScalars right)) && length (parameterScalars left) == length (parameterScalars right)

checkedScalar :: Backend scalar -> String -> scalar -> Either AutodiffFailure (Value scalar 'Scalar)
checkedScalar backend context value = do
    backendValidate backend context 0 value
    Right (ScalarValue value)

checkedVector :: Backend scalar -> String -> [scalar] -> Either AutodiffFailure (Value scalar ('Vector n))
checkedVector backend context values = do
    let value = VectorValue values
    validateValue backend context value
    Right value

scalarPair :: Value scalar ('Product 'Scalar 'Scalar) -> (scalar, scalar)
scalarPair value = let (left, right) = splitProductValue value in (scalarFromValue left, scalarFromValue right)

vectorPair :: Value scalar ('Product ('Vector n) ('Vector n)) -> ([scalar], [scalar])
vectorPair value = let (left, right) = splitProductValue value in (vectorFromValue left, vectorFromValue right)

binaryVector :: Backend scalar -> String -> (scalar -> scalar -> scalar) -> Value scalar ('Product ('Vector n) ('Vector n)) -> Either AutodiffFailure (Value scalar ('Vector n))
binaryVector backend context operation value =
    let (left, right) = vectorPair value
     in if length left /= length right
            then Left (InternalVectorLengthMismatch context)
            else checkedVector backend context (zipWith operation left right)

checkedRaw :: Backend scalar -> String -> Natural -> scalar -> Either AutodiffFailure scalar
checkedRaw backend context coordinate value = do
    backendValidate backend context coordinate value
    Right value

checkedFold :: Backend scalar -> String -> [scalar] -> Either AutodiffFailure scalar
checkedFold backend context = go 0 (backendZero backend)
  where
    go _ accumulator [] = Right accumulator
    go coordinate accumulator (value : rest) = do
        next <- checkedRaw backend context coordinate (backendAdd backend accumulator value)
        go (coordinate + 1) next rest

checkedDot :: Backend scalar -> String -> [scalar] -> [scalar] -> Either AutodiffFailure scalar
checkedDot backend context left right
    | length left /= length right = Left (InternalVectorLengthMismatch context)
    | otherwise = products 0 left right >>= checkedFold backend (context ++ "-sum")
  where
    products _ [] [] = Right []
    products coordinate (x : xs) (y : ys) = do
        value <- checkedRaw backend (context ++ "-product") coordinate (backendMultiply backend x y)
        (value :) <$> products (coordinate + 1) xs ys
    products _ _ _ = Left (InternalVectorLengthMismatch context)

checkedTanhVJP :: Backend scalar -> [scalar] -> [scalar] -> Either AutodiffFailure [scalar]
checkedTanhVJP backend = go 0
  where
    go _ [] [] = Right []
    go coordinate (lambda : lambdas) (y : ys) = do
        square <- checkedRaw backend "vjp/vector-tanh-square" coordinate (backendMultiply backend y y)
        derivative <- checkedRaw backend "vjp/vector-tanh-derivative" coordinate (backendAdd backend (backendOne backend) (backendNegate backend square))
        value <- checkedRaw backend "vjp/vector-tanh" coordinate (backendMultiply backend lambda derivative)
        (value :) <$> go (coordinate + 1) lambdas ys
    go _ _ _ = Left (InternalVectorLengthMismatch "vjp/vector-tanh")
