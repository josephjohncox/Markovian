{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RoleAnnotations #-}

-- The frozen private witness declarations use data with strict captured operands.
{- HLINT ignore "Use newtype instead of data" -}

{- | Bounded exact kernels from one rational real input to a joint reward and
successor law.

The executable syntax is deliberately closed.  Each coordinate is an affine
map of the input, and each owner-table row adds one compact uniform source with
separate reward and successor coefficients.  There is no callback constructor.
Consequently measurability is a syntax-directed obligation: rational affine
maps on 'RealBorel' are continuous and therefore Borel measurable; compact
uniform source laws are Borel probability measures; finite products of those
sources are standard Borel; and each output is a finite affine projection of
that product.  The implementation relies only on these closure steps and does
not claim measurability for arbitrary Haskell functions.

Owner numbers are names in one kernel scope.  The nominal @owner@ parameter and
explicit substitution requests prevent a number from being
used as global sharing evidence.  'alphaRenameJointAffineKernel' performs a
partial rename without changing that scope.  'reScopeJointAffineKernel' changes
the scope only when given a complete injective mapping of every declared owner, including zero rows.
The @sourceLabel@, @rewardLabel@, and @successorLabel@ parameters are nominal
coordinate labels, not measurable-space witnesses.  Materialization always
returns @ExactJointLaw RealBorel RealBorel@.

Every operation validates the limit record in raw-entry, owner, output,
coefficient, work, then rational-bit order.  Re-admission of a stored kernel
then checks those represented dimensions in the same order.  The operation
comments below freeze the remaining precedence.
-}
module Markovian.Continuous.Kernel.JointAffine.Exact (
    ExactAffineInputCoordinate,
    affineInputCoordinate,
    ExactJointAffineKernel,
    exactJointAffineKernel,
    alphaRenameJointAffineKernel,
    reScopeJointAffineKernel,
    materializeJointAffineKernel,
    jointAffineSupportExtrema,
    jointAffineKernelReport,
    ExactJointAffineLimits,
    exactJointAffineLimits,
    ExactJointAffineLimitDimension (..),
    ExactJointAffineOperation (..),
    ExactJointAffineError (..),
    ExactJointAffineReport (..),
    ExactSuccessorOwnerRequest,
    sharedSuccessorOwners,
    freshSuccessorOwners,
    ExactSuccessorSubstitutionLimits,
    exactSuccessorSubstitutionLimits,
    ExactSuccessorSubstitution,
    substituteLeftSuccessor,
    successorSubstitutionReport,
    materializeSuccessorSubstitution,
    successorSubstitutionSupportExtrema,
    SuccessorSubstitutionParticipant (..),
    ExactSuccessorSubstitutionError (..),
    ExactSuccessorSubstitutionMode (..),
    ExactSuccessorSubstitutionReport,
    substitutionMode,
    substitutionLeftDeclaredOwners,
    substitutionRightDeclaredOwners,
    substitutionMappingEntries,
    substitutionRawEntries,
    substitutionOwnerReservationSlots,
    substitutionReservedNames,
    substitutionSharedOwners,
    substitutionFreshOwners,
    substitutionDeclaredResultOwners,
    substitutionRetainedResultOwners,
    substitutionZeroFilteredResultOwners,
    substitutionOutputs,
    substitutionCoefficientSlots,
    substitutionPreflightWork,
    substitutionCoefficientMultiplications,
    substitutionCoefficientAdditions,
    substitutionArithmeticWork,
    substitutionWork,
    substitutionMaximumRationalBits,
) where

import Control.Monad (foldM, unless, when)
import Data.List (find, sortOn)
import Data.Ratio (denominator, numerator)
import Markovian.Continuous.Internal
import Numeric.Natural (Natural)

-- | One closed coordinate @x -> scale*x + offset@.
data ExactAffineInputCoordinate source target = UnsafeExactAffineInputCoordinate !Rational !Rational

type role ExactAffineInputCoordinate nominal nominal

{- | Construct one affine input coordinate.  Rational-size admission occurs in
the enclosing kernel operation so that it uses the same cumulative ledger.
-}
affineInputCoordinate :: Rational -> Rational -> ExactAffineInputCoordinate source target
affineInputCoordinate = UnsafeExactAffineInputCoordinate

{- | Limits for raw owner rows, declared owners, the fixed output layout,
stored rational coefficients, cumulative semantic work, and rational bits.
-}
data ExactJointAffineLimits = UnsafeExactJointAffineLimits
    { jointLimitRawEntries :: !Natural
    , jointLimitOwners :: !Natural
    , jointLimitOutputs :: !Natural
    , jointLimitCoefficients :: !Natural
    , jointLimitWork :: !Natural
    , jointLimitRationalBits :: !Natural
    }
    deriving stock (Eq, Show)

-- | Construct limits.  They are validated before any public operation.
exactJointAffineLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> ExactJointAffineLimits
exactJointAffineLimits = UnsafeExactJointAffineLimits

-- | A separately bounded part of the representation.
data ExactJointAffineLimitDimension
    = JointAffineRawEntries
    | JointAffineOwners
    | JointAffineOutputs
    | JointAffineCoefficients
    | JointAffineWork
    | JointAffineRationalBits
    deriving stock (Eq, Show)

-- | The operation represented by a deterministic report.
data ExactJointAffineOperation
    = JointAffineConstruction
    | JointAffineAlphaRenaming
    | JointAffineScopeRenaming
    | JointAffineMaterialization
    | JointAffineSupportExtrema
    deriving stock (Eq, Show)

-- | Construction, locality, interval, machine, or ledger failure.
data ExactJointAffineError
    = InvalidJointAffineLimit !ExactJointAffineLimitDimension !Natural
    | JointAffineLimitExceeded !ExactJointAffineLimitDimension !Natural !Natural
    | JointAffineMachineIndexExceeded !ExactJointAffineLimitDimension !Natural
    | JointAffineDuplicateOwner
    | JointAffineDuplicateRenamingSource
    | JointAffineNonInjectiveRenaming
    | JointAffineUnknownRenamingSource
    | JointAffineIncompleteRenaming
    | JointAffineInvalidInterval !Rational !Rational
    deriving stock (Eq, Show)

{- | Deterministic semantic accounting.  Coefficients count both scales, both
offsets, and both owner coefficients in every raw row.  Declared owners are
charged before rows with two zero coefficients are removed.
-}
data ExactJointAffineReport = ExactJointAffineReport
    { jointAffineOperation :: !ExactJointAffineOperation
    , jointAffineRawEntryCount :: !Natural
    , jointAffineDeclaredOwnerCount :: !Natural
    , jointAffineRetainedOwnerCount :: !Natural
    , jointAffineOutputCount :: !Natural
    , jointAffineCoefficientCount :: !Natural
    , jointAffinePreflightWork :: !Natural
    , jointAffineArithmeticWork :: !Natural
    , jointAffineWork :: !Natural
    , jointAffineMaximumRationalBits :: !Natural
    }
    deriving stock (Eq, Show)

{- | One canonical owner row.  Its owner number is local to the enclosing
nominal kernel scope.
-}
data OwnerRow = OwnerRow !Natural !RationalInterval !Rational !Rational

{- | Opaque exact joint affine kernel.  All parameters are nominal labels, so
input, reward, successor, and owner roles cannot be changed with @coerce@.
Only the owner parameter scopes run-time owner identity.  The three coordinate
labels do not stand for measurable spaces; the represented domain and both
outputs are always 'RealBorel'.
-}
data ExactJointAffineKernel owner sourceLabel rewardLabel successorLabel
    = UnsafeExactJointAffineKernel
        !(ExactAffineInputCoordinate sourceLabel rewardLabel)
        !(ExactAffineInputCoordinate sourceLabel successorLabel)
        ![OwnerRow] -- Complete canonical declarations, including zeros.
        ![OwnerRow] -- Executable view of those same rows.
        !Natural
        !Natural
        !Natural
        !ExactJointAffineReport

type role ExactJointAffineKernel nominal nominal nominal nominal

{- | Construct a kernel from one reward coordinate, one successor coordinate,
and owner rows @(owner, interval, rewardCoefficient, successorCoefficient)@.

Failure precedence is invalid limits; bounded raw traversal; owner, output,
coefficient, and construction-work preflight; duplicate owner declarations;
then rational and interval validation in input order.  Duplicate detection is
therefore complete before zero-row filtering. The full canonical declaration
manifest retains zero rows; its executable view omits them. Both are ordered
by owner number.
-}
exactJointAffineKernel ::
    ExactJointAffineLimits ->
    ExactAffineInputCoordinate source reward ->
    ExactAffineInputCoordinate source successor ->
    [(NoiseOwner owner, RationalInterval, Rational, Rational)] ->
    Either ExactJointAffineError (ExactJointAffineKernel owner source reward successor)
exactJointAffineKernel limits rewardCoordinate successorCoordinate input = do
    validateJointLimits limits
    raw <- boundedRows limits input
    let rawCount = naturalLength raw
        ownerCount = rawCount
        outputCount = 2
    checkDimension limits JointAffineOwners (jointLimitOwners limits) ownerCount
    checkDimension limits JointAffineOutputs (jointLimitOutputs limits) outputCount
    coefficientCount <- checkedMachineCount JointAffineCoefficients (4 + 2 * rawCount)
    checkDimension limits JointAffineCoefficients (jointLimitCoefficients limits) coefficientCount
    preflightWork <- checkedMachineCount JointAffineWork (rawCount + rawCount * rawCount)
    checkDimension limits JointAffineWork (jointLimitWork limits) preflightWork
    when (hasDuplicate [owner | (NoiseOwner owner, _, _, _) <- raw]) (Left JointAffineDuplicateOwner)
    maximumBits <- validateRationals limits rewardCoordinate successorCoordinate raw
    let declarations =
            orderRows
                [ OwnerRow owner interval rewardValue successorValue
                | (NoiseOwner owner, interval, rewardValue, successorValue) <- raw
                ]
        retained = filter executableRow declarations
        retainedCount = naturalLength retained
        report = makeReport JointAffineConstruction rawCount ownerCount retainedCount coefficientCount preflightWork 0 maximumBits
    pure (UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate declarations retained rawCount ownerCount coefficientCount report)

{- | Partially rename owners without changing the nominal owner scope.

Precedence is stored-kernel admission; bounded mapping-spine admission; the
complete conservative rename-work plan; duplicate sources; duplicate targets;
unknown sources; collisions after partial replacement; then canonical sorting.
The plan is checked before any semantic scan or derived owner list is made.
D079 Gate B supersedes retained-only membership and collisions: both now cover
all declarations, including zeros, and work uses the declared count.
-}
alphaRenameJointAffineKernel ::
    ExactJointAffineLimits ->
    [(NoiseOwner owner, NoiseOwner owner)] ->
    ExactJointAffineKernel owner sourceLabel rewardLabel successorLabel ->
    Either ExactJointAffineError (ExactJointAffineKernel owner sourceLabel rewardLabel successorLabel)
alphaRenameJointAffineKernel = renameJointAffineKernel PreserveOwnerScope

{- | Change the nominal owner scope using a complete injective mapping.

Precedence is the same as 'alphaRenameJointAffineKernel', with completeness
checked after unknown sources and before collisions.  An empty mapping can
change scope only for a kernel with no declarations.
-}
reScopeJointAffineKernel ::
    ExactJointAffineLimits ->
    [(NoiseOwner old, NoiseOwner new)] ->
    ExactJointAffineKernel old sourceLabel rewardLabel successorLabel ->
    Either ExactJointAffineError (ExactJointAffineKernel new sourceLabel rewardLabel successorLabel)
reScopeJointAffineKernel = renameJointAffineKernel ChangeOwnerScope

data OwnerScopeChange = PreserveOwnerScope | ChangeOwnerScope
    deriving stock (Eq)

renameJointAffineKernel ::
    OwnerScopeChange ->
    ExactJointAffineLimits ->
    [(NoiseOwner old, NoiseOwner new)] ->
    ExactJointAffineKernel old sourceLabel rewardLabel successorLabel ->
    Either ExactJointAffineError (ExactJointAffineKernel new sourceLabel rewardLabel successorLabel)
renameJointAffineKernel scopeChange limits inputRenaming kernel@(UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate rows _ rawCount ownerCount coefficientCount _) = do
    validateStored limits kernel
    (mappingCount, renaming) <- boundedRenaming limits inputRenaming
    let retainedCount = jointAffineRetainedOwnerCount (jointAffineKernelReport kernel)
    renameWork <- renamingPreflight limits scopeChange rawCount ownerCount mappingCount
    when (hasDuplicateBy renamingSource renaming) (Left JointAffineDuplicateRenamingSource)
    when (hasDuplicateBy renamingTarget renaming) (Left JointAffineNonInjectiveRenaming)
    unless (all (sourceIsDeclared rows) renaming) (Left JointAffineUnknownRenamingSource)
    when (scopeChange == ChangeOwnerScope && not (all (rowHasSource renaming) rows)) (Left JointAffineIncompleteRenaming)
    let renamedOwners = map (renameOwner renaming . rowOwner) rows
    when (hasDuplicate renamedOwners) (Left JointAffineNonInjectiveRenaming)
    let renamed = sortOn rowOwner (zipWith renameRowTo renamedOwners rows)
        operation = case scopeChange of
            PreserveOwnerScope -> JointAffineAlphaRenaming
            ChangeOwnerScope -> JointAffineScopeRenaming
        report = makeReport operation rawCount ownerCount retainedCount coefficientCount renameWork 0 (storedMaximumBits kernel)
    pure (UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate renamed (filter executableRow renamed) rawCount ownerCount coefficientCount report)

{- | Materialize at one rational input as the existing real-Borel exact joint
law.  Nominal coordinate labels never become space parameters.  The report
includes construction preflight and every checked affine and support-bound
arithmetic operation.

Precedence is stored-kernel admission; complete operation-work preflight; input
rational-bit admission; reward then successor input arithmetic; reward then
successor support arithmetic; and result construction.
-}
materializeJointAffineKernel ::
    ExactJointAffineLimits ->
    ExactJointAffineKernel owner sourceLabel rewardLabel successorLabel ->
    Rational ->
    Either ExactJointAffineError (ExactJointLaw RealBorel RealBorel, ExactJointAffineReport)
materializeJointAffineKernel limits kernel@(UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate _ rows rawCount ownerCount coefficientCount _) input = do
    validateStored limits kernel
    let retainedCount = naturalLength rows
        arithmeticPlan = 4 + 8 * retainedCount
    totalWork <- operationPreflight limits rawCount arithmeticPlan
    meter0 <- observe limits input (Meter 0 (storedMaximumBits kernel))
    (rewardConstant, meter1) <- applyCoordinate limits rewardCoordinate input meter0
    (successorConstant, meter2) <- applyCoordinate limits successorCoordinate input meter1
    meter3 <- accountCoordinateSupport limits rewardConstant rewardCoefficient rows meter2
    meter4 <- accountCoordinateSupport limits successorConstant successorCoefficient rows meter3
    unless (meterArithmetic meter4 == arithmeticPlan) (Left (JointAffineMachineIndexExceeded JointAffineWork (meterArithmetic meter4)))
    let rewardTerms = [(rowOwner row, rewardCoefficient row, rowInterval row) | row <- rows, rewardCoefficient row /= 0]
        successorTerms = [(rowOwner row, successorCoefficient row, rowInterval row) | row <- rows, successorCoefficient row /= 0]
        law = ExactJointLaw (ExactLaw rewardConstant rewardTerms) (ExactLaw successorConstant successorTerms)
        report = makeReport JointAffineMaterialization rawCount ownerCount retainedCount coefficientCount (basePreflight rawCount) (meterArithmetic meter4) (meterBits meter4)
    unless (jointAffineWork report == totalWork) (Left (JointAffineMachineIndexExceeded JointAffineWork (jointAffineWork report)))
    pure (law, report)

{- | Compute exact reward and successor support extrema over a compact rational
input interval.  Signed coefficients select endpoints independently; no
optimizer or floating arithmetic is used.

Precedence is stored-kernel admission; complete operation-work preflight; the
interval-order check; input endpoint rational-bit admission; reward extrema;
then successor extrema.  In particular, a degenerate interval obtained as the
support of a Dirac law is rejected before its endpoint bit size is inspected.
-}
jointAffineSupportExtrema ::
    ExactJointAffineLimits ->
    ExactJointAffineKernel owner sourceLabel rewardLabel successorLabel ->
    RationalInterval ->
    Either ExactJointAffineError ((RationalInterval, RationalInterval), ExactJointAffineReport)
jointAffineSupportExtrema limits kernel@(UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate _ rows rawCount ownerCount coefficientCount _) inputInterval = do
    validateStored limits kernel
    let retainedCount = naturalLength rows
        arithmeticPlan = 8 * (retainedCount + 1)
    totalWork <- operationPreflight limits rawCount arithmeticPlan
    validateInterval limits inputInterval
    meter0 <- observeInterval limits inputInterval (Meter 0 (storedMaximumBits kernel))
    ((rewardLower, rewardUpper), meter1) <- coordinateExtrema limits rewardCoordinate rewardCoefficient rows inputInterval meter0
    ((successorLower, successorUpper), meter2) <- coordinateExtrema limits successorCoordinate successorCoefficient rows inputInterval meter1
    unless (meterArithmetic meter2 == arithmeticPlan) (Left (JointAffineMachineIndexExceeded JointAffineWork (meterArithmetic meter2)))
    let report = makeReport JointAffineSupportExtrema rawCount ownerCount retainedCount coefficientCount (basePreflight rawCount) (meterArithmetic meter2) (meterBits meter2)
    unless (jointAffineWork report == totalWork) (Left (JointAffineMachineIndexExceeded JointAffineWork (jointAffineWork report)))
    pure ((RationalInterval rewardLower rewardUpper, RationalInterval successorLower successorUpper), report)

-- | Read the report retained at construction or either renaming operation.
jointAffineKernelReport :: ExactJointAffineKernel owner source reward successor -> ExactJointAffineReport
jointAffineKernelReport (UnsafeExactJointAffineKernel _ _ _ _ _ _ _ report) = report

-- Internal validation and metering ------------------------------------------------

machineMaximum :: Natural
machineMaximum = fromIntegral (maxBound :: Int)

validateJointLimits :: ExactJointAffineLimits -> Either ExactJointAffineError ()
validateJointLimits limits = mapM_ validate fields
  where
    fields =
        [ (JointAffineRawEntries, jointLimitRawEntries limits)
        , (JointAffineOwners, jointLimitOwners limits)
        , (JointAffineOutputs, jointLimitOutputs limits)
        , (JointAffineCoefficients, jointLimitCoefficients limits)
        , (JointAffineWork, jointLimitWork limits)
        , (JointAffineRationalBits, jointLimitRationalBits limits)
        ]
    validate (dimension, value)
        | value == 0 = Left (InvalidJointAffineLimit dimension value)
        | value > machineMaximum = Left (JointAffineMachineIndexExceeded dimension value)
        | otherwise = Right ()

checkDimension :: ExactJointAffineLimits -> ExactJointAffineLimitDimension -> Natural -> Natural -> Either ExactJointAffineError ()
checkDimension _ dimension maximumAllowed actual
    | actual > maximumAllowed = Left (JointAffineLimitExceeded dimension maximumAllowed actual)
    | otherwise = Right ()

checkedMachineCount :: ExactJointAffineLimitDimension -> Natural -> Either ExactJointAffineError Natural
checkedMachineCount dimension actual
    | actual > machineMaximum = Left (JointAffineMachineIndexExceeded dimension actual)
    | otherwise = Right actual

boundedRows :: ExactJointAffineLimits -> [value] -> Either ExactJointAffineError [value]
boundedRows limits = go 0 []
  where
    maximumAllowed = jointLimitRawEntries limits
    go _ values [] = Right (reverse values)
    go count _ (_ : _) | count >= maximumAllowed = Left (JointAffineLimitExceeded JointAffineRawEntries maximumAllowed (maximumAllowed + 1))
    go count values (value : rest) = go (count + 1) (value : values) rest

boundedRenaming :: ExactJointAffineLimits -> [value] -> Either ExactJointAffineError (Natural, [value])
boundedRenaming limits = go 0 []
  where
    maximumAllowed = jointLimitRawEntries limits
    go count values [] = Right (count, reverse values)
    go count _ (_ : _) | count >= maximumAllowed = Left (JointAffineLimitExceeded JointAffineRawEntries maximumAllowed (maximumAllowed + 1))
    go count values (value : rest) = go (count + 1) (value : values) rest

naturalLength :: [value] -> Natural
naturalLength = foldr (const (+ 1)) 0

basePreflight :: Natural -> Natural
basePreflight rawCount = rawCount + rawCount * rawCount

operationPreflight :: ExactJointAffineLimits -> Natural -> Natural -> Either ExactJointAffineError Natural
operationPreflight limits rawCount arithmeticPlan = do
    arithmeticCount <- checkedMachineCount JointAffineWork arithmeticPlan
    total <- checkedMachineCount JointAffineWork (basePreflight rawCount + arithmeticCount)
    checkDimension limits JointAffineWork (jointLimitWork limits) total
    pure total

{- | Reserve conservative work slots before alpha-renaming scans or derived
lists.  The terms are, in order: stored-kernel preflight, admitted mapping
spine, source-pair comparisons, target-pair comparisons, source membership in
the complete declaration table, optional complete-scope coverage, per-row mapping lookup,
post-rename collision comparisons, and canonical ordering comparisons.
-}
renamingPreflight :: ExactJointAffineLimits -> OwnerScopeChange -> Natural -> Natural -> Natural -> Either ExactJointAffineError Natural
renamingPreflight limits scopeChange rawCount declaredCount mappingCount = do
    let pairComparisons = mappingCount * mappingCount
        membership = mappingCount * declaredCount
        completeness = case scopeChange of
            PreserveOwnerScope -> 0
            ChangeOwnerScope -> declaredCount * mappingCount
        rowLookup = declaredCount * mappingCount
        declarationComparisons = declaredCount * declaredCount
        planned =
            basePreflight rawCount
                + mappingCount
                + pairComparisons
                + pairComparisons
                + membership
                + completeness
                + rowLookup
                + declarationComparisons
                + declarationComparisons
    total <- checkedMachineCount JointAffineWork planned
    checkDimension limits JointAffineWork (jointLimitWork limits) total
    pure total

validateStored :: ExactJointAffineLimits -> ExactJointAffineKernel owner source reward successor -> Either ExactJointAffineError ()
validateStored limits kernel@(UnsafeExactJointAffineKernel _ _ _ _ rawCount ownerCount coefficientCount _) = do
    validateJointLimits limits
    checkDimension limits JointAffineRawEntries (jointLimitRawEntries limits) rawCount
    checkDimension limits JointAffineOwners (jointLimitOwners limits) ownerCount
    checkDimension limits JointAffineOutputs (jointLimitOutputs limits) 2
    checkDimension limits JointAffineCoefficients (jointLimitCoefficients limits) coefficientCount
    checkDimension limits JointAffineWork (jointLimitWork limits) (basePreflight rawCount)
    checkDimension limits JointAffineRationalBits (jointLimitRationalBits limits) (storedMaximumBits kernel)

validateRationals ::
    ExactJointAffineLimits ->
    ExactAffineInputCoordinate source reward ->
    ExactAffineInputCoordinate source successor ->
    [(NoiseOwner owner, RationalInterval, Rational, Rational)] ->
    Either ExactJointAffineError Natural
validateRationals limits (UnsafeExactAffineInputCoordinate rewardScale rewardOffset) (UnsafeExactAffineInputCoordinate successorScale successorOffset) rows = do
    initial <- foldM (flip (observe limits)) (Meter 0 0) [rewardScale, rewardOffset, successorScale, successorOffset]
    final <- foldM validateRow initial rows
    pure (meterBits final)
  where
    validateRow meter (_, interval, rewardValue, successorValue) = do
        validateInterval limits interval
        withInterval <- observeInterval limits interval meter
        foldM (flip (observe limits)) withInterval [rewardValue, successorValue]

validateInterval :: ExactJointAffineLimits -> RationalInterval -> Either ExactJointAffineError ()
validateInterval _ (RationalInterval lower upper)
    | lower < upper = Right ()
    | otherwise = Left (JointAffineInvalidInterval lower upper)

integerBitsLocal :: Integer -> Natural
integerBitsLocal value = go 0 (abs value)
  where
    go count 0 = max 1 count
    go count remaining = go (count + 1) (remaining `div` 2)

rationalBitsLocal :: Rational -> Natural
rationalBitsLocal value = max (integerBitsLocal (numerator value)) (integerBitsLocal (denominator value))

data Meter = Meter
    { meterArithmetic :: !Natural
    , meterBits :: !Natural
    }

observe :: ExactJointAffineLimits -> Rational -> Meter -> Either ExactJointAffineError Meter
observe limits value meter = do
    let actual = rationalBitsLocal value
    checkDimension limits JointAffineRationalBits (jointLimitRationalBits limits) actual
    pure meter{meterBits = max (meterBits meter) actual}

observeInterval :: ExactJointAffineLimits -> RationalInterval -> Meter -> Either ExactJointAffineError Meter
observeInterval limits (RationalInterval lower upper) meter = observe limits lower meter >>= observe limits upper

arithmetic :: ExactJointAffineLimits -> (Rational -> Rational -> Rational) -> Rational -> Rational -> Meter -> Either ExactJointAffineError (Rational, Meter)
arithmetic limits operation left right meter = do
    observedLeft <- observe limits left meter
    observedRight <- observe limits right observedLeft
    let result = operation left right
    observedResult <- observe limits result observedRight
    let work = meterArithmetic observedResult + 1
    checked <- checkedMachineCount JointAffineWork work
    checkDimension limits JointAffineWork (jointLimitWork limits) checked
    pure (result, observedResult{meterArithmetic = checked})

applyCoordinate :: ExactJointAffineLimits -> ExactAffineInputCoordinate source target -> Rational -> Meter -> Either ExactJointAffineError (Rational, Meter)
applyCoordinate limits (UnsafeExactAffineInputCoordinate scale offset) input meter = do
    (scaled, multiplied) <- arithmetic limits (*) scale input meter
    arithmetic limits (+) scaled offset multiplied

accountCoordinateSupport :: ExactJointAffineLimits -> Rational -> (OwnerRow -> Rational) -> [OwnerRow] -> Meter -> Either ExactJointAffineError Meter
accountCoordinateSupport limits constant coefficientOf = go constant constant
  where
    go _ _ [] meter = Right meter
    go lowerTotal upperTotal (row : rest) meter = do
        let coefficient = coefficientOf row
            RationalInterval lower upper = rowInterval row
            (lowerEndpoint, upperEndpoint)
                | coefficient >= 0 = (lower, upper)
                | otherwise = (upper, lower)
        (lowerContribution, meter1) <- arithmetic limits (*) coefficient lowerEndpoint meter
        (upperContribution, meter2) <- arithmetic limits (*) coefficient upperEndpoint meter1
        (nextLower, meter3) <- arithmetic limits (+) lowerTotal lowerContribution meter2
        (nextUpper, meter4) <- arithmetic limits (+) upperTotal upperContribution meter3
        go nextLower nextUpper rest meter4

coordinateExtrema ::
    ExactJointAffineLimits ->
    ExactAffineInputCoordinate source target ->
    (OwnerRow -> Rational) ->
    [OwnerRow] ->
    RationalInterval ->
    Meter ->
    Either ExactJointAffineError ((Rational, Rational), Meter)
coordinateExtrema limits (UnsafeExactAffineInputCoordinate scale offset) coefficientOf rows inputInterval meter = do
    (initialBounds, meter1) <- termBounds limits scale inputInterval meter
    let (initialLower, initialUpper) = initialBounds
    (lowerWithOffset, meter2) <- arithmetic limits (+) offset initialLower meter1
    (upperWithOffset, meter3) <- arithmetic limits (+) offset initialUpper meter2
    go lowerWithOffset upperWithOffset rows meter3
  where
    go lowerTotal upperTotal [] current = Right ((lowerTotal, upperTotal), current)
    go lowerTotal upperTotal (row : rest) current = do
        (bounds, meter1) <- termBounds limits (coefficientOf row) (rowInterval row) current
        (nextLower, meter2) <- arithmetic limits (+) lowerTotal (fst bounds) meter1
        (nextUpper, meter3) <- arithmetic limits (+) upperTotal (snd bounds) meter2
        go nextLower nextUpper rest meter3

termBounds :: ExactJointAffineLimits -> Rational -> RationalInterval -> Meter -> Either ExactJointAffineError ((Rational, Rational), Meter)
termBounds limits coefficient (RationalInterval lower upper) meter = do
    let (lowerEndpoint, upperEndpoint)
            | coefficient >= 0 = (lower, upper)
            | otherwise = (upper, lower)
    (lowerContribution, meter1) <- arithmetic limits (*) coefficient lowerEndpoint meter
    (upperContribution, meter2) <- arithmetic limits (*) coefficient upperEndpoint meter1
    pure ((lowerContribution, upperContribution), meter2)

-- Insertion ordering plus the preceding duplicate scan fits n^2 slots.
orderRows :: [OwnerRow] -> [OwnerRow]
orderRows = foldr insert []
  where
    insert row [] = [row]
    insert row rows@(other : rest)
        | rowOwner row <= rowOwner other = row : rows
        | otherwise = other : insert row rest

executableRow :: OwnerRow -> Bool
executableRow row = rewardCoefficient row /= 0 || successorCoefficient row /= 0

rowOwner :: OwnerRow -> Natural
rowOwner (OwnerRow owner _ _ _) = owner

rowInterval :: OwnerRow -> RationalInterval
rowInterval (OwnerRow _ interval _ _) = interval

rewardCoefficient :: OwnerRow -> Rational
rewardCoefficient (OwnerRow _ _ coefficient _) = coefficient

successorCoefficient :: OwnerRow -> Rational
successorCoefficient (OwnerRow _ _ _ coefficient) = coefficient

renameRowTo :: Natural -> OwnerRow -> OwnerRow
renameRowTo owner (OwnerRow _ interval rewardValue successorValue) = OwnerRow owner interval rewardValue successorValue

renamingSource :: (NoiseOwner old, NoiseOwner new) -> Natural
renamingSource (NoiseOwner source, _) = source

renamingTarget :: (NoiseOwner old, NoiseOwner new) -> Natural
renamingTarget (_, NoiseOwner target) = target

sourceIsDeclared :: [OwnerRow] -> (NoiseOwner old, NoiseOwner new) -> Bool
sourceIsDeclared rows mapping = any ((== renamingSource mapping) . rowOwner) rows

rowHasSource :: [(NoiseOwner old, NoiseOwner new)] -> OwnerRow -> Bool
rowHasSource renaming row = any ((== rowOwner row) . renamingSource) renaming

renameOwner :: [(NoiseOwner old, NoiseOwner new)] -> Natural -> Natural
renameOwner [] owner = owner
renameOwner (mapping : rest) owner
    | renamingSource mapping == owner = renamingTarget mapping
    | otherwise = renameOwner rest owner

hasDuplicateBy :: (Eq key) => (value -> key) -> [value] -> Bool
hasDuplicateBy project = hasDuplicate . map project

hasDuplicate :: (Eq value) => [value] -> Bool
hasDuplicate [] = False
hasDuplicate (value : rest) = value `elem` rest || hasDuplicate rest

storedMaximumBits :: ExactJointAffineKernel owner source reward successor -> Natural
storedMaximumBits (UnsafeExactJointAffineKernel _ _ _ _ _ _ _ report) = jointAffineMaximumRationalBits report

makeReport :: ExactJointAffineOperation -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> ExactJointAffineReport
makeReport operation rawCount ownerCount retainedCount coefficientCount preflightWork arithmeticWork maximumBits =
    ExactJointAffineReport
        { jointAffineOperation = operation
        , jointAffineRawEntryCount = rawCount
        , jointAffineDeclaredOwnerCount = ownerCount
        , jointAffineRetainedOwnerCount = retainedCount
        , jointAffineOutputCount = 2
        , jointAffineCoefficientCount = coefficientCount
        , jointAffinePreflightWork = preflightWork
        , jointAffineArithmeticWork = arithmeticWork
        , jointAffineWork = preflightWork + arithmeticWork
        , jointAffineMaximumRationalBits = maximumBits
        }

-- Left-successor substitution ---------------------------------------------------

-- | Untrusted complete routing of right declarations, never reusable evidence.
data ExactSuccessorOwnerRequest leftOwner rightOwner
    = SharedSuccessorOwners [(NoiseOwner rightOwner, NoiseOwner leftOwner)]
    | FreshSuccessorOwners [(NoiseOwner rightOwner, Natural)]

type role ExactSuccessorOwnerRequest nominal nominal

-- | Request exact interval-checked sharing with the anchored left namespace.
sharedSuccessorOwners :: [(NoiseOwner rightOwner, NoiseOwner leftOwner)] -> ExactSuccessorOwnerRequest leftOwner rightOwner
sharedSuccessorOwners = SharedSuccessorOwners

-- | Propose numeric targets disjoint from both complete input manifests.
freshSuccessorOwners :: [(NoiseOwner rightOwner, Natural)] -> ExactSuccessorOwnerRequest leftOwner rightOwner
freshSuccessorOwners = FreshSuccessorOwners

-- | Combined raw, reservation, output, coefficient, work and bit limits.
data ExactSuccessorSubstitutionLimits = SuccessorSubstitutionLimits !ExactJointAffineLimits

-- | Construct separately interpreted, operation-wide substitution limits.
exactSuccessorSubstitutionLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> ExactSuccessorSubstitutionLimits
exactSuccessorSubstitutionLimits raw owners outputs coefficients work bits =
    SuccessorSubstitutionLimits (exactJointAffineLimits raw owners outputs coefficients work bits)

-- | Input whose complete declaration manifest contains a fresh target.
data SuccessorSubstitutionParticipant = SuccessorSubstitutionLeft | SuccessorSubstitutionRight
    deriving stock (Eq, Show)

{- | Atomic admission, mapping, law, or internal accounting failure. Positions
are one-based; interval mismatch carries the left interval before the right.
-}
data ExactSuccessorSubstitutionError
    = SuccessorSubstitutionAdmission !ExactJointAffineError
    | SuccessorSubstitutionDuplicateSource !Natural !Natural
    | SuccessorSubstitutionNonInjectiveTarget !Natural !Natural
    | SuccessorSubstitutionUnknownSource !Natural
    | SuccessorSubstitutionIncompleteMapping !Natural
    | SuccessorSubstitutionUnknownSharedTarget !Natural
    | SuccessorSubstitutionFreshTargetCollision !Natural !SuccessorSubstitutionParticipant !Natural
    | SuccessorSubstitutionSharedIntervalMismatch !Natural !RationalInterval !RationalInterval
    | SuccessorSubstitutionAccountingMismatch !Natural !Natural
    deriving stock (Eq, Show)

-- | Exactly two complete modes, with no mixed routing.
data ExactSuccessorSubstitutionMode = SharedRightOwners | FreshRightOwners
    deriving stock (Eq, Show)

-- | Opaque immutable reserved-semantic-slot accounting, not measured CPU cost.
data ExactSuccessorSubstitutionReport
    = SuccessorSubstitutionReport
        !ExactSuccessorSubstitutionMode
        !SubstitutionPlan
        !Natural
        !Natural
        !Natural
        !Natural

-- | Selected routing mode.
substitutionMode :: ExactSuccessorSubstitutionReport -> ExactSuccessorSubstitutionMode
substitutionMode (SuccessorSubstitutionReport mode _ _ _ _ _) = mode

reportPlan :: ExactSuccessorSubstitutionReport -> SubstitutionPlan
reportPlan (SuccessorSubstitutionReport _ plan _ _ _ _) = plan

-- | Full left declarations, including zeros.
substitutionLeftDeclaredOwners :: ExactSuccessorSubstitutionReport -> Natural
substitutionLeftDeclaredOwners = planLeft . reportPlan

-- | Full right declarations, including zeros.
substitutionRightDeclaredOwners :: ExactSuccessorSubstitutionReport -> Natural
substitutionRightDeclaredOwners = planRight . reportPlan

-- | Admitted request entries.
substitutionMappingEntries :: ExactSuccessorSubstitutionReport -> Natural
substitutionMappingEntries = planMapping . reportPlan

-- | Combined input declarations and request entries.
substitutionRawEntries :: ExactSuccessorSubstitutionReport -> Natural
substitutionRawEntries = planRaw . reportPlan

-- | Pre-union name reservation upper bound.
substitutionOwnerReservationSlots :: ExactSuccessorSubstitutionReport -> Natural
substitutionOwnerReservationSlots = substitutionRawEntries

-- | Actual reserved-name cardinality; aliases are not extra source laws.
substitutionReservedNames :: ExactSuccessorSubstitutionReport -> Natural
substitutionReservedNames (SuccessorSubstitutionReport _ _ names _ _ _) = names

-- | Shared right declarations, zero in fresh mode.
substitutionSharedOwners :: ExactSuccessorSubstitutionReport -> Natural
substitutionSharedOwners report = if substitutionMode report == SharedRightOwners then substitutionMappingEntries report else 0

-- | Fresh right declarations, zero in shared mode.
substitutionFreshOwners :: ExactSuccessorSubstitutionReport -> Natural
substitutionFreshOwners report = if substitutionMode report == FreshRightOwners then substitutionMappingEntries report else 0

-- | Complete result sources, including canceled and zero rows.
substitutionDeclaredResultOwners :: ExactSuccessorSubstitutionReport -> Natural
substitutionDeclaredResultOwners (SuccessorSubstitutionReport _ _ _ declared _ _) = declared

-- | Executable result sources with at least one nonzero coefficient.
substitutionRetainedResultOwners :: ExactSuccessorSubstitutionReport -> Natural
substitutionRetainedResultOwners (SuccessorSubstitutionReport _ _ _ _ retained _) = retained

-- | Nonexecutable declarations retained privately.
substitutionZeroFilteredResultOwners :: ExactSuccessorSubstitutionReport -> Natural
substitutionZeroFilteredResultOwners report = substitutionDeclaredResultOwners report - substitutionRetainedResultOwners report

-- | Fixed result codomain width.
substitutionOutputs :: ExactSuccessorSubstitutionReport -> Natural
substitutionOutputs _ = 2

-- | Combined input-plus-result logical coefficient reservation.
substitutionCoefficientSlots :: ExactSuccessorSubstitutionReport -> Natural
substitutionCoefficientSlots = planCoefficients . reportPlan

-- | Complete structural reservation, including mode-inapplicable slots.
substitutionPreflightWork :: ExactSuccessorSubstitutionReport -> Natural
substitutionPreflightWork = planStructural . reportPlan

-- | Fixed coefficient multiplication count, including zero operands.
substitutionCoefficientMultiplications :: ExactSuccessorSubstitutionReport -> Natural
substitutionCoefficientMultiplications = planMultiplications . reportPlan

-- | Fixed coefficient addition count, including zero candidates.
substitutionCoefficientAdditions :: ExactSuccessorSubstitutionReport -> Natural
substitutionCoefficientAdditions = planAdditions . reportPlan

-- | Multiplications plus additions.
substitutionArithmeticWork :: ExactSuccessorSubstitutionReport -> Natural
substitutionArithmeticWork = planArithmetic . reportPlan

-- | Complete combined structural and arithmetic reservation.
substitutionWork :: ExactSuccessorSubstitutionReport -> Natural
substitutionWork = planWork . reportPlan

{- | Historical rational maximum, including discarded inputs and intermediates.
Both subsequent projections admit and seed their meters with this maximum.
-}
substitutionMaximumRationalBits :: ExactSuccessorSubstitutionReport -> Natural
substitutionMaximumRationalBits (SuccessorSubstitutionReport _ _ _ _ _ bits) = bits

-- No owner parameter, kernel eliminator, or callback is exported. The private
-- kernel is merely the existing projection representation, built without a
-- separately budgeted public constructor. Reservation aliases stay separate.
data SubstitutionOwner

-- | Sealed, non-chainable right-reward/right-successor substitution result.
data ExactSuccessorSubstitution sourceLabel rewardLabel successorLabel
    = SealedSuccessorSubstitution
        !(ExactJointAffineKernel SubstitutionOwner sourceLabel rewardLabel successorLabel)
        ![Natural]
        !ExactSuccessorSubstitutionReport

type role ExactSuccessorSubstitution nominal nominal nominal

-- | Read immutable substitution accounting without exposing owner identity.
successorSubstitutionReport :: ExactSuccessorSubstitution source reward successor -> ExactSuccessorSubstitutionReport
successorSubstitutionReport (SealedSuccessorSubstitution _ _ report) = report

-- | Separately budgeted real-Borel materialization with historical bit admission.
materializeSuccessorSubstitution :: ExactJointAffineLimits -> ExactSuccessorSubstitution source reward successor -> Rational -> Either ExactJointAffineError (ExactJointLaw RealBorel RealBorel, ExactJointAffineReport)
materializeSuccessorSubstitution limits (SealedSuccessorSubstitution kernel _ _) = materializeJointAffineKernel limits kernel

-- | Separately budgeted extrema with historical bit admission, not a bit reset.
successorSubstitutionSupportExtrema :: ExactJointAffineLimits -> ExactSuccessorSubstitution source reward successor -> RationalInterval -> Either ExactJointAffineError ((RationalInterval, RationalInterval), ExactJointAffineReport)
successorSubstitutionSupportExtrema limits (SealedSuccessorSubstitution kernel _ _) = jointAffineSupportExtrema limits kernel

data SubstitutionPlan = SubstitutionPlan
    { planLeft :: !Natural
    , planRight :: !Natural
    , planMapping :: !Natural
    , planRaw :: !Natural
    , planCoefficients :: !Natural
    , planStructural :: !Natural
    , planMultiplications :: !Natural
    , planAdditions :: !Natural
    }

planArithmetic :: SubstitutionPlan -> Natural
planArithmetic plan = planMultiplications plan + planAdditions plan

planWork :: SubstitutionPlan -> Natural
planWork plan = planStructural plan + planArithmetic plan

substitutionPlan :: Natural -> Natural -> Natural -> SubstitutionPlan
substitutionPlan left right mappings = SubstitutionPlan left right mappings h (12 + 4 * n) structural (4 + 2 * left) (2 + 2 * n)
  where
    n = left + right
    h = n + mappings
    structural =
        basePreflight left
            + basePreflight right
            + 8
            + 5 * n
            + mappings
            + 2 * mappings * mappings
            + 2 * mappings * right
            + mappings * left
            + mappings * n
            + 2 * mappings
            + n * mappings
            + 2 * n * n
            + n
            + h
            + 2 * h * h

-- Strict scalar-only traversal: neither a copied mapping nor an inspected
-- entry exists before the entire combined plan has been admitted.
countSuccessorMapping :: ExactJointAffineLimits -> Natural -> [value] -> Either ExactJointAffineError Natural
countSuccessorMapping limits initial = go initial
  where
    maximumAllowed = jointLimitRawEntries limits
    go !count [] = Right (count - initial)
    go !count (_ : _) | count >= maximumAllowed = Left (JointAffineLimitExceeded JointAffineRawEntries maximumAllowed (maximumAllowed + 1))
    go !count (_ : rest) = go (count + 1) rest

admitSubstitutionDimension :: ExactJointAffineLimits -> ExactJointAffineLimitDimension -> Natural -> Natural -> Either ExactJointAffineError ()
admitSubstitutionDimension limits dimension cap actual = do
    checked <- checkedMachineCount dimension actual
    checkDimension limits dimension cap checked

substitutionAdmission :: Either ExactJointAffineError value -> Either ExactSuccessorSubstitutionError value
substitutionAdmission = either (Left . SuccessorSubstitutionAdmission) Right

-- Immutable references to the ACTUAL admitted full tables and coordinate
-- values, not a count/phantom/cache identity. Execution accepts only witnesses
-- capturing these operands, never a second pair supplied by its caller.
data SubstitutionOperand = SubstitutionOperand !Rational !Rational !Rational !Rational ![OwnerRow] !Natural

-- Membership scans resolve actual rows once; shared endpoint equality must
-- not repeat full-manifest lookups outside its two-per-mapping reservation.
data ResolvedSuccessorOwner = ResolvedSuccessorOwner !OwnerRow !Natural !(Maybe OwnerRow)

data AdmittedSuccessorOperands
    = AdmittedSuccessorOperands
        !ExactJointAffineLimits
        !SubstitutionPlan
        !SubstitutionOperand
        !SubstitutionOperand
        ![ResolvedSuccessorOwner]
        !Meter

data VerifiedSharedSuccessorOwners leftOwner rightOwner = VerifiedSharedSuccessorOwners !AdmittedSuccessorOperands
data VerifiedFreshSuccessorOwners leftOwner rightOwner = VerifiedFreshSuccessorOwners !AdmittedSuccessorOperands

type role VerifiedSharedSuccessorOwners nominal nominal
type role VerifiedFreshSuccessorOwners nominal nominal

captureOperand :: ExactJointAffineKernel owner source reward successor -> SubstitutionOperand
captureOperand kernel@(UnsafeExactJointAffineKernel (UnsafeExactAffineInputCoordinate r d) (UnsafeExactAffineInputCoordinate s f) rows _ _ _ _ _) =
    SubstitutionOperand r d s f rows (storedMaximumBits kernel)

operandRows :: SubstitutionOperand -> [OwnerRow]
operandRows (SubstitutionOperand _ _ _ _ rows _) = rows

admitOperandRationals :: ExactJointAffineLimits -> SubstitutionOperand -> Meter -> Either ExactJointAffineError Meter
admitOperandRationals limits (SubstitutionOperand r d s f rows maximumBits) meter = do
    checkDimension limits JointAffineRationalBits (jointLimitRationalBits limits) maximumBits
    initial <- foldM (flip (observe limits)) meter{meterBits = max (meterBits meter) maximumBits} [r, d, s, f]
    foldM rowRationals initial rows
  where
    rowRationals current (OwnerRow _ interval reward successor) = do
        validateInterval limits interval
        endpoints <- observeInterval limits interval current
        observe limits reward endpoints >>= observe limits successor

{- | Substitute only the left successor into both right coordinates. The left
reward is admitted but lost, never accumulated. Failure order is combined
count-only preflight; duplicate source/target; membership; coverage; target
validity; left then right rational history and declarations; shared interval
equality; fixed arithmetic; invariants; then atomic publication.
-}
substituteLeftSuccessor ::
    ExactSuccessorSubstitutionLimits ->
    ExactJointAffineKernel leftOwner sourceLabel leftRewardLabel intermediateLabel ->
    ExactJointAffineKernel rightOwner intermediateLabel rightRewardLabel successorLabel ->
    ExactSuccessorOwnerRequest leftOwner rightOwner ->
    Either ExactSuccessorSubstitutionError (ExactSuccessorSubstitution sourceLabel rightRewardLabel successorLabel)
substituteLeftSuccessor (SuccessorSubstitutionLimits limits) left right request = do
    substitutionAdmission (validateJointLimits limits)
    let leftCount = jointAffineDeclaredOwnerCount (jointAffineKernelReport left)
        rightCount = jointAffineDeclaredOwnerCount (jointAffineKernelReport right)
        inputCount = leftCount + rightCount
    substitutionAdmission (admitSubstitutionDimension limits JointAffineRawEntries (jointLimitRawEntries limits) inputCount)
    mappingCount <- substitutionAdmission $ case request of
        SharedSuccessorOwners mapping -> countSuccessorMapping limits inputCount mapping
        FreshSuccessorOwners mapping -> countSuccessorMapping limits inputCount mapping
    let plan = substitutionPlan leftCount rightCount mappingCount
    substitutionAdmission $ do
        admitSubstitutionDimension limits JointAffineOwners (jointLimitOwners limits) (planRaw plan)
        admitSubstitutionDimension limits JointAffineOutputs (jointLimitOutputs limits) 2
        admitSubstitutionDimension limits JointAffineCoefficients (jointLimitCoefficients limits) (planCoefficients plan)
        admitSubstitutionDimension limits JointAffineWork (jointLimitWork limits) (planWork plan)
    -- Derived tables and mapping entries are demanded only after all admission.
    let capturedLeft = captureOperand left
        capturedRight = captureOperand right
        leftRows = operandRows capturedLeft
        rightRows = operandRows capturedRight
        (mode, mapping) = case request of
            SharedSuccessorOwners pairs -> (SharedRightOwners, [(source, target) | (NoiseOwner source, NoiseOwner target) <- pairs])
            FreshSuccessorOwners pairs -> (FreshRightOwners, [(source, target) | (NoiseOwner source, target) <- pairs])
    rejectDuplicate fst SuccessorSubstitutionDuplicateSource mapping
    rejectDuplicate snd SuccessorSubstitutionNonInjectiveTarget mapping
    sources <- mapM (resolveRightSource rightRows) (numbered mapping)
    mapM_ (\(position, row) -> unless (any ((== rowOwner row) . resolvedSource) sources) (Left (SuccessorSubstitutionIncompleteMapping position))) (numbered rightRows)
    resolved <- case mode of
        SharedRightOwners -> mapM (resolveSharedTarget leftRows) (numbered sources)
        FreshRightOwners -> do
            mapM_ (checkFreshTarget leftRows rightRows) (numbered mapping)
            pure sources
    meter <- substitutionAdmission (admitOperandRationals limits capturedLeft (Meter 0 0) >>= admitOperandRationals limits capturedRight)
    let admitted = AdmittedSuccessorOperands limits plan capturedLeft capturedRight resolved meter
    case mode of
        SharedRightOwners -> do
            mapM_ checkSharedInterval (numbered resolved)
            executeSharedSuccessor (VerifiedSharedSuccessorOwners admitted)
        FreshRightOwners -> executeFreshSuccessor (VerifiedFreshSuccessorOwners admitted)

numbered :: [value] -> [(Natural, value)]
numbered = zip [1 ..]

rejectDuplicate :: (Eq key) => (value -> key) -> (Natural -> Natural -> ExactSuccessorSubstitutionError) -> [value] -> Either ExactSuccessorSubstitutionError ()
rejectDuplicate key failure = go . numbered
  where
    go [] = Right ()
    go ((position, value) : rest) = do
        mapM_ (\(otherPosition, other) -> when (key value == key other) (Left (failure position otherPosition))) rest
        go rest

checkFreshTarget :: [OwnerRow] -> [OwnerRow] -> (Natural, (Natural, Natural)) -> Either ExactSuccessorSubstitutionError ()
checkFreshTarget left right (position, (_, target)) = do
    check SuccessorSubstitutionLeft left
    check SuccessorSubstitutionRight right
  where
    check participant = mapM_ (\(declaration, row) -> when (rowOwner row == target) (Left (SuccessorSubstitutionFreshTargetCollision position participant declaration))) . numbered

resolvedSource :: ResolvedSuccessorOwner -> Natural
resolvedSource (ResolvedSuccessorOwner row _ _) = rowOwner row

resolvedTarget :: ResolvedSuccessorOwner -> Natural
resolvedTarget (ResolvedSuccessorOwner _ target _) = target

resolveRightSource :: [OwnerRow] -> (Natural, (Natural, Natural)) -> Either ExactSuccessorSubstitutionError ResolvedSuccessorOwner
resolveRightSource rows (position, (source, target)) = case find ((== source) . rowOwner) rows of
    Just row -> Right (ResolvedSuccessorOwner row target Nothing)
    Nothing -> Left (SuccessorSubstitutionUnknownSource position)

resolveSharedTarget :: [OwnerRow] -> (Natural, ResolvedSuccessorOwner) -> Either ExactSuccessorSubstitutionError ResolvedSuccessorOwner
resolveSharedTarget rows (position, ResolvedSuccessorOwner source target _) = case find ((== target) . rowOwner) rows of
    Just row -> Right (ResolvedSuccessorOwner source target (Just row))
    Nothing -> Left (SuccessorSubstitutionUnknownSharedTarget position)

checkSharedInterval :: (Natural, ResolvedSuccessorOwner) -> Either ExactSuccessorSubstitutionError ()
checkSharedInterval (position, ResolvedSuccessorOwner rightRow _ evidence) = case evidence of
    Nothing -> Left (SuccessorSubstitutionAccountingMismatch 1 0)
    Just leftRow -> do
        let leftInterval@(RationalInterval ll lu) = rowInterval leftRow
            rightInterval@(RationalInterval rl ru) = rowInterval rightRow
            mismatch = Left (SuccessorSubstitutionSharedIntervalMismatch position leftInterval rightInterval)
        unless (ll == rl) mismatch
        unless (lu == ru) mismatch

executeSharedSuccessor :: VerifiedSharedSuccessorOwners left right -> Either ExactSuccessorSubstitutionError (ExactSuccessorSubstitution source reward successor)
executeSharedSuccessor (VerifiedSharedSuccessorOwners operands) = executeSuccessor SharedRightOwners operands

executeFreshSuccessor :: VerifiedFreshSuccessorOwners left right -> Either ExactSuccessorSubstitutionError (ExactSuccessorSubstitution source reward successor)
executeFreshSuccessor (VerifiedFreshSuccessorOwners operands) = executeSuccessor FreshRightOwners operands

executeSuccessor :: ExactSuccessorSubstitutionMode -> AdmittedSuccessorOperands -> Either ExactSuccessorSubstitutionError (ExactSuccessorSubstitution source reward successor)
executeSuccessor mode (AdmittedSuccessorOperands limits plan left right mapping initial) = do
    let SubstitutionOperand _ _ a b leftRows _ = left
        SubstitutionOperand c d e f rightRows _ = right
        checked operation x y = substitutionAdmission . arithmetic limits operation x y
    (rewardScale, m1) <- checked (*) c a initial
    (rewardShift, m2) <- checked (*) c b m1
    (rewardOffset, m3) <- checked (+) rewardShift d m2
    (successorScale, m4) <- checked (*) e a m3
    (successorShift, m5) <- checked (*) e b m4
    (successorOffset, m6) <- checked (+) successorShift f m5
    (leftCandidates, m7) <- scaleLeftRows limits c e leftRows m6
    rightCandidates <- mapM translate rightRows
    -- Stable ordering retains left-before-right within every shared bucket.
    let candidates = orderRows (leftCandidates ++ rightCandidates)
    checkCandidateIntervals candidates
    (declarations, final) <- accumulateCandidates limits candidates m7
    accounting (planArithmetic plan) (meterArithmetic final)
    accounting (planWork plan) (planStructural plan + meterArithmetic final)
    let declared = case mode of
            SharedRightOwners -> planLeft plan
            FreshRightOwners -> planLeft plan + planRight plan
    accounting declared (naturalLength declarations)
    let retained = filter executableRow declarations
        reservations = orderedNames (map rowOwner leftRows ++ map rowOwner rightRows ++ if mode == FreshRightOwners then map resolvedTarget mapping else [])
        retainedCount = naturalLength retained
        bits = meterBits final
        report = SuccessorSubstitutionReport mode plan (naturalLength reservations) declared retainedCount bits
        projectionReport = makeReport JointAffineConstruction declared declared retainedCount (4 + 2 * declared) (basePreflight declared) 0 bits
        kernel =
            UnsafeExactJointAffineKernel
                (UnsafeExactAffineInputCoordinate rewardScale rewardOffset)
                (UnsafeExactAffineInputCoordinate successorScale successorOffset)
                declarations
                retained
                declared
                declared
                (4 + 2 * declared)
                projectionReport
    pure (SealedSuccessorSubstitution kernel reservations report)
  where
    translate row = case find ((== rowOwner row) . resolvedSource) mapping of
        Just resolved -> Right (renameRowTo (resolvedTarget resolved) row)
        Nothing -> Left (SuccessorSubstitutionAccountingMismatch 1 0)

accounting :: Natural -> Natural -> Either ExactSuccessorSubstitutionError ()
accounting expected actual = unless (expected == actual) (Left (SuccessorSubstitutionAccountingMismatch expected actual))

scaleLeftRows :: ExactJointAffineLimits -> Rational -> Rational -> [OwnerRow] -> Meter -> Either ExactSuccessorSubstitutionError ([OwnerRow], Meter)
scaleLeftRows limits c e = go []
  where
    go acc [] meter = Right (reverse acc, meter)
    go acc (OwnerRow owner interval _ q : rest) meter = do
        (r, m1) <- substitutionAdmission (arithmetic limits (*) c q meter)
        (s, m2) <- substitutionAdmission (arithmetic limits (*) e q m1)
        go (OwnerRow owner interval r s : acc) rest m2

checkCandidateIntervals :: [OwnerRow] -> Either ExactSuccessorSubstitutionError ()
checkCandidateIntervals [] = Right ()
checkCandidateIntervals (row : rest) = do
    mapM_ (\other -> when (rowOwner row == rowOwner other && rowInterval row /= rowInterval other) (Left (SuccessorSubstitutionAccountingMismatch 1 0))) rest
    checkCandidateIntervals rest

accumulateCandidates :: ExactJointAffineLimits -> [OwnerRow] -> Meter -> Either ExactSuccessorSubstitutionError ([OwnerRow], Meter)
accumulateCandidates limits = go []
  where
    go acc [] meter = Right (reverse acc, meter)
    go acc rows@(row : _) meter = do
        let (bucket, rest) = span ((== rowOwner row) . rowOwner) rows
        (r, s, final) <- foldM add (0, 0, meter) bucket
        go (OwnerRow (rowOwner row) (rowInterval row) r s : acc) rest final
    add (r, s, meter) row = do
        (nextR, m1) <- substitutionAdmission (arithmetic limits (+) r (rewardCoefficient row) meter)
        (nextS, m2) <- substitutionAdmission (arithmetic limits (+) s (successorCoefficient row) m1)
        pure (nextR, nextS, m2)

orderedNames :: [Natural] -> [Natural]
orderedNames = foldr insert []
  where
    insert name [] = [name]
    insert name names@(other : rest)
        | name < other = name : names
        | name == other = names
        | otherwise = other : insert name rest
