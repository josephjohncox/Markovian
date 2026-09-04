{-# LANGUAGE RoleAnnotations #-}

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
absence of a cross-kernel composition operation prevent a number from being
used as global sharing evidence.  'alphaRenameJointAffineKernel' performs a
partial rename without changing that scope.  'reScopeJointAffineKernel' changes
the scope only when given a complete injective mapping of every retained owner.
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
) where

import Control.Monad (foldM, unless, when)
import Data.List (sortOn)
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
input, reward, successor, and owner roles cannot be changed with 'coerce'.
Only the owner parameter scopes run-time owner identity.  The three coordinate
labels do not stand for measurable spaces; the represented domain and both
outputs are always 'RealBorel'.
-}
data ExactJointAffineKernel owner sourceLabel rewardLabel successorLabel
    = UnsafeExactJointAffineKernel
        !(ExactAffineInputCoordinate sourceLabel rewardLabel)
        !(ExactAffineInputCoordinate sourceLabel successorLabel)
        ![OwnerRow]
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
therefore complete before zero-row filtering.  Canonical retained rows are
ordered by owner number.
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
    let retained =
            sortOn
                rowOwner
                [ OwnerRow owner interval rewardValue successorValue
                | (NoiseOwner owner, interval, rewardValue, successorValue) <- raw
                , rewardValue /= 0 || successorValue /= 0
                ]
        retainedCount = naturalLength retained
        report = makeReport JointAffineConstruction rawCount ownerCount retainedCount coefficientCount preflightWork 0 maximumBits
    pure (UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate retained rawCount ownerCount coefficientCount report)

{- | Partially rename owners without changing the nominal owner scope.

Precedence is stored-kernel admission; bounded mapping-spine admission; the
complete conservative rename-work plan; duplicate sources; duplicate targets;
unknown sources; collisions after partial replacement; then canonical sorting.
The plan is checked before any semantic scan or derived owner list is made.
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
change scope only for a kernel with no retained owners.
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
renameJointAffineKernel scopeChange limits inputRenaming kernel@(UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate rows rawCount ownerCount coefficientCount _) = do
    validateStored limits kernel
    (mappingCount, renaming) <- boundedRenaming limits inputRenaming
    let retainedCount = jointAffineRetainedOwnerCount (jointAffineKernelReport kernel)
    renameWork <- renamingPreflight limits scopeChange rawCount retainedCount mappingCount
    when (hasDuplicateBy renamingSource renaming) (Left JointAffineDuplicateRenamingSource)
    when (hasDuplicateBy renamingTarget renaming) (Left JointAffineNonInjectiveRenaming)
    unless (all (sourceIsRetained rows) renaming) (Left JointAffineUnknownRenamingSource)
    when (scopeChange == ChangeOwnerScope && not (all (rowHasSource renaming) rows)) (Left JointAffineIncompleteRenaming)
    let renamedOwners = map (renameOwner renaming . rowOwner) rows
    when (hasDuplicate renamedOwners) (Left JointAffineNonInjectiveRenaming)
    let renamed = sortOn rowOwner (zipWith renameRowTo renamedOwners rows)
        operation = case scopeChange of
            PreserveOwnerScope -> JointAffineAlphaRenaming
            ChangeOwnerScope -> JointAffineScopeRenaming
        report = makeReport operation rawCount ownerCount retainedCount coefficientCount renameWork 0 (storedMaximumBits kernel)
    pure (UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate renamed rawCount ownerCount coefficientCount report)

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
materializeJointAffineKernel limits kernel@(UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate rows rawCount ownerCount coefficientCount _) input = do
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
jointAffineSupportExtrema limits kernel@(UnsafeExactJointAffineKernel rewardCoordinate successorCoordinate rows rawCount ownerCount coefficientCount _) inputInterval = do
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
jointAffineKernelReport (UnsafeExactJointAffineKernel _ _ _ _ _ _ report) = report

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
the retained table, optional complete-scope coverage, per-row mapping lookup,
post-rename collision comparisons, and canonical ordering comparisons.
-}
renamingPreflight :: ExactJointAffineLimits -> OwnerScopeChange -> Natural -> Natural -> Natural -> Either ExactJointAffineError Natural
renamingPreflight limits scopeChange rawCount retainedCount mappingCount = do
    let pairComparisons = mappingCount * mappingCount
        membership = mappingCount * retainedCount
        completeness = case scopeChange of
            PreserveOwnerScope -> 0
            ChangeOwnerScope -> retainedCount * mappingCount
        rowLookup = retainedCount * mappingCount
        retainedComparisons = retainedCount * retainedCount
        planned =
            basePreflight rawCount
                + mappingCount
                + pairComparisons
                + pairComparisons
                + membership
                + completeness
                + rowLookup
                + retainedComparisons
                + retainedComparisons
    total <- checkedMachineCount JointAffineWork planned
    checkDimension limits JointAffineWork (jointLimitWork limits) total
    pure total

validateStored :: ExactJointAffineLimits -> ExactJointAffineKernel owner source reward successor -> Either ExactJointAffineError ()
validateStored limits kernel@(UnsafeExactJointAffineKernel _ _ _ rawCount ownerCount coefficientCount _) = do
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

sourceIsRetained :: [OwnerRow] -> (NoiseOwner old, NoiseOwner new) -> Bool
sourceIsRetained rows mapping = any ((== renamingSource mapping) . rowOwner) rows

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
storedMaximumBits (UnsafeExactJointAffineKernel _ _ _ _ _ _ report) = jointAffineMaximumRationalBits report

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
