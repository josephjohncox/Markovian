{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Markovian.Tensor.Internal where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (AsyncException, Exception, SomeException, displayException, evaluate, fromException, mask, onException, throwIO, toException, try)
import Control.Monad (forM, forM_, when, zipWithM)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.TypeLits (Nat, SomeNat (..), natVal, someNatVal)
import Markovian.Tensor.Shape
import Numeric.Natural (Natural)

-- Dtype -----------------------------------------------------------------------

-- | Closed numerical storage types. The initial runtime supports F64 only.
data DType = F64
    deriving (Eq, Show)

-- | Singleton evidence for one supported storage type.
data SDType (dtype :: DType) where
    SF64 :: SDType 'F64

type role SDType nominal

-- | Haskell scalar represented by a storage type.
type family Scalar (dtype :: DType) where
    Scalar 'F64 = Double

dtypeBytes :: SDType dtype -> Natural
dtypeBytes SF64 = 8

-- Limits and reports ----------------------------------------------------------

-- | Shape, payload, buffer, and cumulative scalar-work limits for one session.
data SessionLimits = SessionLimits
    { limitRank :: !Natural
    , limitDimension :: !Natural
    , limitElements :: !Natural
    , limitSinglePayloadBytes :: !Natural
    , limitFreshPayloadBytes :: !Natural
    , limitBuffers :: !Natural
    , limitScalarWork :: !Natural
    , limitAffine :: !(Maybe AffineLimits)
    }
    deriving (Eq, Show)

-- | Build limits in rank, dimension, elements, single bytes, fresh bytes, buffers, and work order.
tensorSessionLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> SessionLimits
tensorSessionLimits rank dimension elements single fresh buffers work =
    SessionLimits rank dimension elements single fresh buffers work Nothing

data SessionState = SessionState
    { stateNextStorage :: !Natural
    , statePayloadBytes :: !Natural
    , stateBuffers :: !Natural
    , stateScalarWork :: !Natural
    , stateLiveAllocations :: ![ForeignPtr Double]
    , stateClosed :: !Bool
    , stateAffineUsage :: !AffineUsage
    }

-- Private allocator capability. Tests in this package can inject deterministic
-- allocation and cleanup failures without exposing that control publicly.
data TensorAllocator = TensorAllocator
    { allocatorAllocate :: Int -> IO (Either String (ForeignPtr Double))
    , allocatorFinalize :: ForeignPtr Double -> IO (Either String ())
    }

-- | Opaque serialized allocation and work account for one rank-2 region.
data TensorSession region = TensorSession !SessionLimits !TensorAllocator !(MVar SessionState)

type role TensorSession nominal

{- | Exception raised only when an action exception and one or more
cleanup diagnostics must be preserved together. Diagnostics are retained by
the runtime protocol; their String contents and rendering are not numerically bounded.
-}
data TensorSessionException = TensorSessionException !SomeException ![String]

instance Show TensorSessionException where
    show (TensorSessionException primary diagnostics) =
        "TensorSessionException " ++ show (displayException primary) ++ " " ++ show diagnostics

instance Exception TensorSessionException

-- | Checked action failure paired with a deferred cleanup interruption.
data TensorSessionFailureException = TensorSessionFailureException !TensorError !SomeException ![String]

instance Show TensorSessionFailureException where
    show (TensorSessionFailureException primary interruption diagnostics) =
        "TensorSessionFailureException " ++ show primary ++ " " ++ show (displayException interruption) ++ " " ++ show diagnostics

instance Exception TensorSessionFailureException

{- | Run an action in a fresh managed region. Nominal region indices reject
direct tensor escape at a fixed external region index; they do not confine IO
closures or existentially packaged tensors. All region-dependent observation,
refinement, primitives and allocation must execute and complete before the
callback exits. The caller must join or cancel-and-join dependent children
before every exit, including Left, exceptions and asynchronous interruption;
the runner does not join them. Do not later invoke escaped region-dependent
actions or observe retained tensors. Ordinary copied data whose reads completed
in the callback may be returned. On success, Left or exception, the runtime
drains the committed registry and attempts finalization. Asynchronous cleanup
interruptions are retained and the interrupted callback is retried before
propagation. Finalization attempts and logical accounting do not guarantee
prompt physical reclamation or release by a failing callback.
-}
withTensorSession :: SessionLimits -> (forall region. TensorSession region -> IO (Either TensorError value)) -> IO (Either TensorError value)
withTensorSession = withTensorSessionAllocator defaultTensorAllocator

withTensorSessionAllocator :: TensorAllocator -> SessionLimits -> (forall region. TensorSession region -> IO (Either TensorError value)) -> IO (Either TensorError value)
withTensorSessionAllocator allocator limits action = mask $ \restore -> do
    state <- newMVar (SessionState 0 0 0 0 [] False (affineInitialUsage (limitAffine limits)))
    let session = TensorSession limits allocator state
    outcome <- try @SomeException (restore (action session))
    CleanupResult diagnostics cleanupInterruptions <- closeTensorSession session
    case outcome of
        Left problem -> case diagnostics of
            [] -> throwIO problem
            _ -> throwIO (TensorSessionException problem diagnostics)
        Right result -> case cleanupInterruptions of
            interruption : _ -> case result of
                Left primary -> throwIO (TensorSessionFailureException primary interruption diagnostics)
                Right _ -> throwIO (TensorSessionException interruption diagnostics)
            [] -> pure $ case diagnostics of
                [] -> result
                _ -> case result of
                    Left primary -> Left (TensorPrimaryAndCleanupFailure primary diagnostics)
                    Right _ -> Left (TensorSessionCleanupFailure diagnostics)

defaultTensorAllocator :: TensorAllocator
defaultTensorAllocator =
    TensorAllocator
        { allocatorAllocate = trySynchronous . mallocForeignPtrArray
        , allocatorFinalize = trySynchronous . finalizeForeignPtr
        }

trySynchronous :: IO value -> IO (Either String value)
trySynchronous action = do
    outcome <- try @SomeException action
    case outcome of
        Right value -> pure (Right value)
        Left problem -> case fromException problem :: Maybe AsyncException of
            Just asynchronous -> throwIO asynchronous
            Nothing -> pure (Left (displayException problem))

data CleanupResult = CleanupResult ![String] ![SomeException]

closeTensorSession :: TensorSession region -> IO CleanupResult
closeTensorSession (TensorSession _ allocator lock) = do
    pointers <- modifyMVar lock $ \state ->
        if stateClosed state
            then pure (state, [])
            else pure (state{stateLiveAllocations = [], stateClosed = True}, stateLiveAllocations state)
    finalizeAll allocator pointers

-- Every registered finalizer is attempted under the caller's masked state.
-- If an asynchronous exception arrives at an interruptible point before a
-- finalizer completes, retain it and retry that same finalizer before moving
-- to the next pointer. Thus cleanup iteration cannot turn a deferred
-- interruption into a skipped allocation. The retained interruption is
-- rethrown only after all finalizers complete.
finalizeAll :: TensorAllocator -> [ForeignPtr Double] -> IO CleanupResult
finalizeAll allocator = go 1
  where
    go :: Natural -> [ForeignPtr Double] -> IO CleanupResult
    go _ [] = pure (CleanupResult [] [])
    go index (pointer : pointers) = do
        current <- finalizeOne index pointer
        rest <- go (index + 1) pointers
        pure (appendCleanup current rest)

    finalizeOne :: Natural -> ForeignPtr Double -> IO CleanupResult
    finalizeOne index pointer = do
        released <- try @SomeException (allocatorFinalize allocator pointer)
        let prefix problem = "cleanup-" ++ show index ++ ": " ++ problem
        case released of
            Right (Right ()) -> pure (CleanupResult [] [])
            Right (Left problem) -> pure (CleanupResult [prefix problem] [])
            Left problem -> case fromException problem :: Maybe AsyncException of
                Just _ -> do
                    retried <- finalizeOne index pointer
                    pure $
                        appendCleanup
                            (CleanupResult [prefix ("asynchronous interruption: " ++ displayException problem)] [problem])
                            retried
                Nothing -> pure (CleanupResult [prefix (displayException problem)] [])

    appendCleanup :: CleanupResult -> CleanupResult -> CleanupResult
    appendCleanup (CleanupResult leftDiagnostics leftInterruptions) (CleanupResult rightDiagnostics rightInterruptions) =
        CleanupResult (leftDiagnostics ++ rightDiagnostics) (leftInterruptions ++ rightInterruptions)

throwAfterCleanup :: SomeException -> CleanupResult -> IO value
throwAfterCleanup primary (CleanupResult diagnostics interruptions) =
    case (diagnostics, interruptions) of
        ([], []) -> throwIO primary
        _ -> throwIO (TensorSessionException primary diagnostics)

runAllocatorAction :: IO (Either String value) -> IO (Either String value)
runAllocatorAction action = do
    outcome <- trySynchronous action
    pure $ case outcome of
        Left problem -> Left problem
        Right result -> result

allocationFailure :: String -> [String] -> TensorError
allocationFailure primary [] = HostAllocationFailure primary
allocationFailure primary diagnostics = HostAllocationFailure (primary ++ "; " ++ unwords diagnostics)

-- | Runtime-known payload accounting. This is not GHC heap or RSS usage.
data TensorMemoryReport = TensorMemoryReport
    { reportFreshPayloadBytes :: !Natural
    -- ^ New logical payload bytes.
    , reportMaximumSingleAllocationBytes :: !Natural
    -- ^ Largest logical payload.
    , reportScratchBytes :: !Natural
    -- ^ Planned scratch payload; currently zero.
    , reportAllocationCount :: !Natural
    -- ^ New managed payload buffers.
    }
    deriving (Eq, Show)

-- | Deterministic timing-free account for one successful operation.
data TensorOperationReport = TensorOperationReport
    { reportPrimitive :: !String
    -- ^ Stable primitive name.
    , reportScalarWork :: !Natural
    -- ^ Declared scalar operation charge.
    , reportMemory :: !TensorMemoryReport
    -- ^ Logical payload plan.
    }
    deriving (Eq, Show)

-- | Render a stable report without addresses or clock time.
renderTensorOperationReport :: TensorOperationReport -> String
renderTensorOperationReport report =
    unlines
        [ "tensor-operation-report"
        , "primitive: " ++ reportPrimitive report
        , "scalar-work: " ++ show (reportScalarWork report)
        , "fresh-payload-bytes: " ++ show (reportFreshPayloadBytes memory)
        , "maximum-single-allocation-bytes: " ++ show (reportMaximumSingleAllocationBytes memory)
        , "scratch-bytes: " ++ show (reportScratchBytes memory)
        , "allocation-count: " ++ show (reportAllocationCount memory)
        ]
  where
    memory = reportMemory report

-- Errors ----------------------------------------------------------------------

-- | Checked rank, dimension, product, and typed-shape failures.
data ShapeError
    = RankLimitExceeded !Natural !Natural
    | DimensionLimitExceeded !Natural !Natural
    | ElementLimitExceeded !Natural !Natural
    | MachineIndexOverflow !Natural
    | ShapeMismatch ![Natural] ![Natural]
    | MatMulInnerDimensionMismatch !Natural !Natural
    deriving (Eq, Show)

-- | Checked layout and view failures.
data LayoutError
    = LayoutRankMismatch !Natural !Natural
    | LayoutOutOfBounds !Natural !Natural
    | NonContiguousReshape
    deriving (Eq, Show)

-- | Operation preflight failures. No operation result or report escapes.
data BudgetError
    = SinglePayloadLimitExceeded !Natural !Natural
    | FreshPayloadLimitExceeded !Natural !Natural
    | BufferLimitExceeded !Natural !Natural
    | ScalarWorkLimitExceeded !Natural !Natural
    deriving (Eq, Show)

-- | Nonfinite input or arithmetic result with deterministic location.
data NumericError
    = NonFiniteInput !String !Natural
    | NonFiniteIntermediate !String !Natural
    deriving (Eq, Show)

-- | Complete public host-runtime failure type.
data TensorError
    = TensorShapeError !ShapeError
    | TensorAffineError !AffineProblem
    | TensorLayoutError !LayoutError
    | TensorBudgetError !BudgetError
    | TensorNumericError !NumericError
    | InputLengthMismatch !Natural !Natural
    | InputLengthExceedsShape !Natural
    | EmptyOwnerKey
    | HostAllocationFailure !String
    | TensorSessionClosed
    | TensorSessionCleanupFailure ![String]
    | TensorPrimaryAndCleanupFailure !TensorError ![String]
    deriving (Eq, Show)

-- Affine policy and admission -------------------------------------------------

-- | Independent logical metadata limits; tensor payload is a separate account.
data AffineLimit
    = AffineRank
    | AffineDimension
    | AffineElements
    | AffineConstructedCells
    | AffineWork
    | AffineLiveCells
    deriving (Eq, Show)

-- | Bounded diagnostic location.
data AffineInput
    = AffineBaseShape
    | AffineViewShape
    | AffineTargetShape
    | AffineOffset
    | AffineStrides
    | AffinePermutation
    | AffineAxis
    | AffineStarts
    | AffineSteps
    deriving (Eq, Show)

-- | Affine admission or geometry failure. Required credit uses cap+1 sentinels.
data AffineProblem
    = AffineDisabled
    | AffineInvalidLimit !AffineLimit !Int
    | AffineLimitTooSmall !AffineLimit !Natural !Natural
    | AffineLimitExceeded !AffineLimit !Natural !Natural
    | AffineListLength !AffineInput !Natural !Natural
    | AffineSignedRange !AffineInput !Natural
    | AffineArithmeticOverflow !AffineInput !Natural
    | AffineShapeDisagreement !AffineInput !Natural !Natural !Natural
    | AffineAxisOutOfRange !Int !Natural
    | AffineDuplicateAxis !Natural !Natural
    | AffineZeroStep !Natural
    | AffineParentDomain !Natural !Integer !Integer !Natural
    | AffineEmptyDescriptor
    | AffineAddressBounds !Integer !Integer !Natural
    | AffineOverlap !Natural !Natural !Natural
    | AffineNonContiguousBase
    | AffinePhysicalBounds !Natural !Natural !Natural
    | AffineRuntimeCounterOverflow
    deriving (Eq, Show)

-- | Cumulative successful path charges, not a global retry or heap account.
data AffineUsage = AffineUsage
    { affineUsedCells :: !Natural
    , affineUsedWork :: !Natural
    , affineHighWaterCells :: !Natural
    }
    deriving (Eq, Show)

-- | Exact reservation, including newly retained metadata in live cells.
data AffineCharge = AffineCharge
    { affineConstructedCells :: !Natural
    , affineWork :: !Natural
    , affineLiveCells :: !Natural
    , affineRetainedCells :: !Natural
    }
    deriving (Eq, Show)

-- | A pure successful reservation and its resulting immutable path usage.
data AffineMapReport = AffineMapReport
    { affineMapCharge :: !AffineCharge
    , affineMapCumulative :: !AffineUsage
    }
    deriving (Eq, Show)

-- | Runtime affine reservation alongside the unchanged payload/scalar report.
data AffineOperationReport = AffineOperationReport
    { affineOperationCharge :: !AffineCharge
    , affineOperationCumulative :: !AffineUsage
    , affineTensorReport :: !TensorOperationReport
    }
    deriving (Eq, Show)

-- | Validated policy. Construction rejects negative inputs before minima.
data AffineLimits = AffineLimits
    { affineLimitRank :: !Natural
    , affineLimitDimension :: !Natural
    , affineLimitElements :: !Natural
    , affineLimitConstructedCells :: !Natural
    , affineLimitWork :: !Natural
    , affineLimitLiveCells :: !Natural
    }
    deriving (Eq, Show)

-- | Immutable planning path; older budgets remain reusable.
data AffineBudget = AffineBudget !AffineLimits !AffineUsage

-- | Validate rank, dimension, elements, constructed cells, work, and live cells.
affineLimits :: Int -> Int -> Int -> Int -> Int -> Int -> Either TensorError AffineLimits
affineLimits rank dimension elements cells work live = do
    nonnegative AffineRank rank
    nonnegative AffineDimension dimension
    nonnegative AffineElements elements
    nonnegative AffineConstructedCells cells
    nonnegative AffineWork work
    nonnegative AffineLiveCells live
    minimumLimit AffineConstructedCells 4672 cells
    minimumLimit AffineWork 512 work
    minimumLimit AffineLiveCells 576 live
    Right (AffineLimits (fromIntegral rank) (fromIntegral dimension) (fromIntegral elements) (fromIntegral cells) (fromIntegral work) (fromIntegral live))
  where
    nonnegative field value
        | value < 0 = affineFailure (AffineInvalidLimit field value)
        | otherwise = Right ()
    minimumLimit field minimumValue value
        | fromIntegral value < minimumValue = affineFailure (AffineLimitTooSmall field minimumValue (fromIntegral value))
        | otherwise = Right ()

-- | Enable an already validated policy without altering the seven old limits.
tensorSessionLimitsWithAffine :: SessionLimits -> AffineLimits -> SessionLimits
tensorSessionLimitsWithAffine limits policy = limits{limitAffine = Just policy}

-- | Start a planning path at the fixed metadata initialization reservation.
affineBudget :: AffineLimits -> Either TensorError AffineBudget
affineBudget policy = Right (AffineBudget policy (affineInitialUsage (Just policy)))

-- | Observe an existing usage record; this is not a ledger event.
affineBudgetUsage :: AffineBudget -> AffineUsage
affineBudgetUsage (AffineBudget _ usage) = usage

affineInitialUsage :: Maybe AffineLimits -> AffineUsage
affineInitialUsage Nothing = AffineUsage 0 0 0
affineInitialUsage (Just _) = AffineUsage 4672 512 576

affineFailure :: AffineProblem -> Either TensorError value
affineFailure problem = Left (TensorAffineError problem)

affineMachineMaximum :: Natural
affineMachineMaximum = fromIntegral (maxBound :: Int)

affineSignedMaximum :: Integer
affineSignedMaximum = toInteger affineMachineMaximum

affineSignedMinimum :: Integer
affineSignedMinimum = negate affineSignedMaximum

affineMachineFailure :: Either TensorError value
affineMachineFailure = Left (TensorShapeError (MachineIndexOverflow (affineMachineMaximum + 1)))

-- All formula arithmetic saturates at M, independently of configured caps.
-- Multiplication divides before multiplying; no rejected giant product exists.
affineAdd :: Natural -> Natural -> Natural
affineAdd x y
    | x > affineMachineMaximum || y > affineMachineMaximum = affineMachineMaximum + 1
    | x > affineMachineMaximum - y = affineMachineMaximum + 1
    | otherwise = x + y

affineMultiply :: Natural -> Natural -> Natural
affineMultiply x y
    | x == 0 || y == 0 = 0
    | x > affineMachineMaximum || y > affineMachineMaximum = affineMachineMaximum + 1
    | x > affineMachineMaximum `div` y = affineMachineMaximum + 1
    | otherwise = x * y

affineCredit :: AffineLimit -> Natural -> Natural -> Either TensorError ()
affineCredit field cap required
    | required > cap = affineFailure (AffineLimitExceeded field cap (cap + 1))
    | otherwise = Right ()

-- DESIGN candidate: local live is independent of the successful historical peak.
data AffineHeader = AffineHeader !Natural !Natural !Natural

affineHeaderStart :: AffineUsage -> AffineHeader
affineHeaderStart usage = AffineHeader (affineUsedCells usage) (affineUsedWork usage) 576

affineDebit :: AffineLimits -> AffineUsage -> AffineHeader -> Either TensorError AffineHeader
affineDebit policy usage (AffineHeader cells work localQ) = do
    let !nextCells = affineAdd cells 3124
    affineCredit AffineConstructedCells (affineLimitConstructedCells policy) nextCells
    let !nextWork = affineAdd work 384
    affineCredit AffineWork (affineLimitWork policy) nextWork
    let !nextLocalQ = affineAdd localQ 52
    affineCredit AffineLiveCells (affineLimitLiveCells policy) (max (affineHighWaterCells usage) nextLocalQ)
    Right (AffineHeader nextCells nextWork nextLocalQ)

-- Debit before inspecting even SNil. The rank sentinel precedes natVal;
-- product saturation at M preserves ordered old/affine nil diagnostics.
affineScanShape :: Maybe SessionLimits -> AffineLimits -> AffineUsage -> AffineHeader -> SShape shape -> Either TensorError (Natural, Natural, AffineHeader)
affineScanShape old policy usage = go 0 1 False
  where
    go :: Natural -> Natural -> Bool -> AffineHeader -> SShape current -> Either TensorError (Natural, Natural, AffineHeader)
    go !seen !productValue !zero !header shape = do
        next <- affineDebit policy usage header
        case shape of
            SNil -> do
                oldCheck limitElements ElementLimitExceeded productValue
                affineCredit AffineElements (affineLimitElements policy) productValue
                if productValue > affineMachineMaximum `div` 8
                    then affineMachineFailure
                    else Right (seen, productValue, next)
            SCons proxy rest -> do
                oldCheck limitRank RankLimitExceeded (seen + 1)
                affineCredit AffineRank (affineLimitRank policy) (seen + 1)
                let dimension = natVal proxy
                case old of
                    Nothing -> Right ()
                    Just limits ->
                        let cap = min affineMachineMaximum (limitDimension limits)
                         in if dimension > toInteger cap
                                then Left (TensorShapeError (DimensionLimitExceeded cap (cap + 1)))
                                else Right ()
                -- Compare before making a Natural copy of a supplied huge Nat.
                if dimension > toInteger (affineLimitDimension policy)
                    then affineCredit AffineDimension (affineLimitDimension policy) (affineLimitDimension policy + 1)
                    else Right ()
                let !d = fromInteger dimension
                    !nextZero = zero || d == 0
                    !nextProduct = if nextZero then 0 else affineMultiply productValue d
                go (seen + 1) nextProduct nextZero next rest
    oldCheck selector constructor value = case old of
        Nothing -> Right ()
        Just limits ->
            let cap = min affineMachineMaximum (selector limits)
             in if value > cap
                    then Left (TensorShapeError (constructor cap (cap + 1)))
                    else Right ()

-- Length only: an excess cons never demands its element or its tail.
affineScanList :: AffineLimits -> AffineUsage -> AffineInput -> Natural -> AffineHeader -> [value] -> Either TensorError AffineHeader
affineScanList policy usage field expected = go 0
  where
    go !seen !header input = do
        next <- affineDebit policy usage header
        case input of
            []
                | seen == expected -> Right next
                | otherwise -> affineFailure (AffineListLength field expected seen)
            _ : _ | seen == expected -> affineFailure (AffineListLength field expected (expected + 1))
            _ : rest -> go (seen + 1) next rest

data AffinePlanKind = AffineNewPlan | AffineTransformPlan | AffineBindPlan | AffinePullbackPlan

-- DESIGN candidate: expanded positive polynomials; prefix coupons occur once.
affinePlan :: AffinePlanKind -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> AffineCharge
affinePlan kind rb rs rv baseCount viewCount buffers =
    let plus = affineAdd
        mul = affineMultiply
        pairs
            | viewCount == 0 = 0
            | even viewCount = mul (viewCount `div` 2) (viewCount - 1)
            | otherwise = mul viewCount ((viewCount - 1) `div` 2)
        addresses = mul viewCount (plus 208 (mul 96 rv))
        collisions = mul pairs (plus 284 (mul 192 rv))
        (!work, !workspace, !retained) = case kind of
            AffineNewPlan ->
                ( plus (plus (plus (plus 3870 (mul 384 rb)) (mul 1202 rv)) addresses) collisions
                , plus (plus 841 (mul 40 rb)) (mul 99 rv)
                , plus (plus 195 (mul 12 rb)) (mul 28 rv)
                )
            AffineTransformPlan ->
                ( plus (plus (plus (plus (plus (plus 4857 (mul 384 rb)) (mul 2012 rs)) (mul 384 rv)) (mul 80 (mul rs rs))) addresses) collisions
                , plus (plus (plus 921 (mul 40 rb)) (mul 139 rs)) (mul 59 rv)
                , plus (plus (plus 219 (mul 12 rb)) (mul 36 rs)) (mul 16 rv)
                )
            AffineBindPlan ->
                ( plus (plus 3920 (mul 852 rb)) (mul 767 rv)
                , plus (plus 1126 (mul 70 rb)) (mul 63 rv)
                , plus (plus 246 (mul 60 rb)) (mul 20 rv)
                )
            AffinePullbackPlan ->
                ( plus (plus (plus (plus (plus 5168 (mul 785 rb)) (mul 1486 rv)) (mul 12 baseCount)) (mul viewCount (plus 240 (mul 192 rv)))) (mul 24 buffers)
                , plus (plus (plus 1556 (mul 59 rb)) (mul 129 rv)) (mul 4 buffers)
                , plus (plus (plus 277 (mul 20 rb)) (mul 72 rv)) (mul 4 buffers)
                )
        !live = plus workspace retained
        !cells = plus (mul 8 work) live
     in AffineCharge cells work live retained

affineReserve :: AffineLimits -> AffineUsage -> AffineCharge -> Either TensorError AffineUsage
affineReserve policy old charge = do
    let !cells = affineAdd (affineUsedCells old) (affineConstructedCells charge)
    affineCredit AffineConstructedCells (affineLimitConstructedCells policy) cells
    let !work = affineAdd (affineUsedWork old) (affineWork charge)
    affineCredit AffineWork (affineLimitWork policy) work
    let !live = max (affineHighWaterCells old) (affineLiveCells charge)
    affineCredit AffineLiveCells (affineLimitLiveCells policy) live
    Right (AffineUsage cells work live)

-- Pure affine geometry --------------------------------------------------------

-- | Injective flat logical-base map. It contains no storage or owner identity.
data AffineMap (map :: Type) (base :: [Nat]) (view :: [Nat])
    = AffineMap
        !(SShape base)
        !(SShape view)
        !Natural
        !Natural
        !Natural
        !Natural
        !Integer
        ![Integer]
        !Integer
        !Integer
        !Bool

type role AffineMap nominal nominal nominal

-- Post-admission copies only. Both values and the terminating spine are forced.
affineDimensions :: SShape shape -> [Natural]
affineDimensions SNil = []
affineDimensions (SCons proxy rest) =
    let !dimension = fromInteger (natVal proxy)
        !tailDimensions = affineDimensions rest
     in dimension : tailDimensions

affineSignedInput :: AffineInput -> Natural -> Int -> Either TensorError ()
affineSignedInput field axis value
    | value == minBound = affineFailure (AffineSignedRange field axis)
    | otherwise = Right ()

affineSignedInputs :: AffineInput -> [Int] -> Either TensorError ()
affineSignedInputs field = go 0
  where
    go !_ [] = Right ()
    go !axis (value : rest) = do
        affineSignedInput field axis value
        go (axis + 1) rest

-- Operands have been admitted into [-M,M]; the temporary is at most 2b bits.
affineSignedResult :: AffineInput -> Natural -> Integer -> Either TensorError Integer
affineSignedResult field axis value
    | value < negate (toInteger affineMachineMaximum) || value > toInteger affineMachineMaximum = affineFailure (AffineArithmeticOverflow field axis)
    | otherwise = Right value

affineRawEmpty :: Int -> [Int] -> Either TensorError ()
affineRawEmpty offset strides
    | offset /= 0 = affineFailure AffineEmptyDescriptor
    | otherwise = go strides
  where
    go [] = Right ()
    go (stride : rest)
        | stride /= 0 = affineFailure AffineEmptyDescriptor
        | otherwise = go rest

affineIntegerInputs :: [Int] -> [Integer]
affineIntegerInputs [] = []
affineIntegerInputs (value : rest) =
    let !integer = toInteger value
        !tailValues = affineIntegerInputs rest
     in integer : tailValues

affineNormalize :: Bool -> [Natural] -> [Integer] -> [Integer]
affineNormalize _ [] _ = []
affineNormalize empty (dimension : dimensions) (stride : strides) =
    let !normalized = if empty || dimension == 1 then 0 else stride
        !rest = affineNormalize empty dimensions strides
     in normalized : rest
affineNormalize _ (_ : _) [] = []

-- A single reversed dimension/stride zipper is reused by every address kernel.
affineZipper :: [Natural] -> [Integer] -> [(Natural, Integer)]
affineZipper = go []
  where
    go !reversed (dimension : dimensions) (stride : strides) =
        let !pair = (dimension, stride)
         in go (pair : reversed) dimensions strides
    go !reversed _ _ = reversed

affineExtrema :: Integer -> [Natural] -> [Integer] -> Either TensorError (Integer, Integer)
affineExtrema offset = go 0 offset offset
  where
    go !_ !low !high [] [] = Right (low, high)
    go !axis !low !high (dimension : dimensions) (stride : strides) = do
        term <- affineSignedResult AffineStrides axis ((toInteger dimension - 1) * stride)
        nextLow <- affineSignedResult AffineOffset axis (low + min 0 term)
        nextHigh <- affineSignedResult AffineOffset axis (high + max 0 term)
        go (axis + 1) nextLow nextHigh dimensions strides
    go !axis _ _ _ _ = affineFailure (AffineListLength AffineStrides (axis + 1) axis)

affineContiguous :: [(Natural, Integer)] -> Bool
affineContiguous = go 1 True
  where
    go !_ !contiguous [] = contiguous
    go !expected !contiguous ((dimension, stride) : rest) =
        go (expected * toInteger dimension) (contiguous && (dimension <= 1 || stride == expected)) rest

-- Right-to-left quotRem digits, then forward checked dot product. Only one
-- digit workspace survives at a time; no address table or payload is retained.
affineAddress :: Natural -> Integer -> [Integer] -> [(Natural, Integer)] -> Natural -> Either TensorError Natural
affineAddress bound offset strides zipper index =
    let !digits = affineDigits index zipper []
     in dot 0 offset digits strides
  where
    affineDigits !_ [] !digits = digits
    affineDigits !remaining ((dimension, _) : rest) !digits =
        let (!quotient, !digit) = remaining `quotRem` dimension
         in affineDigits quotient rest (digit : digits)
    dot !_ !address [] []
        | address < 0 || address >= toInteger bound = affineFailure (AffineAddressBounds address address bound)
        | otherwise = Right (fromInteger address)
    dot !axis !address (digit : digits) (stride : remainingStrides) =
        let !term = toInteger digit * stride
         in if term < affineSignedMinimum || term > affineSignedMaximum
                then affineFailure (AffineArithmeticOverflow AffineStrides axis)
                else
                    let !next = address + term
                     in if next < affineSignedMinimum || next > affineSignedMaximum
                            then affineFailure (AffineArithmeticOverflow AffineOffset axis)
                            else dot (axis + 1) next digits remainingStrides
    dot !axis _ _ _ = affineFailure (AffineListLength AffineStrides (axis + 1) axis)

affineValidate :: Natural -> Natural -> Integer -> [Natural] -> [Integer] -> Either TensorError (Integer, Integer, Bool)
affineValidate _ 0 _ _ _ = Right (0, 0, True)
affineValidate baseCount count offset dimensions strides = do
    (low, high) <- affineExtrema offset dimensions strides
    if low < 0 || high >= toInteger baseCount
        then affineFailure (AffineAddressBounds low high baseCount)
        else Right ()
    let !zipper = affineZipper dimensions strides
    addresses zipper 0
    pairs zipper 0
    let !contiguous = affineContiguous zipper
    Right (low, high, contiguous)
  where
    address = affineAddress baseCount offset strides
    addresses zipper !index
        | index == count = Right ()
        | otherwise = do
            _ <- address zipper index
            addresses zipper (index + 1)
    pairs zipper !first
        | first == count = Right ()
        | otherwise = do
            suffix zipper first (first + 1)
            pairs zipper (first + 1)
    suffix zipper !first !second
        | second == count = Right ()
        | otherwise = do
            a <- address zipper first
            b <- address zipper second
            if a == b
                then affineFailure (AffineOverlap first second a)
                else suffix zipper first (second + 1)

affineFinish :: AffineLimits -> AffineUsage -> AffineCharge -> SShape base -> SShape view -> Natural -> Natural -> Natural -> Natural -> Integer -> [Natural] -> [Integer] -> (forall map. AffineMap map base view -> AffineBudget -> AffineMapReport -> value) -> Either TensorError value
affineFinish policy usage charge base view rb rv baseCount count offset dimensions strides continuation = do
    (low, high, contiguous) <- affineValidate baseCount count offset dimensions strides
    let !witness = AffineMap base view rb rv baseCount count offset strides low high contiguous
        !budget = AffineBudget policy usage
        !report = AffineMapReport charge usage
    Right (continuation witness budget report)

-- | Compile one checked, injective signed descriptor relative to a logical base.
withAffineMap :: AffineBudget -> SShape base -> SShape view -> Int -> [Int] -> (forall map. AffineMap map base view -> AffineBudget -> AffineMapReport -> value) -> Either TensorError value
withAffineMap (AffineBudget policy old) base view offset inputStrides continuation = do
    (rb, baseCount, afterBase) <- affineScanShape Nothing policy old (affineHeaderStart old) base
    (rv, count, afterView) <- affineScanShape Nothing policy old afterBase view
    _ <- affineScanList policy old AffineStrides rv afterView inputStrides
    let !charge = affinePlan AffineNewPlan rb 0 rv baseCount count 0
    usage <- affineReserve policy old charge
    affineSignedInput AffineOffset 0 offset
    affineSignedInputs AffineStrides inputStrides
    if count == 0 then affineRawEmpty offset inputStrides else Right ()
    let !dimensions = affineDimensions view
        !strides = affineNormalize (count == 0) dimensions (affineIntegerInputs inputStrides)
    affineFinish policy usage charge base view rb rv baseCount count (toInteger offset) dimensions strides continuation

affineTransformHeader :: AffineLimits -> AffineUsage -> SShape base -> SShape source -> SShape target -> Either TensorError (Natural, Natural, Natural, Natural, Natural, AffineHeader)
affineTransformHeader policy old base source target = do
    (rb, baseCount, afterBase) <- affineScanShape Nothing policy old (affineHeaderStart old) base
    (rs, _, afterSource) <- affineScanShape Nothing policy old afterBase source
    (rv, count, afterTarget) <- affineScanShape Nothing policy old afterSource target
    if rv /= rs
        then affineFailure (AffineShapeDisagreement AffineTargetShape 0 rs rv)
        else Right (rb, rs, rv, baseCount, count, afterTarget)

affineAxisRange :: Natural -> Int -> Either TensorError ()
affineAxisRange rank axis
    | axis < 0 || toInteger axis >= toInteger rank = affineFailure (AffineAxisOutOfRange axis rank)
    | otherwise = Right ()

-- Explicit linear lookup; its repetitions are covered by the transform rS^2 row.
affinePick :: AffineInput -> Natural -> [value] -> Either TensorError value
affinePick field requested = go 0
  where
    go !seen [] = affineFailure (AffineListLength field (requested + 1) seen)
    go !seen (value : rest)
        | seen == requested = Right value
        | otherwise = go (seen + 1) rest

affinePermutationRanges :: Natural -> [Int] -> Either TensorError ()
affinePermutationRanges _ [] = Right ()
affinePermutationRanges rank (axis : rest) = do
    affineAxisRange rank axis
    affinePermutationRanges rank rest

-- Walk suffixes: repeated list indexing here would add an unreserved cubic loop.
affinePermutationDuplicates :: [Int] -> Either TensorError ()
affinePermutationDuplicates = outer 0
  where
    outer !_ [] = Right ()
    outer !first (axis : rest) = do
        inner first axis (first + 1) rest
        outer (first + 1) rest
    inner !_ _ !_ [] = Right ()
    inner !first axis !second (candidate : rest)
        | axis == candidate = affineFailure (AffineDuplicateAxis first second)
        | otherwise = inner first axis (second + 1) rest

affinePermutationDimensions :: [Natural] -> [Natural] -> [Int] -> Either TensorError ()
affinePermutationDimensions source = go 0
  where
    go !_ [] [] = Right ()
    go !axis (dimension : dimensions) (selected : permutation) = do
        expected <- affinePick AffinePermutation (fromIntegral selected) source
        if dimension /= expected
            then affineFailure (AffineShapeDisagreement AffineTargetShape axis expected dimension)
            else go (axis + 1) dimensions permutation
    go !axis _ _ = affineFailure (AffineListLength AffinePermutation (axis + 1) axis)

affinePermutationStrides :: [Integer] -> [Int] -> Either TensorError [Integer]
affinePermutationStrides _ [] = Right []
affinePermutationStrides source (axis : permutation) = do
    !stride <- affinePick AffinePermutation (fromIntegral axis) source
    !rest <- affinePermutationStrides source permutation
    Right (stride : rest)

-- | Permute parent coordinates, retaining the original logical base.
permuteAffineMap :: AffineBudget -> AffineMap parent base source -> SShape target -> [Int] -> (forall child. AffineMap child base target -> AffineBudget -> AffineMapReport -> value) -> Either TensorError value
permuteAffineMap (AffineBudget policy old) (AffineMap base source _ _ _ _ offset sourceStrides _ _ _) target permutation continuation = do
    (rb, rs, rv, baseCount, count, header) <- affineTransformHeader policy old base source target
    _ <- affineScanList policy old AffinePermutation rs header permutation
    let !charge = affinePlan AffineTransformPlan rb rs rv baseCount count 0
    usage <- affineReserve policy old charge
    affineSignedInputs AffinePermutation permutation
    affinePermutationRanges rs permutation
    affinePermutationDuplicates permutation
    let !sourceDimensions = affineDimensions source
        !dimensions = affineDimensions target
    affinePermutationDimensions sourceDimensions dimensions permutation
    selected <- affinePermutationStrides sourceStrides permutation
    let !strides = affineNormalize (count == 0) dimensions selected
        !newOffset = if count == 0 then 0 else offset
    affineFinish policy usage charge base target rb rv baseCount count newOffset dimensions strides continuation

affineReverseStrides :: Natural -> [Integer] -> [Integer]
affineReverseStrides _ [] = []
affineReverseStrides axis (stride : strides) =
    let !selected = if axis == 0 then negate stride else stride
        !rest = if axis == 0 then affineCopyStrides strides else affineReverseStrides (axis - 1) strides
     in selected : rest

affineCopyStrides :: [Integer] -> [Integer]
affineCopyStrides [] = []
affineCopyStrides (stride : strides) =
    let !value = stride
        !rest = affineCopyStrides strides
     in value : rest

-- | Reverse one valid parent axis, including its domain checks for empty maps.
reverseAffineMap :: AffineBudget -> AffineMap parent base shape -> Int -> (forall child. AffineMap child base shape -> AffineBudget -> AffineMapReport -> value) -> Either TensorError value
reverseAffineMap (AffineBudget policy old) (AffineMap base source _ _ _ _ offset sourceStrides _ _ _) axis continuation = do
    (rb, rs, rv, baseCount, count, _) <- affineTransformHeader policy old base source source
    let !charge = affinePlan AffineTransformPlan rb rs rv baseCount count 0
    usage <- affineReserve policy old charge
    affineSignedInput AffineAxis 0 axis
    affineAxisRange rs axis
    let !dimensions = affineDimensions source
    newOffset <-
        if count == 0
            then Right 0
            else do
                dimension <- affinePick AffineAxis (fromIntegral axis) dimensions
                stride <- affinePick AffineAxis (fromIntegral axis) sourceStrides
                term <- affineSignedResult AffineOffset (fromIntegral axis) ((toInteger dimension - 1) * stride)
                affineSignedResult AffineOffset (fromIntegral axis) (offset + term)
    let !strides = affineNormalize (count == 0) dimensions (affineReverseStrides (fromIntegral axis) sourceStrides)
    affineFinish policy usage charge base source rb rv baseCount count newOffset dimensions strides continuation

affineNonzeroSteps :: [Int] -> Either TensorError ()
affineNonzeroSteps = go 0
  where
    go !_ [] = Right ()
    go !axis (step : rest)
        | step == 0 = affineFailure (AffineZeroStep axis)
        | otherwise = go (axis + 1) rest

affineSliceDomains :: [Natural] -> [Natural] -> [Int] -> [Int] -> Either TensorError ()
affineSliceDomains = go 0
  where
    go !_ [] [] [] [] = Right ()
    go !axis (parent : parents) (count : counts) (start : starts) (step : steps) = do
        end <-
            if count == 0
                then Right (toInteger start)
                else do
                    term <- affineSignedResult AffineSteps axis ((toInteger count - 1) * toInteger step)
                    affineSignedResult AffineStarts axis (toInteger start + term)
        let !low = min (toInteger start) end
            !high = max (toInteger start) end
        if low < 0 || (if count == 0 then high > toInteger parent else high >= toInteger parent)
            then affineFailure (AffineParentDomain axis low high parent)
            else go (axis + 1) parents counts starts steps
    go !axis _ _ _ _ = affineFailure (AffineListLength AffineStarts (axis + 1) axis)

affineSliceOffset :: Integer -> [Int] -> [Integer] -> Either TensorError Integer
affineSliceOffset = go 0
  where
    go !_ !offset [] [] = Right offset
    go !axis !offset (start : starts) (stride : strides) = do
        term <- affineSignedResult AffineOffset axis (toInteger start * stride)
        next <- affineSignedResult AffineOffset axis (offset + term)
        go (axis + 1) next starts strides
    go !axis _ _ _ = affineFailure (AffineListLength AffineStarts (axis + 1) axis)

affineSliceStrides :: [Integer] -> [Int] -> Either TensorError [Integer]
affineSliceStrides = go 0
  where
    go !_ [] [] = Right []
    go !axis (stride : strides) (step : steps) = do
        !value <- affineSignedResult AffineStrides axis (stride * toInteger step)
        !rest <- go (axis + 1) strides steps
        Right (value : rest)
    go !axis _ _ = affineFailure (AffineListLength AffineSteps (axis + 1) axis)

-- | Slice in parent coordinates; backing-capacity inclusion is not sufficient.
sliceAffineMap :: AffineBudget -> AffineMap parent base source -> SShape target -> [Int] -> [Int] -> (forall child. AffineMap child base target -> AffineBudget -> AffineMapReport -> value) -> Either TensorError value
sliceAffineMap (AffineBudget policy old) (AffineMap base source _ _ _ _ offset sourceStrides _ _ _) target starts steps continuation = do
    (rb, rs, rv, baseCount, count, header) <- affineTransformHeader policy old base source target
    afterStarts <- affineScanList policy old AffineStarts rs header starts
    _ <- affineScanList policy old AffineSteps rs afterStarts steps
    let !charge = affinePlan AffineTransformPlan rb rs rv baseCount count 0
    usage <- affineReserve policy old charge
    affineSignedInputs AffineStarts starts
    affineSignedInputs AffineSteps steps
    affineNonzeroSteps steps
    let !parents = affineDimensions source
        !dimensions = affineDimensions target
    affineSliceDomains parents dimensions starts steps
    newOffset <- if count == 0 then Right 0 else affineSliceOffset offset starts sourceStrides
    derived <- if count == 0 then Right sourceStrides else affineSliceStrides sourceStrides steps
    let !strides = affineNormalize (count == 0) dimensions derived
    affineFinish policy usage charge base target rb rv baseCount count newOffset dimensions strides continuation

-- Affine runtime --------------------------------------------------------------

-- | Actual original base/owner, reusable map, and shared finite physical view.
data OwnedAffineView (region :: Type) (owner :: Type) (map :: Type) (base :: [Nat]) (view :: [Nat])
    = OwnedAffineView !(OwnedTensor region owner 'F64 base) !(AffineMap map base view) !(FiniteTensor region 'F64 view)

type role OwnedAffineView nominal nominal nominal nominal nominal

-- | Observe the bound finite view without reading payload.
affineViewTensor :: OwnedAffineView region owner map base view -> FiniteTensor region 'F64 view
affineViewTensor (OwnedAffineView _ _ tensor) = tensor

-- | The actual explicitly supplied original base, not an inferred identity.
affineViewBase :: OwnedAffineView region owner map base view -> OwnedTensor region owner 'F64 base
affineViewBase (OwnedAffineView base _ _) = base

-- | The binding's original-base-relative map.
affineViewMap :: OwnedAffineView region owner map base view -> AffineMap map base view
affineViewMap (OwnedAffineView _ witness _) = witness

affineRuntimeEntry :: SessionLimits -> SessionState -> Bool -> Either TensorError AffineLimits
affineRuntimeEntry limits state needsStorage
    | stateClosed state = Left TensorSessionClosed
    | otherwise = case limitAffine limits of
        Nothing -> affineFailure AffineDisabled
        Just policy
            | statePayloadBytes state > affineMachineMaximum
                || stateScalarWork state > affineMachineMaximum
                || stateBuffers state > storageCap
                || stateNextStorage state > storageCap
                || affineUsedCells usage < 4672
                || affineUsedWork usage < 512
                || affineHighWaterCells usage < 576
                || affineUsedCells usage > affineLimitConstructedCells policy
                || affineUsedWork usage > affineLimitWork policy
                || affineHighWaterCells usage > affineLimitLiveCells policy ->
                affineFailure AffineRuntimeCounterOverflow
            | otherwise -> Right policy
  where
    usage = stateAffineUsage state
    storageCap = if needsStorage then affineMachineMaximum - 1 else affineMachineMaximum

affinePayloadPreflight :: SessionLimits -> SessionState -> Natural -> Natural -> Natural -> Either TensorError ()
affinePayloadPreflight limits state bytes buffers work = do
    check SinglePayloadLimitExceeded (limitSinglePayloadBytes limits) bytes
    check FreshPayloadLimitExceeded (limitFreshPayloadBytes limits) (affineAdd (statePayloadBytes state) bytes)
    check BufferLimitExceeded (limitBuffers limits) (affineAdd (stateBuffers state) buffers)
    check ScalarWorkLimitExceeded (limitScalarWork limits) (affineAdd (stateScalarWork state) work)
  where
    check constructor oldCap required =
        let cap = min affineMachineMaximum oldCap
         in if required > cap then Left (TensorBudgetError (constructor cap (cap + 1))) else Right ()

affineStoredCount :: AffineInput -> Natural -> Natural -> Either TensorError ()
affineStoredCount field expected actual
    | actual > affineMachineMaximum `div` 8 = affineMachineFailure
    | actual /= expected = affineFailure (AffineShapeDisagreement field 0 expected actual)
    | otherwise = Right ()

affineMetadataLength :: AffineInput -> Natural -> [value] -> Either TensorError ()
affineMetadataLength field expected = go 0
  where
    go !seen []
        | seen == expected = Right ()
        | otherwise = affineFailure (AffineShapeDisagreement field 0 expected seen)
    go !seen (_ : _)
        | seen == expected = affineFailure (AffineShapeDisagreement field 0 expected (expected + 1))
    go !seen (_ : rest) = go (seen + 1) rest

affineMetadataDimensions :: AffineInput -> [Natural] -> [Natural] -> Either TensorError ()
affineMetadataDimensions field = go 0
  where
    go !_ [] [] = Right ()
    go !axis (expected : dimensions) (actual : supplied)
        | expected /= actual = affineFailure (AffineShapeDisagreement field axis expected (min actual (affineMachineMaximum + 1)))
        | otherwise = go (axis + 1) dimensions supplied
    go !axis _ _ = affineFailure (AffineShapeDisagreement field 0 axis (axis + 1))

affineMetadataStrides :: [Integer] -> Either TensorError ()
affineMetadataStrides = go 0
  where
    go !_ [] = Right ()
    go !axis (stride : rest) = do
        _ <- affineSignedResult AffineStrides axis stride
        go (axis + 1) rest

-- Empty descriptors still check dimensions and both bounded list spines, but
-- never demand a stride value or an endpoint. No pointer is accessed here.
affineCheckDescriptor :: AffineInput -> Bool -> Natural -> Natural -> [Natural] -> HostTensor region 'F64 shape -> Either TensorError ()
affineCheckDescriptor field interval rank expected dimensions (HostTensor SF64 _ layout _ count capacity _) =
    affineCheckLayout field interval rank expected dimensions count capacity layout

affineCheckLayout :: AffineInput -> Bool -> Natural -> Natural -> [Natural] -> Natural -> Natural -> CheckedLayout shape -> Either TensorError ()
affineCheckLayout field interval rank expected dimensions count capacity layout = do
    if count > affineMachineMaximum `div` 8 then affineMachineFailure else Right ()
    if capacity > affineMachineMaximum `div` 8 then affineMachineFailure else Right ()
    affineStoredCount field expected count
    affineMetadataLength field rank (layoutDimensions layout)
    affineMetadataLength field rank (layoutStridesElements layout)
    affineMetadataDimensions field dimensions (layoutDimensions layout)
    let offset = layoutOffsetElements layout
    if offset < 0 || offset > toInteger affineMachineMaximum
        then affineFailure (AffineArithmeticOverflow AffineOffset 0)
        else Right ()
    let boundedOffset = fromInteger offset
        badInterval = affineFailure (AffinePhysicalBounds boundedOffset count capacity)
    if count == 0
        then if boundedOffset > capacity then badInterval else Right ()
        else do
            affineMetadataStrides (layoutStridesElements layout)
            (low, high) <- affineExtrema offset dimensions (layoutStridesElements layout)
            if low < 0 || high >= toInteger capacity then badInterval else Right ()
            if interval && (boundedOffset > capacity || count > capacity - boundedOffset)
                then badInterval
                else Right ()
            if interval && not (affineContiguous (affineZipper dimensions (layoutStridesElements layout)))
                then affineFailure AffineNonContiguousBase
                else Right ()

affineRequireContiguous :: HostTensor region dtype shape -> Either TensorError ()
affineRequireContiguous tensor
    | layoutIsContiguous (tensorLayout tensor) = Right ()
    | otherwise = affineFailure AffineNonContiguousBase

-- The opaque map supplied fully forced bounded coefficients and injectivity.
-- Runtime does not repeat its V/P validation or import its planning history.
affineShiftLayout :: Natural -> Natural -> [Natural] -> Integer -> [Integer] -> HostTensor region 'F64 base -> Either TensorError (CheckedLayout view)
affineShiftLayout rank count dimensions offset strides (HostTensor SF64 _ baseLayout _ _ capacity _) = do
    affineMetadataLength AffineViewShape rank strides
    newOffset <- affineSignedResult AffineOffset 0 (layoutOffsetElements baseLayout + offset)
    if newOffset < 0 then affineFailure (AffineArithmeticOverflow AffineOffset 0) else Right ()
    if count == 0 then Right () else affineMetadataStrides strides
    let !normalized = affineNormalize (count == 0) dimensions strides
        badInterval = affineFailure (AffinePhysicalBounds (fromInteger newOffset) count capacity)
    if count == 0
        then if newOffset > toInteger capacity then badInterval else Right ()
        else do
            (low, high) <- affineExtrema newOffset dimensions normalized
            if low < 0 || high >= toInteger capacity then badInterval else Right ()
    -- Dimensions/count are our admitted copies, not another supplied descriptor.
    -- Do not repeat their structural/range scans while constructing this result.
    let !layout = CheckedLayout dimensions newOffset normalized (count == 0 || affineContiguous (affineZipper dimensions normalized))
    Right layout

-- | Bind explicitly to a compatible contiguous owned base. No allocation/read.
bindAffineView :: TensorSession region -> AffineMap map base view -> OwnedTensor region owner 'F64 base -> IO (Either TensorError (OwnedAffineView region owner map base view, AffineOperationReport))
bindAffineView (TensorSession limits _ lock) witness supplied = prepareSessionCommit lock $ \state ->
    case prepare state of
        Left problem -> pure (state, Left problem)
        Right candidate -> evaluate candidate
  where
    prepare state = do
        policy <- affineRuntimeEntry limits state False
        let AffineMap baseShape viewShape _ _ cachedBase cachedView offset strides _ _ _ = witness
            old = stateAffineUsage state
        (rb, baseCount, afterBase) <- affineScanShape (Just limits) policy old (affineHeaderStart old) baseShape
        (rv, count, _) <- affineScanShape (Just limits) policy old afterBase viewShape
        affinePayloadPreflight limits state 0 0 0
        let !charge = affinePlan AffineBindPlan rb 0 rv baseCount count 0
        usage <- affineReserve policy old charge
        -- Actual wrappers are first demanded here, after complete admission.
        let baseTensor = hostTensor (ownedFiniteTensor supplied)
        affineRequireContiguous baseTensor
        affineStoredCount AffineBaseShape baseCount cachedBase
        affineStoredCount AffineViewShape count cachedView
        let !baseDimensions = affineDimensions baseShape
            !viewDimensions = affineDimensions viewShape
        affineCheckDescriptor AffineBaseShape True rb baseCount baseDimensions baseTensor
        layout <- affineShiftLayout rv count viewDimensions offset strides baseTensor
        let HostTensor SF64 _ _ identifier _ capacity pointer = baseTensor
            !view = FiniteTensor (HostTensor SF64 viewShape layout identifier count capacity pointer)
            !binding = OwnedAffineView supplied witness view
            !nested = TensorOperationReport "affine/bind" 0 (TensorMemoryReport 0 0 0 0)
            !report = AffineOperationReport charge usage nested
            !nextState = state{stateAffineUsage = usage}
            !output = (binding, report)
            !result = Right output
            !candidate = (nextState, result)
        Right candidate

-- New empty layouts are canonical without even constructing irrelevant products.
affineCanonicalStrides :: Natural -> [Natural] -> [Integer]
affineCanonicalStrides 0 = zeros
  where
    zeros [] = []
    zeros (_ : rest) = let !tailStrides = zeros rest in 0 : tailStrides
affineCanonicalStrides _ = snd . build
  where
    build [] = (1, [])
    build (dimension : dimensions) =
        let (!spanValue, !strides) = build dimensions
            !stride = if dimension == 1 then 0 else spanValue
            !nextSpan = spanValue * toInteger dimension
         in (nextSpan, stride : strides)

-- Trusted initializer: one destination, all +0 writes before any seed read.
-- Address checks precede every conversion and pointer operation.
affineInitializePullback :: Natural -> Natural -> Integer -> [Integer] -> [(Natural, Integer)] -> HostTensor region 'F64 view -> [(Natural, Integer)] -> ForeignPtr Double -> IO ()
affineInitializePullback baseCount count mapOffset mapStrides mapZipper (HostTensor SF64 _ seedLayout _ _ seedCapacity seedPointer) seedZipper destination =
    withForeignPtr destination $ \output -> do
        let zero !index
                | index == baseCount = pure ()
                | otherwise = do
                    pokeElemOff output (fromIntegral index) (0.0 :: Double)
                    zero (index + 1)
        zero 0
        if count == 0
            then pure ()
            else withForeignPtr seedPointer $ \seed -> do
                let scatter !index
                        | index == count = pure ()
                        | otherwise = do
                            source <- checked (affineAddress seedCapacity (layoutOffsetElements seedLayout) (layoutStridesElements seedLayout) seedZipper index)
                            target <- checked (affineAddress baseCount mapOffset mapStrides mapZipper index)
                            value <- peekElemOff seed (fromIntegral source)
                            pokeElemOff output (fromIntegral target) value
                            scatter (index + 1)
                scatter 0
  where
    checked = either (throwIO . userError . show) pure

{- | Fresh original-base-shaped zero/scatter pullback with the stored owner.
Allocation, initialization, metadata and full registry forcing share one
rollback owner; only prepareSessionCommit's actual put transfers ownership.
-}
pullbackAffineView :: TensorSession region -> OwnedAffineView region owner map base view -> FiniteTensor region 'F64 view -> IO (Either TensorError (OwnedTensor region owner 'F64 base, AffineOperationReport))
pullbackAffineView (TensorSession limits allocator lock) binding seed = prepareSessionCommit lock $ \state ->
    case prepare state of
        Left problem -> pure (state, Left problem)
        Right (baseShape, rb, baseCount, count, dimensions, initializer, charge, usage) -> do
            prepared <- withStagedInitializers allocator [(fromIntegral baseCount, initializer)] $ \pointers -> case pointers of
                [pointer] -> do
                    let !strides = affineCanonicalStrides baseCount dimensions
                        !layout = CheckedLayout dimensions 0 strides True
                        !identifier = StorageId (stateNextStorage state)
                        !tensor = FiniteTensor (HostTensor SF64 baseShape layout identifier baseCount baseCount pointer)
                        !owned = OwnedTensor (ownedTensorOwner (affineViewBase binding)) tensor
                        !bytes = 8 * baseCount
                        !scalarWork = baseCount + count
                        !nested = TensorOperationReport "vjp/affine-base" scalarWork (TensorMemoryReport bytes bytes 0 1)
                        !report = AffineOperationReport charge usage nested
                    registry <- prepareRegistry (stateLiveAllocations state) pointers
                    let !nextState =
                            state
                                { stateNextStorage = stateNextStorage state + 1
                                , statePayloadBytes = statePayloadBytes state + bytes
                                , stateBuffers = stateBuffers state + 1
                                , stateScalarWork = stateScalarWork state + scalarWork
                                , stateLiveAllocations = registry
                                , stateAffineUsage = usage
                                }
                        !output = (owned, report)
                        !result = Right output
                        !candidate = (nextState, result)
                    rb `seq` evaluate candidate
                _ -> throwIO (userError "affine pullback staging count mismatch")
            case prepared of
                Left (problem, diagnostics) -> pure (state, Left (allocationFailure problem diagnostics))
                Right candidate -> pure candidate
  where
    prepare state = do
        policy <- affineRuntimeEntry limits state True
        let witness@(AffineMap baseShape viewShape _ _ cachedBase cachedView offset strides _ _ _) = affineViewMap binding
            old = stateAffineUsage state
        (rb, baseCount, afterBase) <- affineScanShape (Just limits) policy old (affineHeaderStart old) baseShape
        (rv, count, afterView) <- affineScanShape (Just limits) policy old afterBase viewShape
        -- Type-identical retained witness for seed: do not demand its wrapper.
        _ <- affineScanShape (Just limits) policy old afterView viewShape
        affinePayloadPreflight limits state (8 * baseCount) 1 (baseCount + count)
        let !charge = affinePlan AffinePullbackPlan rb 0 rv baseCount count (stateBuffers state)
        usage <- affineReserve policy old charge
        let baseTensor = hostTensor (ownedFiniteTensor (affineViewBase binding))
        affineRequireContiguous baseTensor
        affineStoredCount AffineBaseShape baseCount cachedBase
        affineStoredCount AffineViewShape count cachedView
        let !baseDimensions = affineDimensions baseShape
            !viewDimensions = affineDimensions viewShape
            viewTensor = hostTensor (affineViewTensor binding)
            seedTensor = hostTensor seed
        affineCheckDescriptor AffineBaseShape True rb baseCount baseDimensions baseTensor
        affineCheckDescriptor AffineViewShape False rv count viewDimensions viewTensor
        affineCheckDescriptor AffineViewShape False rv count viewDimensions seedTensor
        let !mapZipper = if count == 0 then [] else affineZipper viewDimensions strides
            !seedZipper = if count == 0 then [] else affineZipper viewDimensions (layoutStridesElements (tensorLayout seedTensor))
            !initializer = affineInitializePullback baseCount count offset strides mapZipper seedTensor seedZipper
        witness `seq` Right (baseShape, rb, baseCount, count, baseDimensions, initializer, charge, usage)

-- Layout and storage ----------------------------------------------------------

-- | Opaque validated logical layout. Public values arise only from supported views.
data CheckedLayout shape = CheckedLayout
    { layoutDimensions :: ![Natural]
    , layoutOffsetElements :: !Integer
    , layoutStridesElements :: ![Integer]
    , layoutIsContiguous :: !Bool
    }
    deriving (Eq, Show)

type role CheckedLayout nominal

contiguousStrides :: [Natural] -> [Integer]
contiguousStrides dimensions = case dimensions of
    [] -> []
    _ -> drop 1 (scanr ((*) . toInteger) 1 dimensions)

contiguousLayout :: SShape shape -> CheckedLayout shape
contiguousLayout shape = CheckedLayout dimensions 0 (contiguousStrides dimensions) True
  where
    dimensions = shapeDimensions shape

-- | Render dimensions, element offset, element strides, and contiguity.
layoutDescription :: CheckedLayout shape -> String
layoutDescription layout =
    "shape="
        ++ show (layoutDimensions layout)
        ++ ";offset-elements="
        ++ show (layoutOffsetElements layout)
        ++ ";strides-elements="
        ++ show (layoutStridesElements layout)
        ++ ";contiguous="
        ++ show (layoutIsContiguous layout)

-- | Physical managed-allocation identity within one session region.
newtype StorageId region = StorageId Natural
    deriving (Eq, Ord, Show)

type role StorageId nominal

-- | Immutable managed host tensor. Raw IEEE values are permitted.
data HostTensor region (dtype :: DType) (shape :: [Nat])
    = HostTensor
        !(SDType dtype)
        !(SShape shape)
        !(CheckedLayout shape)
        !(StorageId region)
        !Natural -- Logical element count.
        !Natural -- Backing storage capacity in elements; views retain this.
        !(ForeignPtr Double)

type role HostTensor nominal nominal nominal

-- | Existential raw F64 tensor used by checked serialization adapters.
data DynamicHostTensor region where
    DynamicHostTensor :: HostTensor region 'F64 shape -> DynamicHostTensor region

type role DynamicHostTensor nominal

-- | Hide a statically shaped raw F64 tensor behind an existential shape.
dynamicHostTensor :: HostTensor region 'F64 shape -> DynamicHostTensor region
dynamicHostTensor = DynamicHostTensor

-- | Eliminate a dynamically shaped tensor without exposing its constructor.
withDynamicHostTensor :: DynamicHostTensor region -> (forall shape. HostTensor region 'F64 shape -> value) -> value
withDynamicHostTensor (DynamicHostTensor tensor) action = action tensor

-- | Read the runtime dimensions of a dynamically shaped tensor.
dynamicHostTensorDimensions :: DynamicHostTensor region -> [Natural]
dynamicHostTensorDimensions tensor = withDynamicHostTensor tensor (shapeDimensions . tensorShape)

-- | Read values in logical contiguous row-major order.
dynamicHostTensorToList :: DynamicHostTensor region -> IO [Double]
dynamicHostTensorToList tensor = withDynamicHostTensor tensor tensorToList

-- | Numerical refinement whose represented values are all finite.
newtype FiniteTensor region dtype shape = FiniteTensor (HostTensor region dtype shape)

type role FiniteTensor nominal nominal nominal

-- | Forget finite-value evidence without changing storage.
hostTensor :: FiniteTensor region dtype shape -> HostTensor region dtype shape
hostTensor (FiniteTensor value) = value

-- | Read static shape evidence.
tensorShape :: HostTensor region dtype shape -> SShape shape
tensorShape (HostTensor _ shape _ _ _ _ _) = shape

-- | Read storage-type evidence.
tensorDType :: HostTensor region dtype shape -> SDType dtype
tensorDType (HostTensor dtype _ _ _ _ _ _) = dtype

-- | Read the opaque checked logical layout.
tensorLayout :: HostTensor region dtype shape -> CheckedLayout shape
tensorLayout (HostTensor _ _ layout _ _ _ _) = layout

-- | Read physical storage identity.
tensorStorageId :: HostTensor region dtype shape -> StorageId region
tensorStorageId (HostTensor _ _ _ identifier _ _ _) = identifier

-- | Test physical allocation identity only. This says nothing about semantic ownership.
sameStorage :: HostTensor region left leftShape -> HostTensor region right rightShape -> Bool
sameStorage left right = tensorStorageId left == tensorStorageId right

-- Shape preflight -------------------------------------------------------------

checkedShape :: SessionLimits -> SDType dtype -> SShape shape -> Either TensorError (Natural, Natural)
checkedShape limits dtype shape = do
    -- Rank is admitted before a complete dimension list or any product is
    -- constructed. At most limitRank + 1 singleton nodes are inspected.
    dimensions <- boundedDimensions (limitRank limits) shape
    forM_ dimensions $ \dimension ->
        when (dimension > limitDimension limits) (Left (TensorShapeError (DimensionLimitExceeded (limitDimension limits) dimension)))
    elements <- cappedProduct (limitElements limits) dimensions
    when (elements > machineMaximum) (Left (TensorShapeError (MachineIndexOverflow elements)))
    bytes <- cappedMultiply machineMaximum elements (dtypeBytes dtype)
    Right (elements, bytes)
  where
    machineMaximum = fromIntegral (maxBound :: Int)

boundedDimensions :: Natural -> SShape shape -> Either TensorError [Natural]
boundedDimensions limit = go 0
  where
    go :: Natural -> SShape rest -> Either TensorError [Natural]
    go _ SNil = Right []
    go seen (SCons (_ :: Proxy dimension) rest)
        | seen >= limit = Left (TensorShapeError (RankLimitExceeded limit (seen + 1)))
        | otherwise = (fromInteger (natVal (Proxy @dimension)) :) <$> go (seen + 1) rest

cappedProduct :: Natural -> [Natural] -> Either TensorError Natural
cappedProduct limit values
    | 0 `elem` values = Right 0
    | otherwise = go 1 values
  where
    go accumulator [] = Right accumulator
    go accumulator (value : rest)
        | accumulator > limit `div` value = Left (TensorShapeError (ElementLimitExceeded limit (limit + 1)))
        | otherwise = go (accumulator * value) rest

cappedMultiply :: Natural -> Natural -> Natural -> Either TensorError Natural
cappedMultiply limit left right
    | left == 0 || right == 0 = Right 0
    | left > limit `div` right = Left (TensorShapeError (MachineIndexOverflow (limit + 1)))
    | otherwise = Right (left * right)

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)

validateFiniteValues :: String -> [Double] -> Either TensorError ()
validateFiniteValues primitive = go 0
  where
    go _ [] = Right ()
    go index (value : rest)
        | finite value = go (index + 1) rest
        | otherwise = Left (TensorNumericError (NonFiniteIntermediate primitive index))

preflightPayloads :: TensorSession region -> Natural -> [Natural] -> IO (Either TensorError ())
preflightPayloads (TensorSession limits _ lock) work sizes = withMVar lock $ \state -> do
    let fresh = sum sizes
        count = fromIntegral (length sizes)
        maximumSingle = foldl' max 0 sizes
        nextFresh = statePayloadBytes state + fresh
        nextBuffers = stateBuffers state + count
        nextWork = stateScalarWork state + work
    pure $
        if stateClosed state
            then Left TensorSessionClosed
            else
                if maximumSingle > limitSinglePayloadBytes limits
                    then Left (TensorBudgetError (SinglePayloadLimitExceeded (limitSinglePayloadBytes limits) maximumSingle))
                    else
                        if nextFresh > limitFreshPayloadBytes limits
                            then Left (TensorBudgetError (FreshPayloadLimitExceeded (limitFreshPayloadBytes limits) nextFresh))
                            else
                                if nextBuffers > limitBuffers limits
                                    then Left (TensorBudgetError (BufferLimitExceeded (limitBuffers limits) nextBuffers))
                                    else
                                        if nextWork > limitScalarWork limits
                                            then Left (TensorBudgetError (ScalarWorkLimitExceeded (limitScalarWork limits) nextWork))
                                            else Right ()

-- Allocation is serialized. Every represented payload and work charge is
-- checked before the first ForeignPtr allocator call.
allocatePayloads ::
    TensorSession region ->
    String ->
    Natural ->
    [([Double], Natural)] ->
    IO (Either TensorError ([(StorageId region, ForeignPtr Double)], TensorOperationReport))
allocatePayloads (TensorSession limits allocator lock) primitive work payloads =
    prepareSessionCommit lock $ \state -> do
        let sizes = map snd payloads
            fresh = sum sizes
            count = fromIntegral (length payloads)
            maximumSingle = foldl' max 0 sizes
            nextFresh = statePayloadBytes state + fresh
            nextBuffers = stateBuffers state + count
            nextWork = stateScalarWork state + work
            memory = TensorMemoryReport fresh maximumSingle 0 count
            report = TensorOperationReport primitive work memory
            reject
                | stateClosed state = Just TensorSessionClosed
                | maximumSingle > limitSinglePayloadBytes limits = Just (TensorBudgetError (SinglePayloadLimitExceeded (limitSinglePayloadBytes limits) maximumSingle))
                | nextFresh > limitFreshPayloadBytes limits = Just (TensorBudgetError (FreshPayloadLimitExceeded (limitFreshPayloadBytes limits) nextFresh))
                | nextBuffers > limitBuffers limits = Just (TensorBudgetError (BufferLimitExceeded (limitBuffers limits) nextBuffers))
                | nextWork > limitScalarWork limits = Just (TensorBudgetError (ScalarWorkLimitExceeded (limitScalarWork limits) nextWork))
                | otherwise = Nothing
        case reject of
            Just problem -> pure (state, Left problem)
            Nothing -> do
                prepared <- withStagedInitializers allocator (map initializer payloads) $ \pointers -> do
                    registry <- prepareRegistry (stateLiveAllocations state) pointers
                    let identified = identifyAllocations (stateNextStorage state) pointers
                        nextState =
                            state
                                { stateNextStorage = stateNextStorage state + count
                                , statePayloadBytes = nextFresh
                                , stateBuffers = nextBuffers
                                , stateScalarWork = nextWork
                                , stateLiveAllocations = registry
                                }
                        output = (identified, report)
                        result = Right output
                        candidate = (nextState, result)
                    forceIdentifiedAllocations identified
                    _ <- evaluate report
                    _ <- evaluate nextState
                    _ <- evaluate output
                    _ <- evaluate result
                    evaluate candidate
                case prepared of
                    Left (problem, diagnostics) -> pure (state, Left (allocationFailure problem diagnostics))
                    Right candidate -> pure candidate
  where
    initializer (values, _) =
        ( length values
        , \pointer -> withForeignPtr pointer $ \raw ->
            forM_ (zip [0 ..] values) (uncurry (pokeElemOff raw))
        )

-- Own the actual empty-MVar put, not just a modifyMVar callback. Preparation is
-- masked and has its own staged rollback owner. Its handler ends before this
-- nonblocking put; an exception after the put must never restore the old state.
prepareSessionCommit :: MVar SessionState -> (SessionState -> IO (SessionState, value)) -> IO value
prepareSessionCommit lock prepare = mask $ \_ -> do
    previous <- takeMVar lock
    (candidate, result) <- (prepare previous >>= evaluate) `onException` putMVar lock previous
    putMVar lock candidate
    pure result

-- Every allocating path establishes this spine invariant, including sessions
-- with affine policy disabled. Empty batches share the already-normal registry.
-- Force only list constructors: neither pointer heads nor payloads are forced.
prepareRegistry :: [ForeignPtr Double] -> [ForeignPtr Double] -> IO [ForeignPtr Double]
prepareRegistry previous [] = pure previous
prepareRegistry previous fresh = do
    let candidate = previous ++ fresh
    forceAllocationSpine candidate
    pure candidate

forceAllocationSpine :: [value] -> IO ()
forceAllocationSpine [] = pure ()
forceAllocationSpine (_ : rest) = forceAllocationSpine rest

identifyAllocations :: Natural -> [ForeignPtr Double] -> [(StorageId region, ForeignPtr Double)]
identifyAllocations _ [] = []
identifyAllocations next (pointer : rest) = (StorageId next, pointer) : identifyAllocations (next + 1) rest

forceIdentifiedAllocations :: [(StorageId region, ForeignPtr Double)] -> IO ()
forceIdentifiedAllocations [] = pure ()
forceIdentifiedAllocations ((identifier, _) : rest) = identifier `seq` forceIdentifiedAllocations rest

-- The continuation executes while every disclosed pointer is still staged.
-- No handler spans a recursive staging call: a deeper failure therefore has
-- exactly one cleanup owner, including when cleanup diagnostics themselves throw.
-- Only the enclosing prepareSessionCommit publishes the prepared result.
withStagedInitializers :: TensorAllocator -> [(Int, ForeignPtr Double -> IO ())] -> ([ForeignPtr Double] -> IO value) -> IO (Either (String, [String]) value)
withStagedInitializers allocator initializers prepare = mask $ \_ -> go (1 :: Natural) [] initializers
  where
    go index reversed pending = do
        inspected <- try @SomeException (evaluate pending)
        case inspected of
            Left problem -> rejectException reversed problem
            Right [] -> do
                prepared <- try @SomeException $ do
                    let pointers = reverse reversed
                    forceAllocationSpine pointers
                    prepare pointers >>= evaluate
                case prepared of
                    Left problem -> rejectException reversed problem
                    Right result -> pure (Right result)
            Right (request : rest) -> do
                allocated <- try @SomeException $ do
                    let (count, _) = request
                    allocatorAllocate allocator count >>= evaluate
                case allocated of
                    Left problem -> rejectInitialization index reversed problem
                    Right (Left problem) -> rejectStaged index reversed problem
                    Right (Right pointer) -> do
                        initialized <- try @SomeException $ do
                            let (_, initialize) = request
                            initialize pointer
                        case initialized of
                            Left problem -> rejectInitialization index (pointer : reversed) problem
                            Right () -> go (index + 1) (pointer : reversed) rest

    rejectInitialization failedIndex staged problem =
        case fromException problem :: Maybe AsyncException of
            Just _ -> rejectException staged problem
            Nothing -> rejectStaged failedIndex staged (displayException problem)

    rejectException staged problem = do
        cleanup <- finalizeAll allocator (reverse staged)
        throwAfterCleanup problem cleanup

    rejectStaged failedIndex staged problem = do
        cleanup@(CleanupResult diagnostics interruptions) <- finalizeAll allocator (reverse staged)
        case interruptions of
            _ : _ -> throwAfterCleanup (toException (userError ("allocation-" ++ show failedIndex ++ ": " ++ problem))) cleanup
            [] -> pure (Left ("allocation-" ++ show failedIndex ++ ": " ++ problem, diagnostics))

consumeExact :: Natural -> [value] -> Either TensorError [value]
consumeExact expected = go expected 0 []
  where
    go 0 _ reversed [] = Right (reverse reversed)
    go 0 _ _ (_ : _) = Left (InputLengthExceedsShape expected)
    go _ observed _ [] = Left (InputLengthMismatch expected observed)
    go remaining observed reversed (value : rest) = go (remaining - 1) (observed + 1) (value : reversed) rest

data SomeShape where
    SomeShape :: SShape shape -> SomeShape

reifyShape :: [Natural] -> Either TensorError SomeShape
reifyShape [] = Right (SomeShape SNil)
reifyShape (dimension : dimensions) =
    case someNatVal (toInteger dimension) of
        Nothing -> Left (TensorShapeError (MachineIndexOverflow dimension))
        Just (SomeNat proxy) -> do
            SomeShape rest <- reifyShape dimensions
            Right (SomeShape (SCons proxy rest))

boundedInputBatch :: Natural -> [value] -> Either TensorError [value]
boundedInputBatch limit = go 0 []
  where
    go _ reversed [] = Right (reverse reversed)
    go seen _ (_ : _) | seen >= limit = Left (TensorBudgetError (BufferLimitExceeded limit (seen + 1)))
    go seen reversed (value : rest) = go (seen + 1) (value : reversed) rest

prepareDynamicShape :: SessionLimits -> [Natural] -> Either TensorError (SomeShape, Natural, Natural)
prepareDynamicShape limits inputDimensions = do
    dimensions <- go 0 inputDimensions
    forM_ dimensions $ \dimension ->
        when (dimension > limitDimension limits) (Left (TensorShapeError (DimensionLimitExceeded (limitDimension limits) dimension)))
    elements <- cappedProduct (limitElements limits) dimensions
    when (elements > machineMaximum) (Left (TensorShapeError (MachineIndexOverflow elements)))
    bytes <- cappedMultiply machineMaximum elements (dtypeBytes SF64)
    shape <- reifyShape dimensions
    Right (shape, elements, bytes)
  where
    machineMaximum = fromIntegral (maxBound :: Int)
    go _ [] = Right []
    go seen _ | seen >= limitRank limits = Left (TensorShapeError (RankLimitExceeded (limitRank limits) (seen + 1)))
    go seen (dimension : rest) = (dimension :) <$> go (seen + 1) rest

{- | Allocate a batch of dynamically shaped raw F64 tensors atomically.

Every shape, payload, buffer, and work limit and every input length is checked
before the first allocator call. A partial allocator set is finalized without
committing storage IDs or session accounting.
-}
hostTensorBatchFromLists :: TensorSession region -> [([Natural], [Double])] -> IO (Either TensorError ([DynamicHostTensor region], TensorOperationReport))
hostTensorBatchFromLists session@(TensorSession limits _ _) requested =
    case boundedInputBatch (limitBuffers limits) requested of
        Left problem -> pure (Left problem)
        Right inputs -> case traverse (prepareDynamicShape limits . fst) inputs of
            Left problem -> pure (Left problem)
            Right plans -> do
                let sizes = map (\(_, _, bytes) -> bytes) plans
                    work = sum (map (\(_, elements, _) -> elements) plans)
                preflight <- preflightPayloads session work sizes
                case preflight of
                    Left problem -> pure (Left problem)
                    Right () -> case zipWithM (\(_, elements, _) (_, values) -> consumeExact elements values) plans inputs of
                        Left problem -> pure (Left problem)
                        Right values -> do
                            allocated <- allocatePayloads session "from-lists" work (zip values sizes)
                            -- Discharge collection-sized library work after commit,
                            -- without forcing individual tensor wrappers/layouts.
                            case allocated of
                                Left problem -> pure (Left problem)
                                Right (payloads, report) ->
                                    if length payloads /= length plans
                                        then pure (Left (HostAllocationFailure "internal allocation-count mismatch"))
                                        else do
                                            let tensors = zipWith makeDynamic plans payloads
                                            forceAllocationSpine tensors
                                            pure (Right (tensors, report))
  where
    makeDynamic (SomeShape shape, elements, _) (identifier, pointer) =
        DynamicHostTensor (HostTensor SF64 shape (contiguousLayout shape) identifier elements elements pointer)

-- | Allocate one raw contiguous tensor after complete shape and payload preflight.
hostTensorFromList :: TensorSession region -> SDType dtype -> SShape shape -> [Scalar dtype] -> IO (Either TensorError (HostTensor region dtype shape, TensorOperationReport))
hostTensorFromList session@(TensorSession limits _ _) SF64 shape inputValues = case checkedShape limits SF64 shape of
    Left problem -> pure (Left problem)
    Right (elements, bytes) -> do
        preflight <- preflightPayloads session elements [bytes]
        case preflight of
            Left problem -> pure (Left problem)
            Right () -> case consumeExact elements inputValues of
                Left problem -> pure (Left problem)
                Right values -> do
                    allocated <- allocatePayloads session "from-list" elements [(values, bytes)]
                    pure $ do
                        (payloads, report) <- allocated
                        case payloads of
                            [(identifier, pointer)] -> Right (HostTensor SF64 shape (contiguousLayout shape) identifier elements elements pointer, report)
                            _ -> Left (HostAllocationFailure "internal allocation-count mismatch")

-- | Validate finite F64 values and allocate one contiguous tensor.
finiteTensorFromList :: TensorSession region -> SShape shape -> [Double] -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
finiteTensorFromList session@(TensorSession limits _ _) shape inputValues = case checkedShape limits SF64 shape of
    Left problem -> pure (Left problem)
    Right (elements, bytes) -> do
        preflight <- preflightPayloads session elements [bytes]
        case preflight of
            Left problem -> pure (Left problem)
            Right () -> case consumeExact elements inputValues of
                Left problem -> pure (Left problem)
                Right values -> case validateFiniteInput 0 values of
                    Left problem -> pure (Left problem)
                    Right () -> do
                        allocated <- allocatePayloads session "from-list" elements [(values, bytes)]
                        pure $ do
                            (payloads, report) <- allocated
                            case payloads of
                                [(identifier, pointer)] -> Right (FiniteTensor (HostTensor SF64 shape (contiguousLayout shape) identifier elements elements pointer), report)
                                _ -> Left (HostAllocationFailure "internal allocation-count mismatch")
  where
    validateFiniteInput _ [] = Right ()
    validateFiniteInput index (value : rest)
        | finite value = validateFiniteInput (index + 1) rest
        | otherwise = Left (TensorNumericError (NonFiniteInput "from-list" index))

-- This observer relies on established layout bounds; it is not affine admission.
-- Empty layouts do not evaluate irrelevant (possibly non-machine-bounded) strides.
logicalOffsets :: Natural -> CheckedLayout shape -> [Int]
logicalOffsets total layout = map offsetFor (take (fromIntegral total) [0 ..])
  where
    dimensions = layoutDimensions layout
    strides = layoutStridesElements layout
    offsetFor linear = fromInteger (layoutOffsetElements layout + sum (zipWith ((*) . toInteger) (coordinates dimensions linear) strides))

coordinates :: [Natural] -> Natural -> [Natural]
coordinates dimensions linear = snd (foldr step (linear, []) dimensions)
  where
    step dimension (remaining, result)
        | dimension == 0 = (0, 0 : result)
        | otherwise = let (quotient, remainder) = remaining `quotRem` dimension in (quotient, remainder : result)

-- | Observe logical values in row-major coordinate order.
tensorToList :: HostTensor region 'F64 shape -> IO [Double]
tensorToList (HostTensor SF64 _ layout _ elements _ pointer) =
    withForeignPtr pointer $ \raw -> forM (logicalOffsets elements layout) (peekElemOff raw)

-- | Check every raw IEEE value and produce the finite numerical refinement.
finiteTensor :: HostTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape))
finiteTensor tensor = do
    values <- tensorToList tensor
    case validateFiniteInput 0 values of
        Left problem -> pure (Left problem)
        Right () -> pure (Right (FiniteTensor tensor))
  where
    validateFiniteInput _ [] = Right ()
    validateFiniteInput index (value : rest)
        | finite value = validateFiniteInput (index + 1) rest
        | otherwise = Left (TensorNumericError (NonFiniteInput "finite-tensor" index))

-- Views -----------------------------------------------------------------------

-- | Make an immutable zero-copy two-dimensional transpose view.
transpose2D :: HostTensor region dtype '[rows, columns] -> HostTensor region dtype '[columns, rows]
transpose2D (HostTensor dtype (SCons _ (SCons _ SNil)) layout identifier elements capacity pointer) =
    HostTensor dtype knownShape transposed identifier elements capacity pointer
  where
    transposed =
        CheckedLayout
            (reverse (layoutDimensions layout))
            (layoutOffsetElements layout)
            (reverse (layoutStridesElements layout))
            False

-- | Transpose a finite tensor without changing values or storage.
transposeFinite2D :: FiniteTensor region dtype '[rows, columns] -> FiniteTensor region dtype '[columns, rows]
transposeFinite2D (FiniteTensor tensor) = FiniteTensor (transpose2D tensor)

-- | Reinterpret contiguous coordinates at an equal checked element count.
reshapeContiguous :: TensorSession region -> SShape target -> HostTensor region dtype source -> Either TensorError (HostTensor region dtype target)
reshapeContiguous (TensorSession limits _ _) target (HostTensor dtype _ layout identifier elements capacity pointer) = do
    _ <- checkedShape limits dtype target
    if not (layoutIsContiguous layout)
        then Left (TensorLayoutError NonContiguousReshape)
        else
            if shapeElements target /= elements
                then Left (TensorShapeError (ShapeMismatch [elements] [shapeElements target]))
                else
                    let reshaped = (contiguousLayout target){layoutOffsetElements = layoutOffsetElements layout}
                     in Right (HostTensor dtype target reshaped identifier elements capacity pointer)

-- | Reshape a finite contiguous tensor without allocation.
reshapeFiniteContiguous :: TensorSession region -> SShape target -> FiniteTensor region dtype source -> Either TensorError (FiniteTensor region dtype target)
reshapeFiniteContiguous session target (FiniteTensor tensor) = FiniteTensor <$> reshapeContiguous session target tensor

-- Owners ----------------------------------------------------------------------

-- | Opaque nominal semantic owner key tied to one parameter shape.
data TensorOwner owner shape = TensorOwner !String !(SShape shape)

type role TensorOwner nominal nominal

-- | Declare a nonempty semantic owner key.
tensorOwner :: String -> SShape shape -> Either TensorError (TensorOwner owner shape)
tensorOwner "" _ = Left EmptyOwnerKey
tensorOwner name shape = Right (TensorOwner name shape)

-- | Observe the stable semantic owner key.
ownerKey :: TensorOwner owner shape -> String
ownerKey (TensorOwner name _) = name

-- | A finite tensor paired with separate semantic owner evidence.
data OwnedTensor region owner dtype shape = OwnedTensor !(TensorOwner owner shape) !(FiniteTensor region dtype shape)

type role OwnedTensor nominal nominal nominal nominal

-- | Associate an immutable value with an existing owner witness.
ownTensor :: TensorOwner owner shape -> FiniteTensor region dtype shape -> OwnedTensor region owner dtype shape
ownTensor = OwnedTensor

-- | Read the owned finite value.
ownedFiniteTensor :: OwnedTensor region owner dtype shape -> FiniteTensor region dtype shape
ownedFiniteTensor (OwnedTensor _ tensor) = tensor

-- | Read semantic ownership independently of storage.
ownedTensorOwner :: OwnedTensor region owner dtype shape -> TensorOwner owner shape
ownedTensorOwner (OwnedTensor owner _) = owner

-- Primitive helpers -----------------------------------------------------------

makeFinite :: TensorSession region -> String -> Natural -> SShape shape -> [Double] -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
makeFinite session@(TensorSession limits _ _) primitive work shape values = case checkedShape limits SF64 shape of
    Left problem -> pure (Left problem)
    Right (count, bytes) -> do
        preflight <- preflightPayloads session work [bytes]
        case preflight of
            Left problem -> pure (Left problem)
            Right () -> case validateFiniteValues primitive values of
                Left problem -> pure (Left problem)
                Right () -> do
                    allocated <- allocatePayloads session primitive work [(values, bytes)]
                    pure $ do
                        (payloads, report) <- allocated
                        case payloads of
                            [(identifier, pointer)] -> Right (FiniteTensor (HostTensor SF64 shape (contiguousLayout shape) identifier count count pointer), report)
                            _ -> Left (HostAllocationFailure "internal allocation-count mismatch")

tensorElementCount :: HostTensor region dtype shape -> Natural
tensorElementCount (HostTensor _ _ _ _ elements _ _) = elements

preflightOne :: TensorSession region -> Natural -> SShape shape -> IO (Either TensorError ())
preflightOne session@(TensorSession limits _ _) work shape = case checkedShape limits SF64 shape of
    Left problem -> pure (Left problem)
    Right (_, bytes) -> preflightPayloads session work [bytes]

binaryElementwise :: TensorSession region -> String -> (Double -> Double -> Double) -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
binaryElementwise session primitive operation (FiniteTensor left) (FiniteTensor right) = do
    let work = tensorElementCount left
        shape = tensorShape left
    preflight <- preflightOne session work shape
    case preflight of
        Left problem -> pure (Left problem)
        Right () -> do
            leftValues <- tensorToList left
            rightValues <- tensorToList right
            makeFinite session primitive work shape (zipWith operation leftValues rightValues)

-- | Pointwise F64 addition without broadcasting.
add :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
add session = binaryElementwise session "add" (+)

-- | Pointwise F64 multiplication without broadcasting.
multiply :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
multiply session = binaryElementwise session "multiply" (*)

-- | Pointwise arithmetic negation.
negateTensor :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
negateTensor session (FiniteTensor tensor) = do
    let work = tensorElementCount tensor
        shape = tensorShape tensor
    preflight <- preflightOne session work shape
    case preflight of
        Left problem -> pure (Left problem)
        Right () -> do
            values <- tensorToList tensor
            makeFinite session "negate" work shape (map negate values)

-- | Pointwise hyperbolic tangent.
tanhTensor :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
tanhTensor session (FiniteTensor tensor) = do
    let work = tensorElementCount tensor
        shape = tensorShape tensor
    preflight <- preflightOne session work shape
    case preflight of
        Left problem -> pure (Left problem)
        Right () -> do
            values <- tensorToList tensor
            makeFinite session "tanh" work shape (map tanh values)

-- | Reduce all logical coordinates from left to right into one scalar tensor.
sumAll :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 '[], TensorOperationReport))
sumAll session (FiniteTensor tensor) = do
    let work = tensorElementCount tensor
    preflight <- preflightPayloads session work [8]
    case preflight of
        Left problem -> pure (Left problem)
        Right () -> do
            values <- tensorToList tensor
            let total = foldl' (+) 0 values
            makeFinite session "sum-all" work SNil [total]

-- | Materialize logical coordinate order in fresh contiguous storage.
contiguousCopy :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
contiguousCopy session (FiniteTensor tensor) = do
    let work = tensorElementCount tensor
        shape = tensorShape tensor
    preflight <- preflightOne session work shape
    case preflight of
        Left problem -> pure (Left problem)
        Right () -> do
            values <- tensorToList tensor
            makeFinite session "contiguous-copy" work shape values

-- | Deterministic row-major matrix multiplication; a zero inner size returns zeros.
matmul :: forall region rows inner columns. TensorSession region -> FiniteTensor region 'F64 '[rows, inner] -> FiniteTensor region 'F64 '[inner, columns] -> IO (Either TensorError (FiniteTensor region 'F64 '[rows, columns], TensorOperationReport))
matmul session (FiniteTensor left) (FiniteTensor right) =
    case (tensorShape left, tensorShape right) of
        (SCons rowsProxy (SCons _ SNil), SCons _ (SCons columnsProxy SNil)) ->
            case (shapeDimensions (tensorShape left), shapeDimensions (tensorShape right)) of
                ([rows, inner], [_, columns]) -> do
                    let work = rows * columns * (2 * inner + 1)
                        outputShape = SCons rowsProxy (SCons columnsProxy SNil)
                    preflight <- preflightOne session work outputShape
                    case preflight of
                        Left problem -> pure (Left problem)
                        Right () -> do
                            leftValues <- tensorToList left
                            rightValues <- tensorToList right
                            let outputValues = matrixProductValues rows inner columns leftValues rightValues
                            makeFinite session "matmul" work outputShape outputValues
                _ -> pure (Left (TensorLayoutError (LayoutRankMismatch 2 0)))

-- Matrix kernels use bounded sequential splitting and zipping. They do not use
-- list indexing, so reported arithmetic work is not hiding quadratic (!!)
-- traversal.
matrixProductValues :: Natural -> Natural -> Natural -> [Double] -> [Double] -> [Double]
matrixProductValues rows inner columns left right =
    concatMap (\row -> map (dotProduct row) rightColumns) leftRows
  where
    leftRows = splitRows rows inner left
    rightRows = splitRows inner columns right
    rightColumns
        | inner == 0 = replicate (fromIntegral columns) []
        | otherwise = transposeRows columns rightRows
    dotProduct xs ys = foldl' (\accumulator (x, y) -> accumulator + x * y) 0 (zip xs ys)

splitRows :: Natural -> Natural -> [value] -> [[value]]
splitRows rowCount width = go rowCount
  where
    machineWidth = fromIntegral width
    go 0 _ = []
    go remaining values =
        let (row, rest) = splitAt machineWidth values
         in row : go (remaining - 1) rest

transposeRows :: Natural -> [[value]] -> [[value]]
transposeRows columns rows = take (fromIntegral columns) (go rows)
  where
    go [] = repeat []
    go current = case peel current of
        Nothing -> []
        Just (column, rest) -> column : go rest

    peel [] = Just ([], [])
    peel ([] : _) = Nothing
    peel ((value : values) : remaining) = do
        (column, rest) <- peel remaining
        Just (value : column, values : rest)

fillLike :: TensorSession region -> String -> FiniteTensor region 'F64 '[] -> SShape shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, TensorOperationReport))
fillLike session primitive (FiniteTensor scalar) shape = do
    let elements = shapeElements shape
    preflight <- preflightOne session elements shape
    case preflight of
        Left problem -> pure (Left problem)
        Right () -> do
            values <- tensorToList scalar
            case values of
                [value] -> makeFinite session primitive elements shape (replicate (fromIntegral elements) value)
                _ -> pure (Left (InputLengthMismatch 1 (fromIntegral (length values))))

-- Reverse tapes ---------------------------------------------------------------

-- | Opaque endpoint-indexed tape for one unary primitive.
data UnaryTape region input output where
    TanhTape :: FiniteTensor region 'F64 input -> UnaryTape region input input
    SumTape :: SShape input -> UnaryTape region input '[]

type role UnaryTape nominal nominal nominal

-- | Opaque endpoint-indexed tape for one binary primitive.
data BinaryTape region left right output where
    AddTape :: BinaryTape region shape shape shape
    MultiplyTape :: FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> BinaryTape region shape shape shape
    MatMulTape :: FiniteTensor region 'F64 '[rows, inner] -> FiniteTensor region 'F64 '[inner, columns] -> BinaryTape region '[rows, inner] '[inner, columns] '[rows, columns]

type role BinaryTape nominal nominal nominal nominal

-- | Execute @tanh@ and retain only its checked output for the VJP.
tanhWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, UnaryTape region shape shape, TensorOperationReport))
tanhWithTape session input = fmap (fmap (\(output, report) -> (output, TanhTape output, report))) (tanhTensor session input)

-- | Execute total reduction and retain its input shape.
sumWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 '[], UnaryTape region shape '[], TensorOperationReport))
sumWithTape session input@(FiniteTensor tensor) = fmap (fmap (\(output, report) -> (output, SumTape (tensorShape tensor), report))) (sumAll session input)

-- | Execute addition and return its allocation-free diagonal tape.
addWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, BinaryTape region shape shape shape, TensorOperationReport))
addWithTape session left right = fmap (fmap (\(output, report) -> (output, AddTape, report))) (add session left right)

-- | Execute pointwise multiplication and retain immutable primal inputs.
multiplyWithTape :: TensorSession region -> FiniteTensor region 'F64 shape -> FiniteTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape, BinaryTape region shape shape shape, TensorOperationReport))
multiplyWithTape session left right = fmap (fmap (\(output, report) -> (output, MultiplyTape left right, report))) (multiply session left right)

-- | Execute matrix multiplication and retain immutable primal operands.
matmulWithTape :: TensorSession region -> FiniteTensor region 'F64 '[rows, inner] -> FiniteTensor region 'F64 '[inner, columns] -> IO (Either TensorError (FiniteTensor region 'F64 '[rows, columns], BinaryTape region '[rows, inner] '[inner, columns] '[rows, columns], TensorOperationReport))
matmulWithTape session left right = fmap (fmap (\(output, report) -> (output, MatMulTape left right, report))) (matmul session left right)

-- | Apply a unary primitive's transposed Jacobian action.
applyUnaryTape :: TensorSession region -> UnaryTape region input output -> FiniteTensor region 'F64 output -> IO (Either TensorError (FiniteTensor region 'F64 input, TensorOperationReport))
applyUnaryTape session (TanhTape output) seed = do
    let shape = tensorShape (hostTensor output)
        work = 3 * tensorElementCount (hostTensor output)
    preflight <- preflightOne session work shape
    case preflight of
        Left problem -> pure (Left problem)
        Right () -> do
            outputValues <- tensorToList (hostTensor output)
            seedValues <- tensorToList (hostTensor seed)
            let values = zipWith (\lambda y -> lambda * (1 - y * y)) seedValues outputValues
            makeFinite session "vjp/tanh" work shape values
applyUnaryTape session (SumTape shape) seed = fillLike session "vjp/sum-all" seed shape

-- | Apply a binary primitive's VJP with atomic multi-output payload preflight.
applyBinaryTape :: TensorSession region -> BinaryTape region left right output -> FiniteTensor region 'F64 output -> IO (Either TensorError ((FiniteTensor region 'F64 left, FiniteTensor region 'F64 right), TensorOperationReport))
applyBinaryTape _ AddTape seed =
    pure (Right ((seed, seed), TensorOperationReport "vjp/add" 0 (TensorMemoryReport 0 0 0 0)))
applyBinaryTape session (MultiplyTape left right) seed = do
    let shape = tensorShape (hostTensor left)
        work = 2 * tensorElementCount (hostTensor seed)
    case checkedShapeFromTwo session work shape shape of
        Left problem -> pure (Left problem)
        Right preflight -> do
            ready <- preflight
            case ready of
                Left problem -> pure (Left problem)
                Right () -> do
                    seedValues <- tensorToList (hostTensor seed)
                    leftValues <- tensorToList (hostTensor left)
                    rightValues <- tensorToList (hostTensor right)
                    makeTwo
                        session
                        "vjp/multiply"
                        work
                        shape
                        (zipWith (*) seedValues rightValues)
                        (zipWith (*) seedValues leftValues)
applyBinaryTape session (MatMulTape left right) seed =
    case ( shapeDimensions (tensorShape (hostTensor left))
         , shapeDimensions (tensorShape (hostTensor right))
         ) of
        ([rows, inner], [_, columns]) -> do
            let work = 4 * rows * inner * columns + rows * inner + inner * columns
                leftShape = tensorShape (hostTensor left)
                rightShape = tensorShape (hostTensor right)
            case checkedShapeFromTwo session work leftShape rightShape of
                Left problem -> pure (Left problem)
                Right preflight -> do
                    ready <- preflight
                    case ready of
                        Left problem -> pure (Left problem)
                        Right () -> do
                            seedValues <- tensorToList (hostTensor seed)
                            leftValues <- tensorToList (hostTensor left)
                            rightValues <- tensorToList (hostTensor right)
                            let rightTranspose = concat (transposeRows columns (splitRows inner columns rightValues))
                                leftTranspose = concat (transposeRows inner (splitRows rows inner leftValues))
                                leftGradient = matrixProductValues rows columns inner seedValues rightTranspose
                                rightGradient = matrixProductValues inner rows columns leftTranspose seedValues
                            makeTwoShapes session "vjp/matmul" work leftShape leftGradient rightShape rightGradient
        _ -> pure (Left (TensorLayoutError (LayoutRankMismatch 2 0)))

checkedShapeFromTwo :: TensorSession region -> Natural -> SShape left -> SShape right -> Either TensorError (IO (Either TensorError ()))
checkedShapeFromTwo session@(TensorSession limits _ _) work left right = do
    (_, leftBytes) <- checkedShape limits SF64 left
    (_, rightBytes) <- checkedShape limits SF64 right
    Right (preflightPayloads session work [leftBytes, rightBytes])

-- Atomic two-output helper used by Hadamard VJP. Both payloads are preflighted
-- before either allocator is called.
makeTwo :: TensorSession region -> String -> Natural -> SShape shape -> [Double] -> [Double] -> IO (Either TensorError ((FiniteTensor region 'F64 shape, FiniteTensor region 'F64 shape), TensorOperationReport))
makeTwo session@(TensorSession limits _ _) primitive work shape leftValues rightValues = case checkedShape limits SF64 shape of
    Left problem -> pure (Left problem)
    Right (count, bytes) -> do
        preflight <- preflightPayloads session work [bytes, bytes]
        case preflight of
            Left problem -> pure (Left problem)
            Right () -> case validateFiniteValues primitive (leftValues ++ rightValues) of
                Left problem -> pure (Left problem)
                Right () -> do
                    allocated <- allocatePayloads session primitive work [(leftValues, bytes), (rightValues, bytes)]
                    pure $ do
                        (payloads, report) <- allocated
                        case payloads of
                            [(leftId, leftPointer), (rightId, rightPointer)] ->
                                let make identifier pointer = FiniteTensor (HostTensor SF64 shape (contiguousLayout shape) identifier count count pointer)
                                 in Right ((make leftId leftPointer, make rightId rightPointer), report)
                            _ -> Left (HostAllocationFailure "internal allocation-count mismatch")

makeTwoShapes :: TensorSession region -> String -> Natural -> SShape leftShape -> [Double] -> SShape rightShape -> [Double] -> IO (Either TensorError ((FiniteTensor region 'F64 leftShape, FiniteTensor region 'F64 rightShape), TensorOperationReport))
makeTwoShapes session@(TensorSession limits _ _) primitive work leftShape leftValues rightShape rightValues =
    case (checkedShape limits SF64 leftShape, checkedShape limits SF64 rightShape) of
        (Left problem, _) -> pure (Left problem)
        (_, Left problem) -> pure (Left problem)
        (Right (leftCount, leftBytes), Right (rightCount, rightBytes)) -> do
            preflight <- preflightPayloads session work [leftBytes, rightBytes]
            case preflight of
                Left problem -> pure (Left problem)
                Right () -> case validateFiniteValues primitive (leftValues ++ rightValues) of
                    Left problem -> pure (Left problem)
                    Right () -> do
                        allocated <- allocatePayloads session primitive work [(leftValues, leftBytes), (rightValues, rightBytes)]
                        pure $ do
                            (payloads, report) <- allocated
                            case payloads of
                                [(leftId, leftPointer), (rightId, rightPointer)] ->
                                    let leftTensor = FiniteTensor (HostTensor SF64 leftShape (contiguousLayout leftShape) leftId leftCount leftCount leftPointer)
                                        rightTensor = FiniteTensor (HostTensor SF64 rightShape (contiguousLayout rightShape) rightId rightCount rightCount rightPointer)
                                     in Right ((leftTensor, rightTensor), report)
                                _ -> Left (HostAllocationFailure "internal allocation-count mismatch")
