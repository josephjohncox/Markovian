{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Markovian.Tensor.Internal where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (AsyncException, Exception, SomeException, displayException, fromException, mask, throwIO, toException, try)
import Control.Monad (forM, forM_, when, zipWithM)
import Data.List (foldl')
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
    }
    deriving (Eq, Show)

-- | Build limits in rank, dimension, elements, single bytes, fresh bytes, buffers, and work order.
tensorSessionLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> SessionLimits
tensorSessionLimits = SessionLimits

data SessionState = SessionState
    { stateNextStorage :: !Natural
    , statePayloadBytes :: !Natural
    , stateBuffers :: !Natural
    , stateScalarWork :: !Natural
    , stateLiveAllocations :: ![ForeignPtr Double]
    , stateClosed :: !Bool
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

{- | Exception raised only when an action exception and one or more bounded
cleanup diagnostics must be preserved together.
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

{- | Run an action in a fresh managed region. Ordinary tensors cannot escape.
All committed payloads are finalized exactly once after success, 'Left', or
an exception. Asynchronous exceptions are rethrown after masked cleanup.
-}
withTensorSession :: SessionLimits -> (forall region. TensorSession region -> IO (Either TensorError value)) -> IO (Either TensorError value)
withTensorSession = withTensorSessionAllocator defaultTensorAllocator

withTensorSessionAllocator :: TensorAllocator -> SessionLimits -> (forall region. TensorSession region -> IO (Either TensorError value)) -> IO (Either TensorError value)
withTensorSessionAllocator allocator limits action = mask $ \restore -> do
    state <- newMVar (SessionState 0 0 0 0 [] False)
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

-- Layout and storage ----------------------------------------------------------

-- | Opaque validated logical layout. Public values arise only from supported views.
data CheckedLayout shape = CheckedLayout
    { layoutDimensions :: ![Natural]
    , layoutOffsetElements :: !Natural
    , layoutStridesElements :: ![Natural]
    , layoutIsContiguous :: !Bool
    }
    deriving (Eq, Show)

type role CheckedLayout nominal

contiguousStrides :: [Natural] -> [Natural]
contiguousStrides dimensions = case dimensions of
    [] -> []
    _ -> drop 1 (scanr (*) 1 dimensions)

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
        !Natural
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
tensorShape (HostTensor _ shape _ _ _ _) = shape

-- | Read storage-type evidence.
tensorDType :: HostTensor region dtype shape -> SDType dtype
tensorDType (HostTensor dtype _ _ _ _ _) = dtype

-- | Read the opaque checked logical layout.
tensorLayout :: HostTensor region dtype shape -> CheckedLayout shape
tensorLayout (HostTensor _ _ layout _ _ _) = layout

-- | Read physical storage identity.
tensorStorageId :: HostTensor region dtype shape -> StorageId region
tensorStorageId (HostTensor _ _ _ identifier _ _) = identifier

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
    modifyMVar lock $ \state -> do
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
                allocated <- allocateStaged allocator payloads
                case allocated of
                    Left (problem, diagnostics) -> pure (state, Left (allocationFailure problem diagnostics))
                    Right pointers -> do
                        let identifiers = map StorageId (take (length pointers) [stateNextStorage state ..])
                            nextState =
                                state
                                    { stateNextStorage = stateNextStorage state + count
                                    , statePayloadBytes = nextFresh
                                    , stateBuffers = nextBuffers
                                    , stateScalarWork = nextWork
                                    , stateLiveAllocations = stateLiveAllocations state ++ pointers
                                    }
                        pure (nextState, Right (zip identifiers pointers, report))

allocateStaged :: TensorAllocator -> [([Double], Natural)] -> IO (Either (String, [String]) [ForeignPtr Double])
allocateStaged allocator payloads = mask $ \_ -> go 1 [] payloads
  where
    go :: Natural -> [ForeignPtr Double] -> [([Double], Natural)] -> IO (Either (String, [String]) [ForeignPtr Double])
    go _ reversed [] = pure (Right (reverse reversed))
    go index reversed ((values, _) : rest) = do
        allocated <- try @SomeException (allocatorAllocate allocator (length values))
        case allocated of
            Left problem -> case fromException problem :: Maybe AsyncException of
                Just _ -> rejectException reversed problem
                Nothing -> rejectStaged index reversed (displayException problem)
            Right (Left problem) -> rejectStaged index reversed problem
            Right (Right pointer) -> do
                initialized <- try @SomeException $ withForeignPtr pointer $ \raw ->
                    forM_ (zip [0 ..] values) (uncurry (pokeElemOff raw))
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
                            pure $ do
                                (payloads, report) <- allocated
                                if length payloads /= length plans
                                    then Left (HostAllocationFailure "internal allocation-count mismatch")
                                    else Right (zipWith makeDynamic plans payloads, report)
  where
    makeDynamic (SomeShape shape, elements, _) (identifier, pointer) =
        DynamicHostTensor (HostTensor SF64 shape (contiguousLayout shape) identifier elements pointer)

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
                            [(identifier, pointer)] -> Right (HostTensor SF64 shape (contiguousLayout shape) identifier elements pointer, report)
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
                                [(identifier, pointer)] -> Right (FiniteTensor (HostTensor SF64 shape (contiguousLayout shape) identifier elements pointer), report)
                                _ -> Left (HostAllocationFailure "internal allocation-count mismatch")
  where
    validateFiniteInput _ [] = Right ()
    validateFiniteInput index (value : rest)
        | finite value = validateFiniteInput (index + 1) rest
        | otherwise = Left (TensorNumericError (NonFiniteInput "from-list" index))

logicalOffsets :: Natural -> CheckedLayout shape -> [Natural]
logicalOffsets total layout = map offsetFor (take (fromIntegral total) [0 ..])
  where
    dimensions = layoutDimensions layout
    strides = layoutStridesElements layout
    offsetFor linear = layoutOffsetElements layout + sum (zipWith (*) (coordinates dimensions linear) strides)

coordinates :: [Natural] -> Natural -> [Natural]
coordinates dimensions linear = snd (foldr step (linear, []) dimensions)
  where
    step dimension (remaining, result)
        | dimension == 0 = (0, 0 : result)
        | otherwise = let (quotient, remainder) = remaining `quotRem` dimension in (quotient, remainder : result)

-- | Observe logical values in row-major coordinate order.
tensorToList :: HostTensor region 'F64 shape -> IO [Double]
tensorToList (HostTensor SF64 _ layout _ elements pointer) =
    withForeignPtr pointer $ \raw -> forM (logicalOffsets elements layout) (peekElemOff raw . fromIntegral)

-- | Check every raw IEEE value and produce the finite numerical refinement.
finiteTensor :: HostTensor region 'F64 shape -> IO (Either TensorError (FiniteTensor region 'F64 shape))
finiteTensor tensor = do
    values <- tensorToList tensor
    pure $ case validateFiniteInput 0 values of
        Left problem -> Left problem
        Right () -> Right (FiniteTensor tensor)
  where
    validateFiniteInput _ [] = Right ()
    validateFiniteInput index (value : rest)
        | finite value = validateFiniteInput (index + 1) rest
        | otherwise = Left (TensorNumericError (NonFiniteInput "finite-tensor" index))

-- Views -----------------------------------------------------------------------

-- | Make an immutable zero-copy two-dimensional transpose view.
transpose2D :: HostTensor region dtype '[rows, columns] -> HostTensor region dtype '[columns, rows]
transpose2D (HostTensor dtype (SCons _ (SCons _ SNil)) layout identifier elements pointer) =
    HostTensor dtype knownShape transposed identifier elements pointer
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
reshapeContiguous (TensorSession limits _ _) target (HostTensor dtype _ layout identifier elements pointer) = do
    _ <- checkedShape limits dtype target
    if not (layoutIsContiguous layout)
        then Left (TensorLayoutError NonContiguousReshape)
        else
            if shapeElements target /= elements
                then Left (TensorShapeError (ShapeMismatch [elements] [shapeElements target]))
                else Right (HostTensor dtype target (contiguousLayout target) identifier elements pointer)

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
    Right (_, bytes) -> do
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
                            [(identifier, pointer)] -> Right (FiniteTensor (HostTensor SF64 shape (contiguousLayout shape) identifier (shapeElements shape) pointer), report)
                            _ -> Left (HostAllocationFailure "internal allocation-count mismatch")

tensorElementCount :: HostTensor region dtype shape -> Natural
tensorElementCount (HostTensor _ _ _ _ elements _) = elements

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
    Right (_, bytes) -> do
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
                                let make identifier pointer = FiniteTensor (HostTensor SF64 shape (contiguousLayout shape) identifier (shapeElements shape) pointer)
                                 in Right ((make leftId leftPointer, make rightId rightPointer), report)
                            _ -> Left (HostAllocationFailure "internal allocation-count mismatch")

makeTwoShapes :: TensorSession region -> String -> Natural -> SShape leftShape -> [Double] -> SShape rightShape -> [Double] -> IO (Either TensorError ((FiniteTensor region 'F64 leftShape, FiniteTensor region 'F64 rightShape), TensorOperationReport))
makeTwoShapes session@(TensorSession limits _ _) primitive work leftShape leftValues rightShape rightValues =
    case (checkedShape limits SF64 leftShape, checkedShape limits SF64 rightShape) of
        (Left problem, _) -> pure (Left problem)
        (_, Left problem) -> pure (Left problem)
        (Right (_, leftBytes), Right (_, rightBytes)) -> do
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
                                    let leftTensor = FiniteTensor (HostTensor SF64 leftShape (contiguousLayout leftShape) leftId (shapeElements leftShape) leftPointer)
                                        rightTensor = FiniteTensor (HostTensor SF64 rightShape (contiguousLayout rightShape) rightId (shapeElements rightShape) rightPointer)
                                     in Right ((leftTensor, rightTensor), report)
                                _ -> Left (HostAllocationFailure "internal allocation-count mismatch")
