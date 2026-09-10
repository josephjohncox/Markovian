{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- Actual API executions of the adopted r4 table, not a formula model.
module AffineMaterializationTests (affineMaterializationTests) where

import Control.Concurrent.MVar (withMVar)
import Control.Monad (forM_, join, replicateM_, unless)
import Data.IORef
import Data.Kind (Type)
import Foreign.ForeignPtr (ForeignPtr)
import Markovian.Tensor.Internal
import Markovian.Tensor.Shape
import Numeric.Natural (Natural)

data Operation = New | Permute | Reverse | Slice | Bind | Pull | Batches | BadBind | BadPull
    deriving (Eq)

data Fixture = Fixture String Operation [Natural] [Natural] Int [Int] Int AffineUsage (Maybe TensorError)

fixtures :: [Fixture]
fixtures =
    [ Fixture "signed-map" New [5] [3] 4 [-2] 0 (AffineUsage 68255 8308 1215) Nothing
    , Fixture "overlap-post-admission" New [4] [2, 2] 0 [1, 1] 0 (AffineUsage 104142 12778 1342) (Just (TensorAffineError (AffineOverlap 1 2 1)))
    , Fixture "empty-map" New [5] [0] 0 [0] 0 (AffineUsage 49535 5968 1215) Nothing
    , Fixture "scalar-map" New [] [] 0 [] 0 (AffineUsage 38332 4590 1036) Nothing
    , Fixture "singleton-map" New [5] [1] 2 [0] 0 (AffineUsage 51967 6272 1215) Nothing
    , Fixture "permute" Permute [5] [3] 4 [-2] 0 (AffineUsage 150153 18365 1442) Nothing
    , Fixture "reverse" Reverse [5] [3] 4 [-2] 0 (AffineUsage 150153 18365 1442) Nothing
    , Fixture "slice" Slice [5] [3] 4 [-2] 0 (AffineUsage 150153 18365 1442) Nothing
    , Fixture "bind" Bind [5] [3] 4 [-2] 2 (AffineUsage 50569 6051 1585) Nothing
    , Fixture "signed-pull" Pull [5] [3] 4 [-2] 2 (AffineUsage 123442 14894 2129) Nothing
    , Fixture "empty-pull" Pull [0] [0] 0 [0] 2 (AffineUsage 112594 13538 2129) Nothing
    , Fixture "raw-empty-post-admission" New [5] [0, 2] 0 [0, 7] 0 (AffineUsage 59278 7170 1342) (Just (TensorAffineError AffineEmptyDescriptor))
    , Fixture "mixed1024" Pull [0] [0] 0 [0] 1024 (AffineUsage 316994 38066 10305) Nothing
    , Fixture "batches512-0-512" Batches [0] [0] 0 [0] 1024 (AffineUsage 316994 38066 10305) Nothing
    , Fixture "rank512-base-scalar-bind" Bind (replicate 512 1) [] 0 [] 2 (AffineUsage 3593756 440656 67932) Nothing
    , Fixture "rank512-base-scalar-pull-peak-before" Pull (replicate 512 1) [] 0 [] 2 (AffineUsage 6895157 848044 67932) Nothing
    , Fixture "scalar-base-rank512-seed" Pull [] (replicate 512 1) 0 (replicate 512 0) 2 (AffineUsage 10243125 1261740 104761) Nothing
    , Fixture "bind-private-failure" BadBind [5] [3] 4 [-2] 2 (AffineUsage 50569 6051 1585) (Just (TensorShapeError (MachineIndexOverflow (affineMachineMaximum + 1))))
    , Fixture "pull-private-failure" BadPull [5] [3] 4 [-2] 2 (AffineUsage 123442 14894 2129) (Just (TensorAffineError (AffineShapeDisagreement AffineViewShape 0 3 (affineMachineMaximum + 1))))
    ]

right :: (Show problem) => Either problem value -> IO value
right = either (fail . show) pure

check :: (Eq value, Show value) => String -> value -> value -> IO ()
check label expected actual = unless (expected == actual) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

limitsFor :: AffineUsage -> IO AffineLimits
limitsFor (AffineUsage cells work live) = right (affineLimits maxBound maxBound maxBound (fromIntegral cells) (fromIntegral work) (fromIntegral live))

-- Compare the observed failure against the first admission event, not a global
-- sort of eventually insufficient caps. A historical bind peak can win first.
expectedFailure :: Fixture -> (Bool, Bool, Bool) -> AffineUsage -> TensorError
expectedFailure (Fixture name _ _ _ _ _ _ _ _) (cellsBelow, workBelow, liveBelow) (AffineUsage cells work live) =
    let field
            | name == "rank512-base-scalar-pull-peak-before" && liveBelow = AffineLiveCells
            | cellsBelow = AffineConstructedCells
            | workBelow = AffineWork
            | otherwise = AffineLiveCells
        cap = case field of AffineConstructedCells -> cells; AffineWork -> work; _ -> live
     in TensorAffineError (AffineLimitExceeded field cap (cap + 1))

affineMaterializationTests :: IO ()
affineMaterializationTests = do
    prefixControls
    forM_ fixtures $ \fixture@(Fixture name _ _ _ _ _ _ exact semantic) ->
        forM_ [(c, w, q) | c <- [False, True], w <- [False, True], q <- [False, True]] $ \below@(c, w, q) -> do
            let AffineUsage cells work live = exact
                lower yes value = if yes then value - 1 else value
                caps = AffineUsage (lower c cells) (lower w work) (lower q live)
                expected = if c || w || q then Just (expectedFailure fixture below caps) else semantic
            policy <- limitsFor caps
            actual <- execute fixture policy
            case (expected, actual) of
                (Nothing, Right usage) -> check (name ++ " exact usage") exact usage
                (Just problem, Left actualProblem) -> check (name ++ " exact diagnostic") problem actualProblem
                (Nothing, Left problem) -> fail (name ++ ": unexpected " ++ show problem)
                (Just problem, Right usage) -> fail (name ++ ": expected " ++ show problem ++ ", committed " ++ show usage)
            case expected of
                Just _ -> do
                    audited <- executeWithObservation True fixture policy
                    check (name ++ " independent transactional overlay") actual audited
                Nothing -> pure ()
    putStrLn "affine materialization: all 19 actual paths and 133 one-below/earliest-failure policies passed"

prefixControls :: IO ()
prefixControls = do
    SomeShape shape <- right (reifyShape (replicate 512 1))
    forM_
        [ (1610408, 197120, 27304, 512, AffineWork, 197120)
        , (1610408, 197888, 27199, 511, AffineLiveCells, 27199)
        , (1604160, 197888, 27304, 512, AffineConstructedCells, 1604160)
        ]
        $ \(cells, work, live, admitted, field, cap) -> do
            policy <- limitsFor (AffineUsage cells work live)
            budget <- right (affineBudget policy)
            let rejected = withAffineMap budget (poisonAfter admitted shape) SNil 0 [] (\_ _ _ -> ())
            case rejected of
                Left problem -> check "growing prefix rejects before poisoned cons/nil" (TensorAffineError (AffineLimitExceeded field cap (cap + 1))) problem
                Right () -> fail "prefix admitted poisoned suffix"
    policy <- limitsFor (AffineUsage affineMachineMaximum affineMachineMaximum affineMachineMaximum)
    let old = AffineUsage 4672 512 10000
    AffineHeader c1 w1 q1 <- right (affineDebit policy old (affineHeaderStart old))
    AffineHeader c2 w2 q2 <- right (affineDebit policy old (AffineHeader c1 w1 q1))
    check "local live is not accumulated from old peak" (10920, 1280, 680) (c2, w2, q2)
    forM_ [(AffineRank, [-1, 0, 0, 4672, 512, 576]), (AffineDimension, [0, -1, 0, 4672, 512, 576]), (AffineElements, [0, 0, -1, 4672, 512, 576]), (AffineConstructedCells, [0, 0, 0, -1, 512, 576]), (AffineWork, [0, 0, 0, 4672, -1, 576]), (AffineLiveCells, [0, 0, 0, 4672, 512, -1])] $ \(field, args) ->
        case args of
            [rank, dimension, elements, cells, work, live] -> case affineLimits rank dimension elements cells work live of
                Left problem -> check "individual negative limit" (TensorAffineError (AffineInvalidLimit field (-1))) problem
                Right _ -> fail "negative policy accepted"
            _ -> fail "negative fixture arity"
  where
    poisonAfter :: Int -> SShape shape -> SShape shape
    poisonAfter 0 _ = error "uncredited next shape constructor"
    poisonAfter n (SCons proxy rest) = SCons proxy (poisonAfter (n - 1) rest)
    poisonAfter _ SNil = SNil

execute :: Fixture -> AffineLimits -> IO (Either TensorError AffineUsage)
execute = executeWithObservation False

executeWithObservation :: Bool -> Fixture -> AffineLimits -> IO (Either TensorError AffineUsage)
executeWithObservation observed fixture@(Fixture _ operation baseDimensions viewDimensions offset strides _ _ _) policy = do
    SomeShape base <- right (reifyShape baseDimensions)
    SomeShape view <- right (reifyShape viewDimensions)
    if operation `elem` [New, Permute, Reverse, Slice]
        then do
            budget <- right (affineBudget policy)
            let result = withAffineMap budget base view offset strides $ \witness next report ->
                    case operation of
                        New -> Right (affineMapCumulative report)
                        Permute -> permuteAffineMap next witness view [0] (\_ _ r -> affineMapCumulative r)
                        Reverse -> reverseAffineMap next witness 0 (\_ _ r -> affineMapCumulative r)
                        Slice -> sliceAffineMap next witness view [0] [1] (\_ _ r -> affineMapCumulative r)
                        _ -> error "runtime fixture in pure branch"
            case join result of
                Left problem | observed -> do
                    check "pure rejection retains original budget" (AffineUsage 4672 512 576) (affineBudgetUsage budget)
                    putStrLn ("pure rejection retains original (4672,512,576) budget: " ++ show problem)
                    pure (Left problem)
                outcome -> pure outcome
        else do
            planning <- limitsFor (AffineUsage affineMachineMaximum affineMachineMaximum affineMachineMaximum) >>= right . affineBudget
            case withAffineMap planning base view offset strides $ \witness _ _ -> runtime observed fixture policy witness base view of
                Left problem -> pure (Left problem)
                Right action -> action

runtime :: forall map base view. Bool -> Fixture -> AffineLimits -> AffineMap map base view -> SShape base -> SShape view -> IO (Either TensorError AffineUsage)
runtime observed (Fixture name operation _ _ _ _ buffers _ _) policy witness baseShape viewShape = do
    calls <- newIORef (0 :: Int)
    let allocator = defaultTensorAllocator{allocatorAllocate = \count -> modifyIORef' calls (+ 1) >> allocatorAllocate defaultTensorAllocator count}
    withTensorSessionAllocator allocator (tensorSessionLimitsWithAffine (tensorSessionLimits 512 affineMachineMaximum affineMachineMaximum 1000000 10000000 2048 10000000) policy) $ \session -> do
        if operation == Batches
            then do
                -- All 1024 allocations are in the two actual batches. No registry
                -- observation or private setup normalization is performed.
                first <- hostTensorBatchFromLists session (replicate 512 ([0], [])) >>= right
                empty <- hostTensorBatchFromLists session [] >>= right
                second <- hostTensorBatchFromLists session (replicate 512 ([0], [])) >>= right
                check "empty batch report" (TensorOperationReport "from-lists" 0 (TensorMemoryReport 0 0 0 0)) (snd empty)
                case (fst first, fst second) of
                    (DynamicHostTensor base : _, DynamicHostTensor seed : _) -> do
                        -- The original typed witnesses are [0]; public reshape
                        -- checks the equal count rather than casting indices.
                        typedBase <- right (reshapeContiguous session baseShape base)
                        typedSeed <- right (reshapeContiguous session viewShape seed)
                        finish calls session (FiniteTensor typedBase) (FiniteTensor typedSeed)
                    _ -> fail "empty nonempty batch fixture"
            else do
                (base, _) <- finiteTensorFromList session baseShape (replicate (fromIntegral (shapeElements baseShape)) 1) >>= right
                (seed, _) <- finiteTensorFromList session viewShape (replicate (fromIntegral (shapeElements viewShape)) 2) >>= right
                replicateM_ (buffers - 2) (finiteTensorFromList session (knownShape @'[0]) [] >>= right)
                finish calls session base seed
  where
    finish :: forall (region :: Type). IORef Int -> TensorSession region -> FiniteTensor region 'F64 base -> FiniteTensor region 'F64 view -> IO (Either TensorError AffineUsage)
    finish calls session base seed = do
        owner <- right (tensorOwner "r4 original" baseShape)
        let supplied = if operation == BadBind then corruptCapacity base else base
        bound <- auditRejection observed (name ++ "/bind") calls session (bindAffineView session witness (ownTensor owner supplied))
        case bound of
            Left problem -> pure (Left problem)
            Right (binding, report)
                | operation == Bind || operation == BadBind -> pure (Right (affineOperationCumulative report))
                | otherwise -> do
                    result <- auditRejection observed (name ++ "/pull") calls session (pullbackAffineView session binding (if operation == BadPull then corruptDimensions seed else seed))
                    case result of
                        Left problem -> pure (Left problem)
                        Right (gradient, pullReport) -> do
                            check (name ++ " gradient count") (shapeElements baseShape) (tensorElementCount (hostTensor (ownedFiniteTensor gradient)))
                            check (name ++ " original owner") "r4 original" (ownerKey (ownedTensorOwner gradient))
                            check (name ++ " fresh ID") (StorageId (fromIntegral buffers)) (tensorStorageId (hostTensor (ownedFiniteTensor gradient)))
                            pure (Right (affineOperationCumulative pullReport))
    corruptCapacity (FiniteTensor (HostTensor dtype shape layout identifier count _ pointer)) =
        FiniteTensor (HostTensor dtype shape layout identifier count (affineMachineMaximum `div` 8 + 1) pointer)
    corruptDimensions (FiniteTensor (HostTensor dtype shape layout identifier count capacity pointer)) =
        FiniteTensor (HostTensor dtype shape (layout{layoutDimensions = [affineMachineMaximum + 37]}) identifier count capacity pointer)

-- Kept separate from natural history execution: this overlay intentionally
-- observes the ordered registry before each admission event.
auditRejection :: Bool -> String -> IORef Int -> TensorSession region -> IO (Either TensorError value) -> IO (Either TensorError value)
auditRejection False _ _ _ action = action
auditRejection True label calls session action = do
    before <- transactionalSnapshot session
    allocatedBefore <- readIORef calls
    result <- action
    case result of
        Left problem -> do
            check (label ++ " rejected event leaves complete state and ordered registry identities") before =<< transactionalSnapshot session
            check (label ++ " rejected event allocates nothing") allocatedBefore =<< readIORef calls
            putStrLn (label ++ " rejected before allocation; unchanged ordered registry/ledgers/ID: " ++ show problem)
        Right _ -> pure ()
    pure result

transactionalSnapshot :: TensorSession region -> IO (Natural, Natural, Natural, Natural, AffineUsage, Bool, [ForeignPtr Double])
transactionalSnapshot (TensorSession _ _ lock) = withMVar lock $ \state -> do
    let pointers = stateLiveAllocations state
    forceAllocationSpine pointers
    pure (stateNextStorage state, statePayloadBytes state, stateBuffers state, stateScalarWork state, stateAffineUsage state, stateClosed state, pointers)
