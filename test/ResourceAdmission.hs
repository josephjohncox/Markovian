module ResourceAdmission (runResourceAdmissionTests, runResourceAdmissionLesson) where

import Control.Monad (foldM, foldM_, forM_, unless, void, when)
import Data.Bifunctor (second)
import Data.List.NonEmpty qualified as NE
import Data.Ratio (denominator, numerator, (%))
import Markovian.Probability.Exact

-- Closed accounting model, NOT a cache or retained-circuit interpreter. A
-- hypothetical hit still runs validation here; its declared executor cost is
-- an assumption of the experiment, not measured Haskell work or a speedup.
data Program = Hundred | CancelEarly | CancelLate | DiscardBoth | RefuseEarly | RefuseLate
    deriving (Eq, Show)
data Path = Reference | Construction | HypotheticalHit deriving (Eq, Show)
data InterpreterIdentity = InterpreterV1 | InterpreterV2 deriving (Eq, Show)
data Layout = Forward | Reversed deriving (Eq, Show)
data SemanticLimits = SemanticLimits
    { sourceStepLimit :: Int
    , semanticWorkLimit :: Int
    , numeratorBitLimit :: Int
    , denominatorBitLimit :: Int
    }
    deriving (Eq, Show)
data ExecutorLimits = ExecutorLimits
    { occurrenceLimit :: Int
    , traceSlotLimit :: Int
    , executorWorkLimit :: Int
    }
    deriving (Eq, Show)

-- Layout is a closed stand-in for exact endpoint order, not set equality.
-- Identity tokens are explicit assumptions, never inferred from closure labels.
data ModelKey = ModelKey InterpreterIdentity Program Layout Layout SemanticLimits
    deriving (Eq, Show)
data Failure
    = InvalidLimits
    | OccurrencesExceeded Int
    | TraceSlotsExceeded Int
    | ExecutorWorkExceeded Int
    | SourceStepsExceeded Int
    | SemanticWorkExceeded Int
    | NumeratorBitsExceeded Int
    | DenominatorBitsExceeded Int
    | PrimitiveRefused
    deriving (Eq, Show)
data Account = Account
    { semanticUsed :: Int
    , maximumNumeratorBits :: Int
    , maximumDenominatorBits :: Int
    , executorReserved :: Int
    }
    deriving (Eq, Show)
data Step = Put Rational | Refuse

programSize :: Program -> Int
programSize Hundred = 100
programSize _ = 3

programSteps :: Program -> [Step]
programSteps Hundred = replicate 100 (Put 1)
programSteps CancelEarly = [Put (1 % 256), Put 0, Put 0]
programSteps CancelLate = [Put 0, Put (1 % 256), Put 0]
programSteps DiscardBoth = [Put (257 % 256), Put 0, Put 0]
programSteps RefuseEarly = [Refuse, Put 0, Put 0]
programSteps RefuseLate = [Put 0, Refuse, Put 0]

-- Limits exclude maxBound so the saturated next-unit sentinel fits in Int.
saturatedAdd :: Int -> Int -> Int -> Int
saturatedAdd limit used amount
    | used > limit || amount > limit - used = limit + 1
    | otherwise = used + amount

reserve :: (Int -> Failure) -> Int -> Int -> Int -> Either Failure Int
reserve problem limit used amount = do
    let next = saturatedAdd limit used amount
    when (next > limit) (Left (problem next))
    pure next

boundedPlan :: Int -> [(Path, Program)] -> Either Failure [(Path, Program)]
boundedPlan limit = go 0 []
  where
    go _ reversed [] = Right (reverse reversed)
    go used reversed (entry : rest) = do
        next <- reserve OccurrencesExceeded limit used 1
        go next (entry : reversed) rest

-- Stop scanning the already supplied integer at limit+1 bits. Input fixture
-- construction is outside the model, just as a supplied Rational already exists.
observedBits :: Int -> Integer -> Int
observedBits limit = go 0 . abs
  where
    go bits 0 = max 1 bits
    go bits remaining
        | bits == limit = limit + 1
        | otherwise = go (bits + 1) (remaining `quot` 2)

-- One request-wide semantic account, including construction and later hits.
-- Left has no partial values, report, or entry. Executor reservations precede
-- source structure, then unit work precedes numerator/denominator/primitive.
runModel :: SemanticLimits -> ExecutorLimits -> [(Path, Program)] -> Either Failure ([Rational], Account)
runModel semantic executor requested = do
    let allLimits = [sourceStepLimit semantic, semanticWorkLimit semantic, numeratorBitLimit semantic, denominatorBitLimit semantic, occurrenceLimit executor, traceSlotLimit executor, executorWorkLimit executor]
    unless (all (\n -> n >= 0 && n < maxBound) allLimits && numeratorBitLimit semantic > 0 && denominatorBitLimit semantic > 0) (Left InvalidLimits)
    plan <- boundedPlan (occurrenceLimit executor) requested
    foldM_ (\used (path, program) -> reserve TraceSlotsExceeded (traceSlotLimit executor) used (if path == Reference then 0 else programSize program)) 0 plan
    execution <- foldM (\used entry -> reserve ExecutorWorkExceeded (executorWorkLimit executor) used (executionCost entry)) 0 plan
    foldM_ (\used (_, program) -> reserve SourceStepsExceeded (sourceStepLimit semantic) used (programSize program)) 0 plan
    (reversed, final) <- foldM execute ([], Account 0 0 0 execution) plan
    pure (reverse reversed, final)
  where
    executionCost (Reference, program) = programSize program
    executionCost (Construction, program) = programSize program + 2
    executionCost (HypotheticalHit, _) = 1
    execute (values, account) (_, program) = do
        (value, final) <- foldM step (0, account) (programSteps program)
        pure (value : values, final)
    step (_, account) operation = do
        used <- reserve SemanticWorkExceeded (semanticWorkLimit semantic) (semanticUsed account) 1
        let charged = account{semanticUsed = used}
        case operation of
            Refuse -> Left PrimitiveRefused
            Put value -> do
                let n = observedBits (numeratorBitLimit semantic) (numerator value)
                    d = observedBits (denominatorBitLimit semantic) (denominator value)
                when (n > numeratorBitLimit semantic) (Left (NumeratorBitsExceeded n))
                when (d > denominatorBitLimit semantic) (Left (DenominatorBitsExceeded d))
                pure (value, charged{maximumNumeratorBits = max n (maximumNumeratorBits charged), maximumDenominatorBits = max d (maximumDenominatorBits charged)})

semanticDefaults :: SemanticLimits
semanticDefaults = SemanticLimits 200 200 1 9
executorDefaults :: ExecutorLimits
executorDefaults = ExecutorLimits 2 200 202

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual = unless (expected == actual) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

answer :: (Eq a, Show a) => String -> a -> a -> IO ()
answer label expected actual = do
    assertEqual label expected actual
    putStrLn (label ++ " = " ++ show actual)

checked :: (Show e) => Either e a -> IO a
checked = either (fail . show) pure

runResourceAdmissionTests :: (String -> IO () -> IO ()) -> IO ()
runResourceAdmissionTests run = do
    run "resource admission exact bounds and cumulative model" testBoundaries
    run "resource admission ordered discarded rationals and primitive failure" testOrder
    run "resource admission explicit key separation" testKeys
    run "resource admission saturating preflight" testSaturation
    run "resource admission checked lesson and bind association" runResourceAdmissionLesson

testBoundaries :: IO ()
testBoundaries = do
    forM_ [Reference, Construction, HypotheticalHit] $ \path -> do
        let cost = case path of Reference -> 100; Construction -> 102; HypotheticalHit -> 1
            slots = if path == Reference then 0 else 100
            semantic = SemanticLimits 100 100 1 1
            executor = ExecutorLimits 1 slots cost
            evaluate = runModel semantic executor [(path, Hundred)]
        assertEqual "exact all limits" (Right ([1], Account 100 1 1 cost)) evaluate
        forM_ [0 .. 99] $ \work ->
            assertEqual "every work prefix" (Left (SemanticWorkExceeded (work + 1))) (runModel semantic{semanticWorkLimit = work} executor [(path, Hundred)])
        assertEqual "source structure one below" (Left (SourceStepsExceeded 100)) (runModel semantic{sourceStepLimit = 99} executor [(path, Hundred)])
        assertEqual "executor one below" (Left (ExecutorWorkExceeded cost)) (runModel semantic executor{executorWorkLimit = cost - 1} [(path, Hundred)])
        assertEqual "occurrences one below" (Left (OccurrencesExceeded 1)) (runModel semantic executor{occurrenceLimit = 0} [(path, Hundred)])
        when (slots > 0) $
            assertEqual "trace slots one below" (Left (TraceSlotsExceeded 100)) (runModel semantic executor{traceSlotLimit = 99} [(path, Hundred)])
    let request = [(Construction, Hundred), (HypotheticalHit, Hundred)]
        executor = ExecutorLimits 2 200 103
    assertEqual "construction not double charged; hit recharged" (Right ([1, 1], Account 200 1 1 103)) (runModel semanticDefaults executor request)
    assertEqual "hit cannot reset semantic meter" (Left (SemanticWorkExceeded 200)) (runModel semanticDefaults{semanticWorkLimit = 199} executor request)
    assertEqual "hit cannot reset executor meter" (Left (ExecutorWorkExceeded 103)) (runModel semanticDefaults executor{executorWorkLimit = 102} request)
    assertEqual "cumulative trace storage" (Left (TraceSlotsExceeded 200)) (runModel semanticDefaults executor{traceSlotLimit = 199} request)
    assertEqual "cumulative source structure" (Left (SourceStepsExceeded 200)) (runModel semanticDefaults{sourceStepLimit = 199} executor request)
    assertEqual "rational maximum survives later occurrence" (Right ([0, 1], Account 103 1 9 6)) (runModel semanticDefaults executorDefaults [(Construction, CancelEarly), (HypotheticalHit, Hundred)])
    assertEqual "bounded infinite plan" (Left (OccurrencesExceeded 3)) (runModel semanticDefaults executorDefaults (repeat (Reference, Hundred)))
    assertEqual "empty request" (Right ([], Account 0 0 0 0)) (runModel semanticDefaults executorDefaults [])
    forM_ [semanticDefaults{sourceStepLimit = -1}, semanticDefaults{semanticWorkLimit = maxBound}, semanticDefaults{numeratorBitLimit = 0}, semanticDefaults{denominatorBitLimit = 0}] $ \bad ->
        assertEqual "invalid semantic limits before plan" (Left InvalidLimits) (runModel bad executorDefaults (error "plan forced before limits"))
    forM_ [ExecutorLimits (-1) 1 1, ExecutorLimits 1 maxBound 1, ExecutorLimits 1 1 (-1)] $ \bad ->
        assertEqual "invalid executor limits before plan" (Left InvalidLimits) (runModel semanticDefaults bad (error "plan forced before limits"))
    assertEqual "infrastructure before source failure" (Left (ExecutorWorkExceeded 1)) (runModel semanticDefaults{sourceStepLimit = 0, semanticWorkLimit = 0} executorDefaults{executorWorkLimit = 0} [(HypotheticalHit, RefuseEarly)])
    assertEqual "trace before executor failure" (Left (TraceSlotsExceeded 1)) (runModel semanticDefaults executorDefaults{traceSlotLimit = 0, executorWorkLimit = 0} [(HypotheticalHit, Hundred)])

testOrder :: IO ()
testOrder = forM_ [Reference, Construction, HypotheticalHit] $ \path -> do
    let evaluate semantic program = runModel semantic executorDefaults [(path, program)]
        narrow = semanticDefaults{semanticWorkLimit = 1, denominatorBitLimit = 8}
        summary = fmap (\(values, account) -> (values, semanticUsed account, maximumDenominatorBits account))
    forM_ [CancelEarly, CancelLate] $ \program -> do
        assertEqual "same admitted aggregates" (Right ([0], 3, 9)) (summary (evaluate semanticDefaults program))
        assertEqual "discarded denominator one below" (Left (DenominatorBitsExceeded 9)) (evaluate semanticDefaults{denominatorBitLimit = 8} program)
    let rationalLimits = semanticDefaults{numeratorBitLimit = 9}
        cost = case path of Reference -> 3; Construction -> 5; HypotheticalHit -> 1
    assertEqual "both rational limits exact" (Right ([0], Account 3 9 9 cost)) (evaluate rationalLimits DiscardBoth)
    assertEqual "discarded numerator one below" (Left (NumeratorBitsExceeded 9)) (evaluate rationalLimits{numeratorBitLimit = 8} DiscardBoth)
    assertEqual "discarded denominator one below with large numerator" (Left (DenominatorBitsExceeded 9)) (evaluate rationalLimits{denominatorBitLimit = 8} DiscardBoth)
    assertEqual "numerator before denominator" (Left (NumeratorBitsExceeded 9)) (evaluate rationalLimits{numeratorBitLimit = 8, denominatorBitLimit = 8} DiscardBoth)
    assertEqual "early discarded value before later work" (Left (DenominatorBitsExceeded 9)) (evaluate narrow CancelEarly)
    assertEqual "work before later discarded value" (Left (SemanticWorkExceeded 2)) (evaluate narrow CancelLate)
    assertEqual "same-step work before rational" (Left (SemanticWorkExceeded 1)) (evaluate narrow{semanticWorkLimit = 0} CancelEarly)
    assertEqual "primitive failure" (Left PrimitiveRefused) (evaluate semanticDefaults RefuseEarly)
    assertEqual "primitive before later work" (Left PrimitiveRefused) (evaluate narrow RefuseEarly)
    assertEqual "work before later primitive" (Left (SemanticWorkExceeded 2)) (evaluate narrow RefuseLate)
    assertEqual "same-step work before primitive" (Left (SemanticWorkExceeded 1)) (evaluate narrow{semanticWorkLimit = 0} RefuseEarly)

testKeys :: IO ()
testKeys = do
    let key = ModelKey InterpreterV1 Hundred Forward Forward semanticDefaults
        changed =
            [ ModelKey InterpreterV2 Hundred Forward Forward semanticDefaults
            , ModelKey InterpreterV1 CancelEarly Forward Forward semanticDefaults
            , ModelKey InterpreterV1 Hundred Reversed Forward semanticDefaults
            , ModelKey InterpreterV1 Hundred Forward Reversed semanticDefaults
            ]
        limits = [semanticDefaults{sourceStepLimit = 199}, semanticDefaults{semanticWorkLimit = 199}, semanticDefaults{numeratorBitLimit = 2}, semanticDefaults{denominatorBitLimit = 8}]
    assertEqual "identical key" True (key == ModelKey InterpreterV1 Hundred Forward Forward semanticDefaults)
    forM_ (changed ++ map (ModelKey InterpreterV1 Hundred Forward Forward) limits) $ \other -> assertEqual "changed key misses" False (key == other)

testSaturation :: IO ()
testSaturation = forM_ [0, 1, 10, 100, maxBound - 1] $ \limit ->
    forM_ [0, 1, 100, maxBound - 1] $ \used ->
        forM_ [0, 1, 100, maxBound - 1] $ \amount ->
            assertEqual "Integer saturation oracle" (fromInteger (min (toInteger limit + 1) (toInteger used + toInteger amount))) (saturatedAdd limit used amount)

-- Actual checked-bind API contrast. These are separate public operations, each
-- with its documented local budget, NOT a cumulative optimization wrapper.
bindAssociationLesson :: IO ()
bindAssociationLesson = do
    coin <- checked (exactFiniteDist [(0 :: Int, 1), (1, 1)])
    let branch x = fmap (+ x) coin
        final = exactDirac . (* 3)
        succeed :: a -> Either String a
        succeed = Right
        work report = exactBindOuterEntries report + exactBindContinuationCalls report + exactBindResultEntries report + exactBindMassMultiplications report
        outcomes = map (second exactProbability) . NE.toList . exactOutcomes
    generous <- checked (exactBindLimits 4 16 1 3)
    narrow <- checked (exactBindLimits 4 12 1 3)
    (first, firstReport) <- checked (bindExactFiniteDistChecked narrow coin (succeed . branch))
    (left, leftReport) <- checked (bindExactFiniteDistChecked generous first (succeed . final))
    (right, rightReport) <- checked (bindExactFiniteDistChecked narrow coin (\x -> fst <$> bindExactFiniteDistChecked narrow (branch x) (succeed . final)))
    branchReports <- traverse (\x -> snd <$> checked (bindExactFiniteDistChecked narrow (branch x) (succeed . final))) [0, 1]
    let expected = [(0, 1 % 4), (3, 1 % 4), (3, 1 % 4), (6, 1 % 4)]
    answer "G admitted bind outcomes (both associations)" [expected, expected] [outcomes left, outcomes right]
    answer "G left per-call work" [12, 16] [work firstReport, work leftReport]
    answer "G right outer and two inner work" [12, 8, 8] (work rightReport : map work branchReports)
    answer "G left second bind at local budget 12" (Left (ExactBindWorkLimitExceeded 13) :: Either (ExactBindError String) ()) (void (bindExactFiniteDistChecked narrow first (succeed . final)))

runResourceAdmissionLesson :: IO ()
runResourceAdmissionLesson = do
    let evaluate budget path = runModel semanticDefaults{semanticWorkLimit = budget} executorDefaults [(path, Hundred)]
        pair budget = map (evaluate budget) [Reference, HypotheticalHit]
    answer "G source budget 10 (reference, hypothetical hit)" [Left (SemanticWorkExceeded 11), Left (SemanticWorkExceeded 11)] (pair 10)
    answer "G source budget 100 (values and separate accounts)" [Right ([1], Account 100 1 1 100), Right ([1], Account 100 1 1 1)] (pair 100)
    answer "G executor budget 10 (reference, hypothetical hit)" [Left (ExecutorWorkExceeded 11), Right ([1], Account 100 1 1 1)] (map (\path -> runModel semanticDefaults executorDefaults{executorWorkLimit = 10} [(path, Hundred)]) [Reference, HypotheticalHit])
    answer "G construction then hit (one request)" (Right ([1, 1], Account 200 1 1 103)) (runModel semanticDefaults executorDefaults [(Construction, Hundred), (HypotheticalHit, Hundred)])
    let key = ModelKey InterpreterV1 Hundred Forward Forward
    answer "G changed semantic limit matches key" False (key semanticDefaults == key semanticDefaults{semanticWorkLimit = 10})
    answer "G same aggregates, different first failures" [Left (DenominatorBitsExceeded 9), Left (SemanticWorkExceeded 2)] (map (\program -> runModel semanticDefaults{semanticWorkLimit = 1, denominatorBitLimit = 8} executorDefaults [(HypotheticalHit, program)]) [CancelEarly, CancelLate])
    bindAssociationLesson
