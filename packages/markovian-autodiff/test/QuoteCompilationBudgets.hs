{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module QuoteCompilationBudgets (run, productOwnerBudgets) where

import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import Data.Proxy (Proxy)
import GHC.TypeLits (SomeSymbol (..), someSymbolVal)
import GHC.TypeNats (SomeNat (..), someNatVal)
import Markovian.Autodiff
import Numeric.Natural (Natural)

-- Balanced *value* products, one owner, no target joins. Ported from the
-- approved ProductOwner.hs; complete budgets are literal independent oracles.
data SomeShape where
    SomeShape :: SShape shape -> SomeShape

balanced :: Int -> SomeShape
balanced 0 = SomeShape SScalar
balanced n = case balanced (n - 1) of SomeShape s -> SomeShape (SProduct s s)

budgets :: Natural -> Natural -> Natural -> Natural -> QuotationLimits
budgets t a r w = quotationLimits t 10000 10000 10000 10000 10000 10000 10000 a r w 256

generous :: QuotationLimits
generous = budgets 1000000000 1000000000 1000000000 1000000000

right :: (Show e) => Either e a -> a
right = either (error . show) id

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name actual expected = unless (actual == expected) (error (name ++ ": " ++ show actual ++ " /= " ++ show expected))

work :: QuoteReport -> (Natural, Natural, Natural, Natural)
work r = (quoteTraversalWork r, quoteAllocationCount r, quoteRuntimeWork r, quoteTotalWork r)

reject :: String -> QuoteError -> Either QuoteCompileError a -> IO ()
reject name expected result = case result of
    Left (QuoteCompilePreflightFailure actual) -> check name actual expected
    Left e -> error (name ++ " wrong stage: " ++ show e)
    Right _ -> error (name ++ " unexpectedly admitted")

productOwnerBudgets :: IO ()
productOwnerBudgets = forM_ [(0, 1, 266, 252, 16, 535), (1, 2, 448, 435, 29, 913), (2, 4, 812, 801, 55, 1669), (7, 128, 23380, 23493, 1667, 48541)] $ \(depth, k, t, a, r, w) -> case balanced depth of
    SomeShape s -> do
        let q = quoteProgram (parameter @"product-owner" SUnit s)
            syntax = right (preflightQuote generous q)
        check "unchanged syntax" (work syntax) (2, 1 + 3 * k, 1 + 5 * k, 5 + 8 * k)
        forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
            let (exe, report) = right (compileExactQuote (budgets t a r w) policy q)
                target = right (compileExactPolynomial (compilerLimits 1 1 1 1 k k (2 * k - 1) (fromIntegral depth + 1) (1 + 5 * k) 256) policy (parameter @"product-owner" SUnit s))
            check "product-owner complete literal budget" (work (quoteCompilationPreflight report)) (t, a, r, w)
            check "actual target report" (quoteCompilationTarget report) (exactCompileReport exe)
            check "derived capacity report agreement" (quoteCompilationTarget report) (exactCompileReport target)
            reject "traversal one below" (QuoteTraversalLimitExceeded (t - 1) t) (compileExactQuote (budgets (t - 1) 1000000 1000000 1000000) policy q)
            reject "allocation one below" (QuoteAllocationLimitExceeded (a - 1) a) (compileExactQuote (budgets 1000000 (a - 1) 1000000 1000000) policy q)
            reject "runtime one below" (QuoteRuntimeWorkLimitExceeded (r - 1) r) (compileExactQuote (budgets 1000000 1000000 (r - 1) 1000000) policy q)
            reject "total one below" (QuoteTotalWorkLimitExceeded (w - 1) w) (compileExactQuote (budgets 1000000 1000000 1000000 (w - 1)) policy q)
            let h = 2 * k - 1
            reject "omitted EQp traversal" (QuoteTraversalLimitExceeded (t - h) (t - h + 1)) (compileExactQuote (budgets (t - h) 1000000 1000000 1000000) policy q)
            reject "omitted EQp allocation" (QuoteAllocationLimitExceeded (a - h) (a - h + 1)) (compileExactQuote (budgets 1000000 (a - h) 1000000 1000000) policy q)
            reject "shared summary not truncated by allocation" (QuoteTraversalLimitExceeded (t - 1) t) (compileExactQuote (budgets (t - 1) (1 + 3 * k) 1000000 1000000) policy q)
            -- Each dimension error must win even when total has no remaining
            -- credit at that exact stage (before A: 190k+87; before R: 370k+155).
            reject "dimension before competing total" (QuoteTraversalLimitExceeded (t - 1) t) (compileExactQuote (budgets (t - 1) 1000000 1000000 (5 + 8 * k + 2 * k + 3)) policy q)
            reject "allocation before competing total" (QuoteAllocationLimitExceeded (a - 1) a) (compileExactQuote (budgets 1000000 (a - 1) 1000000 (190 * k + 87)) policy q)
            reject "runtime before competing total" (QuoteRuntimeWorkLimitExceeded (r - 1) r) (compileExactQuote (budgets 1000000 1000000 (r - 1) (370 * k + 155)) policy q)
            reject "planner traversal before competing total" (QuoteTraversalLimitExceeded 2 3) (compileExactQuote (budgets 2 1000000 1000000 (5 + 8 * k)) policy q)

-- Scalar owner: syntax total 13, five planner entries, traversal coupon 259,
-- then allocation coupon 248. These totals distinguish the two intermediate
-- total charges from dimension failures and runtime's final total charge.
reservationFailureStages :: IO ()
reservationFailureStages = forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
    let scalarOwner = quoteProgram (parameter @"product-owner" SUnit SScalar)
    reject "traversal total charge" (QuoteTotalWorkLimitExceeded 276 277) (compileExactQuote (budgets 10000 10000 10000 276) policy scalarOwner)
    reject "allocation total charge" (QuoteTotalWorkLimitExceeded 524 525) (compileExactQuote (budgets 10000 10000 10000 524) policy scalarOwner)
    let program = compose (parameter @"duplicate" SScalar SScalar) (parameter @"duplicate" SScalar SScalar)
        duplicate = quoteProgram program
        standalone = compileExactPolynomial (compilerLimits 1000 1000 1000 1000 1000 1000 1000 1000 1000000 256) policy program
    -- Two scalar-owner leaves: unchanged syntax traversal 4, planner entries
    -- 10, then bulk traversal. Fourteen admits planning but not reservation.
    reject "duplicate resource before semantics" (QuoteTraversalLimitExceeded 14 15) (compileExactQuote (budgets 14 100000 100000 100000) policy duplicate)
    case (standalone, compileExactQuote generous policy duplicate) of
        (Left expected, Left (QuoteCompileTargetFailure actual)) -> check "exact standalone duplicate error" actual expected
        (Left expected, Left other) -> error ("duplicate wrong wrapper: " ++ show other ++ "; expected " ++ show expected)
        _ -> error "duplicate owner did not fail in both real compilers"

-- Independent numeric source inventory for left-associated scalar owners.
-- This oracle intentionally enumerates nodes, unlike the fixed-size production
-- summaries. At join j: Lp=O=2j-1, C=j(j+1)/2.
ownerWork :: Int -> (Natural, Natural, Natural, Natural)
ownerWork count = (t, a, r, t + n + a + r)
  where
    k = fromIntegral count
    n = 2 * k - 1
    inventories = replicate count (1, 1, 1, 1) ++ [(2 * j - 1, 2 * j - 1, j, j * (j + 1) `quot` 2) | j <- [2 .. k]]
    mt (lp, o, _, _) = let b = lp + 2; u = o + lp + 3; z = o + b + (lp + 1) `quot` 2 + 2 in 7 * u + 2 * b + o + lp + 26 * (o + b) + 3 * b + 2 * z + 18 + 2 * b + 2 * lp + 42
    ot (_, o, j, c) = 2 * (o + c + j + j * (j - 1) `quot` 2)
    oa (_, o, j, c) = 2 * (o + c + j)
    -- Each left-spine join has U(left)=4(j-1)+1 and U(right)=5.
    joins = sum [4 * (j - 1) + 6 | j <- [2 .. k]]
    pathCells = sum [2 * j | j <- [1 .. k - 1]]
    tc = 6 * n + 1 + sum (map mt inventories) + sum (map ot inventories) + 2 * k * (k + 1) + k + pathCells + 3 * joins + 4 * n + k
    ac = 3 * n + k + 5 + 1 + sum (map (subtract 12 . mt) inventories) + sum (map oa inventories) + k * (k + 1) + k + pathCells + 3 * joins + 4 * n + k
    -- Old traversal q+N, planner q+N+three entries per parameter primitive.
    t = 1 + n + 1 + n + 3 * k + tc
    a = 6 * k - 2 + ac
    r = 8 * k - 1 + 14 * k - 2

data SomeOwners where
    SomeOwners :: Program Rational 'Polynomial p 'Scalar 'Scalar -> SomeOwners

singleOwner :: Int -> SomeOwners
singleOwner n = case someSymbolVal ("owner-" ++ show n) of
    SomeSymbol (_ :: Proxy owner) -> SomeOwners (parameter @owner SScalar SScalar)

leftOwners :: Int -> SomeOwners
leftOwners 1 = singleOwner 1
leftOwners n = case (leftOwners (n - 1), singleOwner n) of
    (SomeOwners l, SomeOwners r) -> SomeOwners (compose l r)

balancedOwners :: Int -> Int -> SomeOwners
balancedOwners start end
    | start == end = singleOwner start
    | otherwise = case (balancedOwners start middle, balancedOwners (middle + 1) end) of
        (SomeOwners l, SomeOwners r) -> SomeOwners (compose l r)
  where
    middle = (start + end) `quot` 2

lexicalAndFailureCoverage :: IO ()
lexicalAndFailureCoverage = do
    let lexical = withQuoteScope $ \outer -> withQuoteScope $ \inner ->
            let e0 = rootEnvironment SScalar
                e1 = extendEnvironment outer e0 SScalar
                root = project (pathLeft inner (pathLeft outer (pathHere SScalar) SScalar) SScalar)
                old = project (pathLeft inner (pathRight outer e0 SScalar) SScalar)
                newest = project (pathRight inner e1 SScalar)
             in letQuote
                    outer
                    (quoteProgram negateScalar)
                    ( letQuote
                        inner
                        (quoteProgramAt e1 (constantScalar (SProduct SScalar SScalar) 7))
                        (fanoutQuote root (fanoutQuote old newest))
                    )
        no = noParameters
        pp = parameterProduct
        parameters = pp (pp no no) (pp (pp no no) (pp no (pp no no)))
        square = quoteProgram (compose (fanout (identity SScalar) (identity SScalar)) multiplyScalar)
        squareParameters = pp (pp no no) no
        bits8 = quotationLimits 10000 100 100 100 100 100 100 100 10000 10000 100000 8
        (lexicalPrimal, lexicalTangent) = right (interpretExactQuoteJVP generous lexical parameters parameters (scalarValue 3) (scalarValue 5))
    check "equal-shaped lexical selections primal" (valueScalars lexicalPrimal) [3, -3, 7]
    check "equal-shaped lexical selections tangent" (valueScalars lexicalTangent) [5, -5, 0]
    check "JVP-only overflow preserves direct primal" (right (interpretExactQuote bits8 square squareParameters (scalarValue 8))) (scalarValue 64)
    case interpretExactQuoteJVP bits8 square squareParameters squareParameters (scalarValue 8) (scalarValue 16) of
        Left (QuoteRationalMagnitudeLimitExceeded "jvp/multiply-add" 0 8) -> pure ()
        other -> error ("missing JVP-only intermediate overflow: " ++ show other)
    forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        let (lexicalExe, _) = right (compileExactQuote generous policy lexical)
            lexicalRun = right (runExact lexicalExe parameters (scalarValue 3))
            (dp, di) = right (applyExactTape (exactRunTape lexicalRun) (productValue (scalarValue 2) (productValue (scalarValue 3) (scalarValue 5))))
            (squareExe, _) = right (compileExactQuote bits8 policy square)
            squareRun = right (runExact squareExe squareParameters (scalarValue 8))
        check "compiled lexical selections" (valueScalars (exactRunOutput lexicalRun)) [3, -3, 7]
        check "lexical gradient association" dp parameters
        check "lexical input cotangent" di (scalarValue (-1))
        check "compiled primal independent of JVP overflow" (exactRunOutput squareRun) (scalarValue 64)
        case applyExactTape (exactRunTape squareRun) (scalarValue 16) of
            Left e -> check "reverse overflow keeps actual backend context" ("RationalMagnitudeLimitExceeded \"cotangent-add\"" `isInfixOf` show e) True
            Right _ -> error "reverse overflow was admitted"
        let bound = compose (fanout (constantScalar SScalar 16) (constantScalar SScalar 16)) multiplyScalar
            unused = withQuoteScope $ \scope -> letQuote scope (quoteProgram bound) (project (pathLeft scope (pathHere SScalar) SScalar))
            unusedParameters = pp (pp no squareParameters) no
            (unusedExe, _) = right (compileExactQuote bits8 policy unused)
        case runExact unusedExe unusedParameters (scalarValue 1) of
            Left e -> check "unused bound compiled forward context" ("RationalMagnitudeLimitExceeded \"multiply\"" `isInfixOf` show e) True
            Right _ -> error "compiled unused bound skipped its failure"

data PrimitiveCase where
    PrimitiveCase :: String -> Program Rational 'Polynomial p i o -> ParameterValue Rational p -> Value Rational i -> [Rational] -> PrimitiveCase

-- These small scalar-polynomial outputs are handwritten, including represented
-- empty vectors. The full differential fixture supplies nonvacuous coordinates.
primitiveCoverage :: IO ()
primitiveCoverage = do
    let scalar = scalarValue
        v0 = right (vectorValue @0 0 [])
        v1 = right (vectorValue @1 1 [3])
        v2 = right (vectorValue @2 2 [3, 4])
        pair0 = productValue v0 v0
        pair1 = productValue v1 v1
        pair2 = productValue v2 v2
        p = parameter @"shared" SScalar SScalar
        sharedProgram = shareParameters p p
        cases =
            [ PrimitiveCase "scalar constant" (constantScalar SUnit 7) noParameters unitValue [7]
            , PrimitiveCase "negate" negateScalar noParameters (scalar 3) [-3]
            , PrimitiveCase "add" addScalar noParameters (productValue (scalar 3) (scalar 4)) [7]
            , PrimitiveCase "multiply" multiplyScalar noParameters (productValue (scalar 3) (scalar 4)) [12]
            , PrimitiveCase "constant vector0" (constantVector SUnit (SVector @0) v0) noParameters unitValue []
            , PrimitiveCase "constant vector1" (constantVector SUnit (SVector @1) v1) noParameters unitValue [3]
            , PrimitiveCase "constant vector2" (constantVector SUnit (SVector @2) v2) noParameters unitValue [3, 4]
            , PrimitiveCase "add vector0" (addVector (SVector @0)) noParameters pair0 []
            , PrimitiveCase "add vector1" (addVector (SVector @1)) noParameters pair1 [6]
            , PrimitiveCase "add vector2" (addVector (SVector @2)) noParameters pair2 [6, 8]
            , PrimitiveCase "Hadamard0" (hadamard (SVector @0)) noParameters pair0 []
            , PrimitiveCase "Hadamard1" (hadamard (SVector @1)) noParameters pair1 [9]
            , PrimitiveCase "Hadamard2" (hadamard (SVector @2)) noParameters pair2 [9, 16]
            , PrimitiveCase "dot0" (dot (SVector @0)) noParameters pair0 [0]
            , PrimitiveCase "dot1" (dot (SVector @1)) noParameters pair1 [9]
            , PrimitiveCase "dot2" (dot (SVector @2)) noParameters pair2 [25]
            , PrimitiveCase "sum0" (sumVector (SVector @0)) noParameters v0 [0]
            , PrimitiveCase "sum1" (sumVector (SVector @1)) noParameters v1 [3]
            , PrimitiveCase "sum2" (sumVector (SVector @2)) noParameters v2 [7]
            , PrimitiveCase "first" (first (SVector @2) SScalar) noParameters (productValue v2 (scalar 7)) [3, 4]
            , PrimitiveCase "second" (second SScalar (SVector @2)) noParameters (productValue (scalar 7) v2) [3, 4]
            , PrimitiveCase "shared left endpoint" sharedProgram (ownedParameters (scalar 7)) (productValue (scalar 3) (scalar 4)) [7, 7]
            , PrimitiveCase "parallel" (parallel negateScalar (sumVector (SVector @2))) (parameterProduct noParameters noParameters) (productValue (scalar 3) v2) [-3, 7]
            , PrimitiveCase "zero-coordinate product identity" (identity (SProduct SUnit (SProduct (SVector @0) SUnit))) noParameters (productValue unitValue (productValue v0 unitValue)) []
            ]
    forM_ cases $ \(PrimitiveCase name program parameters input expected) -> forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        let q = quoteProgram program
            (exe, report) = right (compileExactQuote generous policy q)
            standalone = right (compileExactPolynomial (compilerLimits 1000 1000 1000 1000 1000 1000 1000 1000 1000000 256) policy program)
        check (name ++ " direct primal") (valueScalars (right (interpretExactQuote generous q parameters input))) expected
        check (name ++ " compiled primal") (valueScalars (exactRunOutput (right (runExact exe parameters input)))) expected
        check (name ++ " target report") (quoteCompilationTarget report) (exactCompileReport standalone)
        let (_, other) = right (compileExactQuote generous (if policy == StorePullbacks then RecomputePullbacks else StorePullbacks) q)
        check (name ++ " policy reservation") (quoteCompilationPreflight report) (quoteCompilationPreflight other)

-- Small independent explicit inventories (N/P/G/Tp/Tc/Ac/F/V):
-- projection 1/1/4/3/258/247/3/3; identity 1/0/1/3/249/239/2/2;
-- square 5/1/1/9/2027/1956/11/13; lets 5/2/16/9/2052/1978/11/11;
-- embedded projection 1/1/1/9/455/444/5/5. No production planner helper is used.
smallBudgets :: IO ()
smallBudgets = do
    exactCase "projection" (project (pathHere SScalar)) (263, 249, 9, 523)
    exactCase "identity" (quoteProgram (identity SScalar)) (254, 241, 6, 502)
    -- Both primitive branches contribute, but the shared root retains one
    -- owner: MT=900, OT=OA=18, J=10, EQp=2; Tc/Ac=997/953.
    let sharedOwner = parameter @"shared-budget" SScalar SScalar
    exactCase "shared parameter endpoint" (quoteProgram (shareParameters sharedOwner sharedOwner)) (1011, 964, 41, 2019)
    exactCase "square" (quoteProgram (compose (fanout (identity SScalar) (identity SScalar)) multiplyScalar)) (2042, 1969, 41, 4057)
    let used = withQuoteScope $ \scope -> letQuote scope (project (pathHere SScalar)) (project (pathRight scope (rootEnvironment SScalar) SScalar))
        unused = withQuoteScope $ \scope -> letQuote scope (project (pathHere SScalar)) (project (pathLeft scope (pathHere SScalar) SScalar))
        embedded = withQuoteScope $ \outer -> withQuoteScope $ \inner ->
            lowerQuote generous (project (pathLeft inner (pathRight outer (rootEnvironment SScalar) SScalar) SScalar))
    exactCase "used let" used (2066, 1989, 34, 4099)
    exactCase "unused let" unused (2067, 1989, 34, 4100)
    exactCase "requoted nested ProjectionLeft/Right" (quoteProgram (right embedded)) (466, 448, 15, 930)
  where
    exactCase :: String -> Quote Rational 'Polynomial e p o -> (Natural, Natural, Natural, Natural) -> IO ()
    exactCase name q expected@(t, a, r, w) = forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        let (_, report) = right (compileExactQuote (budgets t a r w) policy q)
        check (name ++ " exact report") (work (quoteCompilationPreflight report)) expected
        reject (name ++ " traversal") (QuoteTraversalLimitExceeded (t - 1) t) (compileExactQuote (budgets (t - 1) 100000 100000 100000) policy q)
        reject (name ++ " allocation") (QuoteAllocationLimitExceeded (a - 1) a) (compileExactQuote (budgets 100000 (a - 1) 100000 100000) policy q)
        reject (name ++ " runtime") (QuoteRuntimeWorkLimitExceeded (r - 1) r) (compileExactQuote (budgets 100000 100000 (r - 1) 100000) policy q)
        reject (name ++ " total") (QuoteTotalWorkLimitExceeded (w - 1) w) (compileExactQuote (budgets 100000 100000 100000 (w - 1)) policy q)

run :: IO ()
run = do
    reservationFailureStages
    smallBudgets
    productOwnerBudgets
    forM_ [1, 2, 3, 128] $ \k -> case leftOwners k of
        SomeOwners program -> do
            let q = quoteProgram program
                (_, report) = right (compileExactQuote generous StorePullbacks q)
            check "left-associated owner cumulative bookkeeping" (work (quoteCompilationPreflight report)) (ownerWork k)
            let (t, a, r, w) = ownerWork k
            case compileExactQuote (budgets t a r w) RecomputePullbacks q of
                Left e -> error (show e)
                Right (_, other) -> check "owner policy account" (quoteCompilationPreflight other) (quoteCompilationPreflight report)
    case balancedOwners 1 4 of
        SomeOwners program -> forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
            -- C(root)=3+3+2=8 (not the left-associated value 10), J=38,
            -- path cells=10; Tc/Ac=2839/2707 from the independent inventory.
            let (_, report) = right (compileExactQuote (budgets 2867 2729 85 5688) policy (quoteProgram program))
            check "balanced owners exact association" (work (quoteCompilationPreflight report)) (2867, 2729, 85, 5688)
    -- A literal-free executable must retain the caller's allowance, not zero
    -- observed source bits; both input and reverse seed are nonzero.
    forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        let q = quoteProgram (identity SScalar)
            bits8 = quotationLimits 10000 100 100 100 100 100 100 100 10000 10000 100000 8
            (exe, report) = right (compileExactQuote bits8 policy q)
            execution = right (runExact exe noParameters (scalarValue 13))
        check "literal-free observed bits" (quoteMaximumRationalBits (quoteCompilationPreflight report)) 0
        check "caller bits input" (exactRunOutput execution) (scalarValue 13)
        check "caller bits seed" (right (applyExactTape (exactRunTape execution) (scalarValue 1))) (noParameters, scalarValue 1)
    -- Each source owner extent is one; the aggregate parameter tree is two.
    let aggregate = quoteProgram (compose (parameter @"left" SScalar SScalar) (parameter @"right" SScalar SScalar))
        extent1 = quotationLimits 100000 100 100 100 100 100 1 100 100000 100000 1000000 256
    case preflightQuote extent1 aggregate of { Left e -> error (show e); Right _ -> pure () }
    case lowerQuote extent1 aggregate of { Left e -> error (show e); Right _ -> pure () }
    reject "aggregate parameter extent" (QuoteCoordinateExtentLimitExceeded 1 2) (compileExactQuote extent1 StorePullbacks aggregate)
    -- Neither huge vector is materialized: the old pass admits each n, and
    -- compilation rejects aggregate parameter extent 2n before any builder.
    let n = fromIntegral (maxBound :: Int) `quot` 2 + 1
        bulk = 100 * n * n
        machineLimits = quotationLimits bulk 100 100 100 100 100 n 100 bulk bulk bulk 256
    case someNatVal n of
        SomeNat (_ :: Proxy length) -> do
            let vector = SVector @length
                machineAggregate = quoteProgram (compose (parameter @"huge-left" SUnit vector) (parameter @"huge-right" vector vector))
            case preflightQuote machineLimits machineAggregate of { Left e -> error (show e); Right _ -> pure () }
            reject "aggregate machine before coordinate" (QuoteMachineExtentExceeded (2 * n)) (compileExactQuote machineLimits StorePullbacks machineAggregate)
    primitiveCoverage
    lexicalAndFailureCoverage
    putStrLn "PASS: D080 product-owner exact/one-below/EQp, cumulative owners, caller bits and aggregate extents"
