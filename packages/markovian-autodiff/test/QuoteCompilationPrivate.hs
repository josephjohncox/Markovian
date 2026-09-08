{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless, void)
import D080Probe (readEvents, resetEvents)
import Data.List (isPrefixOf, isSubsequenceOf)
import Markovian.Autodiff
import Markovian.Autodiff.Internal.Syntax (Primitive (Parameter), Program (PrimitiveNode))
import Numeric.Natural (Natural)
import QuoteCompilationBudgets qualified

check :: String -> Bool -> IO ()
check name condition = unless condition (error name)

limits :: Natural -> Natural -> Natural -> Natural -> QuotationLimits
limits t a r w = quotationLimits t 1000 1000 1000 1000 1000 1000 1000 a r w 256

generous :: QuotationLimits
generous = limits 1000000 1000000 1000000 1000000

forceReport :: Either QuoteCompileError (ExactExecutable p i o, QuoteCompilationReport) -> IO ()
forceReport result = case result of
    Left e -> void (evaluate (length (show e)))
    Right (exe, report) -> do
        _ <- evaluate (length (show (quoteCompilationPreflight report)))
        _ <- evaluate (length (renderCompileReport (exactCompileReport exe)))
        _ <- evaluate (length (renderCompileReport (quoteCompilationTarget report)))
        pure ()

noTarget :: [String] -> IO ()
noTarget events = forM_ ["builder-call", "buildQuote", "buildPath", "pathProjection", "generated-program", "generated-projection-primitive", "projection-witness", "compiler-call", "compileExactPolynomial", "lower", "target-primitive", "prepareReverseProgram", "prepared-node", "prepared-form", "prepared-program", "primal-space", "cotangent-space", "zero-value", "zero-parameter", "exact-executable", "runtime-value", "runtime-product", "reverse-tape"] $ \name ->
    check ("pre-build failure entered " ++ name ++ ": " ++ show events) (name `notElem` events)

data SomeProduct where
    SomeProduct :: SShape s -> SomeProduct

productShape :: Int -> SomeProduct
productShape 0 = SomeProduct SScalar
productShape depth = case productShape (depth - 1) of SomeProduct shape -> SomeProduct (SProduct shape shape)

productAdmissionEvents :: IO ()
productAdmissionEvents = forM_ [(0, 266, 252, 16, 535, 1), (1, 448, 435, 29, 913, 3), (2, 812, 801, 55, 1669, 7), (7, 23380, 23493, 1667, 48541, 255)] $ \(depth, t, a, r, w, h) -> case productShape depth of
    SomeProduct shape -> do
        let q = quoteProgram (parameter @"product-owner" SUnit shape)
        _ <- evaluate (preflightQuote generous q)
        forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
            resetEvents
            let success = compileExactQuote (limits t a r w) policy q
            forceReport success
            case success of
                Left e -> error (show e)
                Right (_, report) -> do
                    let complete = quoteCompilationPreflight report
                    check "private exact product work" ((quoteTraversalWork complete, quoteAllocationCount complete, quoteRuntimeWork complete, quoteTotalWork complete) == (t, a, r, w))
            positive <- readEvents
            forM_ ["admission-witness", "builder-call", "compiler-call", "prepared-node", "exact-executable"] $ \name -> check ("product positive missing " ++ name) (name `elem` positive)
            check "product compile tape" ("reverse-tape" `notElem` positive)
            let planned = ["syntax-start", "syntax-success", "planner-start", "planner-success"]
                traversed = planned ++ ["reserve-traversal"]
                allocated = traversed ++ ["reserve-allocation"]
            check "product positive admission order" ((allocated ++ ["reserve-runtime", "admission-witness", "builder-call", "compiler-call"]) `isSubsequenceOf` positive)
            forM_
                [ (limits (t - 1) 1000000 1000000 1000000, QuoteTraversalLimitExceeded (t - 1) t, planned)
                , (limits 1000000 (a - 1) 1000000 1000000, QuoteAllocationLimitExceeded (a - 1) a, traversed)
                , (limits 1000000 1000000 (r - 1) 1000000, QuoteRuntimeWorkLimitExceeded (r - 1) r, allocated)
                , (limits 1000000 1000000 1000000 (w - 1), QuoteTotalWorkLimitExceeded (w - 1) w, allocated)
                , (limits (t - h) 1000000 1000000 1000000, QuoteTraversalLimitExceeded (t - h) (t - h + 1), planned)
                , (limits 1000000 (a - h) 1000000 1000000, QuoteAllocationLimitExceeded (a - h) (a - h + 1), traversed)
                ]
                $ \(active, expected, phases) -> do
                    resetEvents
                    let failed = compileExactQuote active policy q
                    forceReport failed
                    case failed of
                        Left (QuoteCompilePreflightFailure actual) -> check "product negative exact payload" (actual == expected)
                        _ -> error "product negative unexpectedly admitted"
                    events <- readEvents
                    noTarget events
                    check "product negative phase prefix" (filter (not . isPrefixOf "plan-shape/") events == phases)

-- The identity endpoints are Unit/environment/environment, already admitted
-- by bound completion. Fanout parameters have the bound's extent (Unit adds
-- zero), its input is the same environment, and walkQuote's LetQuote already
-- checked Product environment boundOutput. Thus neither synthetic completion
-- can introduce a new extent failure for finite well-typed admitted syntax.
-- Observe real completions instead of manufacturing unreachable failures.
letPlanningOrder :: IO ()
letPlanningOrder = do
    let quotation = withQuoteScope $ \scope ->
            let environment = rootEnvironment SScalar
                extended = extendEnvironment scope environment SScalar
             in letQuote scope (quoteProgram (identity SScalar)) (quoteProgramAt extended (identity (SProduct SScalar SScalar)))
        complete = ["let-bound-start", "let-bound-complete", "let-identity-complete", "let-fanout-complete", "let-body-start", "let-body-complete"]
    syntax <- evaluate (either (error . show) id (preflightQuote generous quotation))
    check "let independent syntax traversal" (quoteTraversalWork syntax == 5)
    forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        -- Five old visits. Planner visits: let, bound quote, bound program,
        -- scalar shape. Synthetic completions add no visits; body starts next.
        forM_ [(8, ["let-bound-start"]), (9, take 5 complete)] $ \(credit, expected) -> do
            resetEvents
            let result = compileExactQuote (limits credit 100000 100000 100000) policy quotation
            forceReport result
            case result of
                Left (QuoteCompilePreflightFailure actual) -> check "let planner saturated failure" (actual == QuoteTraversalLimitExceeded credit (credit + 1))
                _ -> error "let planner failure wrong stage"
            events <- readEvents
            noTarget events
            check ("let failure completion prefix: " ++ show events) (filter (isPrefixOf "let-") events == expected)
            check "let failure admission prefix" (filter (\event -> not (isPrefixOf "let-" event || isPrefixOf "plan-shape/" event)) events == ["syntax-start", "syntax-success", "planner-start"])
        resetEvents
        let result = compileExactQuote generous policy quotation
        forceReport result
        case result of { Left e -> error (show e); Right _ -> pure () }
        events <- readEvents
        check ("let completion order: " ++ show events) (filter (isPrefixOf "let-") events == complete)
        check "let bound shape before body shape" (filter (isPrefixOf "plan-shape/") events == ["plan-shape/scalar", "plan-shape/product", "plan-shape/scalar", "plan-shape/scalar"])

main :: IO ()
main = do
    -- Literal independent budget tests execute the production pipeline, not a
    -- copied preflight. Their negative controls detect omitted/reset charges.
    QuoteCompilationBudgets.run
    productAdmissionEvents
    letPlanningOrder
    let q = quoteProgram (parameter @"product-owner" SUnit SScalar)
    _ <- evaluate (preflightQuote generous q)
    forM_
        [ ("syntax", limits 0 10000 10000 10000, QuoteTraversalLimitExceeded 0 1, ["syntax-start"])
        , ("planner", limits 2 10000 10000 10000, QuoteTraversalLimitExceeded 2 3, ["syntax-start", "syntax-success", "planner-start"])
        , ("traversal", limits 265 10000 10000 10000, QuoteTraversalLimitExceeded 265 266, ["syntax-start", "syntax-success", "planner-start", "planner-success"])
        , ("traversal-total", limits 10000 10000 10000 276, QuoteTotalWorkLimitExceeded 276 277, ["syntax-start", "syntax-success", "planner-start", "planner-success"])
        , ("allocation-total", limits 10000 10000 10000 524, QuoteTotalWorkLimitExceeded 524 525, ["syntax-start", "syntax-success", "planner-start", "planner-success", "reserve-traversal"])
        , ("allocation", limits 10000 251 10000 10000, QuoteAllocationLimitExceeded 251 252, ["syntax-start", "syntax-success", "planner-start", "planner-success", "reserve-traversal"])
        , ("runtime", limits 10000 10000 15 10000, QuoteRuntimeWorkLimitExceeded 15 16, ["syntax-start", "syntax-success", "planner-start", "planner-success", "reserve-traversal", "reserve-allocation"])
        , ("total", limits 10000 10000 10000 534, QuoteTotalWorkLimitExceeded 534 535, ["syntax-start", "syntax-success", "planner-start", "planner-success", "reserve-traversal", "reserve-allocation"])
        ]
        $ \(name, active, expected, phases) -> forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
            resetEvents
            let result = compileExactQuote active policy q
            forceReport result
            case result of
                Left (QuoteCompilePreflightFailure actual) -> check (name ++ " error") (actual == expected)
                _ -> error (name ++ " did not fail in preflight")
            events <- readEvents
            noTarget events
            check (name ++ " phase order: " ++ show events) (filter (not . isPrefixOf "plan-shape/") events == phases)
    -- Both separate demands fit 264, their cumulative traversal 266 does not.
    let resetDiscriminator = limits 264 10000 10000 10000
    _ <- evaluate (either (error . show) id (preflightQuote resetDiscriminator q))
    _ <- evaluate (either (error . show) id (lowerQuote resetDiscriminator q))
    resetEvents
    forceReport (compileExactQuote resetDiscriminator StorePullbacks q)
    readEvents >>= noTarget

    -- Poisoned metadata that the frozen schedule explicitly does not inspect.
    -- The redundant ProgramQuote environment is skipped even on success.
    let redundantEnvironment :: Quote Rational 'Polynomial (RootEnvironment 'Scalar) 'NoParameters 'Scalar
        redundantEnvironment = quoteProgramAt (error "redundant environment inspected") (identity SScalar)
    resetEvents
    forceReport (compileExactQuote generous StorePullbacks redundantEnvironment)
    -- Owner identity is atomic during planning; a later target resolver would
    -- demand this poison. A planner child-entry failure must stop before that.
    let poisonOwner :: Program Rational 'Polynomial (Owner "poison" 'Scalar) 'Scalar 'Scalar
        poisonOwner = PrimitiveNode (Parameter (error "later owner identity demanded") SScalar SScalar)
        poisonQuotation = quoteProgram (compose (identity SScalar) poisonOwner)
    _ <- evaluate (preflightQuote generous poisonQuotation)
    resetEvents
    let poisonResult = compileExactQuote (limits 8 10000 10000 10000) StorePullbacks poisonQuotation
    forceReport poisonResult
    case poisonResult of
        Left (QuoteCompilePreflightFailure (QuoteTraversalLimitExceeded 8 9)) -> pure ()
        _ -> error "planner did not stop before later child"
    readEvents >>= noTarget

    -- Requote an actual lowered nested path: ProjectionRight must visit its
    -- stored vector-left field before its scalar inner projection. ProjectionLeft
    -- then visits the trailing vector. This is not a replica planner.
    let nested = withQuoteScope $ \outer -> withQuoteScope $ \inner ->
            lowerQuote generous (project (pathLeft inner (pathRight outer (rootEnvironment (SVector @2)) SScalar) (SVector @1)))
        embedded = quoteProgram (either (error . show) id nested)
    _ <- evaluate (preflightQuote generous embedded)
    resetEvents
    forceReport (compileExactQuote generous StorePullbacks embedded)
    projectionEvents <- filter (isPrefixOf "plan-shape/") <$> readEvents
    check ("stored Projection field order: " ++ show projectionEvents) (projectionEvents == ["plan-shape/vector", "plan-shape/scalar", "plan-shape/vector"])

    -- Resources win on the identical duplicate syntax; only admitted failures
    -- may reach preparation. Compare opaque errors by Eq, not a wrapper wildcard.
    let duplicateProgram = compose (parameter @"duplicate" SScalar SScalar) (parameter @"duplicate" SScalar SScalar)
        duplicate = quoteProgram duplicateProgram
    _ <- evaluate (preflightQuote generous duplicate)
    forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        expected <- case compileExactPolynomial (compilerLimits 1000 1000 1000 1000 1000 1000 1000 1000 1000000 256) policy duplicateProgram of
            Left e -> evaluate (length (show e)) >> pure e
            Right _ -> error "standalone duplicate unexpectedly admitted"
        resetEvents
        let insufficient = compileExactQuote (limits 14 100000 100000 100000) policy duplicate
        forceReport insufficient
        case insufficient of
            Left (QuoteCompilePreflightFailure (QuoteTraversalLimitExceeded 14 15)) -> pure ()
            _ -> error "duplicate resource failure wrong stage or payload"
        insufficientEvents <- readEvents
        noTarget insufficientEvents
        check "duplicate insufficient phase prefix" (filter (not . isPrefixOf "plan-shape/") insufficientEvents == ["syntax-start", "syntax-success", "planner-start", "planner-success"])
        resetEvents
        let failed = compileExactQuote generous policy duplicate
        forceReport failed
        case failed of
            Left (QuoteCompileTargetFailure actual) -> check "exact underlying duplicate error" (actual == expected)
            _ -> error "duplicate-owner target failure not forwarded"
        failedEvents <- readEvents
        forM_ ["admission-witness", "builder-call", "compiler-call", "prepareReverseProgram", "prepared-node"] $ \name -> check ("missing duplicate event " ++ name) (name `elem` failedEvents)
        check "duplicate failure constructed executable/tape" ("exact-executable" `notElem` failedEvents && "reverse-tape" `notElem` failedEvents)

    forM_ [StorePullbacks, RecomputePullbacks] $ \policy -> do
        -- Let plus old/new selections exercises generated source, environment
        -- and projection witnesses independently of evaluated runtime products.
        let quotation = withQuoteScope $ \scope ->
                let env = rootEnvironment SScalar
                    old = project (pathLeft scope (pathHere SScalar) SScalar)
                    newest = project (pathRight scope env SScalar)
                 in letQuote scope (project (pathHere SScalar)) (fanoutQuote old newest)
            parameters = parameterProduct (parameterProduct noParameters noParameters) (parameterProduct noParameters noParameters)
        _ <- evaluate (preflightQuote generous quotation)
        resetEvents
        let result = compileExactQuote generous policy quotation
        forceReport result
        events <- readEvents
        check "ordered admission before builders" (["syntax-start", "syntax-success", "planner-start", "planner-success", "reserve-traversal", "reserve-allocation", "reserve-runtime", "admission-witness", "builder-call", "compiler-call"] `isSubsequenceOf` events)
        forM_ ["buildQuote", "buildPath", "pathProjection", "generated-program", "generated-projection-primitive", "projection-witness", "environment-witness", "shape-witness", "compileExactPolynomial", "lower", "target-primitive", "prepareReverseProgram", "prepared-node", "prepared-form", "prepared-program", "primal-space", "cotangent-space", "zero-value", "zero-parameter", "exact-executable"] $ \name -> check ("positive compile missing event " ++ name ++ ": " ++ show events) (name `elem` events)
        check "compile constructed evaluated values/tape" (all (`notElem` events) ["runtime-value", "runtime-product", "reverse-tape"])
        let exe = either (error . show) fst result
        resetEvents
        let execution = either (error . show) id (runExact exe parameters (scalarValue 13))
        _ <- evaluate (sum (valueScalars (exactRunOutput execution)))
        _ <- evaluate (either (error . show) (sum . valueScalars . snd) (applyExactTape (exactRunTape execution) (productValue (scalarValue 1) (scalarValue 2))))
        runtimeEvents <- readEvents
        forM_ ["runtime-value", "runtime-product", "reverse-tape"] $ \name -> check ("positive run missing " ++ name ++ ": " ++ show runtimeEvents) (name `elem` runtimeEvents)
    putStrLn "PASS: private production quotation admission, logical constructor entries and separate runtime tape phase"
