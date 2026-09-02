{-# LANGUAGE GADTs #-}

module Effect (tests) where

import Control.Monad (unless)
import Markovian.Reverse
import Markovian.Reverse.Program
import Markovian.Reverse.Program.Effect

data FixtureError
    = ForwardFailure
    | RecomputationFailure
    | PullbackFailure
    | AdditionFailure
    deriving (Eq, Show)

data Fault = NoFault | FailForward | FailRecomputation | FailPullback | FailAddition
    deriving (Eq, Show)

data RationalPrimitive p pc x xc y yc where
    RationalScale :: String -> PrimitiveTapePolicy -> Fault -> RationalPrimitive Rational Rational Rational Rational Rational Rational

data DoublePrimitive p pc x xc y yc where
    DoubleScale :: String -> PrimitiveTapePolicy -> Fault -> DoublePrimitive Double Double Double Double Double Double

tests :: IO ()
tests = do
    exactStoredAndRecomputed
    doubleDifferential
    failureTests

exactStoredAndRecomputed :: IO ()
exactStoredAndRecomputed = do
    let storedSyntax = composeProgram (primitiveProgram (RationalScale "first" StoreCapturedPullback NoFault)) (primitiveProgram (RationalScale "second" StoreCapturedPullback NoFault))
    stored <- prepareRational "stored" storedSyntax
    recomputed <- prepareRational "recomputed" (composeProgram (primitiveProgram (RationalScale "first" RecomputePrimitive NoFault)) (primitiveProgram (RationalScale "second" RecomputePrimitive NoFault)))
    storedRun <- rightIO "stored forward" (runPreparedReverseM stored (2, 3) 4)
    recomputedRun <- rightIO "recomputed forward" (runPreparedReverseM recomputed (2, 3) 4)
    assertEqual "exact stored output" 24 (effectReverseRunOutput storedRun)
    assertEqual "exact recomputed output" 24 (effectReverseRunOutput recomputedRun)
    firstStored <- rightIO "stored pullback" (applyReverseTapeM (effectReverseRunTape storedRun) 1)
    secondStored <- rightIO "repeated stored pullback" (applyReverseTapeM (effectReverseRunTape storedRun) 1)
    firstRecomputed <- rightIO "recomputed pullback" (applyReverseTapeM (effectReverseRunTape recomputedRun) 1)
    secondRecomputed <- rightIO "repeated recomputed pullback" (applyReverseTapeM (effectReverseRunTape recomputedRun) 1)
    assertEqual "exact stored VJP" ((12, 8), 6, firstStoredReport) firstStored
    assertEqual "stored tape repeat" firstStored secondStored
    purePrepared <- requireRight "Identity specialization preparation" (prepareReverseProgram limits resolvePureRational storedSyntax)
    pureRun <- requireRight "Identity specialization forward" (runPreparedReverse purePrepared (2, 3) 4)
    purePullback <- requireRight "Identity specialization pullback" (applyReverseTape (reverseRunTape pureRun) 1)
    assertEqual "pure/effect Identity forward differential" (effectReverseRunOutput storedRun) (reverseRunOutput pureRun)
    assertEqual "pure/effect Identity pullback differential" ((12, 8), 6) purePullback
    assertEqual "exact recomputed VJP" ((12, 8), 6, recomputedReport) firstRecomputed
    assertEqual "recomputed tape repeat" firstRecomputed secondRecomputed
    assertEqual
        "deterministic forward report"
        (unlines ["effect-reverse-execution-report", "forward-calls: 2", "recomputation-calls: 0", "pullback-calls: 0", "cotangent-addition-calls: 0"])
        (renderEffectReverseExecutionReport (runReport storedRun))
  where
    firstStoredReport = EffectReverseExecutionReport 0 0 2 0
    recomputedReport = EffectReverseExecutionReport 0 2 2 0

doubleDifferential :: IO ()
doubleDifferential = do
    prepared <- prepareDouble (composeProgram (primitiveProgram (DoubleScale "first" StoreCapturedPullback NoFault)) (primitiveProgram (DoubleScale "second" StoreCapturedPullback NoFault)))
    let parameters = (1.7, -0.8)
        input = 0.6
    run <- rightIO "double forward" (runPreparedReverseM prepared parameters input)
    ((firstGradient, secondGradient), inputGradient, _) <- rightIO "double pullback" (applyReverseTapeM (effectReverseRunTape run) 1)
    assertApprox "double primal" (uncurry (*) parameters * input) (effectReverseRunOutput run)
    assertApprox "double first parameter finite difference" (central (\value -> objective (value, snd parameters) input) (fst parameters)) firstGradient
    assertApprox "double second parameter finite difference" (central (\value -> objective (fst parameters, value) input) (snd parameters)) secondGradient
    assertApprox "double input finite difference" (central (objective parameters) input) inputGradient
  where
    objective (first, second) input = second * (first * input)

failureTests :: IO ()
failureTests = do
    forward <- prepareRational "forward failure" (primitiveProgram (RationalScale "scale" StoreCapturedPullback FailForward))
    expectExecutionFailure "forward failure" (ReversePrimitiveForwardFailure [] ForwardFailure) (EffectReverseExecutionReport 1 0 0 0) =<< runPreparedReverseM forward 2 4

    pullback <- prepareRational "pullback failure" (primitiveProgram (RationalScale "scale" StoreCapturedPullback FailPullback))
    pullbackRun <- rightIO "pullback-failure forward" (runPreparedReverseM pullback 2 4)
    expectExecutionFailure "pullback failure" (ReversePrimitivePullbackFailure [] PullbackFailure) (EffectReverseExecutionReport 0 0 1 0) =<< applyReverseTapeM (effectReverseRunTape pullbackRun) 1

    recomputation <- prepareRational "recomputation failure" (primitiveProgram (RationalScale "scale" RecomputePrimitive FailRecomputation))
    recomputationRun <- rightIO "recomputation-failure forward" (runPreparedReverseM recomputation 2 4)
    expectExecutionFailure "recomputation failure" (ReversePrimitiveRecomputationFailure [] RecomputationFailure) (EffectReverseExecutionReport 0 1 0 0) =<< applyReverseTapeM (effectReverseRunTape recomputationRun) 1

    addition <- prepareRational "addition failure" (pairInputProgram (primitiveProgram (RationalScale "left" StoreCapturedPullback FailAddition)) (primitiveProgram (RationalScale "right" StoreCapturedPullback NoFault)))
    additionRun <- rightIO "addition-failure forward" (runPreparedReverseM addition (2, 3) 4)
    expectExecutionFailure "addition failure" (ReverseCotangentAdditionFailure [] InputCotangentStage AdditionFailure) (EffectReverseExecutionReport 0 0 2 1) =<< applyReverseTapeM (effectReverseRunTape additionRun) (1, 1)

prepareRational :: String -> ReverseProgram RationalPrimitive FixtureError Rational p pc x xc y yc -> IO (PreparedEffectReverseProgram IO RationalPrimitive FixtureError Rational p pc x xc y yc)
prepareRational label = requireRight label . prepareEffectReverseProgram limits resolveRational

prepareDouble :: ReverseProgram DoublePrimitive FixtureError Double p pc x xc y yc -> IO (PreparedEffectReverseProgram IO DoublePrimitive FixtureError Double p pc x xc y yc)
prepareDouble = requireRight "double preparation" . prepareEffectReverseProgram limits resolveDouble

limits :: ReverseLimits
limits = reverseLimits 16 8 8 8 8 8

resolveRational :: EffectReversePrimitiveResolver IO RationalPrimitive FixtureError Rational
resolveRational (RationalScale owner policy fault) = makePrimitive owner rationalPrimal (==) ExactCotangentEquality policy fault

resolvePureRational :: ReversePrimitiveResolver RationalPrimitive FixtureError Rational
resolvePureRational (RationalScale ownerKey policy fault) = do
    layout <- maybe (Left EmptyParameterOwner) Right (finiteLayout "scalar" 1)
    ownership <- parameterOwner ownerKey layout
    let cotangent = case declaredCotangentSpace "scalar" layout (const (Right ())) 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality of
            Nothing -> error "valid pure scalar cotangent declaration was rejected"
            Just space -> space
        circuit = primitiveReverseCircuit cotangent cotangent cotangent (forward fault)
        forward FailForward _ _ = Left ForwardFailure
        forward selected parameter input = Right (reverseEvaluation (parameter * input) (pullback selected parameter input))
        recompute FailRecomputation _ _ = Left RecomputationFailure
        recompute selected parameter input = Right (reverseEvaluation (parameter * input) (pullback selected parameter input))
        pullback FailPullback _ _ _ = Left PullbackFailure
        pullback _ parameter input seed = Right (seed * input, seed * parameter)
    case policy of
        StoreCapturedPullback -> ownedReversePrimitive "scale" "1" ownership rationalPrimal rationalPrimal rationalPrimal circuit StoreCapturedPullback
        RecomputePrimitive -> ownedReversePrimitiveWithRecomputation "scale" "1" ownership rationalPrimal rationalPrimal rationalPrimal circuit (primitiveRecomputation (recompute fault))

resolveDouble :: EffectReversePrimitiveResolver IO DoublePrimitive FixtureError Double
resolveDouble (DoubleScale owner policy fault) = makePrimitive owner doublePrimal doubleEquivalent doubleEquality policy fault

makePrimitive :: (Num scalar) => String -> FinitePrimalSpace FixtureError scalar -> (scalar -> scalar -> Bool) -> CotangentEqualityMode -> PrimitiveTapePolicy -> Fault -> Either ReverseDefinitionError (EffectOwnedReversePrimitive IO FixtureError scalar scalar scalar scalar scalar scalar scalar)
makePrimitive ownerKey primal equivalent mode policy fault = do
    let cotangent = scalarCotangent equivalent mode fault
    layout <- maybe (Left EmptyParameterOwner) Right (finiteLayout "scalar" 1)
    ownership <- parameterOwner ownerKey layout
    case policy of
        StoreCapturedPullback -> effectOwnedReversePrimitive "scale" "1" ownership primal cotangent primal cotangent primal cotangent (forward fault)
        RecomputePrimitive -> effectOwnedReversePrimitiveWithRecomputation "scale" "1" ownership primal cotangent primal cotangent primal cotangent (forward fault) (effectPrimitiveRecomputation (recompute fault))
  where
    forward FailForward _ _ = pure (Left ForwardFailure)
    forward selected parameter input = pure (Right (effectReverseEvaluation (parameter * input) (pullback selected parameter input)))
    recompute FailRecomputation _ _ = pure (Left RecomputationFailure)
    recompute selected parameter input = pure (Right (effectReverseEvaluation (parameter * input) (pullback selected parameter input)))
    pullback FailPullback _ _ _ = pure (Left PullbackFailure)
    pullback _ parameter input seed = pure (Right (seed * input, seed * parameter))

rationalPrimal :: FinitePrimalSpace FixtureError Rational
rationalPrimal = finitePrimalSpace scalarLayout (const (Right ())) (==) ExactCotangentEquality

doublePrimal :: FinitePrimalSpace FixtureError Double
doublePrimal = finitePrimalSpace scalarLayout (const (Right ())) doubleEquivalent doubleEquality

doubleEquality :: CotangentEqualityMode
doubleEquality = ApproximateCotangentEquality "abs<=2e-9 plus rel<=2e-7"

doubleEquivalent :: Double -> Double -> Bool
doubleEquivalent expected actual =
    not (isNaN expected || isInfinite expected || isNaN actual || isInfinite actual)
        && abs (expected - actual) <= 2e-9 + 2e-7 * max (abs expected) (abs actual)

scalarCotangent :: (Num scalar) => (scalar -> scalar -> Bool) -> CotangentEqualityMode -> Fault -> EffectCotangentSpace IO FixtureError scalar scalar
scalarCotangent equivalent mode fault = effectCotangentSpace pureSpace addEffect
  where
    pureSpace = case declaredCotangentSpace "scalar" scalarLayout (const (Right ())) 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) equivalent mode of
        Nothing -> error "valid scalar cotangent declaration was rejected"
        Just space -> space
    addEffect _ _ | fault == FailAddition = pure (Left AdditionFailure)
    addEffect left right = pure (Right (left + right))

scalarLayout :: FiniteLayout
scalarLayout = case finiteLayout "scalar" 1 of
    Nothing -> error "valid scalar layout was rejected"
    Just layout -> layout

runReport :: EffectReverseRun m error scalar p pc x xc y yc -> EffectReverseExecutionReport
runReport = effectReverseRunReport

central :: (Double -> Double) -> Double -> Double
central function coordinate =
    let step = 1e-6 * max 1 (abs coordinate)
     in (function (coordinate + step) - function (coordinate - step)) / (2 * step)

expectExecutionFailure :: String -> ReverseProgramError FixtureError -> EffectReverseExecutionReport -> Either (EffectReverseExecutionError FixtureError) value -> IO ()
expectExecutionFailure label expected expectedReport result = case result of
    Left (EffectReverseExecutionError actual actualReport) -> do
        assertEqual (label ++ " error") expected actual
        assertEqual (label ++ " report") expectedReport actualReport
    Right _ -> fail (label ++ ": unexpectedly succeeded")

rightIO :: (Show error) => String -> IO (Either error value) -> IO value
rightIO label action = action >>= requireRight label

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left problem) = fail (label ++ ": " ++ show problem)

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual = unless (expected == actual) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

assertApprox :: String -> Double -> Double -> IO ()
assertApprox label expected actual =
    let tolerance = 2e-9 + 2e-7 * max (abs expected) (abs actual)
     in unless (not (isNaN actual || isInfinite actual) && abs (expected - actual) <= tolerance) (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))
