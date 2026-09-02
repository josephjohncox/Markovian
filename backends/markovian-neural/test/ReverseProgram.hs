{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module ReverseProgram (tests) where

import Data.Maybe (isNothing)
import Markovian.Reverse
import Markovian.Reverse.Program
import Numeric.Natural (Natural)
import TestSupport (assert, assertCloseWith, centralDifference, requireRight)

data FixtureError
    = ForwardFailure
    | PullbackFailure
    | AdditionFailure
    | InvalidPrimal
    | InvalidCotangent
    deriving (Eq, Show)

data RationalPrimitive parameter parameterCotangent input inputCotangent output outputCotangent where
    RationalScale :: String -> PrimitiveTapePolicy -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    RationalShift :: String -> PrimitiveTapePolicy -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    SharedScale :: String -> PrimitiveTapePolicy -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    TaggedOutput :: String -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    FailingForward :: String -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    FailingPullback :: String -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    FailingInputAddition :: String -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    NonReflexiveRecompute :: String -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    FailingRecompute :: String -> RationalPrimitive Rational Rational Rational Rational Rational Rational
    DeepOwnerFree :: RationalPrimitive () () Rational Rational Rational Rational
    CyclicOwnerFree :: RationalPrimitive () () Rational Rational Rational Rational

type RationalProgram parameter parameterCotangent input inputCotangent output outputCotangent =
    ReverseProgram RationalPrimitive FixtureError Rational parameter parameterCotangent input inputCotangent output outputCotangent

data DoublePrimitive parameter parameterCotangent input inputCotangent output outputCotangent where
    QuadraticScale :: String -> PrimitiveTapePolicy -> DoublePrimitive Double Double Double Double Double Double
    SineScale :: String -> PrimitiveTapePolicy -> DoublePrimitive Double Double Double Double Double Double

type DoubleParameters = (((Double, Double), (Double, Double)), Double)
type DoubleInputs = ((Double, Double), (Double, Double))
type DoubleOutputs = ((Double, (Double, Double)), (Double, Double))
type DoubleProgram = ReverseProgram DoublePrimitive FixtureError Double DoubleParameters DoubleParameters DoubleInputs DoubleInputs DoubleOutputs DoubleOutputs

tests :: IO ()
tests = do
    metadataAndPreparationEvidence
    exactCompositionEvidence
    exactTensorEvidence
    diagonalEvidence
    tapeEvidence
    failureEvidence
    structuralBoundEvidence
    finiteDifferenceEvidence StoreCapturedPullback
    finiteDifferenceEvidence RecomputePrimitive
    floatingCounterexample
    putStrLn "PASS: finite owned reverse programs"

metadataAndPreparationEvidence :: IO ()
metadataAndPreparationEvidence = do
    let scalar = layout "scalar" 1
        pairLayout = productFiniteLayout scalar scalar
    assert "finite layout extent" (finiteLayoutExtent pairLayout == 2)
    assert "finite layout structural description" (finiteLayoutDescription pairLayout == "(scalar[1] * scalar[1])")
    assert "same declared cotangent layout" (sameCotangentLayout rationalCotangent rationalCotangent)
    assert "legacy cotangent unexpectedly had a finite layout" (isNothing (cotangentFiniteLayout legacyRationalCotangent))
    expectError
        "undeclared cotangent layout"
        (ReverseDefinitionFailure [] UndeclaredCotangentLayout)
        (prepareReverseProgram generousLimits resolveRational (identityProgram rationalPrimal legacyRationalCotangent))

    let mismatched = composeProgram (primitiveProgram (TaggedOutput "tag")) (primitiveProgram (RationalShift "next" StoreCapturedPullback))
    case prepareReverseProgram generousLimits resolveRational mismatched of
        Left (ReversePrimalLayoutMismatch [] left right) ->
            assert "same Haskell type layout mismatch was not visible" (left /= right)
        result -> failWith "same-type primal layout mismatch" result

    let duplicate = composeProgram (primitiveProgram (SharedScale "duplicate" StoreCapturedPullback)) (primitiveProgram (SharedScale "duplicate" StoreCapturedPullback))
    expectError
        "duplicate independent owner"
        (DuplicateIndependentOwner [] "duplicate")
        (prepareReverseProgram generousLimits resolveRational duplicate)

    let badShare = shareParameterProgram (primitiveProgram (SharedScale "left-owner" StoreCapturedPullback)) (primitiveProgram (SharedScale "right-owner" StoreCapturedPullback))
    case prepareReverseProgram generousLimits resolveRational badShare of
        Left (MismatchedSharedOwnership [] left right) -> assert "shared ownership descriptions collapsed" (left /= right)
        result -> failWith "mismatched shared ownership" result

    let boundaryProgram = composeProgram (primitiveProgram (RationalScale "first" StoreCapturedPullback)) (primitiveProgram (RationalShift "second" RecomputePrimitive))
    _ <- requireRight "exact preparation boundaries" (prepareReverseProgram (reverseLimits 3 2 2 2 2 2) resolveRational boundaryProgram)
    expectError
        "node boundary"
        (ReverseNodeLimitExceeded [CompositionRight] 2)
        (prepareReverseProgram (reverseLimits 2 2 2 2 2 2) resolveRational boundaryProgram)
    expectError
        "primitive boundary"
        (ReversePrimitiveLimitExceeded [CompositionRight] 1)
        (prepareReverseProgram (reverseLimits 3 1 2 2 2 2) resolveRational boundaryProgram)
    expectError
        "depth boundary"
        (ReverseDepthLimitExceeded [CompositionLeft] 1)
        (prepareReverseProgram (reverseLimits 3 2 1 2 2 2) resolveRational boundaryProgram)
    expectError
        "owner boundary"
        (ReverseOwnerLimitExceeded [CompositionRight] 1)
        (prepareReverseProgram (reverseLimits 3 2 2 1 2 2) resolveRational boundaryProgram)
    case prepareReverseProgram (reverseLimits 3 2 2 2 1 2) resolveRational boundaryProgram of
        Left (ReversePrimalLayoutLimitExceeded [] offending 1) -> assert "wrong primal extent at limit" (finiteLayoutExtent offending == 2)
        result -> failWith "primal layout boundary" result
    case prepareReverseProgram (reverseLimits 3 2 2 2 2 1) resolveRational boundaryProgram of
        Left (ReverseCotangentLayoutLimitExceeded [] offending 1) -> assert "wrong cotangent extent at limit" (finiteLayoutExtent offending == 2)
        result -> failWith "cotangent layout boundary" result
    expectError
        "deterministic budget precedence"
        (ReverseNodeLimitExceeded [] 0)
        (prepareReverseProgram (reverseLimits 0 0 0 0 0 0) resolveRational boundaryProgram)

    prepared <- requireRight "deterministic report preparation" (prepareReverseProgram generousLimits resolveRational boundaryProgram)
    assert "deterministic reverse report changed" (renderReverseProgramReport (preparedReverseProgramReport prepared) == expectedReport)

exactCompositionEvidence :: IO ()
exactCompositionEvidence = do
    let scale owner = primitiveProgram (RationalScale owner StoreCapturedPullback)
        shift owner = primitiveProgram (RationalShift owner StoreCapturedPullback)
        identity = identityProgram rationalPrimal rationalCotangent
        direct = composeProgram (scale "scale") (shift "shift")
    (output, parameterCotangent, inputCotangent) <- runRational "composition" direct (2, 3) 4 5
    assert "composition primal" (output == 11)
    assert "composition parameter cotangent" (parameterCotangent == (20, 5))
    assert "composition input cotangent" (inputCotangent == 10)

    leftIdentity <- runRational "left identity" (composeProgram identity (scale "left-id")) ((), 2) 4 5
    rightIdentity <- runRational "right identity" (composeProgram (scale "right-id") identity) (2, ()) 4 5
    assert "composition identity primal/input law" (projectRun leftIdentity == projectRun rightIdentity)

    let leftAssociated = composeProgram (composeProgram (scale "a") (shift "b")) (scale "c")
        rightAssociated = composeProgram (scale "a") (composeProgram (shift "b") (scale "c"))
    (leftOutput, leftParameter, leftInput) <- runRational "left-associated composition" leftAssociated ((2, 3), 5) 4 7
    (rightOutput, rightParameter, rightInput) <- runRational "right-associated composition" rightAssociated (2, (3, 5)) 4 7
    assert "composition associativity primal" (leftOutput == rightOutput)
    assert "composition associativity input cotangent" (leftInput == rightInput)
    let ((leftA, leftB), leftC) = leftParameter
        (rightA, (rightB, rightC)) = rightParameter
    assert "composition associativity parameter cotangent" ((leftA, leftB, leftC) == (rightA, rightB, rightC))

exactTensorEvidence :: IO ()
exactTensorEvidence = do
    let scale owner = primitiveProgram (RationalScale owner StoreCapturedPullback)
        shift owner = primitiveProgram (RationalShift owner StoreCapturedPullback)
        leftAssociated = tensorProgram (tensorProgram (scale "ta") (shift "tb")) (scale "tc")
        rightAssociated = tensorProgram (scale "ta") (tensorProgram (shift "tb") (scale "tc"))
    left <- runRational "left tensor association" leftAssociated ((2, 3), 5) ((4, 6), 7) ((11, 13), 17)
    right <- runRational "right tensor association" rightAssociated (2, (3, 5)) (4, (6, 7)) (11, (13, 17))
    let (leftOutput, ((leftPA, leftPB), leftPC), ((leftXA, leftXB), leftXC)) = left
        (rightOutput, (rightPA, (rightPB, rightPC)), (rightXA, (rightXB, rightXC))) = right
        flattenLeftOutput = let ((a, b), c) = leftOutput in (a, b, c)
        flattenRightOutput = let (a, (b, c)) = rightOutput in (a, b, c)
    assert "tensor associativity primal" (flattenLeftOutput == flattenRightOutput)
    assert "tensor associativity parameter cotangent" ((leftPA, leftPB, leftPC) == (rightPA, rightPB, rightPC))
    assert "tensor associativity input cotangent" ((leftXA, leftXB, leftXC) == (rightXA, rightXB, rightXC))

    normal <- runRational "tensor symmetry normal" (tensorProgram (scale "sa") (shift "sb")) (2, 3) (4, 6) (11, 13)
    swapped <- runRational "tensor symmetry swapped" (tensorProgram (shift "sb") (scale "sa")) (3, 2) (6, 4) (13, 11)
    let (normalOutput, (normalPA, normalPB), (normalXA, normalXB)) = normal
        (swappedOutput, (swappedPB, swappedPA), (swappedXB, swappedXA)) = swapped
    assert "tensor symmetry primal" (normalOutput == swapPair swappedOutput)
    assert "tensor symmetry parameter cotangent" ((normalPA, normalPB) == (swappedPA, swappedPB))
    assert "tensor symmetry input cotangent" ((normalXA, normalXB) == (swappedXA, swappedXB))

    let before = tensorProgram (scale "ia") (shift "ib")
        after = tensorProgram (shift "ic") (scale "id")
        sequential = composeProgram before after
        pointwise = tensorProgram (composeProgram (scale "ia") (shift "ic")) (composeProgram (shift "ib") (scale "id"))
    sequentialRun <- runRational "tensor interchange sequential" sequential ((2, 3), (5, 7)) (4, 6) (11, 13)
    pointwiseRun <- runRational "tensor interchange pointwise" pointwise ((2, 5), (3, 7)) (4, 6) (11, 13)
    let (sequentialOutput, ((spa, spb), (spc, spd)), sequentialInput) = sequentialRun
        (pointwiseOutput, ((ppa, ppc), (ppb, ppd)), pointwiseInput) = pointwiseRun
    assert "tensor interchange primal" (sequentialOutput == pointwiseOutput)
    assert "tensor interchange parameter cotangent" ((spa, spb, spc, spd) == (ppa, ppb, ppc, ppd))
    assert "tensor interchange input cotangent" (sequentialInput == pointwiseInput)

    let unitPrimal = finitePrimalSpace unitFiniteLayout (const (Right ())) (==) ExactCotangentEquality
        unitProgram = identityProgram unitPrimal unitCotangent
    unitRun <- runRational "tensor unit" (tensorProgram (scale "unit-scale") unitProgram) (2, ()) (4, ()) (5, ())
    assert "tensor unit law" (unitRun == ((8, ()), (20, ()), (10, ())))

diagonalEvidence :: IO ()
diagonalEvidence = do
    let scale owner = primitiveProgram (RationalScale owner StoreCapturedPullback)
        shift owner = primitiveProgram (RationalShift owner StoreCapturedPullback)
    paired <- runRational "input diagonal" (pairInputProgram (scale "pair-scale") (shift "pair-shift")) (2, 3) 4 (5, 7)
    assert "input diagonal accumulation" (paired == ((8, 7), (20, 7), 17))

    shared <- runRational "parameter diagonal" (shareParameterProgram (scale "shared") (scale "shared")) 2 (3, 4) (5, 7)
    assert "parameter diagonal accumulation" (shared == ((6, 8), 43, (10, 14)))

    let threeWay = shareParameterProgram (shareParameterProgram (scale "three") (scale "three")) (scale "three")
    three <- runRational "three-way sharing" threeWay 2 ((3, 4), 5) ((7, 11), 13)
    assert "three-way diagonal accumulation" (three == (((6, 8), 10), 130, ((14, 22), 26)))

    independent <- runRational "independent parameters" (tensorProgram (scale "independent-left") (scale "independent-right")) (2, 2) (3, 4) (5, 7)
    assert "independent parameter structure" (independent == ((6, 8), (15, 28), (10, 14)))
    assert "missing input diagonal addition counterexample" ((10 :: Rational) /= 10 + 14)

tapeEvidence :: IO ()
tapeEvidence = do
    let storedProgram = composeProgram (primitiveProgram (RationalScale "stored-a" StoreCapturedPullback)) (primitiveProgram (RationalShift "stored-b" StoreCapturedPullback))
        recomputedProgram = composeProgram (primitiveProgram (RationalScale "recomputed-a" RecomputePrimitive)) (primitiveProgram (RationalShift "recomputed-b" RecomputePrimitive))
    storedPrepared <- requireRight "stored preparation" (prepareReverseProgram generousLimits resolveRational storedProgram)
    recomputedPrepared <- requireRight "recomputed preparation" (prepareReverseProgram generousLimits resolveRational recomputedProgram)
    storedRun <- requireRight "stored forward" (runPreparedReverse storedPrepared (2, 3) 4)
    recomputedRun <- requireRight "recomputed forward" (runPreparedReverse recomputedPrepared (2, 3) 4)
    storedPullback <- requireRight "stored pullback" (applyReverseTape (reverseRunTape storedRun) 5)
    repeatedPullback <- requireRight "repeated stored pullback" (applyReverseTape (reverseRunTape storedRun) 5)
    recomputedPullback <- requireRight "recomputed pullback" (applyReverseTape (reverseRunTape recomputedRun) 5)
    repeatedRecomputedPullback <- requireRight "repeated recomputed pullback" (applyReverseTape (reverseRunTape recomputedRun) 5)
    assert "tape policies changed primal" (reverseRunOutput storedRun == reverseRunOutput recomputedRun)
    assert "tape policies changed pullback" (storedPullback == recomputedPullback)
    assert "stored tape could not be applied repeatedly" (storedPullback == repeatedPullback)
    assert "recomputed tape could not be applied repeatedly" (recomputedPullback == repeatedRecomputedPullback)
    assert "stored tape report" (reverseTapeReport (reverseRunTape storedRun) == ReverseTapeReport 3 2 0)
    assert "recomputed tape report" (reverseTapeReport (reverseRunTape recomputedRun) == ReverseTapeReport 3 0 2)

    directEvaluation <- requireRight "legacy circuit compatibility" (evaluateReverseCircuit (composeReverseCircuit (scaleCircuit "legacy-a" normalParameterCotangent) (shiftCircuit "legacy-b" normalParameterCotangent)) (2, 3) 4)
    directPullback <- requireRight "legacy circuit pullback" (applyReverseVJP directEvaluation 5)
    assert "program disagreed with ParametricReverseCircuit" (reversePrimalOutput directEvaluation == reverseRunOutput storedRun && directPullback == storedPullback)

    malformedPrepared <- requireRight "nonreflexive recomputation preparation" (prepareReverseProgram generousLimits resolveRational (primitiveProgram (NonReflexiveRecompute "nonreflexive")))
    malformedRun <- requireRight "nonreflexive recomputation forward" (runPreparedReverse malformedPrepared 2 4)
    expectError
        "recomputed output agreement"
        (ReverseRecomputedOutputMismatch [])
        (applyReverseTape (reverseRunTape malformedRun) 5)

failureEvidence :: IO ()
failureEvidence = do
    forwardPrepared <- requireRight "forward-failure preparation" (prepareReverseProgram generousLimits resolveRational (primitiveProgram (FailingForward "forward")))
    expectError "primitive forward failure" (ReversePrimitiveForwardFailure [] ForwardFailure) (runPreparedReverse forwardPrepared 2 4)

    pullbackPrepared <- requireRight "pullback-failure preparation" (prepareReverseProgram generousLimits resolveRational (primitiveProgram (FailingPullback "pullback")))
    pullbackRun <- requireRight "pullback-failure forward" (runPreparedReverse pullbackPrepared 2 4)
    expectError "primitive pullback failure" (ReversePrimitivePullbackFailure [] PullbackFailure) (applyReverseTape (reverseRunTape pullbackRun) 5)

    let additionProgram = pairInputProgram (primitiveProgram (FailingInputAddition "add-left")) (primitiveProgram (FailingInputAddition "add-right"))
    additionPrepared <- requireRight "addition-failure preparation" (prepareReverseProgram generousLimits resolveRational additionProgram)
    additionRun <- requireRight "addition-failure forward" (runPreparedReverse additionPrepared (2, 3) 4)
    expectError
        "input diagonal addition failure"
        (ReverseCotangentAdditionFailure [] InputCotangentStage AdditionFailure)
        (applyReverseTape (reverseRunTape additionRun) (5, 7))

    recomputePrepared <- requireRight "recomputation-failure preparation" (prepareReverseProgram generousLimits resolveRational (primitiveProgram (FailingRecompute "recompute")))
    recomputeRun <- requireRight "recomputation-failure forward" (runPreparedReverse recomputePrepared 2 4)
    expectError
        "distinct recomputation callback failure"
        (ReversePrimitiveRecomputationFailure [] ForwardFailure)
        (applyReverseTape (reverseRunTape recomputeRun) 5)

structuralBoundEvidence :: IO ()
structuralBoundEvidence = do
    let nestedZero 0 = unitFiniteLayout
        nestedZero amount = productFiniteLayout unitFiniteLayout (nestedZero (amount - 1))
        deepLayout = nestedZero (3 :: Natural)
        deepPrimal :: FinitePrimalSpace FixtureError ()
        deepPrimal = finitePrimalSpace deepLayout (const (Right ())) (==) ExactCotangentEquality
        deepCotangent = declared "deep-zero" deepLayout (const (Right ())) () (\() () -> Right ()) (\_ () -> Right ()) (==) ExactCotangentEquality
        identity = identityProgram deepPrimal deepCotangent
    _ <- requireRight "exact structural layout boundary" (prepareReverseProgram (reverseLimitsWithStructure 1 0 1 0 0 0 7 4) resolveRational identity)
    expectError
        "layout structural node boundary"
        (ReverseLayoutNodeLimitExceeded [] 6)
        (prepareReverseProgram (reverseLimitsWithStructure 1 0 1 0 0 0 6 4) resolveRational identity)
    expectError
        "layout structural depth boundary"
        (ReverseLayoutDepthLimitExceeded [] 3)
        (prepareReverseProgram (reverseLimitsWithStructure 1 0 1 0 0 0 7 3) resolveRational identity)

    let cyclicLayout = productFiniteLayout unitFiniteLayout cyclicLayout
        cyclicPrimal :: FinitePrimalSpace FixtureError ()
        cyclicPrimal = finitePrimalSpace cyclicLayout (const (Right ())) (==) ExactCotangentEquality
        cyclicCotangent = declared "cyclic-zero" cyclicLayout (const (Right ())) () (\() () -> Right ()) (\_ () -> Right ()) (==) ExactCotangentEquality
    expectError
        "cyclic layout bounded rejection"
        (ReverseLayoutNodeLimitExceeded [] 8)
        (prepareReverseProgram (reverseLimitsWithStructure 1 0 1 0 0 0 8 20) resolveRational (identityProgram cyclicPrimal cyclicCotangent))

    expectError
        "deep owner-free structure bounded rejection"
        (ReverseOwnershipNodeLimitExceeded [] 5)
        (prepareReverseProgram (reverseLimitsWithStructure 1 1 1 0 0 1 5 20) resolveRational (primitiveProgram DeepOwnerFree))
    expectError
        "cyclic owner-free structure bounded rejection"
        (ReverseOwnershipNodeLimitExceeded [] 8)
        (prepareReverseProgram (reverseLimitsWithStructure 1 1 1 0 0 1 8 20) resolveRational (primitiveProgram CyclicOwnerFree))

finiteDifferenceEvidence :: PrimitiveTapePolicy -> IO ()
finiteDifferenceEvidence policy = do
    prepared <- requireRight ("finite-difference preparation " ++ show policy) (prepareReverseProgram (reverseLimits 30 20 8 8 8 8) resolveDouble (complexDoubleProgram policy))
    let parameters = (((0.7, -1.1), (0.9, 1.2)), -0.8)
        inputs = ((0.4, -0.3), (0.6, -0.5))
        outputCotangent = ((1.3, (-0.6, 0.8)), (1.1, -0.9))
    run <- requireRight "finite-difference analytic forward" (runPreparedReverse prepared parameters inputs)
    (parameterCotangent, inputCotangent) <- requireRight "finite-difference analytic pullback" (applyReverseTape (reverseRunTape run) outputCotangent)
    numericalParameters <- traverse (numericalParameter prepared parameters inputs outputCotangent) [0 .. 4]
    numericalInputs <- traverse (numericalInput prepared parameters inputs outputCotangent) [0 .. 3]
    sequence_
        [ assertCloseWith ("parameter coordinate " ++ show index ++ " " ++ show policy) 2e-10 2e-8 expected actual
        | (index, (expected, actual)) <- zip [0 :: Int ..] (zip numericalParameters (flattenParameters parameterCotangent))
        ]
    sequence_
        [ assertCloseWith ("input coordinate " ++ show index ++ " " ++ show policy) 2e-10 2e-8 expected actual
        | (index, (expected, actual)) <- zip [0 :: Int ..] (zip numericalInputs (flattenInputs inputCotangent))
        ]

floatingCounterexample :: IO ()
floatingCounterexample = do
    let x = 1e16 :: Double
        y = -1e16
        z = 1
    assert "floating reassociation counterexample disappeared" ((x + y) + z /= x + (y + z))

resolveRational :: ReversePrimitiveResolver RationalPrimitive FixtureError Rational
resolveRational primitive = case primitive of
    RationalScale owner policy -> owned "scale" owner policy (parameterPrimal owner) (rationalParameterCotangent owner) rationalPrimal rationalCotangent rationalPrimal rationalCotangent (scaleCircuit owner (rationalParameterCotangent owner))
    RationalShift owner policy -> owned "shift" owner policy (parameterPrimal owner) (rationalParameterCotangent owner) rationalPrimal rationalCotangent rationalPrimal rationalCotangent (shiftCircuit owner (rationalParameterCotangent owner))
    SharedScale owner policy -> ownedWithLayout "shared-scale" owner policy sharedParameterPrimal sharedParameterCotangent rationalPrimal rationalCotangent rationalPrimal rationalCotangent (scaleCircuit owner sharedParameterCotangent)
    TaggedOutput owner -> owned "tagged" owner StoreCapturedPullback (parameterPrimal owner) (rationalParameterCotangent owner) rationalPrimal rationalCotangent taggedPrimal rationalCotangent (scaleCircuit owner (rationalParameterCotangent owner))
    FailingForward owner -> owned "failing-forward" owner StoreCapturedPullback (parameterPrimal owner) (rationalParameterCotangent owner) rationalPrimal rationalCotangent rationalPrimal rationalCotangent (primitiveReverseCircuit (rationalParameterCotangent owner) rationalCotangent rationalCotangent (\_ _ -> Left ForwardFailure))
    FailingPullback owner -> owned "failing-pullback" owner StoreCapturedPullback (parameterPrimal owner) (rationalParameterCotangent owner) rationalPrimal rationalCotangent rationalPrimal rationalCotangent (primitiveReverseCircuit (rationalParameterCotangent owner) rationalCotangent rationalCotangent (\parameter input -> Right (reverseEvaluation (parameter * input) (\_ -> Left PullbackFailure))))
    FailingInputAddition owner -> owned "failing-addition" owner StoreCapturedPullback (parameterPrimal owner) (rationalParameterCotangent owner) rationalPrimal failingAdditionCotangent rationalPrimal rationalCotangent (primitiveReverseCircuit (rationalParameterCotangent owner) failingAdditionCotangent rationalCotangent (\parameter input -> Right (reverseEvaluation (parameter * input) (\outputCotangent -> Right (outputCotangent * input, outputCotangent * parameter)))))
    NonReflexiveRecompute owner -> owned "nonreflexive" owner RecomputePrimitive (parameterPrimal owner) (rationalParameterCotangent owner) rationalPrimal rationalCotangent nonReflexivePrimal rationalCotangent (scaleCircuit owner (rationalParameterCotangent owner))
    FailingRecompute owner -> do
        ownership <- parameterOwner owner (primalFiniteLayout (parameterPrimal owner))
        let circuit = scaleCircuit owner (rationalParameterCotangent owner)
        ownedReversePrimitiveWithRecomputation
            "failing-recompute"
            "1"
            ownership
            (parameterPrimal owner)
            rationalPrimal
            rationalPrimal
            circuit
            (primitiveRecomputation (\_ _ -> Left ForwardFailure))
    DeepOwnerFree -> ownerFreePrimitive deepOwnerFree
    CyclicOwnerFree -> ownerFreePrimitive cyclicOwnerFree
  where
    deepOwnerFree =
        parameterOwnershipProduct
            noParameterOwnership
            (parameterOwnershipProduct noParameterOwnership (parameterOwnershipProduct noParameterOwnership noParameterOwnership))
    cyclicOwnerFree = parameterOwnershipProduct noParameterOwnership cyclicOwnerFree
    ownerFreePrimitive ownership =
        ownedReversePrimitive
            "owner-free"
            "1"
            ownership
            unitPrimal
            rationalPrimal
            rationalPrimal
            (primitiveReverseCircuit unitCotangent rationalCotangent rationalCotangent (\() input -> Right (reverseEvaluation input (\outputCotangent -> Right ((), outputCotangent)))))
            StoreCapturedPullback
    unitPrimal = finitePrimalSpace unitFiniteLayout (const (Right ())) (==) ExactCotangentEquality

owned :: String -> String -> PrimitiveTapePolicy -> FinitePrimalSpace FixtureError Rational -> CotangentSpace FixtureError Rational Rational -> FinitePrimalSpace FixtureError Rational -> CotangentSpace FixtureError Rational Rational -> FinitePrimalSpace FixtureError Rational -> CotangentSpace FixtureError Rational Rational -> ParametricReverseCircuit FixtureError Rational Rational Rational Rational Rational Rational Rational -> Either ReverseDefinitionError (OwnedReversePrimitive FixtureError Rational Rational Rational Rational Rational Rational Rational)
owned name owner policy parameterSpace _ inputSpace _ outputSpace _ circuit = do
    ownership <- parameterOwner owner (primalFiniteLayout parameterSpace)
    case policy of
        StoreCapturedPullback ->
            ownedReversePrimitive name "1" ownership parameterSpace inputSpace outputSpace circuit StoreCapturedPullback
        RecomputePrimitive ->
            ownedReversePrimitiveWithRecomputation
                name
                "1"
                ownership
                parameterSpace
                inputSpace
                outputSpace
                circuit
                (primitiveRecomputation (evaluateReverseCircuit circuit))

ownedWithLayout :: String -> String -> PrimitiveTapePolicy -> FinitePrimalSpace FixtureError Rational -> CotangentSpace FixtureError Rational Rational -> FinitePrimalSpace FixtureError Rational -> CotangentSpace FixtureError Rational Rational -> FinitePrimalSpace FixtureError Rational -> CotangentSpace FixtureError Rational Rational -> ParametricReverseCircuit FixtureError Rational Rational Rational Rational Rational Rational Rational -> Either ReverseDefinitionError (OwnedReversePrimitive FixtureError Rational Rational Rational Rational Rational Rational Rational)
ownedWithLayout = owned

resolveDouble :: ReversePrimitiveResolver DoublePrimitive FixtureError Double
resolveDouble primitive = case primitive of
    QuadraticScale owner policy -> doubleOwned "quadratic" owner policy quadraticCircuit
    SineScale owner policy -> doubleOwned "sine" owner policy sineCircuit

doubleOwned :: String -> String -> PrimitiveTapePolicy -> (CotangentSpace FixtureError Double Double -> ParametricReverseCircuit FixtureError Double Double Double Double Double Double Double) -> Either ReverseDefinitionError (OwnedReversePrimitive FixtureError Double Double Double Double Double Double Double)
doubleOwned name owner policy build = do
    ownership <- parameterOwner owner (primalFiniteLayout (doubleParameterPrimal owner))
    let circuit = build (doubleParameterCotangent owner)
    case policy of
        StoreCapturedPullback ->
            ownedReversePrimitive name "1" ownership (doubleParameterPrimal owner) doublePrimal doublePrimal circuit StoreCapturedPullback
        RecomputePrimitive ->
            ownedReversePrimitiveWithRecomputation
                name
                "1"
                ownership
                (doubleParameterPrimal owner)
                doublePrimal
                doublePrimal
                circuit
                (primitiveRecomputation (evaluateReverseCircuit circuit))

complexDoubleProgram :: PrimitiveTapePolicy -> DoubleProgram
complexDoubleProgram policy =
    tensorProgram
        ( tensorProgram
            (composeProgram (primitiveProgram (QuadraticScale "a" policy)) (primitiveProgram (SineScale "b" policy)))
            (pairInputProgram (primitiveProgram (QuadraticScale "c" policy)) (primitiveProgram (SineScale "d" policy)))
        )
        (shareParameterProgram (primitiveProgram (QuadraticScale "e" policy)) (primitiveProgram (QuadraticScale "e" policy)))

runRational :: String -> RationalProgram parameter parameterCotangent input inputCotangent output outputCotangent -> parameter -> input -> outputCotangent -> IO (output, parameterCotangent, inputCotangent)
runRational label program parameter input outputCotangent = do
    prepared <- requireRight (label ++ " preparation") (prepareReverseProgram generousLimits resolveRational program)
    run <- requireRight (label ++ " forward") (runPreparedReverse prepared parameter input)
    (parameterCotangent, inputCotangent) <- requireRight (label ++ " pullback") (applyReverseTape (reverseRunTape run) outputCotangent)
    pure (reverseRunOutput run, parameterCotangent, inputCotangent)

scaleCircuit :: String -> CotangentSpace FixtureError Rational Rational -> ParametricReverseCircuit FixtureError Rational Rational Rational Rational Rational Rational Rational
scaleCircuit _ parameterSpace = primitiveReverseCircuit parameterSpace rationalCotangent rationalCotangent $ \parameter input ->
    Right (reverseEvaluation (parameter * input) (\outputCotangent -> Right (outputCotangent * input, outputCotangent * parameter)))

shiftCircuit :: String -> CotangentSpace FixtureError Rational Rational -> ParametricReverseCircuit FixtureError Rational Rational Rational Rational Rational Rational Rational
shiftCircuit _ parameterSpace = primitiveReverseCircuit parameterSpace rationalCotangent rationalCotangent $ \parameter input ->
    Right (reverseEvaluation (parameter + input) (\outputCotangent -> Right (outputCotangent, outputCotangent)))

quadraticCircuit :: CotangentSpace FixtureError Double Double -> ParametricReverseCircuit FixtureError Double Double Double Double Double Double Double
quadraticCircuit parameterSpace = primitiveReverseCircuit parameterSpace doubleCotangent doubleCotangent $ \parameter input ->
    Right (reverseEvaluation (parameter * input * input) (\outputCotangent -> Right (outputCotangent * input * input, outputCotangent * 2 * parameter * input)))

sineCircuit :: CotangentSpace FixtureError Double Double -> ParametricReverseCircuit FixtureError Double Double Double Double Double Double Double
sineCircuit parameterSpace = primitiveReverseCircuit parameterSpace doubleCotangent doubleCotangent $ \parameter input ->
    Right (reverseEvaluation (parameter * sin input) (\outputCotangent -> Right (outputCotangent * sin input, outputCotangent * parameter * cos input)))

doublePrimal :: FinitePrimalSpace FixtureError Double
doublePrimal = finitePrimalSpace (layout "double" 1) validateFinite close (ApproximateCotangentEquality "finite absolute tolerance 1e-12")

doubleParameterPrimal :: String -> FinitePrimalSpace FixtureError Double
doubleParameterPrimal owner = finitePrimalSpace (layout ("double-parameter/" ++ owner) 1) validateFinite close (ApproximateCotangentEquality "finite absolute tolerance 1e-12")

doubleCotangent :: CotangentSpace FixtureError Double Double
doubleCotangent = declared "double-module" (layout "double" 1) validateFinite 0 (\left right -> checkedFinite (left + right)) (\scalar value -> checkedFinite (scalar * value)) close (ApproximateCotangentEquality "finite absolute tolerance 1e-12")

doubleParameterCotangent :: String -> CotangentSpace FixtureError Double Double
doubleParameterCotangent owner = declared ("double-parameter-module/" ++ owner) (layout ("double-parameter/" ++ owner) 1) validateFinite 0 (\left right -> checkedFinite (left + right)) (\scalar value -> checkedFinite (scalar * value)) close (ApproximateCotangentEquality "finite absolute tolerance 1e-12")

rationalPrimal :: FinitePrimalSpace FixtureError Rational
rationalPrimal = finitePrimalSpace (layout "scalar" 1) (const (Right ())) (==) ExactCotangentEquality

taggedPrimal :: FinitePrimalSpace FixtureError Rational
taggedPrimal = finitePrimalSpace (layout "tagged-scalar" 1) (const (Right ())) (==) ExactCotangentEquality
nonReflexivePrimal :: FinitePrimalSpace FixtureError Rational
nonReflexivePrimal = finitePrimalSpace (layout "scalar" 1) (const (Right ())) (\_ _ -> False) ExactCotangentEquality
parameterPrimal :: String -> FinitePrimalSpace FixtureError Rational
parameterPrimal owner = finitePrimalSpace (layout ("parameter/" ++ owner) 1) (const (Right ())) (==) ExactCotangentEquality
sharedParameterPrimal :: FinitePrimalSpace FixtureError Rational
sharedParameterPrimal = finitePrimalSpace (layout "shared-parameter" 1) (const (Right ())) (==) ExactCotangentEquality

rationalCotangent :: CotangentSpace FixtureError Rational Rational
rationalCotangent = declared "rational-module" (layout "scalar" 1) (const (Right ())) 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality
rationalParameterCotangent :: String -> CotangentSpace FixtureError Rational Rational
rationalParameterCotangent owner = declared ("parameter-module/" ++ owner) (layout ("parameter/" ++ owner) 1) (const (Right ())) 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality
normalParameterCotangent :: CotangentSpace FixtureError Rational Rational
normalParameterCotangent = declared "legacy-parameter-module" (layout "legacy-parameter" 1) (const (Right ())) 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality
sharedParameterCotangent :: CotangentSpace FixtureError Rational Rational
sharedParameterCotangent = declared "shared-parameter-module" (layout "shared-parameter" 1) (const (Right ())) 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality
failingAdditionCotangent :: CotangentSpace FixtureError Rational Rational
failingAdditionCotangent = declared "failing-input-module" (layout "scalar" 1) (const (Right ())) 0 (\_ _ -> Left AdditionFailure) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality
legacyRationalCotangent :: CotangentSpace FixtureError Rational Rational
legacyRationalCotangent = cotangentSpace 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality
unitCotangent :: CotangentSpace FixtureError Rational ()
unitCotangent = declared "unit" unitFiniteLayout (const (Right ())) () (\() () -> Right ()) (\_ () -> Right ()) (==) ExactCotangentEquality

declared :: String -> FiniteLayout -> (value -> Either FixtureError ()) -> value -> (value -> value -> Either FixtureError value) -> (scalar -> value -> Either FixtureError value) -> (value -> value -> Bool) -> CotangentEqualityMode -> CotangentSpace FixtureError scalar value
declared owner finite valueValidator zero add scale equivalent mode =
    case declaredCotangentSpace owner finite valueValidator zero add scale equivalent mode of
        Just witness -> witness
        Nothing -> error "test declared a nonempty cotangent owner"

layout :: String -> Natural -> FiniteLayout
layout name extent = case finiteLayout name extent of
    Just finite -> finite
    Nothing -> error "test declared a nonempty finite layout"

validateFinite :: Double -> Either FixtureError ()
validateFinite value = if isNaN value || isInfinite value then Left InvalidPrimal else Right ()

checkedFinite :: Double -> Either FixtureError Double
checkedFinite value = if isNaN value || isInfinite value then Left InvalidCotangent else Right value

close :: Double -> Double -> Bool
close left right = not (isNaN left || isNaN right) && abs (left - right) <= 1e-12

generousLimits :: ReverseLimits
generousLimits = reverseLimits 100 100 20 20 20 20

projectRun :: (output, parameterCotangent, inputCotangent) -> (output, inputCotangent)
projectRun (output, _, inputCotangent) = (output, inputCotangent)

swapPair :: (left, right) -> (right, left)
swapPair (left, right) = (right, left)

expectError :: (Eq error, Show error) => String -> error -> Either error value -> IO ()
expectError _ expected (Left actual) | actual == expected = pure ()
expectError label expected (Left actual) = fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
expectError label expected (Right _) = fail (label ++ ": expected " ++ show expected ++ ", got success")

failWith :: (Show error) => String -> Either error value -> IO a
failWith label (Left failure) = fail (label ++ ": " ++ show failure)
failWith label (Right _) = fail (label ++ ": unexpectedly succeeded")

expectedReport :: String
expectedReport =
    unlines
        [ "reverse-program-report"
        , "nodes: 3"
        , "primitives: 2"
        , "maximum-depth: 2"
        , "identity/composition/tensor/input-share/parameter-share: 0/1/0/0/0"
        , "primitive-tapes stored/recomputed: 1/1"
        , "owners: [\"first\",\"second\"]"
        , "ownership-tree: (first:parameter/first[1] * second:parameter/second[1])"
        , "maximum-primal-extent: 2"
        , "maximum-cotangent-extent: 2"
        , "primitive-uses: [\"scale@1:StoreCapturedPullback:1\",\"shift@1:RecomputePrimitive:1\"]"
        ]

numericalParameter :: PreparedReverseProgram DoublePrimitive FixtureError Double DoubleParameters DoubleParameters DoubleInputs DoubleInputs DoubleOutputs DoubleOutputs -> DoubleParameters -> DoubleInputs -> DoubleOutputs -> Int -> IO Double
numericalParameter prepared parameters inputs cotangent index =
    centralDifference 1e-6 (\value -> doubleObjective prepared (setParameter index value parameters) inputs cotangent) (parameterCoordinate index parameters)

numericalInput :: PreparedReverseProgram DoublePrimitive FixtureError Double DoubleParameters DoubleParameters DoubleInputs DoubleInputs DoubleOutputs DoubleOutputs -> DoubleParameters -> DoubleInputs -> DoubleOutputs -> Int -> IO Double
numericalInput prepared parameters inputs cotangent index =
    centralDifference 1e-6 (\value -> doubleObjective prepared parameters (setInput index value inputs) cotangent) (inputCoordinate index inputs)

doubleObjective :: PreparedReverseProgram DoublePrimitive FixtureError Double DoubleParameters DoubleParameters DoubleInputs DoubleInputs DoubleOutputs DoubleOutputs -> DoubleParameters -> DoubleInputs -> DoubleOutputs -> IO Double
doubleObjective prepared parameters inputs cotangent = do
    run <- requireRight "finite-difference objective" (runPreparedReverse prepared parameters inputs)
    pure (pairDoubleOutputs cotangent (reverseRunOutput run))

pairDoubleOutputs :: DoubleOutputs -> DoubleOutputs -> Double
pairDoubleOutputs ((a, (b, c)), (d, e)) ((u, (v, w)), (x, y)) = a * u + b * v + c * w + d * x + e * y

flattenParameters :: DoubleParameters -> [Double]
flattenParameters (((a, b), (c, d)), e) = [a, b, c, d, e]
flattenInputs :: DoubleInputs -> [Double]
flattenInputs ((a, b), (c, d)) = [a, b, c, d]

parameterCoordinate :: Int -> DoubleParameters -> Double
parameterCoordinate index (((a, b), (c, d)), e) = case index of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    _ -> e

inputCoordinate :: Int -> DoubleInputs -> Double
inputCoordinate index ((a, b), (c, d)) = case index of
    0 -> a
    1 -> b
    2 -> c
    _ -> d

setParameter :: Int -> Double -> DoubleParameters -> DoubleParameters
setParameter index value (((a, b), (c, d)), e) = case index of
    0 -> (((value, b), (c, d)), e)
    1 -> (((a, value), (c, d)), e)
    2 -> (((a, b), (value, d)), e)
    3 -> (((a, b), (c, value)), e)
    _ -> (((a, b), (c, d)), value)

setInput :: Int -> Double -> DoubleInputs -> DoubleInputs
setInput index value ((a, b), (c, d)) = case index of
    0 -> ((value, b), (c, d))
    1 -> ((a, value), (c, d))
    2 -> ((a, b), (value, d))
    _ -> ((a, b), (c, value))
