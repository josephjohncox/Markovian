module ParametricReverse (tests) where

import Markovian.Reverse (
    CotangentEqualityMode (..),
    CotangentSpace,
    ParametricReverseCircuit,
    addCotangents,
    applyReverseVJP,
    composeReverseCircuit,
    cotangentEqualityMode,
    cotangentSpace,
    cotangentZero,
    cotangentsEquivalent,
    evaluateReverseCircuit,
    identityReverseCircuit,
    pairReverseCircuit,
    primitiveReverseCircuit,
    reverseEvaluation,
    reversePrimalOutput,
    scaleCotangent,
    shareParametersReverseCircuit,
    tensorReverseCircuit,
 )
import TestSupport (assert, assertCloseWith, centralDifference, requireRight)

data ReverseFixtureError
    = PrimitiveFailure
    | CotangentAdditionFailure
    deriving (Eq, Show)

type ScalarCircuit scalar =
    ParametricReverseCircuit
        ReverseFixtureError
        scalar
        scalar
        scalar
        scalar
        scalar
        scalar
        scalar

tests :: IO ()
tests = do
    exactCotangentModuleLaws
    exactPrimitiveVJPLaws
    exactIdentity
    exactComposition
    exactCompositionLaws
    exactInputDiagonal
    exactParameterDiagonal
    exactTensor
    finiteDifferenceComposition
    finiteDifferenceInputDiagonal
    failurePropagation
    putStrLn "PASS: parametric reverse circuits"

exactCotangentModuleLaws :: IO ()
exactCotangentModuleLaws = do
    let space = rationalSpace
    assert "cotangent zero changed" (cotangentZero space == 0)
    assert "cotangent equality mode changed" (cotangentEqualityMode space == ExactCotangentEquality)
    assert "cotangent equality changed" (cotangentsEquivalent space 3 3)
    leftIdentity <- requireRight "cotangent left identity" (addCotangents space 0 5)
    rightIdentity <- requireRight "cotangent right identity" (addCotangents space 5 0)
    leftAssoc <- requireRight "cotangent left associativity" (addCotangents space 2 3 >>= \sum23 -> addCotangents space sum23 4)
    rightAssoc <- requireRight "cotangent right associativity" (addCotangents space 3 4 >>= addCotangents space 2)
    leftCommute <- requireRight "cotangent left commutativity" (addCotangents space 2 7)
    rightCommute <- requireRight "cotangent right commutativity" (addCotangents space 7 2)
    scaledSum <- requireRight "cotangent scaled sum" (addCotangents space 2 3 >>= scaleCotangent space 4)
    sumScaled <- requireRight "cotangent sum scaled" $ do
        left <- scaleCotangent space 4 2
        right <- scaleCotangent space 4 3
        addCotangents space left right
    assert "cotangent left identity failed" (leftIdentity == 5)
    assert "cotangent right identity failed" (rightIdentity == 5)
    assert "cotangent addition is not associative" (leftAssoc == rightAssoc)
    assert "cotangent addition is not commutative" (leftCommute == rightCommute)
    assert "scalar action is not additive" (scaledSum == sumScaled)

exactPrimitiveVJPLaws :: IO ()
exactPrimitiveVJPLaws = do
    evaluation <- requireRight "primitive VJP laws" (evaluateReverseCircuit rationalScale 3 4)
    zeroResult <- requireRight "primitive zero VJP" (applyReverseVJP evaluation 0)
    sumResult <- requireRight "primitive additive VJP" (applyReverseVJP evaluation (5 + 7))
    left <- requireRight "primitive left VJP" (applyReverseVJP evaluation 5)
    right <- requireRight "primitive right VJP" (applyReverseVJP evaluation 7)
    homogeneous <- requireRight "primitive homogeneous VJP" (applyReverseVJP evaluation (4 * 5))
    assert "primitive VJP did not preserve zero" (zeroResult == (0, 0))
    assert "primitive VJP did not preserve addition" (sumResult == addPair left right)
    assert "primitive VJP was not homogeneous" (homogeneous == scalePair 4 left)
  where
    addPair (leftP, leftX) (rightP, rightX) = (leftP + rightP, leftX + rightX)
    scalePair scalar (parameter, input) = (scalar * parameter, scalar * input)

exactIdentity :: IO ()
exactIdentity = do
    let circuit ::
            ParametricReverseCircuit
                ReverseFixtureError
                Rational
                ()
                ()
                Rational
                Rational
                Rational
                Rational
        circuit = identityReverseCircuit rationalSpace
    evaluation <- requireRight "reverse identity" (evaluateReverseCircuit circuit () 4)
    assert "identity primal" (reversePrimalOutput evaluation == 4)
    (parameterCotangent, inputCotangent) <- requireRight "reverse identity VJP" (applyReverseVJP evaluation 5)
    assert "identity unit parameter cotangent" (parameterCotangent == ())
    assert "identity input cotangent" (inputCotangent == 5)

exactComposition :: IO ()
exactComposition = do
    evaluation <- requireRight "exact reverse composition" (evaluateReverseCircuit (composeReverseCircuit rationalScale rationalShift) (2, 3) 4)
    assert "composed primal" (reversePrimalOutput evaluation == (11 :: Rational))
    ((scaleCotangentValue, shiftCotangent), inputCotangent) <- requireRight "exact reverse composition VJP" (applyReverseVJP evaluation 5)
    assert "first parameter cotangent" (scaleCotangentValue == 20)
    assert "second parameter cotangent" (shiftCotangent == 5)
    assert "reverse chain input cotangent" (inputCotangent == 10)

exactCompositionLaws :: IO ()
exactCompositionLaws = do
    let identity = identityReverseCircuit rationalSpace
        leftIdentity = composeReverseCircuit identity rationalScale
        rightIdentity = composeReverseCircuit rationalScale identity
        leftAssociated = composeReverseCircuit (composeReverseCircuit rationalScale rationalShift) rationalScale
        rightAssociated = composeReverseCircuit rationalScale (composeReverseCircuit rationalShift rationalScale)
    left <- requireRight "composition left identity" (evaluateReverseCircuit leftIdentity ((), 2) 4)
    right <- requireRight "composition right identity" (evaluateReverseCircuit rightIdentity (2, ()) 4)
    assert "composition identity changed primal" (reversePrimalOutput left == reversePrimalOutput right)
    (_, leftInput) <- requireRight "composition left identity VJP" (applyReverseVJP left 5)
    (_, rightInput) <- requireRight "composition right identity VJP" (applyReverseVJP right 5)
    assert "composition identity changed pullback" (leftInput == rightInput)
    associatedLeft <- requireRight "left-associated composition" (evaluateReverseCircuit leftAssociated ((2, 3), 5) 4)
    associatedRight <- requireRight "right-associated composition" (evaluateReverseCircuit rightAssociated (2, (3, 5)) 4)
    assert "composition association changed primal" (reversePrimalOutput associatedLeft == reversePrimalOutput associatedRight)
    (_, leftAssociatedInput) <- requireRight "left-associated VJP" (applyReverseVJP associatedLeft 7)
    (_, rightAssociatedInput) <- requireRight "right-associated VJP" (applyReverseVJP associatedRight 7)
    assert "composition association changed input pullback" (leftAssociatedInput == rightAssociatedInput)

exactInputDiagonal :: IO ()
exactInputDiagonal = do
    evaluation <- requireRight "input diagonal" (evaluateReverseCircuit (pairReverseCircuit rationalSpace rationalScale rationalShift) (2, 3) 4)
    assert "input diagonal primal" (reversePrimalOutput evaluation == ((8, 7) :: (Rational, Rational)))
    ((scaleCotangentValue, shiftCotangent), inputCotangent) <- requireRight "input diagonal VJP" (applyReverseVJP evaluation (5, 7))
    assert "input diagonal parameter product" ((scaleCotangentValue, shiftCotangent) == (20, 7))
    assert "input diagonal adds branch cotangents" (inputCotangent == 17)
    let identity = identityReverseCircuit rationalSpace
    identityEvaluation <- requireRight "identity diagonal" (evaluateReverseCircuit (pairReverseCircuit rationalSpace identity identity) ((), ()) (9 :: Rational))
    (_, identityCotangent) <- requireRight "identity diagonal VJP" (applyReverseVJP identityEvaluation (1, 2))
    assert "identity diagonal did not add cotangents" (identityCotangent == 3)

exactParameterDiagonal :: IO ()
exactParameterDiagonal = do
    evaluation <- requireRight "parameter diagonal" (evaluateReverseCircuit (shareParametersReverseCircuit rationalSpace rationalScale rationalScale) 2 (3, 4))
    assert "parameter diagonal primal" (reversePrimalOutput evaluation == ((6, 8) :: (Rational, Rational)))
    (parameterCotangent, inputCotangents) <- requireRight "parameter diagonal VJP" (applyReverseVJP evaluation (5, 7))
    assert "parameter diagonal adds use cotangents" (parameterCotangent == 43)
    assert "parameter diagonal preserves input products" (inputCotangents == (10, 14))

exactTensor :: IO ()
exactTensor = do
    evaluation <- requireRight "reverse tensor" (evaluateReverseCircuit (tensorReverseCircuit rationalScale rationalShift) (2, 3) (4, 5))
    assert "tensor primal" (reversePrimalOutput evaluation == ((8, 8) :: (Rational, Rational)))
    (parameterCotangents, inputCotangents) <- requireRight "reverse tensor VJP" (applyReverseVJP evaluation (7, 11))
    assert "tensor parameter products" (parameterCotangents == (28, 11))
    assert "tensor input products" (inputCotangents == (14, 11))

finiteDifferenceComposition :: IO ()
finiteDifferenceComposition = do
    let circuit = composeReverseCircuit quadraticScale sineScale
        parameters = (0.7, -1.1)
        input = 0.4
        outputCotangent = 1.3
    evaluation <- requireRight "nonlinear reverse composition" (evaluateReverseCircuit circuit parameters input)
    ((firstParameterCotangent, secondParameterCotangent), inputCotangent) <- requireRight "nonlinear reverse composition VJP" (applyReverseVJP evaluation outputCotangent)
    numericalFirst <- centralDifference 1e-6 (\value -> objective circuit (value, snd parameters) input outputCotangent) (fst parameters)
    numericalSecond <- centralDifference 1e-6 (\value -> objective circuit (fst parameters, value) input outputCotangent) (snd parameters)
    numericalInput <- centralDifference 1e-6 (\value -> objective circuit parameters value outputCotangent) input
    assertCloseWith "first parameter finite difference" 2e-10 2e-8 numericalFirst firstParameterCotangent
    assertCloseWith "second parameter finite difference" 2e-10 2e-8 numericalSecond secondParameterCotangent
    assertCloseWith "input finite difference" 2e-10 2e-8 numericalInput inputCotangent

finiteDifferenceInputDiagonal :: IO ()
finiteDifferenceInputDiagonal = do
    let circuit = pairReverseCircuit doubleSpace quadraticScale sineScale
        parameters = (0.7, -1.1)
        input = 0.4
        outputCotangents = (1.3, -0.6)
    evaluation <- requireRight "nonlinear input diagonal" (evaluateReverseCircuit circuit parameters input)
    (_, inputCotangent) <- requireRight "nonlinear input diagonal VJP" (applyReverseVJP evaluation outputCotangents)
    numericalInput <- centralDifference 1e-6 (\value -> pairObjective circuit parameters value outputCotangents) input
    assertCloseWith "input diagonal finite difference" 2e-10 2e-8 numericalInput inputCotangent

failurePropagation :: IO ()
failurePropagation = do
    case evaluateReverseCircuit failingPrimitive () () of
        Left PrimitiveFailure -> pure ()
        result -> assert ("primitive failure was not preserved: " ++ describe result) False
    let diagonal = pairReverseCircuit failingSpace rationalScale rationalScale
    diagonalEvaluation <- requireRight "failing diagonal forward" (evaluateReverseCircuit diagonal (2, 3) 4)
    expectPrimitiveFailure "diagonal addition" CotangentAdditionFailure (applyReverseVJP diagonalEvaluation (5, 7))
    compositionEvaluation <- requireRight "failing composition forward" (evaluateReverseCircuit (composeReverseCircuit rationalScale failingPullback) (2, ()) 4)
    expectPrimitiveFailure "composition pullback" PrimitiveFailure (applyReverseVJP compositionEvaluation 5)
    tensorEvaluation <- requireRight "failing tensor forward" (evaluateReverseCircuit (tensorReverseCircuit failingPullback failingPullback) ((), ()) (2, 3))
    expectPrimitiveFailure "tensor pullback" PrimitiveFailure (applyReverseVJP tensorEvaluation (5, 7))
    sharedEvaluation <- requireRight "failing shared forward" (evaluateReverseCircuit (shareParametersReverseCircuit rationalSpace failingPullback failingPullback) () (2, 3))
    expectPrimitiveFailure "shared pullback" PrimitiveFailure (applyReverseVJP sharedEvaluation (5, 7))
  where
    describe (Left err) = show err
    describe (Right _) = "successful evaluation"

expectPrimitiveFailure :: (Show value) => String -> ReverseFixtureError -> Either ReverseFixtureError value -> IO ()
expectPrimitiveFailure _ expected (Left actual) | actual == expected = pure ()
expectPrimitiveFailure label _ result = fail (label ++ " failure was not preserved: " ++ show result)

rationalScale :: ScalarCircuit Rational
rationalScale =
    primitiveReverseCircuit rationalSpace rationalSpace rationalSpace $ \parameter input ->
        Right (reverseEvaluation (parameter * input) (\outputCotangent -> Right (outputCotangent * input, outputCotangent * parameter)))

rationalShift :: ScalarCircuit Rational
rationalShift =
    primitiveReverseCircuit rationalSpace rationalSpace rationalSpace $ \parameter input ->
        Right (reverseEvaluation (input + parameter) (\outputCotangent -> Right (outputCotangent, outputCotangent)))

quadraticScale :: ScalarCircuit Double
quadraticScale =
    primitiveReverseCircuit doubleSpace doubleSpace doubleSpace $ \parameter input ->
        Right
            ( reverseEvaluation
                (parameter * input * input)
                (\outputCotangent -> Right (outputCotangent * input * input, outputCotangent * 2 * parameter * input))
            )

sineScale :: ScalarCircuit Double
sineScale =
    primitiveReverseCircuit doubleSpace doubleSpace doubleSpace $ \parameter input ->
        Right
            ( reverseEvaluation
                (parameter * sin input)
                (\outputCotangent -> Right (outputCotangent * sin input, outputCotangent * parameter * cos input))
            )

rationalSpace :: CotangentSpace ReverseFixtureError Rational Rational
rationalSpace = cotangentSpace 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality

doubleSpace :: CotangentSpace ReverseFixtureError Double Double
doubleSpace = cotangentSpace 0 (\left right -> Right (left + right)) (\scalar value -> Right (scalar * value)) close (ApproximateCotangentEquality "absolute tolerance 1e-12")
  where
    close left right = abs (left - right) <= 1e-12

failingSpace :: CotangentSpace ReverseFixtureError Rational Rational
failingSpace = cotangentSpace 0 (\_ _ -> Left CotangentAdditionFailure) (\scalar value -> Right (scalar * value)) (==) ExactCotangentEquality

failingPrimitive :: ParametricReverseCircuit ReverseFixtureError Rational () () () () () ()
failingPrimitive = primitiveReverseCircuit unitRationalSpace unitRationalSpace unitRationalSpace (\() () -> Left PrimitiveFailure)

failingPullback :: ParametricReverseCircuit ReverseFixtureError Rational () Rational Rational Rational Rational Rational
failingPullback =
    primitiveReverseCircuit rationalSpace rationalSpace rationalSpace $ \() input ->
        Right (reverseEvaluation input (\_ -> Left PrimitiveFailure))

unitRationalSpace :: CotangentSpace ReverseFixtureError Rational ()
unitRationalSpace = cotangentSpace () (\() () -> Right ()) (\_ () -> Right ()) (\() () -> True) ExactCotangentEquality

objective ::
    ParametricReverseCircuit ReverseFixtureError Double parameter parameterCotangent Double Double Double Double ->
    parameter ->
    Double ->
    Double ->
    IO Double
objective circuit parameter input outputCotangent = do
    evaluation <- requireRight "finite-difference primal" (evaluateReverseCircuit circuit parameter input)
    pure (outputCotangent * reversePrimalOutput evaluation)

pairObjective ::
    ParametricReverseCircuit ReverseFixtureError Double parameter parameterCotangent Double Double (Double, Double) (Double, Double) ->
    parameter ->
    Double ->
    (Double, Double) ->
    IO Double
pairObjective circuit parameter input (leftCotangent, rightCotangent) = do
    evaluation <- requireRight "finite-difference pair primal" (evaluateReverseCircuit circuit parameter input)
    let (leftOutput, rightOutput) = reversePrimalOutput evaluation
    pure (leftCotangent * leftOutput + rightCotangent * rightOutput)
