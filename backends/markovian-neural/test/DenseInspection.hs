module DenseInspection (runTests) where

import Data.Maybe (isNothing)
import Markovian.Backend.Neural.Dense (
    DenseError (..),
    DenseNetwork,
    DensePatchError (..),
    HiddenActivation (..),
    denseForward,
    denseParameters,
    densePatchDonorInput,
    densePatchDonorOutput,
    densePatchEffectivePostactivation,
    densePatchHiddenLayer,
    densePatchOutputDelta,
    densePatchPatchedOutput,
    densePatchRecipientInput,
    densePatchRecipientOutput,
    densePatchRecipientPostactivation,
    densePatchReplacementValues,
    densePatchUnits,
    denseTraceLayerActivation,
    denseTraceLayerInput,
    denseTraceLayerPostactivation,
    denseTraceLayerPreactivation,
    denseTraceLayers,
    denseTraceOutput,
    mkDenseNetwork,
    patchDenseHidden,
    traceDense,
 )
import TestSupport (assert, assertVectorClose, requireRight)

runTests :: IO ()
runTests = do
    traceChecks
    patchChecks
    rejectionChecks
    putStrLn "PASS: dense inspection"

traceChecks :: IO ()
traceChecks = do
    linear <- requireRight "linear network" (mkDenseNetwork 2 [] 1 [2, -1, 0.5])
    linearTrace <- requireRight "linear trace" (traceDense linear [3, 4])
    linearOutput <- requireRight "linear forward" (denseForward linear [3, 4])
    assert "linear trace output" (denseTraceOutput linearTrace == linearOutput)
    case denseTraceLayers linearTrace of
        [layer] -> do
            assert "linear input" (denseTraceLayerInput layer == [3, 4])
            assert "linear preactivation" (denseTraceLayerPreactivation layer == [2.5])
            assert "linear postactivation" (denseTraceLayerPostactivation layer == [2.5])
            assert "linear activation marker" (isNothing (denseTraceLayerActivation layer))
        layers -> assert ("linear trace layer count: " ++ show (length layers)) False

    oneHidden <- requireRight "one-hidden network" (mkDenseNetwork 1 [1] 1 [2, 0.1, 3, -0.2])
    oneTrace <- requireRight "one-hidden trace" (traceDense oneHidden [0.5])
    oneOutput <- requireRight "one-hidden forward" (denseForward oneHidden [0.5])
    assert "one-hidden output" (denseTraceOutput oneTrace == oneOutput)
    case denseTraceLayers oneTrace of
        [hidden, output] -> do
            assert "one-hidden marker" (denseTraceLayerActivation hidden == Just Tanh)
            assert "one-hidden preactivation" (denseTraceLayerPreactivation hidden == [1.1])
            assertVectorClose "one-hidden postactivation" 1e-15 [tanh 1.1] (denseTraceLayerPostactivation hidden)
            assert "one-hidden continuity" (denseTraceLayerInput output == denseTraceLayerPostactivation hidden)
            assert "one-hidden output marker" (isNothing (denseTraceLayerActivation output))
        layers -> assert ("one-hidden trace layer count: " ++ show (length layers)) False

    network <- twoHiddenNetwork
    multiTrace <- requireRight "two-hidden trace" (traceDense network recipientInput)
    multiOutput <- requireRight "two-hidden forward" (denseForward network recipientInput)
    assert "two-hidden output" (denseTraceOutput multiTrace == multiOutput)
    case denseTraceLayers multiTrace of
        [first, second, output] -> do
            assert "first hidden input" (denseTraceLayerInput first == recipientInput)
            assert "first hidden preactivation" (denseTraceLayerPreactivation first == recipientInput)
            assertVectorClose "first hidden postactivation" 1e-15 (fmap tanh recipientInput) (denseTraceLayerPostactivation first)
            assert "first to second continuity" (denseTraceLayerInput second == denseTraceLayerPostactivation first)
            assert "second to output continuity" (denseTraceLayerInput output == denseTraceLayerPostactivation second)
            assert "two hidden markers" (fmap denseTraceLayerActivation [first, second, output] == [Just Tanh, Just Tanh, Nothing])
            assert "linear output pre equals post" (denseTraceLayerPreactivation output == denseTraceLayerPostactivation output)
        layers -> assert ("two-hidden trace layer count: " ++ show (length layers)) False

patchChecks :: IO ()
patchChecks = do
    network <- twoHiddenNetwork
    let originalParameters = denseParameters network
    recipientOutput <- requireRight "recipient baseline" (denseForward network recipientInput)
    donorOutput <- requireRight "donor baseline" (denseForward network donorInput)

    self <- requireRight "same-input patch" (patchDenseHidden network recipientInput recipientInput 0 [1])
    assert "same-input patch output" (densePatchPatchedOutput self == recipientOutput)
    assert "same-input delta" (densePatchOutputDelta self == [0])

    full <- requireRight "full donor patch" (patchDenseHidden network recipientInput donorInput 0 [1, 0])
    assert "full donor equivalence" (densePatchPatchedOutput full == donorOutput)
    assert "full donor baseline" (densePatchDonorOutput full == donorOutput)
    assert "full recipient baseline" (densePatchRecipientOutput full == recipientOutput)
    assertVectorClose "full patch delta" 1e-15 (zipWith (-) donorOutput recipientOutput) (densePatchOutputDelta full)

    partial <- requireRight "partial donor patch" (patchDenseHidden network recipientInput donorInput 0 [0])
    let recipientFirst = fmap tanh recipientInput
        donorFirstValue = tanh (-0.4)
        recipientSecondValue = tanh (-0.5)
        expectedEffective = [donorFirstValue, recipientSecondValue]
        expectedPatched =
            [ 3 * tanh (donorFirstValue + 2 * recipientSecondValue + 0.1)
                - 2 * tanh (-donorFirstValue + recipientSecondValue - 0.2)
                + 0.5
            ]
    assert "reported recipient input" (densePatchRecipientInput partial == recipientInput)
    assert "reported donor input" (densePatchDonorInput partial == donorInput)
    assert "reported hidden site" (densePatchHiddenLayer partial == 0 && densePatchUnits partial == [0])
    assertVectorClose "reported donor value" 1e-15 [donorFirstValue] (densePatchReplacementValues partial)
    assertVectorClose "reported recipient hidden values" 1e-15 recipientFirst (densePatchRecipientPostactivation partial)
    assertVectorClose "reported effective hidden values" 1e-15 expectedEffective (densePatchEffectivePostactivation partial)
    assertVectorClose "partial downstream result" 1e-15 expectedPatched (densePatchPatchedOutput partial)
    repeated <- requireRight "repeat partial patch" (patchDenseHidden network recipientInput donorInput 0 [0])
    assert "deterministic report" (partial == repeated)

    secondLayer <- requireRight "second hidden patch" (patchDenseHidden network recipientInput donorInput 1 [0, 1])
    assert "second full donor equivalence" (densePatchPatchedOutput secondLayer == donorOutput)
    secondSelf <- requireRight "second hidden self-patch" (patchDenseHidden network recipientInput recipientInput 1 [0])
    assert "second hidden self-patch identity" (densePatchPatchedOutput secondSelf == recipientOutput)
    assert "parameters unchanged" (denseParameters network == originalParameters)

    twoOutputs <- requireRight "two-output network" (mkDenseNetwork 1 [1] 2 [1, 0, 2, -3, 0, 0])
    vectorPatch <- requireRight "two-output patch" (patchDenseHidden twoOutputs [0] [1] 0 [0])
    assertVectorClose "two-output patched values" 0 [2 * tanh 1, -(3 * tanh 1)] (densePatchPatchedOutput vectorPatch)
    assertVectorClose "two-output deltas" 0 [2 * tanh 1, -(3 * tanh 1)] (densePatchOutputDelta vectorPatch)

rejectionChecks :: IO ()
rejectionChecks = do
    network <- twoHiddenNetwork
    linear <- requireRight "zero-hidden network" (mkDenseNetwork 1 [] 1 [1, 0])
    expectPatchError "zero-hidden site" (DensePatchInvalidHiddenLayer 0) (patchDenseHidden linear [1] [2] 0 [0])
    expectPatchError "negative layer" (DensePatchInvalidHiddenLayer (-1)) (patchDenseHidden network recipientInput donorInput (-1) [0])
    expectPatchError "out-of-range layer" (DensePatchInvalidHiddenLayer 2) (patchDenseHidden network recipientInput donorInput 2 [0])
    expectPatchError "empty selection" DensePatchEmptySelection (patchDenseHidden network recipientInput donorInput 0 [])
    expectPatchError "negative unit" (DensePatchInvalidUnit 0 (-1)) (patchDenseHidden network recipientInput donorInput 0 [-1])
    expectPatchError "out-of-range unit" (DensePatchInvalidUnit 0 2) (patchDenseHidden network recipientInput donorInput 0 [2])
    expectPatchError "duplicate unit" (DensePatchDuplicateUnit 1) (patchDenseHidden network recipientInput donorInput 0 [1, 0, 1])
    expectPatchError "recipient width" (DensePatchDenseFailure (DenseInputShapeMismatch 2 1)) (patchDenseHidden network [0] donorInput 0 [0])
    expectPatchError "donor width" (DensePatchDenseFailure (DenseInputShapeMismatch 2 1)) (patchDenseHidden network recipientInput [0] 0 [0])
    case traceDense network [0 / 0, 0] of
        Left (DenseNumericFailure _) -> pure ()
        result -> assert ("nonfinite trace input: " ++ show result) False
    case patchDenseHidden network recipientInput [0 / 0, 0] 0 [0] of
        Left (DensePatchDenseFailure (DenseNumericFailure _)) -> pure ()
        result -> assert ("nonfinite donor input: " ++ show result) False

    forwardOverflow <- requireRight "forward-overflow network" (mkDenseNetwork 1 [1] 1 [1e308, 0, 1, 0])
    case (denseForward forwardOverflow [2], traceDense forwardOverflow [2]) of
        (Left forwardError, Left traceError) -> assert "trace and forward share overflow failure" (traceError == forwardError)
        result -> assert ("trace and forward overflow mismatch: " ++ show result) False

    downstreamOverflow <- requireRight "downstream-overflow network" (mkDenseNetwork 2 [2] 1 [1, 0, 0, 1, 0, 0, 1e308, 1e308, 0])
    _ <- requireRight "downstream recipient baseline" (denseForward downstreamOverflow [2, -2])
    _ <- requireRight "downstream donor baseline" (denseForward downstreamOverflow [-2, 2])
    case patchDenseHidden downstreamOverflow [2, -2] [-2, 2] 0 [1] of
        Left (DensePatchDenseFailure (DenseNumericFailure _)) -> pure ()
        result -> assert ("patch downstream overflow: " ++ show result) False

    deltaOverflow <- requireRight "delta-overflow network" (mkDenseNetwork 1 [1] 1 [1, 0, 1.5e308, 0])
    _ <- requireRight "delta recipient baseline" (denseForward deltaOverflow [-2])
    _ <- requireRight "delta donor baseline" (denseForward deltaOverflow [2])
    case patchDenseHidden deltaOverflow [-2] [2] 0 [0] of
        Left (DensePatchDenseFailure (DenseNumericFailure _)) -> pure ()
        result -> assert ("unchecked output delta overflow: " ++ show result) False

expectPatchError :: (Show value) => String -> DensePatchError -> Either DensePatchError value -> IO ()
expectPatchError label expected result =
    case result of
        Left actual -> assert (label ++ ": expected " ++ show expected ++ ", got " ++ show actual) (actual == expected)
        Right value -> assert (label ++ ": unexpectedly accepted " ++ show value) False

recipientInput :: [Double]
recipientInput = [0.25, -0.5]

donorInput :: [Double]
donorInput = [-0.4, 0.7]

twoHiddenNetwork :: IO DenseNetwork
twoHiddenNetwork =
    requireRight
        "two-hidden network"
        (mkDenseNetwork 2 [2, 2] 1 [1, 0, 0, 1, 0, 0, 1, 2, -1, 1, 0.1, -0.2, 3, -2, 0.5])
