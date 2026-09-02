{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (unless, void)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Markovian.Reverse.Program
import Markovian.Reverse.Program.Effect
import Markovian.Tensor
import Markovian.Tensor.Reverse.Program
import Numeric.Natural (Natural)

shape :: SShape '[2]
shape = SCons (Proxy @2) SNil

limits :: SessionLimits
limits = tensorSessionLimits 2 8 64 512 4096 32 4096

reverseLimitsFixture :: ReverseLimits
reverseLimitsFixture = reverseLimits 8 4 4 2 8 8

main :: IO ()
main = do
    result <- withTensorReverseExecutor limits $ \executor -> do
        parameterResult <- finiteTensorFor executor [1.5, -0.75]
        inputResult <- finiteTensorFor executor [0.2, -0.4]
        seedResult <- finiteTensorFor executor [1, -0.5]
        zeroResult <- tensorReverseZero executor shape
        case (parameterResult, inputResult, seedResult, zeroResult) of
            (Right parameter, Right input, Right seed, Right zero) -> do
                let program = composeProgram (tensorMultiplyProgram "weights" shape zero) (tensorTanhProgram shape zero)
                case prepareEffectReverseProgram reverseLimitsFixture (resolveTensorReversePrimitive executor) program of
                    Left problem -> pure (Left (TensorReverseActionException (show problem)))
                    Right prepared -> do
                        runResult <- runPreparedReverseM prepared (parameter, ()) input
                        case runResult of
                            Left problem -> pure (Left (TensorReverseActionException (show problem)))
                            Right run -> do
                                first <- applyReverseTapeM (effectReverseRunTape run) seed
                                second <- applyReverseTapeM (effectReverseRunTape run) seed
                                case (first, second) of
                                    (Right ((parameterGradient, ()), inputGradient, report), Right repeated) -> do
                                        outputValues <- tensorToList (hostTensor (effectReverseRunOutput run))
                                        parameterValues <- tensorToList (hostTensor parameterGradient)
                                        inputValues <- tensorToList (hostTensor inputGradient)
                                        assertApproxList "output" (map tanh [0.3, 0.3]) outputValues
                                        let derivative = 1 - tanh 0.3 * tanh 0.3
                                        assertApproxList "parameter VJP" [0.2 * derivative, 0.2 * derivative] parameterValues
                                        assertApproxList "input VJP" [1.5 * derivative, 0.375 * derivative] inputValues
                                        unless (effectPullbackCalls report == 2) (fail "tensor pullback report changed")
                                        unless (sameReports first repeated) (fail "repeated tensor tape changed deterministic report")
                                        let completeReportEvidence =
                                                renderReverseProgramReport (preparedEffectReverseProgramReport prepared)
                                                    ++ renderEffectReverseExecutionReport (effectReverseRunReport run)
                                                    ++ renderEffectReverseExecutionReport report
                                            reportChecksum = semanticChecksum completeReportEvidence
                                        unless (reportChecksum == 7629828571403724821) (fail ("tensor-reverse complete-report semantic checksum changed: " ++ show reportChecksum))
                                        pure (Right ())
                                    _ -> pure (Left (TensorReverseActionException "tensor pullback failed"))
            _ -> pure (Left (TensorReverseActionException "tensor fixture allocation failed"))
    case result of
        Left problem -> fail (show problem)
        Right () -> pure ()

    allocationFailure <- withTensorReverseExecutor (tensorSessionLimits 1 2 2 0 0 0 0) $ \executor -> fmap void (tensorReverseFromList executor shape [1, 2])
    case allocationFailure of
        Left (TensorReverseTensorFailure (TensorBudgetError (SinglePayloadLimitExceeded 0 16))) -> pure ()
        other -> fail ("tensor adapter allocation failure changed: " ++ showResult other)

    tensorEffectFaultBoundaries

    actionFailure <- withTensorReverseExecutor limits $ \executor -> do
        allocated <- tensorReverseFromList executor shape [1, 2]
        case allocated of
            Left problem -> pure (Left problem)
            Right _ -> error "injected tensor reverse action exception"
    case actionFailure of
        Left (TensorReverseActionException _) -> pure ()
        other -> fail ("tensor adapter action exception changed: " ++ showResult other)

    putStrLn "markovian-tensor-reverse: focused effect adapter passed"

tensorEffectFaultBoundaries :: IO ()
tensorEffectFaultBoundaries = do
    expectEffectFailure "forward allocation failure" 4 $ \executor -> do
        parameter <- requireTensor executor [1, 2]
        input <- requireTensor executor [3, 4]
        _seed <- requireTensor executor [1, 1]
        zero <- requireZero executor
        prepared <- requirePrepared executor (tensorMultiplyProgram "fault/forward" shape zero)
        result <- runPreparedReverseM prepared parameter input
        assertFailureContains "ReversePrimitiveForwardFailure" result

    expectEffectFailure "pullback allocation failure" 6 $ \executor -> do
        parameter <- requireTensor executor [1, 2]
        input <- requireTensor executor [3, 4]
        seed <- requireTensor executor [1, 1]
        zero <- requireZero executor
        prepared <- requirePrepared executor (composeProgram (tensorMultiplyProgram "fault/pullback" shape zero) (tensorTanhProgram shape zero))
        run <- requireEffectRun "pullback-failure forward" =<< runPreparedReverseM prepared (parameter, ()) input
        result <- applyReverseTapeM (effectReverseRunTape run) seed
        assertFailureContains "ReversePrimitivePullbackFailure" result

    expectEffectFailure "atomic binary pullback allocation failure" 7 $ \executor -> do
        parameter <- requireTensor executor [1, 2]
        input <- requireTensor executor [3, 4]
        seed <- requireTensor executor [1, 1]
        zero <- requireZero executor
        prepared <- requirePrepared executor (composeProgram (tensorMultiplyProgram "fault/binary-pullback" shape zero) (tensorTanhProgram shape zero))
        run <- requireEffectRun "binary-pullback forward" =<< runPreparedReverseM prepared (parameter, ()) input
        result <- applyReverseTapeM (effectReverseRunTape run) seed
        assertFailureContains "ReversePrimitivePullbackFailure" result

    expectEffectFailure "cotangent addition allocation failure" 7 $ \executor -> do
        input <- requireTensor executor [3, 4]
        seed <- requireTensor executor [1, 1]
        zero <- requireZero executor
        let program = pairInputProgram (tensorTanhProgram shape zero) (tensorTanhProgram shape zero)
        prepared <- requirePrepared executor program
        run <- requireEffectRun "addition-failure forward" =<< runPreparedReverseM prepared ((), ()) input
        result <- applyReverseTapeM (effectReverseRunTape run) (seed, seed)
        assertFailureContains "ReverseCotangentAdditionFailure" result
  where
    expectEffectFailure :: String -> Natural -> (forall region. TensorReverseExecutor region -> IO ()) -> IO ()
    expectEffectFailure label buffers action = do
        outcome <- withTensorReverseExecutor (tensorSessionLimits 2 8 64 512 4096 buffers 4096) $ \executor -> action executor >> pure (Right ())
        case outcome of
            Right () -> pure ()
            Left problem -> fail (label ++ " escaped its checked effect boundary: " ++ show problem)

    requireTensor executor values = do
        result <- tensorReverseFromList executor shape values
        case result of
            Right tensor -> pure tensor
            Left problem -> fail ("fault fixture allocation failed: " ++ show problem)

    requireZero executor = do
        result <- tensorReverseZero executor shape
        case result of
            Right zero -> pure zero
            Left problem -> fail ("fault fixture zero allocation failed: " ++ show problem)

    requirePrepared executor program =
        case prepareEffectReverseProgram reverseLimitsFixture (resolveTensorReversePrimitive executor) program of
            Right prepared -> pure prepared
            Left problem -> fail ("fault fixture preparation failed: " ++ show problem)

    requireEffectRun label result = case result of
        Right run -> pure run
        Left problem -> fail (label ++ ": " ++ show problem)

    assertFailureContains wanted result = case result of
        Left problem | wanted `isInfixOf` show problem && "BufferLimitExceeded" `isInfixOf` show problem -> pure ()
        other -> fail (wanted ++ " evidence changed: " ++ showResult other)

finiteTensorFor :: TensorReverseExecutor region -> [Double] -> IO (Either TensorReverseError (FiniteTensor region 'F64 '[2]))
finiteTensorFor executor = tensorReverseFromList executor shape

sameReports :: Either error (pc, xc, EffectReverseExecutionReport) -> (pc, xc, EffectReverseExecutionReport) -> Bool
sameReports first second = case first of
    Right (_, _, report) -> let (_, _, repeatedReport) = second in report == repeatedReport
    Left _ -> False

showResult :: (Show error) => Either error value -> String
showResult (Left problem) = "Left " ++ show problem
showResult (Right _) = "Right <value>"

semanticChecksum :: String -> Word64
semanticChecksum = foldl step 14695981039346656037
  where
    step checksum character = (checksum `xor` fromIntegral (ord character)) * 1099511628211

assertApproxList :: String -> [Double] -> [Double] -> IO ()
assertApproxList label expected actual = unless (length expected == length actual && and (zipWith close expected actual)) (fail (label ++ " changed"))
  where
    close left right = abs (left - right) <= 2e-9 + 2e-7 * max (abs left) (abs right)
