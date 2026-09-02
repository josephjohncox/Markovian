module ExactBind (runExactBindTests) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Markovian.Kernel.Exact
import Markovian.Probability.Exact
import Numeric.Natural (Natural)

runExactBindTests :: (String -> IO () -> IO ()) -> IO ()
runExactBindTests run = do
    run "exact distribution bounds finite and infinite inputs" testConstructorBounds
    run "exact distribution constructor work and rational limits are exact" testConstructorAccounting
    run "checked exact bind preserves duplicates and safe mapping" testDuplicates
    run "checked exact bind admits 64 by 64" testMaximumProduct
    run "checked exact bind stops at entry 4097" testOversizedProduct
    run "checked exact bind result limit is exact" testResultLimit
    run "checked exact bind work limit is exact" testWorkLimit
    run "checked exact bind rational limits are exact" testRationalLimits
    run "checked exact bind continuation failure is atomic" testContinuationFailure
    run "checked exact bind admitted semantic laws" testAdmittedLaws
    run "checked exact kernel admitted semantic laws" testKernelLaws

assertB :: String -> Bool -> IO ()
assertB message condition =
    if condition then pure () else ioError (userError message)

requireRightB :: (Show error) => String -> Either error value -> IO value
requireRightB _ (Right value) = pure value
requireRightB label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

uniform :: Int -> IO (ExactFiniteDist Int)
uniform count = requireRightB "uniform distribution" (exactFiniteDist [(value, 1) | value <- [1 .. count]])

limits :: Natural -> Natural -> Natural -> IO ExactBindLimits
limits result work rationalBits = limitsWith result work rationalBits rationalBits

limitsWith :: Natural -> Natural -> Natural -> Natural -> IO ExactBindLimits
limitsWith result work numeratorBits denominatorBits =
    requireRightB "exact bind limits" (exactBindLimits result work numeratorBits denominatorBits)

testConstructorBounds :: IO ()
testConstructorBounds = do
    atLimit <- uniform 4096
    assertB "constructor rejected its exact support bound" (NonEmpty.length (exactOutcomes atLimit) == 4096)
    case exactFiniteDist (repeat (0 :: Int, 1)) of
        Left (ExactSupportLimitExceeded 4096) -> pure ()
        Left problem -> ioError (userError ("infinite support returned the wrong error: " ++ show problem))
        Right _ -> ioError (userError "infinite support was admitted")
    case exactBindLimits 4097 1 1 1 of
        Left (ExactBindResultLimitAboveGlobalMaximum 4097) -> pure ()
        result -> ioError (userError ("bind limits bypassed the global support cap: " ++ show result))

testConstructorAccounting :: IO ()
testConstructorAccounting = do
    admitted <- requireRightB "constructor limits" (exactDistributionLimits 1 4 2 3)
    (_, report) <- requireRightB "constructor exact limits" (exactFiniteDistChecked admitted [("only", 3 % 5)])
    assertB "constructor work report changed" (exactDistributionWork report == 4)
    assertB "constructor numerator report changed" (exactDistributionMaximumNumeratorBits report == 2)
    assertB "constructor denominator report changed" (exactDistributionMaximumDenominatorBits report == 3)
    belowWork <- requireRightB "constructor below-work limits" (exactDistributionLimits 1 3 2 3)
    case exactFiniteDistChecked belowWork [("only", 3 % 5)] of
        Left (ExactDistributionWorkLimitExceeded 4) -> pure ()
        result -> ioError (userError ("constructor one-below work limit was not exact: " ++ show result))
    belowNumerator <- requireRightB "constructor below-numerator limits" (exactDistributionLimits 1 4 1 3)
    case exactFiniteDistChecked belowNumerator [("only", 3 % 5)] of
        Left (ExactDistributionNumeratorBitsExceeded 2) -> pure ()
        result -> ioError (userError ("constructor one-below numerator limit was not exact: " ++ show result))
    belowDenominator <- requireRightB "constructor below-denominator limits" (exactDistributionLimits 1 4 2 2)
    case exactFiniteDistChecked belowDenominator [("only", 3 % 5)] of
        Left (ExactDistributionDenominatorBitsExceeded 3) -> pure ()
        result -> ioError (userError ("constructor one-below denominator limit was not exact: " ++ show result))

testDuplicates :: IO ()
testDuplicates = do
    outer <- requireRightB "duplicate outer" (exactFiniteDist [(1 :: Int, 1), (1, 1)])
    inner <- requireRightB "duplicate inner" (exactFiniteDist [(7 :: Int, 1), (7, 3)])
    admitted <- limitsWith 4 12 2 4
    (result, report) <- requireRightB "duplicate bind" (bindExactFiniteDistChecked admitted outer (const (Right inner :: Either String (ExactFiniteDist Int))))
    let observed =
            [ (value, exactProbability mass)
            | (value, mass) <- NonEmpty.toList (exactOutcomes result)
            ]
        mapped = fmap (+ 1) result
    assertB
        "checked bind combined or reordered labeled duplicates"
        (observed == [(7, 1 % 8), (7, 3 % 8), (7, 1 % 8), (7, 3 % 8)])
    assertB "safe fmap changed support size" (NonEmpty.length (exactOutcomes mapped) == 4)
    assertB
        "safe fmap changed exact masses"
        ( fmap (exactProbability . snd) (NonEmpty.toList (exactOutcomes mapped))
            == [1 % 8, 3 % 8, 1 % 8, 3 % 8]
        )
    assertB "duplicate report changed" (exactBindOuterEntries report == 2)
    assertB "duplicate continuation report changed" (exactBindContinuationCalls report == 2)
    assertB "duplicate result report changed" (exactBindResultEntries report == 4)
    assertB "duplicate multiplication report changed" (exactBindMassMultiplications report == 4)
    assertB "duplicate numerator report changed" (exactBindMaximumNumeratorBits report == 2)
    assertB "duplicate denominator report changed" (exactBindMaximumDenominatorBits report == 4)

-- The operation charges 64 outer traversals, 64 calls, 4096 inner traversals,
-- and 4096 multiplications: 8320 units in total.
testMaximumProduct :: IO ()
testMaximumProduct = do
    outer <- uniform 64
    inner <- uniform 64
    admitted <- limits 4096 8320 13
    (result, report) <- requireRightB "64 by 64 bind" (bindExactFiniteDistChecked admitted outer (const (Right inner :: Either String (ExactFiniteDist Int))))
    assertB "64 by 64 did not produce 4096 entries" (NonEmpty.length (exactOutcomes result) == 4096)
    assertB "outer-entry report changed" (exactBindOuterEntries report == 64)
    assertB "continuation-call report changed" (exactBindContinuationCalls report == 64)
    assertB "result-entry report changed" (exactBindResultEntries report == 4096)
    assertB "multiplication report changed" (exactBindMassMultiplications report == 4096)
    assertB "numerator report changed" (exactBindMaximumNumeratorBits report == 1)
    assertB "denominator report changed" (exactBindMaximumDenominatorBits report == 13)

testOversizedProduct :: IO ()
testOversizedProduct = do
    outer <- uniform 65
    admitted <- limits 4096 10000 20
    case bindExactFiniteDistChecked admitted outer continuation of
        Left (ExactBindResultLimitExceeded 4097) -> pure ()
        result -> ioError (userError ("oversized bind did not stop at 4097: " ++ show result))
  where
    -- Entry 4097 is reached in row 64. Evaluating row 65 would prove that the
    -- complete 4225-entry product was traversed after rejection.
    continuation :: Int -> Either String (ExactFiniteDist Int)
    continuation 65 = error "checked bind evaluated the 65th continuation"
    continuation _ = Right inner
    inner =
        case exactFiniteDist [(value, 1) | value <- [1 :: Int .. 65]] of
            Right distribution -> distribution
            Left problem -> error (show problem)

testResultLimit :: IO ()
testResultLimit = do
    outer <- uniform 2
    inner <- uniform 2
    exactResult <- limits 4 12 3
    _ <- requireRightB "exact result limit" (bindExactFiniteDistChecked exactResult outer (const (Right inner :: Either String (ExactFiniteDist Int))))
    below <- limits 3 12 3
    case bindExactFiniteDistChecked below outer (const (Right inner :: Either String (ExactFiniteDist Int))) of
        Left (ExactBindResultLimitExceeded 4) -> pure ()
        result -> ioError (userError ("one-below result limit was not exact: " ++ show result))

testWorkLimit :: IO ()
testWorkLimit = do
    outer <- uniform 64
    inner <- uniform 64
    exactWork <- limits 4096 8320 13
    _ <- requireRightB "exact work limit" (bindExactFiniteDistChecked exactWork outer (const (Right inner :: Either String (ExactFiniteDist Int))))
    below <- limits 4096 8319 13
    case bindExactFiniteDistChecked below outer (const (Right inner :: Either String (ExactFiniteDist Int))) of
        Left (ExactBindWorkLimitExceeded 8320) -> pure ()
        result -> ioError (userError ("one-below work limit was not exact: " ++ show result))

testRationalLimits :: IO ()
testRationalLimits = do
    outer <- requireRightB "weighted outer" (exactFiniteDist [(1 :: Int, 3), (2, 1)])
    inner <- requireRightB "weighted inner" (exactFiniteDist [(3 :: Int, 3), (4, 1)])
    exactRational <- limitsWith 4 12 4 5
    (_, report) <- requireRightB "exact rational limits" (bindExactFiniteDistChecked exactRational outer (const (Right inner :: Either String (ExactFiniteDist Int))))
    assertB "exact numerator maximum changed" (exactBindMaximumNumeratorBits report == 4)
    assertB "exact denominator maximum changed" (exactBindMaximumDenominatorBits report == 5)
    belowNumerator <- limitsWith 4 12 3 5
    case bindExactFiniteDistChecked belowNumerator outer (const (Right inner :: Either String (ExactFiniteDist Int))) of
        Left (ExactBindNumeratorBitsExceeded 4) -> pure ()
        result -> ioError (userError ("one-below numerator limit was not exact: " ++ show result))
    belowDenominator <- limitsWith 4 12 4 4
    case bindExactFiniteDistChecked belowDenominator outer (const (Right inner :: Either String (ExactFiniteDist Int))) of
        Left (ExactBindDenominatorBitsExceeded 5) -> pure ()
        result -> ioError (userError ("one-below denominator limit was not exact: " ++ show result))

testContinuationFailure :: IO ()
testContinuationFailure = do
    outer <- uniform 3
    admitted <- limits 16 100 16
    case bindExactFiniteDistChecked admitted outer continuation of
        Left (ExactBindContinuationFailure 2 "second") -> pure ()
        result -> ioError (userError ("continuation failure was not atomic: " ++ show result))
  where
    continuation 1 = Right (exactDirac (1 :: Int))
    continuation 2 = Left "second"
    continuation _ = error "checked bind continued after a continuation failure"

testAdmittedLaws :: IO ()
testAdmittedLaws = do
    distribution <- requireRightB "law distribution" (exactFiniteDist [(1 :: Int, 1), (2, 2)])
    admitted <- limits 64 1000 64
    (leftIdentity, _) <- requireRightB "left identity" (bindExactFiniteDistChecked admitted (exactDirac (2 :: Int)) (succeed . continuation))
    assertB "admitted left identity changed" (leftIdentity == continuation 2)
    (rightIdentity, _) <- requireRightB "right identity" (bindExactFiniteDistChecked admitted distribution (succeed . exactDirac))
    assertB "admitted right identity changed" (rightIdentity == distribution)
    (first, _) <- requireRightB "first association inner" (bindExactFiniteDistChecked admitted distribution (succeed . continuation))
    (leftAssociated, _) <- requireRightB "left association" (bindExactFiniteDistChecked admitted first (succeed . finalStep))
    (rightAssociated, _) <- requireRightB "right association" (bindExactFiniteDistChecked admitted distribution rightBranch)
    assertB "admitted associativity changed semantic result" (leftAssociated == rightAssociated)
  where
    continuation value =
        case exactFiniteDist [(value, 1), (value + 1, 1)] of
            Right distribution -> distribution
            Left problem -> error (show problem)
    finalStep value = exactDirac (value * 3)
    rightBranch value =
        fst <$> bindExactFiniteDistChecked admitted (continuation value) (succeed . finalStep)
      where
        admitted =
            case exactBindLimits 64 1000 1 64 of
                Right checked -> checked
                Left problem -> error (show problem)

testKernelLaws :: IO ()
testKernelLaws = do
    admitted <- limits 64 1000 64
    coin <- requireRightB "kernel coin" (exactFiniteDist [(0 :: Int, 1), (1, 1)])
    weighted <- requireRightB "kernel weighted" (exactFiniteDist [(2 :: Int, 1), (3, 2)])
    let first = exactKernel (\input -> fmap (+ input) coin)
        second = exactKernel (\input -> fmap (* input) weighted)
        third = exactKernel (\input -> fmap (subtract input) coin)
        identityKernel = exactDeterministic id
        run kernel input = requireRightB "checked kernel run" (runExactKernel kernel input)
    direct <- run first 4
    leftIdentity <- run (composeExactKernel admitted identityKernel first) 4
    rightIdentity <- run (composeExactKernel admitted first identityKernel) 4
    assertB "checked kernel left identity changed" (leftIdentity == direct)
    assertB "checked kernel right identity changed" (rightIdentity == direct)
    leftAssociated <- run (composeExactKernel admitted (composeExactKernel admitted first second) third) 4
    rightAssociated <- run (composeExactKernel admitted first (composeExactKernel admitted second third)) 4
    assertB "checked kernel admitted associativity changed" (leftAssociated == rightAssociated)

succeed :: value -> Either String value
succeed = Right
