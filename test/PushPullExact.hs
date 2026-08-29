module PushPullExact (runPushPullExactTests) where

import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Bayesian.Exact (Prior, prior, priorStochastic, pushforward)
import Markovian.Category.Finite.Object (FiniteObject, finiteObject, forgetNonempty)
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix (matrixFromRows)
import Markovian.Category.Matrix.Stochastic
import Markovian.Category.Payoff.Exact

runPushPullExactTests :: (String -> IO () -> IO ()) -> IO ()
runPushPullExactTests run = do
    run "checked exact finite payoff tables" testPayoffConstruction
    run "exact payoff pullback laws" testPullbackLaws
    run "exact state-payoff pairing law" testStatePayoffPairing
    run "push-pull layout and failure boundaries" testLayoutsAndFailures

data Source = SourceLeft | SourceRight | SourceOutside
    deriving (Eq, Show)

data Target = TargetFirst | TargetSecond | TargetThird | TargetOutside
    deriving (Eq, Show)

data Result = ResultGood | ResultBad
    deriving (Eq, Show)

assertP :: String -> Bool -> IO ()
assertP message condition =
    if condition then pure () else ioError (userError message)

requireRightP :: (Show error) => String -> Either error value -> IO value
requireRightP _ (Right value) = pure value
requireRightP label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

nnP :: Rational -> NonNegativeRational
nnP value =
    case nonNegativeRational value of
        Right scalar -> scalar
        Left problem -> error ("invalid push-pull test scalar: " ++ show problem)

setP :: (Eq value, Show value) => [value] -> IO (FiniteSet value)
setP values = requireRightP "push-pull finite set" (finiteSet values)

objectP :: (Eq value, Show value) => [value] -> IO (FiniteObject value)
objectP values = requireRightP "push-pull finite object" (finiteObject values)

stochasticP ::
    FiniteSet source ->
    FiniteSet target ->
    [[Rational]] ->
    IO (StochasticMatrix NonNegativeRational source target)
stochasticP source target rows = do
    raw <- requireRightP "push-pull raw matrix" (matrixFromRows source target (map (map nnP) rows))
    requireRightP "push-pull stochastic matrix" (stochasticMatrix raw)

fixture ::
    IO
        ( FiniteObject Source
        , FiniteObject Target
        , Prior Source
        , StochasticMatrix NonNegativeRational Source Target
        , ExactPayoff Target
        )
fixture = do
    source <- objectP [SourceLeft, SourceRight]
    target <- objectP [TargetFirst, TargetSecond, TargetThird]
    sourceState <-
        requireRightP
            "push-pull source state"
            (prior source [(SourceLeft, 1 % 4), (SourceRight, 3 % 4)])
    channel <-
        stochasticP
            (forgetNonempty source)
            (forgetNonempty target)
            [ [1 % 2, 1 % 2, 0]
            , [0, 1 % 3, 2 % 3]
            ]
    payoff <-
        requireRightP
            "push-pull target payoff"
            ( exactPayoff
                (forgetNonempty target)
                [ (TargetThird, 5)
                , (TargetFirst, 4)
                , (TargetSecond, -2)
                ]
            )
    pure (source, target, sourceState, channel, payoff)

testPayoffConstruction :: IO ()
testPayoffConstruction = do
    target <- setP [TargetFirst, TargetSecond]
    payoff <-
        requireRightP
            "checked payoff"
            (exactPayoff target [(TargetSecond, -(7 % 3)), (TargetFirst, 5 % 2)])
    assertP
        "payoff table did not canonicalize object layout"
        (exactPayoffValues payoff == [(TargetFirst, 5 % 2), (TargetSecond, -(7 % 3))])
    assertP "signed exact payoff changed" (exactPayoffValue payoff TargetSecond == Just (-(7 % 3)))
    assertP "outside payoff lookup did not fail" (isNothing (exactPayoffValue payoff TargetOutside))
    case exactPayoff target [(TargetFirst, 1), (TargetFirst, 2), (TargetSecond, 3)] of
        Left (DuplicatePayoffValue TargetFirst) -> pure ()
        result -> ioError (userError ("duplicate payoff entry accepted: " ++ showResult result))
    case exactPayoff target [(TargetFirst, 1), (TargetOutside, 2)] of
        Left (PayoffValueOutsideObject TargetOutside) -> pure ()
        result -> ioError (userError ("outside payoff entry accepted: " ++ showResult result))
    case exactPayoff target [(TargetFirst, 1)] of
        Left (MissingPayoffValue TargetSecond) -> pure ()
        result -> ioError (userError ("incomplete payoff table accepted: " ++ showResult result))
    empty <- setP ([] :: [Target])
    emptyPayoff <- requireRightP "empty payoff" (exactPayoff empty [])
    assertP "empty finite payoff changed" (null (exactPayoffValues emptyPayoff))
  where
    showResult (Left problem) = show problem
    showResult (Right _) = "Right ExactPayoff"

testPullbackLaws :: IO ()
testPullbackLaws = do
    (source, target, _, channel, payoff) <- fixture
    pulled <- requireRightP "hand-calculated payoff pullback" (pullbackPayoff channel payoff)
    assertP "first conditional payoff changed" (exactPayoffValue pulled SourceLeft == Just 1)
    assertP "second conditional payoff changed" (exactPayoffValue pulled SourceRight == Just (8 % 3))

    identityPull <-
        requireRightP
            "identity payoff pullback"
            (pullbackPayoff (identityStochastic (forgetNonempty target)) payoff)
    assertP "identity payoff pullback failed" (exactPayoffEquivalent identityPull payoff)

    result <- setP [ResultGood, ResultBad]
    second <-
        stochasticP
            (forgetNonempty target)
            result
            [ [1, 0]
            , [0, 1]
            , [1 % 4, 3 % 4]
            ]
    resultPayoff <-
        requireRightP
            "result payoff"
            (exactPayoff result [(ResultGood, 6), (ResultBad, -2)])
    composite <- requireRightP "channel composition" (composeStochastic channel second)
    direct <- requireRightP "composite payoff pullback" (pullbackPayoff composite resultPayoff)
    throughSecond <- requireRightP "second payoff pullback" (pullbackPayoff second resultPayoff)
    reversed <- requireRightP "reversed payoff pullback" (pullbackPayoff channel throughSecond)
    assertP
        "payoff pullback did not reverse channel composition"
        (exactPayoffEquivalent direct reversed)

    emptySource <- setP ([] :: [Source])
    emptyTarget <- setP ([] :: [Target])
    emptyChannel <- stochasticP emptySource emptyTarget []
    emptyPayoff <- requireRightP "empty target payoff" (exactPayoff emptyTarget [])
    emptyPulled <- requireRightP "empty payoff pullback" (pullbackPayoff emptyChannel emptyPayoff)
    assertP "empty payoff pullback changed" (null (exactPayoffValues emptyPulled))
    assertP
        "pullback source object changed"
        (sameFiniteSet (exactPayoffObject pulled) (forgetNonempty source))

testStatePayoffPairing :: IO ()
testStatePayoffPairing = do
    (_, _, sourceState, channel, payoff) <- fixture
    outputState <- requireRightP "pairing-law state pushforward" (pushforward sourceState channel)
    leftPairing <-
        requireRightP
            "output state-payoff pairing"
            (pairStatePayoff (priorStochastic outputState) payoff)
    pulled <- requireRightP "pairing-law payoff pullback" (pullbackPayoff channel payoff)
    rightPairing <-
        requireRightP
            "input state-payoff pairing"
            (pairStatePayoff (priorStochastic sourceState) pulled)
    assertP "hand-calculated state-payoff expectation changed" (leftPairing == 9 % 4)
    assertP
        "exact state-payoff pairing law failed"
        (leftPairing == rightPairing)

testLayoutsAndFailures :: IO ()
testLayoutsAndFailures = do
    (source, _, sourceState, channel, payoff) <- fixture
    reorderedTarget <- setP [TargetThird, TargetFirst, TargetSecond]
    reorderedChannel <-
        stochasticP
            (forgetNonempty source)
            reorderedTarget
            [ [0, 1 % 2, 1 % 2]
            , [2 % 3, 0, 1 % 3]
            ]
    reorderedPayoff <-
        requireRightP
            "reordered payoff"
            ( exactPayoff
                reorderedTarget
                [ (TargetSecond, -2)
                , (TargetThird, 5)
                , (TargetFirst, 4)
                ]
            )
    reorderedOutput <- requireRightP "reordered pushforward" (pushforward sourceState reorderedChannel)
    reorderedLeft <-
        requireRightP
            "reordered output pairing"
            (pairStatePayoff (priorStochastic reorderedOutput) reorderedPayoff)
    reorderedPulled <- requireRightP "reordered pullback" (pullbackPayoff reorderedChannel reorderedPayoff)
    reorderedRight <-
        requireRightP
            "reordered input pairing"
            (pairStatePayoff (priorStochastic sourceState) reorderedPulled)
    assertP "layout changed the exact output pairing" (reorderedLeft == 9 % 4)
    assertP "layout changed the exact pairing law" (reorderedLeft == reorderedRight)
    originalPulled <- requireRightP "original-layout pullback" (pullbackPayoff channel payoff)
    assertP
        "layout changed the pulled labelled payoff"
        (exactPayoffEquivalent reorderedPulled originalPulled)

    wrongTarget <- setP [TargetFirst, TargetSecond]
    wrongTargetPayoff <-
        requireRightP
            "wrong-target payoff"
            (exactPayoff wrongTarget [(TargetFirst, 1), (TargetSecond, 0)])
    case pullbackPayoff reorderedChannel wrongTargetPayoff of
        Left PayoffPullbackTargetMismatch -> pure ()
        _ -> ioError (userError "payoff pullback accepted a mismatched target object")
    wrongSource <- setP [SourceLeft]
    wrongSourcePayoff <- requireRightP "wrong-state payoff" (exactPayoff wrongSource [(SourceLeft, 1)])
    case pairStatePayoff (priorStochastic sourceState) wrongSourcePayoff of
        Left StatePayoffObjectMismatch -> pure ()
        _ -> ioError (userError "state-payoff pairing accepted mismatched objects")

    emptyUnit <- setP ([] :: [()])
    sourceSet <- setP [SourceLeft, SourceRight]
    vacuousState <- stochasticP emptyUnit sourceSet []
    sourcePayoff <-
        requireRightP
            "source payoff"
            (exactPayoff sourceSet [(SourceLeft, 0), (SourceRight, 1)])
    case pairStatePayoff vacuousState sourcePayoff of
        Left StatePayoffSourceNotSingleton -> pure ()
        _ -> ioError (userError "state-payoff pairing accepted a vacuous state source")
