module BayesianExact (runBayesianExactTests) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Bayesian.Channel.Exact
import Markovian.Bayesian.Exact
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Probability.Exact (ExactDistributionError (..), ExactWeightError (..))

runBayesianExactTests :: (String -> IO () -> IO ()) -> IO ()
runBayesianExactTests run = do
    run "exact Bayesian construction, evidence, and support" testBayesianConstruction
    run "exact Bayesian inversion laws" testBayesianInversionLaws
    run "almost-sure equality and Bayesian channel composition" testAlmostSureAndChannels

data Source = SourceLeft | SourceRight | SourceDead | SourceOutside
    deriving (Eq, Show)

data Observation = ObservationA | ObservationB | ObservationNever | ObservationOutside
    deriving (Eq, Show)

data Result = ResultLeft | ResultRight
    deriving (Eq, Show)

assertB :: String -> Bool -> IO ()
assertB message predicate =
    if predicate then pure () else ioError (userError message)

requireRightB :: (Show error) => String -> Either error value -> IO value
requireRightB _ (Right value) = pure value
requireRightB label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

requireJustB :: String -> Maybe value -> IO value
requireJustB _ (Just value) = pure value
requireJustB label Nothing = ioError (userError (label ++ ": missing represented entry"))

nnB :: Rational -> NonNegativeRational
nnB value =
    case nonNegativeRational value of
        Right scalar -> scalar
        Left problem -> error ("invalid Bayesian test scalar: " ++ show problem)

objectB :: (Eq value, Show value) => [value] -> IO (FiniteObject value)
objectB values = requireRightB "finite Bayesian object" (finiteObject values)

stochasticB ::
    FiniteObject source ->
    FiniteObject target ->
    [[Rational]] ->
    IO (StochasticMatrix NonNegativeRational source target)
stochasticB source target rows = do
    raw <- requireRightB "Bayesian raw matrix" (matrixFromRows (forgetNonempty source) (forgetNonempty target) (map (map nnB) rows))
    requireRightB "Bayesian stochastic matrix" (stochasticMatrix raw)

fixture ::
    IO
        ( FiniteObject Source
        , FiniteObject Observation
        , FiniteObject Result
        , Prior Source
        , StochasticMatrix NonNegativeRational Source Observation
        , StochasticMatrix NonNegativeRational Observation Result
        )
fixture = do
    source <- objectB [SourceLeft, SourceRight, SourceDead]
    observation <- objectB [ObservationA, ObservationB, ObservationNever]
    result <- objectB [ResultLeft, ResultRight]
    sourcePrior <-
        requireRightB
            "source prior"
            ( prior
                source
                [ (SourceLeft, 1 % 4)
                , (SourceRight, 3 % 4)
                , (SourceDead, 0)
                ]
            )
    forward <-
        stochasticB
            source
            observation
            [ [1, 0, 0]
            , [1 % 3, 2 % 3, 0]
            , [0, 0, 1]
            ]
    second <-
        stochasticB
            observation
            result
            [ [1, 0]
            , [0, 1]
            , [1 % 2, 1 % 2]
            ]
    pure (source, observation, result, sourcePrior, forward, second)

testBayesianConstruction :: IO ()
testBayesianConstruction = do
    (source, _, _, sourcePrior, forward, _) <- fixture
    assertB
        "prior support must remove represented zero mass"
        (NonEmpty.toList (supportValues (priorSupport sourcePrior)) == [SourceLeft, SourceRight])
    assertB "prior mass changed" (priorRational sourcePrior SourceRight == 3 % 4)

    output <- requireRightB "Bayesian pushforward" (pushforward sourcePrior forward)
    assertB "pushforward A mass changed" (priorRational output ObservationA == 1 % 2)
    assertB "pushforward B mass changed" (priorRational output ObservationB == 1 % 2)
    assertB "pushforward zero mass changed" (priorRational output ObservationNever == 0)
    assertB "pushforward did not normalize" (sumPrior output == 1)
    assertB
        "pushforward support restriction changed"
        (NonEmpty.toList (supportValues (priorSupport output)) == [ObservationA, ObservationB])

    jointPrior <- requireRightB "Bayesian joint" (joint sourcePrior forward)
    assertB "joint left-A mass changed" (priorRational jointPrior (SourceLeft, ObservationA) == 1 % 4)
    assertB "joint right-A mass changed" (priorRational jointPrior (SourceRight, ObservationA) == 1 % 4)
    assertB "joint right-B mass changed" (priorRational jointPrior (SourceRight, ObservationB) == 1 % 2)
    assertB "joint did not normalize" (sumPrior jointPrior == 1)

    evidenceA <- requireRightB "observation evidence" (observationEvidence sourcePrior forward ObservationA)
    assertB "observation evidence changed" (getNonNegativeRational evidenceA == 1 % 2)
    posterior <- requireRightB "exact conditioning" (condition sourcePrior forward ObservationA)
    assertB "posterior left mass changed" (posteriorRational posterior SourceLeft == 1 % 2)
    assertB "posterior right mass changed" (posteriorRational posterior SourceRight == 1 % 2)
    assertB "posterior dead mass changed" (posteriorRational posterior SourceDead == 0)
    assertB
        "posterior support changed"
        (NonEmpty.toList (supportValues (posteriorSupport posterior)) == [SourceLeft, SourceRight])

    case condition sourcePrior forward ObservationNever of
        Left (ZeroEvidence ObservationNever) -> pure ()
        _ -> ioError (userError "represented zero evidence did not return ZeroEvidence")
    case condition sourcePrior forward ObservationOutside of
        Left (ObservationOutsideTarget ObservationOutside) -> pure ()
        _ -> ioError (userError "outside observation was not distinguished from zero evidence")

    case prior source [(SourceLeft, 1 % 2)] of
        Left (PriorMassNotOne total) -> assertB "prior error total changed" (total == 1 % 2)
        _ -> ioError (userError "unnormalized prior was accepted")
    case prior source [(SourceLeft, -1), (SourceRight, 2)] of
        Left (NegativePriorMass 0 (-1)) -> pure ()
        _ -> ioError (userError "negative prior mass was accepted")
    case prior source [(SourceLeft, 1), (SourceOutside, 0)] of
        Left (PriorValueOutsideObject SourceOutside) -> pure ()
        _ -> ioError (userError "prior accepted a label outside its finite object")
    case canonicalExactDistribution [('x', -1), ('x', 2)] of
        Left (InvalidExactWeight 0 (NegativeExactWeight (-1))) -> pure ()
        _ -> ioError (userError "canonical distribution hid a negative duplicate weight")
    case canonicalExactDistribution (replicate 4097 ('x', 1)) of
        Left (ExactSupportLimitExceeded 4096) -> pure ()
        result -> ioError (userError ("canonical duplicate support limit changed: " ++ show result))
    case canonicalExactDistribution (repeat ('x', 1)) of
        Left (ExactSupportLimitExceeded 4096) -> pure ()
        result -> ioError (userError ("infinite canonical duplicates did not terminate at the raw limit: " ++ show result))

testBayesianInversionLaws :: IO ()
testBayesianInversionLaws = do
    (source, _, _, sourcePrior, forward, second) <- fixture
    output <- requireRightB "inversion pushforward" (pushforward sourcePrior forward)
    inverse <- requireRightB "prior-indexed inverse" (bayesianInverse sourcePrior forward)
    disintegration <- requireRightB "support disintegration" (disintegrate sourcePrior forward)
    assertB
        "disintegration and inversion differ"
        (stochasticEquivalent (inverseMatrix inverse) (inverseMatrix disintegration))
    assertB
        "inverse source support changed"
        (finiteSetValues (stochasticTarget (inverseMatrix inverse)) == [SourceLeft, SourceRight])
    assertB
        "inverse target support changed"
        (finiteSetValues (stochasticSource (inverseMatrix inverse)) == [ObservationA, ObservationB])
    case stochasticMatrix (forgetStochastic (inverseMatrix inverse)) of
        Right _ -> pure ()
        Left problem -> ioError (userError ("Bayesian inverse was not normalized: " ++ show problem))
    case stochasticMatrix (transposeMatrix (forgetStochastic forward)) of
        Left _ -> pure ()
        Right _ -> ioError (userError "raw transpose was confused with normalized Bayesian inversion")

    let supportedInput = inverseInputPrior inverse
        supportedOutput = inverseOutputPrior inverse
        restrictedForward = inverseForwardRestriction inverse
        sourceValues = NonEmpty.toList (finiteObjectValues (priorObject supportedInput))
        outputValues = NonEmpty.toList (finiteObjectValues (priorObject supportedOutput))
    balanceEquations <-
        traverse
            (uncurry (bayesBalanceAt supportedInput restrictedForward supportedOutput (inverseMatrix inverse)))
            [(sourceValue, outputValue) | sourceValue <- sourceValues, outputValue <- outputValues]
    assertB "Bayes joint equation failed on positive supports" (and balanceEquations)

    identityInverse <- requireRightB "identity Bayesian inverse" (bayesianInverse sourcePrior (identityStochastic (forgetNonempty source)))
    assertB
        "identity Bayesian inversion failed on positive support"
        ( stochasticEquivalent
            (inverseMatrix identityInverse)
            (identityStochastic (forgetNonempty (supportObject (priorSupport sourcePrior))))
        )

    composite <- requireRightB "forward composition" (composeStochastic forward second)
    compositeInverse <- requireRightB "composite Bayesian inverse" (bayesianInverse sourcePrior composite)
    secondInverse <- requireRightB "second Bayesian inverse" (bayesianInverse output second)
    reversed <- requireRightB "reversed inverse composition" (composeStochastic (inverseMatrix secondInverse) (inverseMatrix inverse))
    assertB
        "Bayesian inversion did not reverse composition"
        (stochasticEquivalent (inverseMatrix compositeInverse) reversed)

    tensorInput <- requireRightB "tensor prior" (tensorPrior sourcePrior sourcePrior)
    let tensorForward = tensorStochastic forward forward
    tensorInverse <- requireRightB "tensor Bayesian inverse" (bayesianInverse tensorInput tensorForward)
    assertB
        "Bayesian inversion did not preserve independent tensor"
        ( stochasticEquivalent
            (inverseMatrix tensorInverse)
            (tensorStochastic (inverseMatrix inverse) (inverseMatrix inverse))
        )

    doubleInverse <-
        requireRightB
            "double Bayesian inverse"
            (bayesianInverse (inverseOutputPrior inverse) (inverseMatrix inverse))
    assertB
        "double Bayesian inversion did not recover the support restriction"
        ( stochasticEquivalent
            (inverseMatrix doubleInverse)
            (inverseForwardRestriction inverse)
        )

testAlmostSureAndChannels :: IO ()
testAlmostSureAndChannels = do
    (source, observation, _, sourcePrior, forward, second) <- fixture
    output <- requireRightB "almost-sure pushforward" (pushforward sourcePrior forward)
    firstExtension <-
        stochasticB
            observation
            source
            [ [1 % 2, 1 % 2, 0]
            , [0, 1, 0]
            , [1, 0, 0]
            ]
    secondExtension <-
        stochasticB
            observation
            source
            [ [1 % 2, 1 % 2, 0]
            , [0, 1, 0]
            , [0, 0, 1]
            ]
    equalAlmostSurely <- requireRightB "almost-sure inverse uniqueness" (almostSureEqual output firstExtension secondExtension)
    assertB "zero-output fillers must be equal almost surely" equalAlmostSurely
    assertB
        "almost-sure equality must not become extensional equality"
        (not (stochasticEquivalent firstExtension secondExtension))
    assertB
        "almost-sure equality must be reflexive"
        =<< requireRightB "almost-sure reflexivity" (almostSureEqual output firstExtension firstExtension)
    symmetric <- requireRightB "almost-sure symmetry" (almostSureEqual output secondExtension firstExtension)
    assertB "almost-sure equality must be symmetric" symmetric
    thirdExtension <-
        stochasticB
            observation
            source
            [ [1 % 2, 1 % 2, 0]
            , [0, 1, 0]
            , [0, 1, 0]
            ]
    secondThird <- requireRightB "almost-sure second-third comparison" (almostSureEqual output secondExtension thirdExtension)
    firstThird <- requireRightB "almost-sure transitive comparison" (almostSureEqual output firstExtension thirdExtension)
    assertB "almost-sure transitivity fixture failed" (secondThird && firstThird)

    postcomposedFirst <- requireRightB "first postcomposition" (composeStochastic firstExtension forward)
    postcomposedSecond <- requireRightB "second postcomposition" (composeStochastic secondExtension forward)
    preserved <- requireRightB "almost-sure postcomposition" (almostSureEqual output postcomposedFirst postcomposedSecond)
    assertB "postcomposition did not preserve almost-sure equality" preserved

    changedPositive <-
        stochasticB
            observation
            source
            [ [1, 0, 0]
            , [0, 1, 0]
            , [0, 0, 1]
            ]
    unequal <- requireRightB "almost-sure positive-row comparison" (almostSureEqual output firstExtension changedPositive)
    assertB "positive-support disagreement was ignored" (not unequal)

    firstChannel <- requireRightB "first Bayesian channel" (bayesianChannel sourcePrior forward)
    secondChannel <- requireRightB "second Bayesian channel" (bayesianChannel output second)
    composedChannel <- requireRightB "Bayesian channel composition" (composeBayesianChannel firstChannel secondChannel)
    directForward <- requireRightB "direct channel composition" (composeStochastic forward second)
    assertB
        "Bayesian channel composition changed the forward map"
        (stochasticEquivalent (bayesianChannelForward composedChannel) directForward)
    directOutput <- requireRightB "direct composed pushforward" (pushforward sourcePrior directForward)
    assertB
        "Bayesian channel composition changed prior flow"
        (priorEquivalent (bayesianChannelOutputPrior composedChannel) directOutput)
    firstChannelInverse <- requireRightB "first typed Bayesian channel inverse" (bayesianChannelInverse firstChannel)
    secondChannelInverse <- requireRightB "second typed Bayesian channel inverse" (bayesianChannelInverse secondChannel)
    composedChannelInverse <- requireRightB "composed typed Bayesian channel inverse" (bayesianChannelInverse composedChannel)
    directInverse <- requireRightB "direct Bayesian inverse" (bayesianInverse sourcePrior forward)
    assertB
        "typed Bayesian inverse changed"
        (stochasticEquivalent (inverseMatrix firstChannelInverse) (inverseMatrix directInverse))
    reversedChannelInverse <-
        requireRightB
            "reversed typed Bayesian channel inverses"
            (composeStochastic (inverseMatrix secondChannelInverse) (inverseMatrix firstChannelInverse))
    assertB
        "Bayesian channel composition did not reverse its backward maps"
        (stochasticEquivalent (inverseMatrix composedChannelInverse) reversedChannelInverse)

    mismatchedPrior <-
        requireRightB
            "mismatched middle prior"
            (prior observation [(ObservationA, 1), (ObservationB, 0), (ObservationNever, 0)])
    mismatchedSecond <- requireRightB "mismatched second Bayesian channel" (bayesianChannel mismatchedPrior second)
    case composeBayesianChannel firstChannel mismatchedSecond of
        Left BayesianChannelPriorMismatch -> pure ()
        _ -> ioError (userError "Bayesian channel composition accepted a mismatched middle prior")

    pure ()

priorScalar :: Prior value -> value -> NonNegativeRational
priorScalar sourcePrior value = fromMaybe zero (priorMass sourcePrior value)

priorRational :: Prior value -> value -> Rational
priorRational sourcePrior = getNonNegativeRational . priorScalar sourcePrior

posteriorRational :: Posterior value -> value -> Rational
posteriorRational posterior value =
    getNonNegativeRational (fromMaybe zero (posteriorMass posterior value))

sumPrior :: Prior value -> Rational
sumPrior sourcePrior =
    sum
        [ priorRational sourcePrior value
        | value <- NonEmpty.toList (finiteObjectValues (priorObject sourcePrior))
        ]

bayesBalanceAt ::
    Prior source ->
    StochasticMatrix NonNegativeRational source target ->
    Prior target ->
    StochasticMatrix NonNegativeRational target source ->
    source ->
    target ->
    IO Bool
bayesBalanceAt input forward output backward sourceValue targetValue = do
    inputMass <- requireJustB "supported input prior" (priorMass input sourceValue)
    forwardMass <- requireJustB "support-restricted forward channel" (matrixEntry (forgetStochastic forward) sourceValue targetValue)
    outputMass <- requireJustB "supported output prior" (priorMass output targetValue)
    backwardMass <- requireJustB "support-restricted Bayesian inverse" (matrixEntry (forgetStochastic backward) targetValue sourceValue)
    pure (inputMass `times` forwardMass == outputMass `times` backwardMass)
