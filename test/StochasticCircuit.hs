{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module StochasticCircuit (runStochasticCircuitTests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Backend.CPU.Exact
import Markovian.Category.Finite.Exact
import Markovian.Category.Finite.Object (forgetNonempty)
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Circuit.Compile.Deterministic
import Markovian.Circuit.Interpret.Approximate
import Markovian.Circuit.Interpret.Exact
import Markovian.Probability.Exact

runStochasticCircuitTests :: (String -> IO () -> IO ()) -> IO ()
runStochasticCircuitTests run = do
    run "raw circuit purity, sharing, and deterministic copy optimization" testPurityAndSharing
    run "exact circuit algebra and basic structural laws" testHomomorphismAndCoherence
    run "exact symmetric-monoidal coherence fixtures" testMonoidalCoherence
    run "exact circuit convex choice and approximation boundary" testConvexAndApproximation
    run "exact circuit and legacy IR differential lowering" testLegacyDifferential
    run "first-order deterministic categorical compilation" testDeterministicCompilation
    run "circuit and compiler validation boundaries" testConstructionErrors

data CircuitPrimitive purity source target where
    FairCoin :: CircuitPrimitive 'Stochastic () Bool
    BooleanNot :: CircuitPrimitive 'Deterministic Bool Bool

data PrimitiveError = PrimitiveInvariant
    deriving (Eq, Show)

assertC :: String -> Bool -> IO ()
assertC message predicate =
    if predicate then pure () else ioError (userError message)

requireRightC :: (Show error) => String -> Either error value -> IO value
requireRightC _ (Right value) = pure value
requireRightC label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

setC :: (Eq value, Show value) => [value] -> IO (FiniteSet value)
setC values = requireRightC "finite circuit set" (finiteSet values)

objectC :: (Eq value, Show value) => [value] -> IO (FiniteObject value)
objectC values = requireRightC "finite circuit object" (finiteObject values)

nnC :: Rational -> NonNegativeRational
nnC value =
    case nonNegativeRational value of
        Right scalar -> scalar
        Left problem -> error ("invalid circuit test scalar: " ++ show problem)

primitiveInterpreter :: ExactPrimitiveInterpreter CircuitPrimitive PrimitiveError
primitiveInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = interpretDeterministicC
        , interpretStochasticPrimitive = interpretStochasticC
        }

interpretDeterministicC ::
    FiniteSet source ->
    FiniteSet target ->
    CircuitPrimitive 'Deterministic source target ->
    Either PrimitiveError (DeterministicMatrix NonNegativeRational source target)
interpretDeterministicC source target BooleanNot =
    case deterministicFromFunction source target not of
        Left _ -> Left PrimitiveInvariant
        Right arrow -> Right arrow

interpretStochasticC ::
    FiniteSet source ->
    FiniteSet target ->
    CircuitPrimitive 'Stochastic source target ->
    Either PrimitiveError (StochasticMatrix NonNegativeRational source target)
interpretStochasticC source target FairCoin = do
    raw <-
        case matrixFromRows source target [[nnC (1 % 2), nnC (1 % 2)]] of
            Left _ -> Left PrimitiveInvariant
            Right matrix -> Right matrix
    case stochasticMatrix raw of
        Left _ -> Left PrimitiveInvariant
        Right arrow -> Right arrow

failingInterpreter :: ExactPrimitiveInterpreter CircuitPrimitive PrimitiveError
failingInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \_ _ _ -> Left PrimitiveInvariant
        , interpretStochasticPrimitive = \_ _ _ -> Left PrimitiveInvariant
        }

reorderedInterpreter :: ExactPrimitiveInterpreter CircuitPrimitive PrimitiveError
reorderedInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = interpretReorderedDeterministic
        , interpretStochasticPrimitive = interpretReorderedStochastic
        }

interpretReorderedDeterministic ::
    FiniteSet source ->
    FiniteSet target ->
    CircuitPrimitive 'Deterministic source target ->
    Either PrimitiveError (DeterministicMatrix NonNegativeRational source target)
interpretReorderedDeterministic _ target BooleanNot = do
    reordered <-
        case finiteSet [True, False] of
            Left _ -> Left PrimitiveInvariant
            Right object -> Right object
    case deterministicFromFunction reordered target not of
        Left _ -> Left PrimitiveInvariant
        Right arrow -> Right arrow

interpretReorderedStochastic ::
    FiniteSet source ->
    FiniteSet target ->
    CircuitPrimitive 'Stochastic source target ->
    Either PrimitiveError (StochasticMatrix NonNegativeRational source target)
interpretReorderedStochastic source _ FairCoin = do
    reordered <-
        case finiteSet [True, False] of
            Left _ -> Left PrimitiveInvariant
            Right object -> Right object
    raw <-
        case matrixFromRows source reordered [[nnC (1 % 2), nnC (1 % 2)]] of
            Left _ -> Left PrimitiveInvariant
            Right matrix -> Right matrix
    case stochasticMatrix raw of
        Left _ -> Left PrimitiveInvariant
        Right arrow -> Right arrow

emptyInterpreter :: ExactPrimitiveInterpreter EmptyPrimitive ()
emptyInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \_ _ primitive -> case primitive of {}
        , interpretStochasticPrimitive = \_ _ primitive -> case primitive of {}
        }

testPurityAndSharing :: IO ()
testPurityAndSharing = do
    unit <- setC [()]
    boolean <- setC [False, True]
    let coin = stochasticPrimitive unit boolean FairCoin
        negation = deterministicPrimitive boolean boolean BooleanNot
        shared = shareCircuit coin
    independent <- requireRightC "independent circuit fanout" (fanoutCircuit coin coin)
    assertC "stochastic primitive lost provenance" (isStochastic (circuitPurity coin))
    assertC "share changed stochastic provenance" (isStochastic (circuitPurity shared))

    sharedMatrix <- requireRightC "shared circuit matrix" (interpretExactCircuit primitiveInterpreter shared)
    independentMatrix <- requireRightC "independent circuit matrix" (interpretExactCircuit primitiveInterpreter independent)
    explicitShared <- requireRightC "explicit share expansion" (composeCircuit coin (copyCircuit boolean))
    explicitFanout <-
        requireRightC
            "explicit fanout expansion"
            (composeCircuit (copyCircuit unit) (tensorCircuit coin coin))
    explicitSharedMatrix <- requireRightC "explicit share matrix" (interpretExactCircuit primitiveInterpreter explicitShared)
    explicitFanoutMatrix <- requireRightC "explicit fanout matrix" (interpretExactCircuit primitiveInterpreter explicitFanout)
    assertC "share did not derive from composition and copy" (stochasticEquivalent sharedMatrix explicitSharedMatrix)
    assertC "fanout did not derive from copy, tensor, and composition" (stochasticEquivalent independentMatrix explicitFanoutMatrix)
    assertC "shared draw lost false diagonal" (entryR sharedMatrix () (False, False) == 1 % 2)
    assertC "shared draw lost true diagonal" (entryR sharedMatrix () (True, True) == 1 % 2)
    assertC "shared draw invented off-diagonal mass" (entryR sharedMatrix () (False, True) == 0)
    assertC "independent draw did not have product mass" (entryR independentMatrix () (False, True) == 1 % 4)
    assertC "sharing and duplication were conflated" (not (stochasticEquivalent sharedMatrix independentMatrix))

    let sharedNegation = shareCircuit negation
        optimizedNegation = copyNaturalDeterministic negation
    sharedNegationMatrix <- requireRightC "shared deterministic circuit" (interpretExactCircuit primitiveInterpreter sharedNegation)
    optimizedNegationMatrix <- requireRightC "copy-natural deterministic circuit" (interpretExactCircuit primitiveInterpreter optimizedNegation)
    assertC
        "deterministic copy-naturality optimization changed denotation"
        (stochasticEquivalent sharedNegationMatrix optimizedNegationMatrix)

    table <-
        requireRightC
            "validated deterministic table circuit"
            (deterministicTable boolean boolean [(False, True), (True, False)])
    tableMatrix <- requireRightC "table circuit interpretation" (interpretDeterministicCircuit primitiveInterpreter table)
    primitiveMatrix <- requireRightC "deterministic primitive interpretation" (interpretDeterministicCircuit primitiveInterpreter negation)
    assertC "quoted table and deterministic primitive differ" (deterministicEquivalent tableMatrix primitiveMatrix)
    case deterministicTable boolean boolean [(False, True)] of
        Left CircuitDeterministicTableDomainMismatch -> pure ()
        _ -> ioError (userError "partial deterministic circuit table was accepted")

testHomomorphismAndCoherence :: IO ()
testHomomorphismAndCoherence = do
    unit <- setC [()]
    boolean <- setC [False, True]
    let coin = stochasticPrimitive unit boolean FairCoin
        negation = deterministicPrimitive boolean boolean BooleanNot
    leftIdentity <- requireRightC "circuit left identity" (composeCircuit (identityCircuit unit) coin)
    rightIdentity <- requireRightC "circuit right identity" (composeCircuit coin (identityCircuit boolean))
    coinMatrix <- requireRightC "direct coin matrix" (interpretExactCircuit primitiveInterpreter coin)
    leftMatrix <- requireRightC "left identity matrix" (interpretExactCircuit primitiveInterpreter leftIdentity)
    rightMatrix <- requireRightC "right identity matrix" (interpretExactCircuit primitiveInterpreter rightIdentity)
    assertC "exact fold did not preserve left identity" (stochasticEquivalent coinMatrix leftMatrix)
    assertC "exact fold did not preserve right identity" (stochasticEquivalent coinMatrix rightMatrix)

    coinThenNot <- requireRightC "coin then not circuit" (composeCircuit coin negation)
    leftAssociatedPrefix <- requireRightC "left associated prefix" (composeCircuit coinThenNot negation)
    rightAssociatedSuffix <- requireRightC "right associated suffix" (composeCircuit negation negation)
    rightAssociated <- requireRightC "right associated circuit" (composeCircuit coin rightAssociatedSuffix)
    leftAssociatedMatrix <- requireRightC "left associated matrix" (interpretExactCircuit primitiveInterpreter leftAssociatedPrefix)
    rightAssociatedMatrix <- requireRightC "right associated matrix" (interpretExactCircuit primitiveInterpreter rightAssociated)
    assertC "exact fold did not preserve composition associativity" (stochasticEquivalent leftAssociatedMatrix rightAssociatedMatrix)

    let swapTwiceFirst = symmetryCircuit boolean boolean
    swapTwice <- requireRightC "symmetry involution circuit" (composeCircuit swapTwiceFirst (symmetryCircuit boolean boolean))
    swapMatrix <- requireRightC "symmetry involution matrix" (interpretExactCircuit primitiveInterpreter swapTwice)
    identityPairMatrix <- requireRightC "pair identity matrix" (interpretExactCircuit primitiveInterpreter (identityCircuit (productSetC boolean boolean)))
    assertC "symmetry coherence failed" (stochasticEquivalent swapMatrix identityPairMatrix)

    associatorRoundTrip <-
        requireRightC
            "associator round trip"
            (composeCircuit (associateCircuit boolean boolean boolean) (unassociateCircuit boolean boolean boolean))
    associatorMatrix <- requireRightC "associator matrix" (interpretExactCircuit primitiveInterpreter associatorRoundTrip)
    nestedIdentity <- requireRightC "nested identity matrix" (interpretExactCircuit primitiveInterpreter (identityCircuit (productSetC (productSetC boolean boolean) boolean)))
    assertC "associator coherence failed" (stochasticEquivalent associatorMatrix nestedIdentity)

    leftUnitorRoundTrip <- requireRightC "left unitor round trip" (composeCircuit (leftUnitorInverseCircuit boolean) (leftUnitorCircuit boolean))
    leftUnitorMatrix <- requireRightC "left unitor matrix" (interpretExactCircuit primitiveInterpreter leftUnitorRoundTrip)
    booleanIdentity <- requireRightC "boolean identity matrix" (interpretExactCircuit primitiveInterpreter (identityCircuit boolean))
    assertC "left unitor coherence failed" (stochasticEquivalent leftUnitorMatrix booleanIdentity)

    copyThenSwap <- requireRightC "copy then symmetry" (composeCircuit (copyCircuit boolean) (symmetryCircuit boolean boolean))
    copied <- requireRightC "copy matrix" (interpretExactCircuit primitiveInterpreter (copyCircuit boolean))
    copiedSwapped <- requireRightC "copy-symmetry matrix" (interpretExactCircuit primitiveInterpreter copyThenSwap)
    assertC "copy was not cocommutative" (stochasticEquivalent copied copiedSwapped)

    let discardLeft = tensorCircuit (discardCircuit boolean) (identityCircuit boolean)
    counit <- requireRightC "copy counit prefix" (composeCircuit (copyCircuit boolean) discardLeft)
    counitComplete <- requireRightC "copy counit" (composeCircuit counit (leftUnitorCircuit boolean))
    counitMatrix <- requireRightC "copy counit matrix" (interpretExactCircuit primitiveInterpreter counitComplete)
    assertC "copy/discard counit law failed" (stochasticEquivalent counitMatrix booleanIdentity)

    let parallel = tensorCircuit coin coin
    parallelMatrix <- requireRightC "tensor circuit matrix" (interpretExactCircuit primitiveInterpreter parallel)
    assertC "tensor fold did not multiply independent masses" (entryR parallelMatrix ((), ()) (False, True) == 1 % 4)
    coinDiscard <- requireRightC "stochastic discard naturality" (composeCircuit coin (discardCircuit boolean))
    coinDiscardMatrix <- requireRightC "stochastic discard matrix" (interpretExactCircuit primitiveInterpreter coinDiscard)
    unitDiscardMatrix <- requireRightC "unit discard matrix" (interpretExactCircuit primitiveInterpreter (discardCircuit unit))
    assertC "exact fold did not preserve stochastic discard naturality" (stochasticEquivalent coinDiscardMatrix unitDiscardMatrix)

    emptyBoolean <- setC ([] :: [Bool])
    emptyIdentity <- requireRightC "empty circuit interpretation" (interpretExactCircuit primitiveInterpreter (identityCircuit emptyBoolean))
    assertC "empty circuit identity acquired rows" (null (matrixRows (forgetStochastic emptyIdentity)))
    case lowerExactCircuit primitiveInterpreter (identityCircuit emptyBoolean) of
        Left DenseCircuitEmptySource -> pure ()
        _ -> ioError (userError "dense circuit lowering accepted an empty compatibility source")

testMonoidalCoherence :: IO ()
testMonoidalCoherence = do
    unit <- setC [()]
    boolean <- setC [False, True]
    reorderedBoolean <- setC [True, False]
    let pair = productSetC boolean boolean
        negation = deterministicPrimitive boolean boolean BooleanNot
        identityBoolean = identityCircuit boolean

    pentagonShortFirst <-
        requireRightC
            "pentagon short first edge"
            (composeCircuit (associateCircuit pair boolean boolean) (associateCircuit boolean boolean pair))
    let pentagonLongFirst = tensorCircuit (associateCircuit boolean boolean boolean) identityBoolean
    pentagonLongSecond <-
        requireRightC
            "pentagon long second edge"
            (composeCircuit pentagonLongFirst (associateCircuit boolean pair boolean))
    pentagonLong <-
        requireRightC
            "pentagon long third edge"
            (composeCircuit pentagonLongSecond (tensorCircuit identityBoolean (associateCircuit boolean boolean boolean)))
    pentagonShortMatrix <- requireRightC "pentagon short matrix" (interpretExactCircuit primitiveInterpreter pentagonShortFirst)
    pentagonLongMatrix <- requireRightC "pentagon long matrix" (interpretExactCircuit primitiveInterpreter pentagonLong)
    assertC "associator pentagon failed" (stochasticEquivalent pentagonShortMatrix pentagonLongMatrix)

    triangleLeft <-
        requireRightC
            "triangle associator edge"
            ( composeCircuit
                (associateCircuit boolean unit boolean)
                (tensorCircuit identityBoolean (leftUnitorCircuit boolean))
            )
    let triangleRight = tensorCircuit (rightUnitorCircuit boolean) identityBoolean
    triangleLeftMatrix <- requireRightC "triangle left matrix" (interpretExactCircuit primitiveInterpreter triangleLeft)
    triangleRightMatrix <- requireRightC "triangle right matrix" (interpretExactCircuit primitiveInterpreter triangleRight)
    assertC "associator-unitor triangle failed" (stochasticEquivalent triangleLeftMatrix triangleRightMatrix)

    hexagonFirst <-
        requireRightC
            "hexagon first edge"
            ( composeCircuit
                (unassociateCircuit boolean boolean boolean)
                (tensorCircuit (symmetryCircuit boolean boolean) identityBoolean)
            )
    hexagonSecond <-
        requireRightC
            "hexagon second edge"
            (composeCircuit hexagonFirst (associateCircuit boolean boolean boolean))
    hexagonThird <-
        requireRightC
            "hexagon third edge"
            (composeCircuit hexagonSecond (tensorCircuit identityBoolean (symmetryCircuit boolean boolean)))
    hexagonLong <-
        requireRightC
            "hexagon final edge"
            (composeCircuit hexagonThird (unassociateCircuit boolean boolean boolean))
    let hexagonShort = symmetryCircuit boolean pair
    hexagonLongMatrix <- requireRightC "hexagon long matrix" (interpretExactCircuit primitiveInterpreter hexagonLong)
    hexagonShortMatrix <- requireRightC "hexagon short matrix" (interpretExactCircuit primitiveInterpreter hexagonShort)
    assertC "symmetry hexagon failed" (stochasticEquivalent hexagonLongMatrix hexagonShortMatrix)

    symmetryNaturalLeft <-
        requireRightC
            "symmetry naturality left"
            (composeCircuit (tensorCircuit negation identityBoolean) (symmetryCircuit boolean boolean))
    symmetryNaturalRight <-
        requireRightC
            "symmetry naturality right"
            (composeCircuit (symmetryCircuit boolean boolean) (tensorCircuit identityBoolean negation))
    symmetryNaturalLeftMatrix <- requireRightC "symmetry naturality left matrix" (interpretExactCircuit primitiveInterpreter symmetryNaturalLeft)
    symmetryNaturalRightMatrix <- requireRightC "symmetry naturality right matrix" (interpretExactCircuit primitiveInterpreter symmetryNaturalRight)
    assertC "symmetry naturality failed" (stochasticEquivalent symmetryNaturalLeftMatrix symmetryNaturalRightMatrix)

    let associatorNaturalLeftFirst = tensorCircuit (tensorCircuit negation identityBoolean) negation
    associatorNaturalLeft <-
        requireRightC
            "associator naturality left"
            (composeCircuit associatorNaturalLeftFirst (associateCircuit boolean boolean boolean))
    associatorNaturalRight <-
        requireRightC
            "associator naturality right"
            (composeCircuit (associateCircuit boolean boolean boolean) (tensorCircuit negation (tensorCircuit identityBoolean negation)))
    associatorNaturalLeftMatrix <- requireRightC "associator naturality left matrix" (interpretExactCircuit primitiveInterpreter associatorNaturalLeft)
    associatorNaturalRightMatrix <- requireRightC "associator naturality right matrix" (interpretExactCircuit primitiveInterpreter associatorNaturalRight)
    assertC "associator naturality failed" (stochasticEquivalent associatorNaturalLeftMatrix associatorNaturalRightMatrix)

    rightUnitorRoundTrip <-
        requireRightC
            "right unitor round trip"
            (composeCircuit (rightUnitorInverseCircuit boolean) (rightUnitorCircuit boolean))
    rightUnitorMatrix <- requireRightC "right unitor matrix" (interpretExactCircuit primitiveInterpreter rightUnitorRoundTrip)
    booleanIdentityMatrix <- requireRightC "coherence identity matrix" (interpretExactCircuit primitiveInterpreter identityBoolean)
    assertC "right unitor coherence failed" (stochasticEquivalent rightUnitorMatrix booleanIdentityMatrix)

    reorderedIdentity <-
        requireRightC
            "reordered identity composition"
            (composeCircuit (identityCircuit boolean) (identityCircuit reorderedBoolean))
    expectedReordered <-
        requireRightC
            "reordered identity table"
            (deterministicTable boolean reorderedBoolean [(False, False), (True, True)])
    reorderedMatrix <- requireRightC "reordered identity matrix" (interpretExactCircuit primitiveInterpreter reorderedIdentity)
    expectedReorderedMatrix <- requireRightC "expected reordered matrix" (interpretExactCircuit primitiveInterpreter expectedReordered)
    assertC "coherence failed under reordered finite layouts" (stochasticEquivalent reorderedMatrix expectedReorderedMatrix)

    pure ()

testConvexAndApproximation :: IO ()
testConvexAndApproximation = do
    unit <- setC [()]
    boolean <- setC [False, True]
    falseCircuit <- requireRightC "false table" (deterministicTable unit boolean [((), False)])
    trueCircuit <- requireRightC "true table" (deterministicTable unit boolean [((), True)])
    mixture <-
        requireRightC
            "circuit convex choice"
            ( convexChoice
                ( (nnC (1 % 4), weakenPurity falseCircuit)
                    :| [(nnC (3 % 4), weakenPurity trueCircuit)]
                )
            )
    mixtureMatrix <- requireRightC "convex circuit interpretation" (interpretExactCircuit primitiveInterpreter mixture)
    assertC "convex false coefficient changed" (entryR mixtureMatrix () False == 1 % 4)
    assertC "convex true coefficient changed" (entryR mixtureMatrix () True == 3 % 4)
    assertC "convex choice was not conservatively stochastic" (isStochastic (circuitPurity mixture))

    let negation = deterministicPrimitive boolean boolean BooleanNot
    mixedThenNot <- requireRightC "convex postcomposition" (composeCircuit mixture negation)
    falseThenNot <- requireRightC "false branch postcomposition" (composeCircuit (weakenPurity falseCircuit) negation)
    trueThenNot <- requireRightC "true branch postcomposition" (composeCircuit (weakenPurity trueCircuit) negation)
    postcomposedMixture <-
        requireRightC
            "separately affine composition"
            (convexChoice ((nnC (1 % 4), falseThenNot) :| [(nnC (3 % 4), trueThenNot)]))
    mixedThenNotMatrix <- requireRightC "postcomposed convex matrix" (interpretExactCircuit primitiveInterpreter mixedThenNot)
    postcomposedMixtureMatrix <- requireRightC "convex postcomposition matrix" (interpretExactCircuit primitiveInterpreter postcomposedMixture)
    assertC "circuit composition was not separately affine" (stochasticEquivalent mixedThenNotMatrix postcomposedMixtureMatrix)

    let identityBoolean = identityCircuit boolean
        tensorMixture = tensorCircuit mixture identityBoolean
        tensorFalse = tensorCircuit (weakenPurity falseCircuit) identityBoolean
        tensorTrue = tensorCircuit (weakenPurity trueCircuit) identityBoolean
    affineTensor <-
        requireRightC
            "separately affine tensor"
            (convexChoice ((nnC (1 % 4), tensorFalse) :| [(nnC (3 % 4), tensorTrue)]))
    tensorMixtureMatrix <- requireRightC "tensor mixture matrix" (interpretExactCircuit primitiveInterpreter tensorMixture)
    affineTensorMatrix <- requireRightC "affine tensor matrix" (interpretExactCircuit primitiveInterpreter affineTensor)
    assertC "circuit tensor was not separately affine" (stochasticEquivalent tensorMixtureMatrix affineTensorMatrix)

    singleton <- requireRightC "singleton convex circuit" (convexChoice ((nnC 1, weakenPurity falseCircuit) :| []))
    assertC "singleton convex syntax was incorrectly strengthened" (isStochastic (circuitPurity singleton))
    case convexChoice ((nnC (1 % 2), weakenPurity falseCircuit) :| []) of
        Left CircuitInvalidConvexCoefficients -> pure ()
        _ -> ioError (userError "invalid circuit convex coefficients were accepted")

    boundary <-
        requireRightC
            "validated approximation boundary"
            ( approximateInterpreterBoundary
                53
                (AbsoluteRationalTolerance (1 % 1000000))
                (\exact approximate -> abs (exact - approximate) <= (1 % 1000000))
            )
    assertC "approximation precision changed" (approximationPrecisionBits boundary == 53)
    assertC "approximation relation was replaced by equality" (approximatelyObserves boundary (1 :: Rational) (10000005 % 10000000))
    case approximateInterpreterBoundary 53 (AbsoluteRationalTolerance (-1)) ((==) :: Rational -> Rational -> Bool) of
        Left ApproximationToleranceMustBeNonnegative -> pure ()
        _ -> ioError (userError "negative approximation tolerance was accepted")

testLegacyDifferential :: IO ()
testLegacyDifferential = do
    unitObject <- objectC [()]
    booleanObject <- objectC [False, True]
    coinDistribution <- requireRightC "legacy coin distribution" (exactFiniteDist [(False, 1), (True, 1)])
    oldCoin <- requireRightC "legacy coin IR" (primitiveExactIR unitObject booleanObject (const coinDistribution))
    oldShared <- requireRightC "legacy shared IR" (composeExactIR oldCoin (copyExactIR booleanObject))
    oldIndependent <- requireRightC "legacy independent IR" (fanoutExactIR oldCoin oldCoin)

    let unit = forgetNonempty unitObject
        boolean = forgetNonempty booleanObject
        newCoin = stochasticPrimitive unit boolean FairCoin
        newShared = shareCircuit newCoin
    newIndependent <- requireRightC "new independent circuit" (fanoutCircuit newCoin newCoin)

    oldSharedResult <- requireRightC "legacy shared denotation" (denoteExactIR oldShared ())
    newSharedResult <- requireRightC "new shared denotation" (runExactCircuit primitiveInterpreter newShared ())
    oldIndependentResult <- requireRightC "legacy independent denotation" (denoteExactIR oldIndependent ())
    newIndependentResult <- requireRightC "new independent denotation" (runExactCircuit primitiveInterpreter newIndependent ())
    let pairSupport = [(False, False), (False, True), (True, False), (True, True)]
    assertC "new shared fold differs from denoteExactIR" (sameDistribution pairSupport oldSharedResult newSharedResult)
    assertC "new fanout fold differs from denoteExactIR" (sameDistribution pairSupport oldIndependentResult newIndependentResult)

    oldDense <- requireRightC "legacy dense lowering" (lowerExactIR oldShared)
    newDense <- requireRightC "circuit dense lowering" (lowerExactCircuit primitiveInterpreter newShared)
    assertC "circuit CPU lowering changed source layout" (denseExactSource oldDense == denseExactSource newDense)
    assertC "circuit CPU lowering changed target layout" (denseExactTarget oldDense == denseExactTarget newDense)
    assertC "circuit CPU lowering changed source-by-target rows" (denseExactRows oldDense == denseExactRows newDense)
    oldDenseIndependent <- requireRightC "legacy independent dense lowering" (lowerExactIR oldIndependent)
    newDenseIndependent <- requireRightC "circuit independent dense lowering" (lowerExactCircuit primitiveInterpreter newIndependent)
    assertC "independent circuit CPU lowering differs from legacy lowering" (denseExactRows oldDenseIndependent == denseExactRows newDenseIndependent)

testDeterministicCompilation :: IO ()
testDeterministicCompilation = do
    boolean <- setC [False, True]
    negation <- requireRightC "quoted not term" (quotedTableTerm boolean boolean [(False, True), (True, False)])
    let identity = identityTerm boolean
    doubleNegation <- requireRightC "source composition" (composeTerm negation negation)
    compiledDouble <- requireRightC "compiled composition" (compileDeterministicTerm doubleNegation)
    denotedDouble <- requireRightC "source composition denotation" (denoteDeterministicTerm doubleNegation)
    interpretedDouble <- requireRightC "compiled composition denotation" (interpretDeterministicCircuit emptyInterpreter compiledDouble)
    assertC "deterministic composition compilation was unsound" (deterministicEquivalent denotedDouble interpretedDouble)

    compiledNegation <- requireRightC "compiled negation" (compileDeterministicTerm negation)
    compiledIdentity <- requireRightC "compiled identity" (compileDeterministicTerm identity)
    expectedComposition <- requireRightC "expected compiled composition" (composeCircuit compiledNegation compiledNegation)
    expectedCompositionMatrix <- requireRightC "expected composition matrix" (interpretExactCircuit emptyInterpreter expectedComposition)
    compiledDoubleMatrix <- requireRightC "compiled double matrix" (interpretExactCircuit emptyInterpreter compiledDouble)
    assertC "C(g compose f) differed from C(f);C(g)" (stochasticEquivalent compiledDoubleMatrix expectedCompositionMatrix)

    let productSource = productTerm negation identity
    compiledProduct <- requireRightC "compiled product" (compileDeterministicTerm productSource)
    expectedProductMatrix <- requireRightC "expected product matrix" (interpretExactCircuit emptyInterpreter (tensorCircuit compiledNegation compiledIdentity))
    compiledProductMatrix <- requireRightC "compiled product matrix" (interpretExactCircuit emptyInterpreter compiledProduct)
    assertC "deterministic product compilation was unsound" (stochasticEquivalent compiledProductMatrix expectedProductMatrix)

    paired <- requireRightC "source pairing" (pairTerm negation identity)
    compiledPair <- requireRightC "compiled pairing" (compileDeterministicTerm paired)
    expectedPair <- requireRightC "expected pair circuit" (fanoutCircuit compiledNegation compiledIdentity)
    compiledPairMatrix <- requireRightC "compiled pair matrix" (interpretExactCircuit emptyInterpreter compiledPair)
    expectedPairMatrix <- requireRightC "expected pair matrix" (interpretExactCircuit emptyInterpreter expectedPair)
    assertC "pairing did not compile through copy and tensor" (stochasticEquivalent compiledPairMatrix expectedPairMatrix)

    firstProjection <- requireRightC "compiled first projection" (compileDeterministicTerm (firstProjectionTerm boolean boolean))
    secondProjection <- requireRightC "compiled second projection" (compileDeterministicTerm (secondProjectionTerm boolean boolean))
    firstMatrix <- requireRightC "first projection matrix" (interpretExactCircuit emptyInterpreter firstProjection)
    secondMatrix <- requireRightC "second projection matrix" (interpretExactCircuit emptyInterpreter secondProjection)
    assertC "first projection compilation changed" (entryR firstMatrix (False, True) False == 1)
    assertC "second projection compilation changed" (entryR secondMatrix (False, True) True == 1)

    case quotedTableTerm boolean boolean [(False, True)] of
        Left DeterministicTermTableDomainMismatch -> pure ()
        _ -> ioError (userError "partial quoted source table was accepted")

testConstructionErrors :: IO ()
testConstructionErrors = do
    unit <- setC [()]
    boolean <- setC [False, True]
    falseOnly <- setC [False]
    trueOnly <- setC [True]

    case composeCircuit (identityCircuit falseOnly) (identityCircuit trueOnly) of
        Left CircuitCompositionObjectMismatch -> pure ()
        _ -> ioError (userError "circuit composition accepted mismatched objects")
    case fanoutCircuit (identityCircuit falseOnly) (identityCircuit trueOnly) of
        Left CircuitFanoutSourceObjectMismatch -> pure ()
        _ -> ioError (userError "circuit fanout accepted mismatched source objects")
    case deterministicTable boolean falseOnly [(False, False), (True, True)] of
        Left (CircuitDeterministicTableTargetMismatch 1) -> pure ()
        _ -> ioError (userError "circuit table accepted an outside target")

    falseConstant <- requireRightC "false endpoint fixture" (deterministicTable unit falseOnly [((), False)])
    trueConstant <- requireRightC "true endpoint fixture" (deterministicTable unit trueOnly [((), True)])
    case convexChoice ((nnC (1 % 2), weakenPurity (identityCircuit falseOnly)) :| [(nnC (1 % 2), weakenPurity (identityCircuit trueOnly))]) of
        Left (CircuitConvexSourceObjectMismatch 1) -> pure ()
        _ -> ioError (userError "convex circuit accepted mismatched source objects")
    case convexChoice ((nnC (1 % 2), weakenPurity falseConstant) :| [(nnC (1 % 2), weakenPurity trueConstant)]) of
        Left (CircuitConvexTargetObjectMismatch 1) -> pure ()
        _ -> ioError (userError "convex circuit accepted mismatched target objects")

    case composeTerm (identityTerm falseOnly) (identityTerm trueOnly) of
        Left DeterministicTermCompositionObjectMismatch -> pure ()
        _ -> ioError (userError "deterministic term composition accepted mismatched objects")
    case pairTerm (identityTerm falseOnly) (identityTerm trueOnly) of
        Left DeterministicTermPairSourceObjectMismatch -> pure ()
        _ -> ioError (userError "deterministic term pairing accepted mismatched sources")
    case quotedTableTerm boolean falseOnly [(False, False), (True, True)] of
        Left (DeterministicTermTableTargetMismatch 1) -> pure ()
        _ -> ioError (userError "quoted term table accepted an outside target")

    let coin = stochasticPrimitive unit boolean FairCoin
        negation = deterministicPrimitive boolean boolean BooleanNot
    case interpretExactCircuit failingInterpreter coin of
        Left (ExactCircuitPrimitiveError PrimitiveInvariant) -> pure ()
        _ -> ioError (userError "exact circuit fold lost a primitive error")
    case interpretExactCircuit reorderedInterpreter coin of
        Left ExactCircuitPrimitiveTargetMismatch -> pure ()
        _ -> ioError (userError "exact circuit fold accepted a reordered primitive target")
    case interpretExactCircuit reorderedInterpreter negation of
        Left ExactCircuitPrimitiveSourceMismatch -> pure ()
        _ -> ioError (userError "exact circuit fold accepted a reordered primitive source")

    case approximateInterpreterBoundary 0 (AbsoluteRationalTolerance 0) ((==) :: Rational -> Rational -> Bool) of
        Left ApproximationPrecisionMustBePositive -> pure ()
        _ -> ioError (userError "zero approximation precision was accepted")
    case approximateInterpreterBoundary 53 (BackendDocumentedError "") ((==) :: Rational -> Rational -> Bool) of
        Left ApproximationBackendPolicyMustBeDocumented -> pure ()
        _ -> ioError (userError "empty backend approximation policy was accepted")

isStochastic :: SPurity purity -> Bool
isStochastic SDeterministic = False
isStochastic SStochastic = True

entryR ::
    StochasticMatrix NonNegativeRational source target ->
    source ->
    target ->
    Rational
entryR arrow sourceValue targetValue =
    getNonNegativeRational
        (fromMaybe zero (matrixEntry (forgetStochastic arrow) sourceValue targetValue))

sameDistribution ::
    (Eq value) =>
    [value] ->
    ExactFiniteDist value ->
    ExactFiniteDist value ->
    Bool
sameDistribution support left right =
    all (\value -> mass value left == mass value right) support
  where
    mass requested distribution =
        sum
            [ exactProbability probability
            | (value, probability) <- NonEmpty.toList (exactOutcomes distribution)
            , value == requested
            ]

productSetC ::
    (Eq left, Eq right) =>
    FiniteSet left ->
    FiniteSet right ->
    FiniteSet (left, right)
productSetC left right =
    case finiteSet [(leftValue, rightValue) | leftValue <- finiteSetValues left, rightValue <- finiteSetValues right] of
        Right productObject -> productObject
        Left _ -> error "duplicate product fixture"
