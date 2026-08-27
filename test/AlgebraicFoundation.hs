module AlgebraicFoundation (runAlgebraicFoundationTests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Category.Convex.Exact
import Markovian.Category.Finite.Object (
    FiniteObjectError (EmptyFiniteObject),
    finiteObject,
    forgetNonempty,
    requireNonempty,
    sameFiniteObjectLayout,
    sameFiniteSupport,
 )
import Markovian.Category.Finite.Set
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic

runAlgebraicFoundationTests :: (String -> IO () -> IO ()) -> IO ()
runAlgebraicFoundationTests run = do
    run "exact scalar and finite-witness laws" testScalarAndFiniteWitnesses
    run "finite semiring matrix category and biproduct laws" testMatrixCategory
    run "matrix dagger and compact laws" testDaggerAndCompact
    run "finite matrix trace laws" testTraceLaws
    run "stochastic normalization and transpose counterexample" testStochasticNormalization
    run "proof-carrying deterministic copy laws" testDeterministic
    run "exact convex enrichment laws" testConvex

assertF :: String -> Bool -> IO ()
assertF message condition =
    if condition then pure () else ioError (userError message)

requireRightF :: (Show error) => String -> Either error value -> IO value
requireRightF _ (Right value) = pure value
requireRightF label (Left problem) = ioError (userError (label ++ ": " ++ show problem))

nn :: Rational -> NonNegativeRational
nn value =
    case nonNegativeRational value of
        Right scalar -> scalar
        Left problem -> error ("invalid nonnegative test scalar: " ++ show problem)

set :: (Eq value, Show value) => [value] -> IO (FiniteSet value)
set values = requireRightF "finite set" (finiteSet values)

matrix ::
    FiniteSet source ->
    FiniteSet target ->
    [[Rational]] ->
    IO (Matrix NonNegativeRational source target)
matrix source target rows =
    requireRightF "matrix" (matrixFromRows source target (map (map nn) rows))

compose ::
    Matrix NonNegativeRational source middle ->
    Matrix NonNegativeRational middle target ->
    IO (Matrix NonNegativeRational source target)
compose left right = requireRightF "matrix composition" (composeMatrix left right)

-- A commutative scalar with nontrivial conjugation.
data TestComplex = TestComplex !Rational !Rational
    deriving (Eq, Show)

instance Semiring TestComplex where
    zero = TestComplex 0 0
    one = TestComplex 1 0
    TestComplex ar ai `plus` TestComplex br bi = TestComplex (ar + br) (ai + bi)
    TestComplex ar ai `times` TestComplex br bi =
        TestComplex (ar * br - ai * bi) (ar * bi + ai * br)

instance CommutativeSemiring TestComplex

instance InvolutiveSemiring TestComplex where
    involute (TestComplex real imaginary) = TestComplex real (-imaginary)

-- A noncommutative scalar whose transpose involution reverses products.
data TestMatrix2 = TestMatrix2 !Rational !Rational !Rational !Rational
    deriving (Eq, Show)

instance Semiring TestMatrix2 where
    zero = TestMatrix2 0 0 0 0
    one = TestMatrix2 1 0 0 1
    TestMatrix2 a b c d `plus` TestMatrix2 e f g h =
        TestMatrix2 (a + e) (b + f) (c + g) (d + h)
    TestMatrix2 a b c d `times` TestMatrix2 e f g h =
        TestMatrix2
            (a * e + b * g)
            (a * f + b * h)
            (c * e + d * g)
            (c * f + d * h)

instance InvolutiveSemiring TestMatrix2 where
    involute (TestMatrix2 a b c d) = TestMatrix2 a c b d

testScalarAndFiniteWitnesses :: IO ()
testScalarAndFiniteWitnesses = do
    let a = nn (2 % 3)
        b = nn (5 % 7)
        c = nn (11 % 13)
        fixtures = [zero, a, b, c]
    assertF "scalar addition must be associative" ((a `plus` b) `plus` c == a `plus` (b `plus` c))
    assertF "scalar addition must be commutative" (a `plus` b == b `plus` a)
    assertF "scalar additive identity failed" (zero `plus` a == a && a `plus` zero == a)
    assertF "scalar multiplication must be associative" ((a `times` b) `times` c == a `times` (b `times` c))
    assertF "scalar multiplicative identity failed" (one `times` a == a && a `times` one == a)
    assertF "zero must annihilate multiplication" (zero `times` a == zero && a `times` zero == zero)
    assertF "left scalar distributivity failed" (a `times` (b `plus` c) == (a `times` b) `plus` (a `times` c))
    assertF "right scalar distributivity failed" ((a `plus` b) `times` c == (a `times` c) `plus` (b `times` c))
    assertF "scalar multiplication must be commutative" (a `times` b == b `times` a)
    assertF "involution must preserve zero and one" (involute zero == (zero :: NonNegativeRational) && involute one == (one :: NonNegativeRational))
    assertF "involution must preserve addition" (involute (a `plus` b) == involute a `plus` involute b)
    assertF "involution must reverse multiplication" (involute (a `times` b) == involute b `times` involute a)
    assertF "scalar involution must be involutive" (involute (involute a) == a)
    let complexA = TestComplex 1 2
        complexB = TestComplex 3 (-4)
        noncommutativeA = TestMatrix2 1 2 0 1
        noncommutativeB = TestMatrix2 0 1 1 1
    assertF "complex involution fixture must be nontrivial" (involute complexA /= complexA)
    assertF "complex involution must preserve addition" (involute (complexA `plus` complexB) == involute complexA `plus` involute complexB)
    assertF "complex involution must reverse multiplication" (involute (complexA `times` complexB) == involute complexB `times` involute complexA)
    assertF "complex involution must be involutive" (involute (involute complexA) == complexA)
    assertF "matrix-scalar involution must preserve addition" (involute (noncommutativeA `plus` noncommutativeB) == involute noncommutativeA `plus` involute noncommutativeB)
    assertF "matrix-scalar involution must reverse multiplication" (involute (noncommutativeA `times` noncommutativeB) == involute noncommutativeB `times` involute noncommutativeA)
    assertF "matrix-scalar involution must be involutive" (involute (involute noncommutativeA) == noncommutativeA)
    assertF "zero recognition must agree with equality" (all (\value -> isZero value == (value == zero)) fixtures)
    assertF "positivity must be the complement of zero" (all (\value -> isPositive value == not (isZero value)) fixtures)
    assertF "zero and one must differ" ((zero :: NonNegativeRational) /= one)
    assertF
        "nonnegative addition must be zero-sum-free"
        (and [not (isZero (left `plus` right)) || isZero left && isZero right | left <- fixtures, right <- fixtures])
    assertF
        "nonnegative multiplication must have no zero divisors"
        (and [not (isZero (left `times` right)) || isZero left || isZero right | left <- fixtures, right <- fixtures])
    assertF "scalar division must invert nonzero multiplication" (divideNonZero (a `times` b) b == Just a)
    assertF
        "every accepted quotient must satisfy its exact equation"
        (and [maybe False (\quotient -> quotient `times` denominator == numerator) (divideNonZero numerator denominator) | numerator <- fixtures, denominator <- [a, b, c]])
    case divideNonZero a zero of
        Nothing -> pure ()
        Just quotient -> ioError (userError ("division by zero returned " ++ show quotient))
    assertF "negative rationals must be rejected" (nonNegativeRational (-1) == Left (NegativeRational (-1)))
    assertF "one-half coefficients must be convex" (validConvexCoefficients (nn (1 % 2) :| [nn (1 % 2)]))
    assertF
        "unnormalized coefficients must not be convex"
        (not (validConvexCoefficients ((one :: NonNegativeRational) :| [one])))

    empty <- set ([] :: [Bool])
    ordered <- set [False, True]
    reversed <- set [True, False]
    assertF "finite sets may be empty" (finiteSetCardinality empty == 0)
    assertF "support equality must ignore layout" (sameFiniteSet ordered reversed)
    assertF "layout equality must observe order" (not (sameFiniteSetLayout ordered reversed))
    assertF "ordinary finite-set equality must mean layout equality" (ordered /= reversed)
    object <- requireRightF "finite object" (finiteObject [False, True])
    reversedObject <- requireRightF "reversed finite object" (finiteObject [True, False])
    assertF "finite-object support equality must ignore layout" (sameFiniteSupport object reversedObject)
    assertF "finite-object layout equality must observe order" (not (sameFiniteObjectLayout object reversedObject))
    assertF "ordinary finite-object equality must mean layout equality" (object /= reversedObject)
    assertF "nonempty forgetting must preserve values" (finiteSetValues (forgetNonempty object) == [False, True])
    assertF "empty finite sets must not refine to objects" (requireNonempty empty == Left EmptyFiniteObject)
    case finiteSet [False, False] of
        Left (DuplicateFiniteSetValue False) -> pure ()
        result -> ioError (userError ("duplicate finite set accepted: " ++ show result))

testMatrixCategory :: IO ()
testMatrixCategory = do
    object <- set [False, True]
    reordered <- set [True, False]
    a <- matrix object reordered [[1, 2], [3, 4]]
    b <- matrix object object [[0, 1], [1, 1]]
    c <- matrix object object [[1, 1], [0, 1]]
    leftIdentity <- compose (identityMatrix object) b
    rightIdentity <- compose b (identityMatrix object)
    assertF "matrix left identity failed" (matrixEquivalent leftIdentity b)
    assertF "matrix right identity failed" (matrixEquivalent rightIdentity b)

    ab <- compose a b
    leftAssociated <- compose ab c
    bc <- compose b c
    rightAssociated <- compose a bc
    assertF "matrix associativity failed under middle reindexing" (matrixEquivalent leftAssociated rightAssociated)

    aPlusA <- requireRightF "matrix addition" (addMatrix a a)
    distributedLeft <- compose aPlusA b
    abAgain <- compose a b
    distributedRight <- requireRightF "matrix sum" (addMatrix abAgain abAgain)
    assertF "left distributivity failed" (matrixEquivalent distributedLeft distributedRight)

    bPlusC <- requireRightF "matrix addition" (addMatrix b c)
    rightDistributedLeft <- compose a bPlusC
    ac <- compose a c
    rightDistributedRight <- requireRightF "matrix sum" (addMatrix abAgain ac)
    assertF "right distributivity failed" (matrixEquivalent rightDistributedLeft rightDistributedRight)

    tensorCompositeLeft <- compose (tensorMatrix a b) (tensorMatrix b c)
    let tensorCompositeRight = tensorMatrix abAgain bc
    assertF "tensor interchange failed" (matrixEquivalent tensorCompositeLeft tensorCompositeRight)

    let block = directSumMatrix b c
        leftInjection = leftInjectionMatrix object object
        rightInjection = rightInjectionMatrix object object
        leftProjection = leftProjectionMatrix object object
        rightProjection = rightProjectionMatrix object object
        sumObject = matrixTarget leftInjection
    leftInjected <- compose leftInjection block
    leftBlock <- compose leftInjected leftProjection
    rightInjected <- compose rightInjection block
    rightBlock <- compose rightInjected rightProjection
    assertF "left biproduct block equation failed" (matrixEquivalent leftBlock b)
    assertF "right biproduct block equation failed" (matrixEquivalent rightBlock c)
    leftIdentityBiproduct <- compose leftInjection leftProjection
    rightIdentityBiproduct <- compose rightInjection rightProjection
    assertF "left injection-projection equation failed" (matrixEquivalent leftIdentityBiproduct (identityMatrix object))
    assertF "right injection-projection equation failed" (matrixEquivalent rightIdentityBiproduct (identityMatrix object))
    leftCross <- compose leftInjection rightProjection
    rightCross <- compose rightInjection leftProjection
    assertF "left-to-right biproduct cross term must be zero" (matrixEquivalent leftCross (zeroMatrix object object))
    assertF "right-to-left biproduct cross term must be zero" (matrixEquivalent rightCross (zeroMatrix object object))
    leftProjector <- compose leftProjection leftInjection
    rightProjector <- compose rightProjection rightInjection
    decomposition <- requireRightF "biproduct decomposition" (addMatrix leftProjector rightProjector)
    assertF "biproduct projector decomposition failed" (matrixEquivalent decomposition (identityMatrix sumObject))

testDaggerAndCompact :: IO ()
testDaggerAndCompact = do
    object <- set [False, True]
    unitScalar <- set [()]
    complexMatrix <-
        requireRightF
            "nontrivial involutive matrix"
            ( matrixFromRows
                object
                object
                [ [TestComplex 1 2, TestComplex 0 1]
                , [TestComplex 3 (-1), TestComplex 2 0]
                ]
            )
    assertF
        "conjugate transpose must differ from plain transpose"
        (not (matrixEquivalent (conjugateTransposeMatrix complexMatrix) (transposeMatrix complexMatrix)))
    assertF
        "nontrivial dagger involution failed"
        (matrixEquivalent (conjugateTransposeMatrix (conjugateTransposeMatrix complexMatrix)) complexMatrix)
    assertF
        "nontrivial dagger tensor law failed"
        ( matrixEquivalent
            (conjugateTransposeMatrix (tensorMatrix complexMatrix complexMatrix))
            (tensorMatrix (conjugateTransposeMatrix complexMatrix) (conjugateTransposeMatrix complexMatrix))
        )

    let firstScalar = TestMatrix2 1 2 0 1
        secondScalar = TestMatrix2 0 1 1 1
    firstNoncommutative <- requireRightF "first noncommutative scalar matrix" (matrixFromRows unitScalar unitScalar [[firstScalar]])
    secondNoncommutative <- requireRightF "second noncommutative scalar matrix" (matrixFromRows unitScalar unitScalar [[secondScalar]])
    noncommutativeProduct <- requireRightF "noncommutative composition" (composeMatrix firstNoncommutative secondNoncommutative)
    reversedProduct <-
        requireRightF
            "reversed noncommutative dagger composition"
            (composeMatrix (conjugateTransposeMatrix secondNoncommutative) (conjugateTransposeMatrix firstNoncommutative))
    assertF "test scalars must witness noncommutativity" (firstScalar `times` secondScalar /= secondScalar `times` firstScalar)
    assertF
        "dagger must reverse noncommutative scalar products"
        (matrixEquivalent (conjugateTransposeMatrix noncommutativeProduct) reversedProduct)

    a <- matrix object object [[0, 2], [3, 1]]
    b <- matrix object object [[1, 1], [0, 2]]
    assertF
        "dagger involution failed"
        (matrixEquivalent (conjugateTransposeMatrix (conjugateTransposeMatrix a)) a)
    ab <- compose a b
    daggerComposite <- compose (conjugateTransposeMatrix b) (conjugateTransposeMatrix a)
    assertF
        "dagger must reverse composition"
        (matrixEquivalent (conjugateTransposeMatrix ab) daggerComposite)
    assertF
        "dagger must preserve tensor"
        ( matrixEquivalent
            (conjugateTransposeMatrix (tensorMatrix a b))
            (tensorMatrix (conjugateTransposeMatrix a) (conjugateTransposeMatrix b))
        )

    unit <- set [()]
    let cup = cupMatrix object
        cap = capMatrix object
        identity = identityMatrix object
        leftStart = tensorMatrix identity cup
        leftFinish = tensorMatrix cap identity
        leftMiddle =
            matrixFromFunction
                (matrixTarget leftStart)
                (matrixSource leftFinish)
                (\(x, (u, v)) ((x', u'), v') -> if x == x' && u == u' && v == v' then one else zero)
    leftOne <- compose leftStart leftMiddle
    leftTwo <- compose leftOne leftFinish
    let leftUnitor =
            matrixFromFunction (matrixTarget leftTwo) object $ \((), value) target ->
                if value == target then one else zero
        rightUnitorInverse =
            matrixFromFunction object (productSet object unit) $ \source (target, ()) ->
                if source == target then one else zero
    leftThree <- compose leftTwo leftUnitor
    leftSnake <- compose rightUnitorInverse leftThree
    assertF "left compact snake equation failed" (matrixEquivalent leftSnake identity)

    let rightStart = tensorMatrix cup identity
        rightFinish = tensorMatrix identity cap
        rightMiddle =
            matrixFromFunction
                (matrixTarget rightStart)
                (matrixSource rightFinish)
                (\((u, v), x) (u', (v', x')) -> if u == u' && v == v' && x == x' then one else zero)
    rightOne <- compose rightStart rightMiddle
    rightTwo <- compose rightOne rightFinish
    let rightUnitor =
            matrixFromFunction (matrixTarget rightTwo) object $ \(value, ()) target ->
                if value == target then one else zero
        leftUnitorInverse =
            matrixFromFunction object (productSet unit object) $ \source ((), target) ->
                if source == target then one else zero
    rightThree <- compose rightTwo rightUnitor
    rightSnake <- compose leftUnitorInverse rightThree
    assertF "right compact snake equation failed" (matrixEquivalent rightSnake identity)

testTraceLaws :: IO ()
testTraceLaws = do
    object <- set [False, True]
    unit <- set [()]
    a <- matrix (productSet object object) (productSet object object) (map (map fromInteger) traceFixture)
    f <- matrix object object [[1, 1], [0, 1]]
    g <- matrix object object [[1, 0], [1, 1]]

    pre <- compose (tensorMatrix f (identityMatrix object)) a
    transformed <- compose pre (tensorMatrix g (identityMatrix object))
    naturalLeft <- requireRightF "trace naturality" (traceMatrix object object object transformed)
    tracedA <- requireRightF "trace" (traceMatrix object object object a)
    naturalMiddle <- compose f tracedA
    naturalRight <- compose naturalMiddle g
    assertF "trace naturality failed" (matrixEquivalent naturalLeft naturalRight)

    tracedSource <- set [False, True]
    tracedSourceReordered <- set [True, False]
    tracedTarget <- set [Nothing, Just ()]
    distinctTraceArrow <-
        matrix
            (productSet object tracedSource)
            (productSet object tracedTarget)
            (map (map fromInteger) traceFixture)
    changeTraceObject <- matrix tracedTarget tracedSourceReordered [[1, 2], [0, 1]]
    dinaturalLeftComposite <- compose distinctTraceArrow (tensorMatrix (identityMatrix object) changeTraceObject)
    dinaturalLeft <- requireRightF "trace dinaturality left" (traceMatrix object object tracedSource dinaturalLeftComposite)
    dinaturalRightComposite <- compose (tensorMatrix (identityMatrix object) changeTraceObject) distinctTraceArrow
    dinaturalRight <- requireRightF "trace dinaturality right" (traceMatrix object object tracedTarget dinaturalRightComposite)
    assertF "trace dinaturality with distinct reordered supports failed" (matrixEquivalent dinaturalLeft dinaturalRight)

    base <- matrix object object [[1, 2], [3, 4]]
    let unitLift =
            matrixFromFunction (productSet object unit) (productSet object unit) $ \(x, ()) (y, ()) ->
                entryOrZero base x y
    vanished <- requireRightF "trace vanishing" (traceMatrix object object unit unitLift)
    assertF "trace vanishing over the unit failed" (matrixEquivalent vanished base)

    let symmetry :: Matrix NonNegativeRational (Bool, Bool) (Bool, Bool)
        symmetry =
            matrixFromFunction (productSet object object) (productSet object object) $ \(x, u) (u', x') ->
                if x == x' && u == u' then one else zero
    yanked <- requireRightF "trace yanking" (traceMatrix object object object symmetry)
    assertF "trace yanking failed" (matrixEquivalent yanked (identityMatrix object))

    let nested =
            matrixFromFunction
                (productSet object (productSet object object))
                (productSet object (productSet object object))
                (\(x, (u, v)) (y, (u', v')) -> if u == u' && v == v' then entryOrZero base x y else zero)
    directVanishing <- requireRightF "trace product" (traceMatrix object object (productSet object object) nested)
    let reassociated =
            matrixFromFunction
                (productSet (productSet object object) object)
                (productSet (productSet object object) object)
                (\((x, u), v) ((y, u'), v') -> entryOrZero nested (x, (u, v)) (y, (u', v')))
    firstTrace <- requireRightF "first iterated trace" (traceMatrix (productSet object object) (productSet object object) object reassociated)
    iteratedVanishing <- requireRightF "second iterated trace" (traceMatrix object object object firstTrace)
    assertF "trace iterated vanishing failed" (matrixEquivalent directVanishing iteratedVanishing)

    b <- matrix object object [[1, 0], [2, 1]]
    let superposed = tensorMatrix a b
        shuffled =
            matrixFromFunction
                (productSet (productSet object object) object)
                (productSet (productSet object object) object)
                (\((x, w), u) ((y, w'), u') -> entryOrZero superposed ((x, u), w) ((y, u'), w'))
    superposingLeft <- requireRightF "trace superposing" (traceMatrix (productSet object object) (productSet object object) object shuffled)
    let superposingRight = tensorMatrix tracedA b
    assertF "trace superposing failed" (matrixEquivalent superposingLeft superposingRight)
  where
    traceFixture =
        [ [1, 2, 0, 1]
        , [0, 1, 1, 0]
        , [2, 0, 1, 1]
        , [1, 1, 0, 2]
        ]

testStochasticNormalization :: IO ()
testStochasticNormalization = do
    source <- set [()]
    target <- set [False, True]
    raw <- matrix source target [[1 % 2, 1 % 2]]
    kernel <- requireRightF "stochastic matrix" (stochasticMatrix raw)
    let transposed = transposeMatrix (forgetStochastic kernel)
    case stochasticMatrix transposed of
        Left (StochasticRowNotNormalized 0 total) ->
            assertF "transpose counterexample must have row mass one-half" (total == nn (1 % 2))
        result -> ioError (userError ("transpose unexpectedly preserved normalization: " ++ showStochasticResult result))

    let sourceIdentity = identityStochastic source
        targetIdentity = identityStochastic target
        targetCopy = copyStochastic target
        targetDiscard = discardStochastic target
    channelRaw <- matrix target target [[1 % 3, 2 % 3], [3 % 4, 1 % 4]]
    channel <- requireRightF "stochastic channel" (stochasticMatrix channelRaw)
    composed <- requireRightF "stochastic composition" (composeStochastic kernel channel)
    let tensorProduct = tensorStochastic kernel channel
    assertNormalizedF "stochastic source identity" sourceIdentity
    assertNormalizedF "stochastic target identity" targetIdentity
    assertNormalizedF "stochastic composition" composed
    assertNormalizedF "stochastic tensor" tensorProduct
    assertNormalizedF "stochastic copy" targetCopy
    assertNormalizedF "stochastic discard" targetDiscard

    leftIdentity <- requireRightF "stochastic left identity" (composeStochastic sourceIdentity kernel)
    rightIdentity <- requireRightF "stochastic right identity" (composeStochastic kernel targetIdentity)
    assertF "stochastic left identity failed" (stochasticEquivalent leftIdentity kernel)
    assertF "stochastic right identity failed" (stochasticEquivalent rightIdentity kernel)
    firstAssociation <- requireRightF "first stochastic association" (composeStochastic kernel channel)
    leftAssociated <- requireRightF "left-associated stochastic composition" (composeStochastic firstAssociation channel)
    secondAssociation <- requireRightF "second stochastic association" (composeStochastic channel channel)
    rightAssociated <- requireRightF "right-associated stochastic composition" (composeStochastic kernel secondAssociation)
    assertF "stochastic composition associativity failed" (stochasticEquivalent leftAssociated rightAssociated)

    tensorCompositeLeft <-
        requireRightF
            "stochastic tensor interchange left"
            (composeStochastic (tensorStochastic kernel channel) (tensorStochastic channel channel))
    kernelChannel <- requireRightF "kernel then channel" (composeStochastic kernel channel)
    channelSquare <- requireRightF "channel square" (composeStochastic channel channel)
    let tensorCompositeRight = tensorStochastic kernelChannel channelSquare
    assertF "stochastic tensor interchange failed" (stochasticEquivalent tensorCompositeLeft tensorCompositeRight)

    discarded <- requireRightF "discard naturality" (composeStochastic kernel targetDiscard)
    assertF
        "discard must be natural for stochastic arrows"
        (stochasticEquivalent discarded (discardStochastic source))

    empty <- set ([] :: [Bool])
    impossible <- matrix source empty [[]]
    case stochasticMatrix impossible of
        Left (StochasticRowNotNormalized 0 total) -> assertF "nonempty-to-empty row mass must be zero" (isZero total)
        result -> ioError (userError ("nonempty stochastic arrow to empty accepted: " ++ showStochasticResult result))

    emptyRaw <- matrix empty empty []
    _ <- requireRightF "empty stochastic arrow" (stochasticMatrix emptyRaw)
    pure ()

testDeterministic :: IO ()
testDeterministic = do
    object <- set [False, True]
    deterministic <-
        requireRightF
            "deterministic function"
            ( deterministicFromFunction object object not ::
                Either
                    (DeterministicMatrixError Bool)
                    (DeterministicMatrix NonNegativeRational Bool Bool)
            )
    left <- requireRightF "deterministic copy left" (composeStochastic (embedDeterministic deterministic) (copyStochastic object))
    copied <- requireRightF "deterministic copy first" (composeStochastic (copyStochastic object) (tensorStochastic (embedDeterministic deterministic) (embedDeterministic deterministic)))
    assertF "deterministic copy naturality failed" (stochasticEquivalent left copied)
    discarded <- requireRightF "deterministic discard" (composeStochastic (embedDeterministic deterministic) (discardStochastic object))
    assertF "deterministic maps must preserve discard" (stochasticEquivalent discarded (discardStochastic object))
    let deterministicIdentity = identityDeterministic object
    constantFalse <- requireRightF "constant deterministic function" (deterministicFromFunction object object (const False))
    leftIdentity <- requireRightF "deterministic left identity" (composeDeterministic deterministicIdentity deterministic)
    rightIdentity <- requireRightF "deterministic right identity" (composeDeterministic deterministic deterministicIdentity)
    assertF "deterministic left identity failed" (deterministicEquivalent leftIdentity deterministic)
    assertF "deterministic right identity failed" (deterministicEquivalent rightIdentity deterministic)

    firstComposition <- requireRightF "first deterministic composition" (composeDeterministic deterministic deterministic)
    leftAssociated <- requireRightF "left-associated deterministic composition" (composeDeterministic firstComposition constantFalse)
    secondComposition <- requireRightF "second deterministic composition" (composeDeterministic deterministic constantFalse)
    rightAssociated <- requireRightF "right-associated deterministic composition" (composeDeterministic deterministic secondComposition)
    assertF "deterministic composition associativity failed" (deterministicEquivalent leftAssociated rightAssociated)

    let pairObject = productSet object object
    expectedTensor <-
        requireRightF
            "expected deterministic tensor table"
            (deterministicFromFunction pairObject pairObject (\(leftValue, _) -> (not leftValue, False)))
    let actualTensor = tensorDeterministic deterministic constantFalse
    assertF "deterministic tensor table failed" (deterministicEquivalent actualTensor expectedTensor)
    assertOneHotF "deterministic identity" deterministicIdentity
    assertOneHotF "deterministic composition" firstComposition
    assertOneHotF "deterministic tensor" actualTensor

    tensorCompositeLeft <-
        requireRightF
            "deterministic tensor interchange left"
            ( composeDeterministic
                (tensorDeterministic deterministic deterministicIdentity)
                (tensorDeterministic deterministic constantFalse)
            )
    tensorLeftComposite <- requireRightF "deterministic tensor left composite" (composeDeterministic deterministic deterministic)
    tensorRightComposite <- requireRightF "deterministic tensor right composite" (composeDeterministic deterministicIdentity constantFalse)
    let tensorCompositeRight = tensorDeterministic tensorLeftComposite tensorRightComposite
    assertF "deterministic tensor interchange failed" (deterministicEquivalent tensorCompositeLeft tensorCompositeRight)

    embeddedComposition <- requireRightF "embedded deterministic composition" (composeStochastic (embedDeterministic deterministic) (embedDeterministic deterministic))
    assertF
        "embedding must preserve deterministic identity"
        (stochasticEquivalent (embedDeterministic deterministicIdentity) (identityStochastic object))
    assertF
        "embedding must preserve deterministic composition"
        (stochasticEquivalent (embedDeterministic firstComposition) embeddedComposition)
    assertF
        "embedding must preserve deterministic tensor"
        ( stochasticEquivalent
            (embedDeterministic actualTensor)
            (tensorStochastic (embedDeterministic deterministic) (embedDeterministic constantFalse))
        )
    assertNormalizedF "embedded deterministic matrix" (embedDeterministic actualTensor)

    source <- set [()]
    coinRaw <- matrix source object [[1 % 2, 1 % 2]]
    coin <- requireRightF "coin" (stochasticMatrix coinRaw)
    case deterministicMatrix coinRaw of
        Left (DeterministicRowNotOneHot 0) -> pure ()
        result -> ioError (userError ("non-one-hot matrix gained deterministic proof: " ++ showDeterministicResult result))
    shared <- requireRightF "shared coin" (composeStochastic coin (copyStochastic object))
    independent <- requireRightF "independent coin" (composeStochastic (copyStochastic source) (tensorStochastic coin coin))
    assertF "copy must not be natural for arbitrary stochastic arrows" (not (stochasticEquivalent shared independent))

testConvex :: IO ()
testConvex = do
    source <- set [()]
    target <- set [False, True]
    falseArrow <- requireRightF "false deterministic" (deterministicFromFunction source target (const False))
    trueArrow <- requireRightF "true deterministic" (deterministicFromFunction source target (const True))
    family <-
        requireRightF
            "convex family"
            (convexFamily ((nn (1 % 4), embedDeterministic falseArrow) :| [(nn (3 % 4), embedDeterministic trueArrow)]))
    expectedRaw <- matrix source target [[1 % 4, 3 % 4]]
    expected <- requireRightF "expected convex mixture" (stochasticMatrix expectedRaw)
    assertF "convex mixture values failed" (stochasticEquivalent (convexMixture family) expected)

    singleton <- requireRightF "singleton family" (convexFamily ((one, expected) :| []))
    assertF "convex singleton law failed" (stochasticEquivalent (convexMixture singleton) expected)
    zeroElimination <-
        requireRightF
            "zero elimination family"
            (convexFamily ((zero, embedDeterministic falseArrow) :| [(one, expected)]))
    assertF "zero coefficient elimination failed" (stochasticEquivalent (convexMixture zeroElimination) expected)
    permutation <-
        requireRightF
            "permuted family"
            (convexFamily ((nn (3 % 4), embedDeterministic trueArrow) :| [(nn (1 % 4), embedDeterministic falseArrow)]))
    assertF "convex permutation invariance failed" (stochasticEquivalent (convexMixture permutation) expected)

    half <-
        requireRightF
            "half family"
            (convexFamily ((nn (1 % 2), embedDeterministic falseArrow) :| [(nn (1 % 2), embedDeterministic trueArrow)]))
    flattened <-
        requireRightF
            "flattened family"
            (convexFamily ((nn (1 % 2), convexMixture half) :| [(nn (1 % 2), embedDeterministic trueArrow)]))
    direct <-
        requireRightF
            "direct flattened family"
            (convexFamily ((nn (1 % 4), embedDeterministic falseArrow) :| [(nn (3 % 4), embedDeterministic trueArrow)]))
    assertF "convex flattening failed" (stochasticEquivalent (convexMixture flattened) (convexMixture direct))

    flipArrow <- requireRightF "flip" (deterministicFromFunction target target not)
    composedMixture <- requireRightF "compose mixture" (composeStochastic (convexMixture family) (embedDeterministic flipArrow))
    falseComposed <- requireRightF "compose false" (composeStochastic (embedDeterministic falseArrow) (embedDeterministic flipArrow))
    trueComposed <- requireRightF "compose true" (composeStochastic (embedDeterministic trueArrow) (embedDeterministic flipArrow))
    composedFamily <-
        requireRightF
            "composed convex family"
            (convexFamily ((nn (1 % 4), falseComposed) :| [(nn (3 % 4), trueComposed)]))
    assertF "composition must be affine in its left argument" (stochasticEquivalent composedMixture (convexMixture composedFamily))

    identityArrow <- requireRightF "identity table" (deterministicFromFunction target target id)
    endomorphismFamily <-
        requireRightF
            "endomorphism family"
            (convexFamily ((nn (1 % 3), embedDeterministic identityArrow) :| [(nn (2 % 3), embedDeterministic flipArrow)]))
    rightComposedMixture <- requireRightF "right compose mixture" (composeStochastic expected (convexMixture endomorphismFamily))
    rightIdentity <- requireRightF "right compose identity" (composeStochastic expected (embedDeterministic identityArrow))
    rightFlip <- requireRightF "right compose flip" (composeStochastic expected (embedDeterministic flipArrow))
    rightComposedFamily <-
        requireRightF
            "right composed family"
            (convexFamily ((nn (1 % 3), rightIdentity) :| [(nn (2 % 3), rightFlip)]))
    assertF "composition must be affine in its right argument" (stochasticEquivalent rightComposedMixture (convexMixture rightComposedFamily))

    let tensorMixture = tensorStochastic (convexMixture family) (embedDeterministic falseArrow)
    tensorFamily <-
        requireRightF
            "tensor convex family"
            ( convexFamily
                ( (nn (1 % 4), tensorStochastic (embedDeterministic falseArrow) (embedDeterministic falseArrow))
                    :| [(nn (3 % 4), tensorStochastic (embedDeterministic trueArrow) (embedDeterministic falseArrow))]
                )
            )
    assertF "tensor must be affine in its left argument" (stochasticEquivalent tensorMixture (convexMixture tensorFamily))
    let rightTensorMixture = tensorStochastic (embedDeterministic falseArrow) (convexMixture family)
    rightTensorFamily <-
        requireRightF
            "right tensor convex family"
            ( convexFamily
                ( (nn (1 % 4), tensorStochastic (embedDeterministic falseArrow) (embedDeterministic falseArrow))
                    :| [(nn (3 % 4), tensorStochastic (embedDeterministic falseArrow) (embedDeterministic trueArrow))]
                )
            )
    assertF "tensor must be affine in its right argument" (stochasticEquivalent rightTensorMixture (convexMixture rightTensorFamily))

    correlatedTensorFamily <-
        requireRightF
            "correlated tensor family"
            ( convexFamily
                ( (nn (1 % 2), tensorStochastic (embedDeterministic falseArrow) (embedDeterministic falseArrow))
                    :| [(nn (1 % 2), tensorStochastic (embedDeterministic trueArrow) (embedDeterministic trueArrow))]
                )
            )
    assertF
        "tensor must not be jointly affine over correlated pairs"
        (not (stochasticEquivalent (convexMixture correlatedTensorFamily) (tensorStochastic (convexMixture half) (convexMixture half))))

    halfEndomorphism <-
        requireRightF
            "half endomorphism family"
            (convexFamily ((nn (1 % 2), embedDeterministic identityArrow) :| [(nn (1 % 2), embedDeterministic flipArrow)]))
    correlatedFirst <- requireRightF "correlated first composition" (composeStochastic (embedDeterministic falseArrow) (embedDeterministic identityArrow))
    correlatedSecond <- requireRightF "correlated second composition" (composeStochastic (embedDeterministic trueArrow) (embedDeterministic flipArrow))
    correlatedCompositionFamily <-
        requireRightF
            "correlated composition results"
            (convexFamily ((nn (1 % 2), correlatedFirst) :| [(nn (1 % 2), correlatedSecond)]))
    mixedCompositions <- requireRightF "independent mixed composition" (composeStochastic (convexMixture half) (convexMixture halfEndomorphism))
    assertF
        "composition must not be jointly affine over correlated pairs"
        (not (stochasticEquivalent (convexMixture correlatedCompositionFamily) mixedCompositions))

assertNormalizedF ::
    String ->
    StochasticMatrix NonNegativeRational source target ->
    IO ()
assertNormalizedF label stochastic =
    case stochasticMatrix (forgetStochastic stochastic) of
        Right checked ->
            assertF (label ++ " changed during independent normalization") (stochasticEquivalent checked stochastic)
        Left problem -> ioError (userError (label ++ " is not normalized: " ++ show problem))

assertOneHotF ::
    String ->
    DeterministicMatrix NonNegativeRational source target ->
    IO ()
assertOneHotF label deterministic =
    assertF
        (label ++ " is not one-hot")
        ( all
            (\row -> length (filter (== one) row) == 1 && all (\entry -> entry == one || isZero entry) row)
            (matrixRows (forgetDeterministic deterministic))
        )

entryOrZero ::
    Matrix NonNegativeRational source target ->
    source ->
    target ->
    NonNegativeRational
entryOrZero value source target =
    fromMaybe zero (matrixEntry value source target)

productSet :: (Eq left, Eq right, Show left, Show right) => FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productSet left right =
    case finiteSet [(x, y) | x <- finiteSetValues left, y <- finiteSetValues right] of
        Right result -> result
        Left problem -> error (show problem)

showStochasticResult ::
    Either (StochasticMatrixError NonNegativeRational) (StochasticMatrix NonNegativeRational source target) ->
    String
showStochasticResult (Left problem) = show problem
showStochasticResult (Right _) = "Right <stochastic matrix>"

showDeterministicResult ::
    Either (DeterministicMatrixError target) (DeterministicMatrix NonNegativeRational source target) ->
    String
showDeterministicResult (Left _) = "Left <unexpected deterministic error>"
showDeterministicResult (Right _) = "Right <deterministic matrix>"
