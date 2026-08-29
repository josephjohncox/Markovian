{- | A small typed interpreter for parametric reverse circuits.

A 'CotangentSpace' supplies zero, addition, scalar multiplication, and explicit
equality metadata. Primitive pullbacks must preserve zero and addition and be
homogeneous over the same scalar type. Addition must form a commutative monoid,
scalar multiplication must satisfy the module laws, and exact or approximate
law checks must use the equality declared by the witness. Diagonal reverse
rules use that witness's addition; an arbitrary associative merge is not a
valid VJP rule.

These laws remain primitive-author obligations. The API associates each
primitive with its cotangent witnesses and requires the primitive callback to
return a primal output and its captured pullback together; it does not perform
automatic differentiation or prove callback laws.
-}
module Markovian.Backend.Neural.Reverse (
    CotangentEqualityMode (..),
    CotangentSpace,
    cotangentSpace,
    cotangentZero,
    addCotangents,
    scaleCotangent,
    cotangentsEquivalent,
    cotangentEqualityMode,
    ParametricReverseCircuit,
    ReverseEvaluation,
    reverseEvaluation,
    primitiveReverseCircuit,
    identityReverseCircuit,
    composeReverseCircuit,
    tensorReverseCircuit,
    pairReverseCircuit,
    shareParametersReverseCircuit,
    evaluateReverseCircuit,
    reversePrimalOutput,
    applyReverseVJP,
) where

-- | Whether cotangent laws use exact equality or a documented approximation.
data CotangentEqualityMode
    = ExactCotangentEquality
    | ApproximateCotangentEquality !String
    deriving (Eq, Show)

-- | Checked operations and equality for one cotangent module.
data CotangentSpace error scalar cotangent = CotangentSpaceWitness
    { spaceZero :: !cotangent
    , spaceAdd :: cotangent -> cotangent -> Either error cotangent
    , spaceScale :: scalar -> cotangent -> Either error cotangent
    , spaceEquivalent :: cotangent -> cotangent -> Bool
    , spaceEqualityMode :: !CotangentEqualityMode
    }

{- | Declare one cotangent module.

The supplied operations must satisfy commutative-monoid and module laws under
the supplied equality. Approximate equality descriptions should name the
backend tolerance or comparison rule used by tests.
-}
cotangentSpace ::
    cotangent ->
    (cotangent -> cotangent -> Either error cotangent) ->
    (scalar -> cotangent -> Either error cotangent) ->
    (cotangent -> cotangent -> Bool) ->
    CotangentEqualityMode ->
    CotangentSpace error scalar cotangent
cotangentSpace = CotangentSpaceWitness

-- | Read the additive identity.
cotangentZero :: CotangentSpace error scalar cotangent -> cotangent
cotangentZero = spaceZero

-- | Add cotangents using the declared commutative-monoid operation.
addCotangents ::
    CotangentSpace error scalar cotangent ->
    cotangent ->
    cotangent ->
    Either error cotangent
addCotangents = spaceAdd

-- | Scale a cotangent using the declared scalar action.
scaleCotangent ::
    CotangentSpace error scalar cotangent ->
    scalar ->
    cotangent ->
    Either error cotangent
scaleCotangent = spaceScale

-- | Compare cotangents using the witness's exact or approximate equality.
cotangentsEquivalent ::
    CotangentSpace error scalar cotangent ->
    cotangent ->
    cotangent ->
    Bool
cotangentsEquivalent = spaceEquivalent

-- | Read the equality mode used for cotangent laws.
cotangentEqualityMode :: CotangentSpace error scalar cotangent -> CotangentEqualityMode
cotangentEqualityMode = spaceEqualityMode

-- | One primal output paired with its pullback at the captured primal point.
data ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent
    = ReverseEvaluationValue
        !output
        (outputCotangent -> Either error (parameterCotangent, inputCotangent))

-- | Capture a primal output and the pullback evaluated at that primal point.
reverseEvaluation ::
    output ->
    (outputCotangent -> Either error (parameterCotangent, inputCotangent)) ->
    ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent
reverseEvaluation = ReverseEvaluationValue

{- | A typed parameterized map with a compositional reverse interpreter.

The scalar type is shared by every cotangent witness attached to the circuit.
The constructor is private so composition cannot bypass parameter products or
diagonal addition.
-}
data ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = ParametricReverseCircuit
        !(CotangentSpace error scalar parameterCotangent)
        !(CotangentSpace error scalar inputCotangent)
        !(CotangentSpace error scalar outputCotangent)
        ( parameter ->
          input ->
          Either error (ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent)
        )

{- | Introduce a primitive and its cotangent-space declarations.

The callback returns its output and captured pullback together. Under the
witness equalities, that pullback must map zero to zero, preserve addition, and
preserve scalar multiplication for both returned cotangents.
-}
primitiveReverseCircuit ::
    CotangentSpace error scalar parameterCotangent ->
    CotangentSpace error scalar inputCotangent ->
    CotangentSpace error scalar outputCotangent ->
    ( parameter ->
      input ->
      Either error (ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent)
    ) ->
    ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent
primitiveReverseCircuit = ParametricReverseCircuit

-- | Identity map with the unit parameter object and no parameter cotangent.
identityReverseCircuit ::
    CotangentSpace error scalar cotangent ->
    ParametricReverseCircuit error scalar () () value cotangent value cotangent
identityReverseCircuit valueSpace =
    ParametricReverseCircuit unitSpace valueSpace valueSpace $ \() value ->
        Right (reverseEvaluation value (\cotangent -> Right ((), cotangent)))

-- | Sequential composition with the explicit parameter product @(p, q)@.
composeReverseCircuit ::
    ParametricReverseCircuit error scalar p pCotangent x xCotangent y yCotangent ->
    ParametricReverseCircuit error scalar q qCotangent y yCotangent z zCotangent ->
    ParametricReverseCircuit error scalar (p, q) (pCotangent, qCotangent) x xCotangent z zCotangent
composeReverseCircuit
    (ParametricReverseCircuit firstParameterSpace inputSpace _ evaluateFirst)
    (ParametricReverseCircuit secondParameterSpace _ outputSpace evaluateSecond) =
        ParametricReverseCircuit
            (productCotangentSpace firstParameterSpace secondParameterSpace)
            inputSpace
            outputSpace
            ( \(firstParameter, secondParameter) input -> do
                first <- evaluateFirst firstParameter input
                second <- evaluateSecond secondParameter (reversePrimalOutput first)
                Right
                    ( reverseEvaluation
                        (reversePrimalOutput second)
                        ( \outputCotangent -> do
                            (secondParameterCotangent, middleCotangent) <-
                                applyReverseVJP second outputCotangent
                            (firstParameterCotangent, inputCotangent) <-
                                applyReverseVJP first middleCotangent
                            Right
                                ( (firstParameterCotangent, secondParameterCotangent)
                                , inputCotangent
                                )
                        )
                    )
            )

-- | Independent parallel composition with parameter, input, and output pairs.
tensorReverseCircuit ::
    ParametricReverseCircuit error scalar p pCotangent x xCotangent y yCotangent ->
    ParametricReverseCircuit error scalar q qCotangent u uCotangent v vCotangent ->
    ParametricReverseCircuit error scalar (p, q) (pCotangent, qCotangent) (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)
tensorReverseCircuit
    (ParametricReverseCircuit leftParameterSpace leftInputSpace leftOutputSpace evaluateLeft)
    (ParametricReverseCircuit rightParameterSpace rightInputSpace rightOutputSpace evaluateRight) =
        ParametricReverseCircuit
            (productCotangentSpace leftParameterSpace rightParameterSpace)
            (productCotangentSpace leftInputSpace rightInputSpace)
            (productCotangentSpace leftOutputSpace rightOutputSpace)
            ( \(leftParameter, rightParameter) (leftInput, rightInput) -> do
                left <- evaluateLeft leftParameter leftInput
                right <- evaluateRight rightParameter rightInput
                Right
                    ( reverseEvaluation
                        (reversePrimalOutput left, reversePrimalOutput right)
                        ( \(leftOutputCotangent, rightOutputCotangent) -> do
                            (leftParameterCotangent, leftInputCotangent) <-
                                applyReverseVJP left leftOutputCotangent
                            (rightParameterCotangent, rightInputCotangent) <-
                                applyReverseVJP right rightOutputCotangent
                            Right
                                ( (leftParameterCotangent, rightParameterCotangent)
                                , (leftInputCotangent, rightInputCotangent)
                                )
                        )
                    )
            )

{- | Send one input to two independently parameterized branches.

The reverse rule uses addition from the supplied cotangent module.
-}
pairReverseCircuit ::
    CotangentSpace error scalar xCotangent ->
    ParametricReverseCircuit error scalar p pCotangent x xCotangent y yCotangent ->
    ParametricReverseCircuit error scalar q qCotangent x xCotangent z zCotangent ->
    ParametricReverseCircuit error scalar (p, q) (pCotangent, qCotangent) x xCotangent (y, z) (yCotangent, zCotangent)
pairReverseCircuit
    inputSpace
    (ParametricReverseCircuit leftParameterSpace _ leftOutputSpace evaluateLeft)
    (ParametricReverseCircuit rightParameterSpace _ rightOutputSpace evaluateRight) =
        ParametricReverseCircuit
            (productCotangentSpace leftParameterSpace rightParameterSpace)
            inputSpace
            (productCotangentSpace leftOutputSpace rightOutputSpace)
            ( \(leftParameter, rightParameter) input -> do
                left <- evaluateLeft leftParameter input
                right <- evaluateRight rightParameter input
                Right
                    ( reverseEvaluation
                        (reversePrimalOutput left, reversePrimalOutput right)
                        ( \(leftOutputCotangent, rightOutputCotangent) -> do
                            (leftParameterCotangent, leftInputCotangent) <-
                                applyReverseVJP left leftOutputCotangent
                            (rightParameterCotangent, rightInputCotangent) <-
                                applyReverseVJP right rightOutputCotangent
                            inputCotangent <- addCotangents inputSpace leftInputCotangent rightInputCotangent
                            Right
                                ( (leftParameterCotangent, rightParameterCotangent)
                                , inputCotangent
                                )
                        )
                    )
            )

{- | Use one parameter value in two branches with separate inputs.

This explicit parameter diagonal uses addition from the supplied module.
-}
shareParametersReverseCircuit ::
    CotangentSpace error scalar parameterCotangent ->
    ParametricReverseCircuit error scalar parameter parameterCotangent x xCotangent y yCotangent ->
    ParametricReverseCircuit error scalar parameter parameterCotangent u uCotangent v vCotangent ->
    ParametricReverseCircuit error scalar parameter parameterCotangent (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)
shareParametersReverseCircuit
    parameterSpace
    (ParametricReverseCircuit _ leftInputSpace leftOutputSpace evaluateLeft)
    (ParametricReverseCircuit _ rightInputSpace rightOutputSpace evaluateRight) =
        ParametricReverseCircuit
            parameterSpace
            (productCotangentSpace leftInputSpace rightInputSpace)
            (productCotangentSpace leftOutputSpace rightOutputSpace)
            ( \parameter (leftInput, rightInput) -> do
                left <- evaluateLeft parameter leftInput
                right <- evaluateRight parameter rightInput
                Right
                    ( reverseEvaluation
                        (reversePrimalOutput left, reversePrimalOutput right)
                        ( \(leftOutputCotangent, rightOutputCotangent) -> do
                            (leftParameterCotangent, leftInputCotangent) <-
                                applyReverseVJP left leftOutputCotangent
                            (rightParameterCotangent, rightInputCotangent) <-
                                applyReverseVJP right rightOutputCotangent
                            parameterCotangent <-
                                addCotangents parameterSpace leftParameterCotangent rightParameterCotangent
                            Right
                                ( parameterCotangent
                                , (leftInputCotangent, rightInputCotangent)
                                )
                        )
                    )
            )

-- | Evaluate the primal computation and retain its pullback.
evaluateReverseCircuit ::
    ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    parameter ->
    input ->
    Either error (ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent)
evaluateReverseCircuit (ParametricReverseCircuit _ _ _ evaluate) = evaluate

-- | Observe the primal output captured by an evaluation.
reversePrimalOutput ::
    ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent ->
    output
reversePrimalOutput (ReverseEvaluationValue output _) = output

-- | Apply the captured pullback to one output cotangent.
applyReverseVJP ::
    ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent ->
    outputCotangent ->
    Either error (parameterCotangent, inputCotangent)
applyReverseVJP (ReverseEvaluationValue _ pullback) = pullback

unitSpace :: CotangentSpace error scalar ()
unitSpace =
    cotangentSpace
        ()
        (\() () -> Right ())
        (\_ () -> Right ())
        (\() () -> True)
        ExactCotangentEquality

productCotangentSpace ::
    CotangentSpace error scalar left ->
    CotangentSpace error scalar right ->
    CotangentSpace error scalar (left, right)
productCotangentSpace leftSpace rightSpace =
    cotangentSpace
        (cotangentZero leftSpace, cotangentZero rightSpace)
        ( \(leftA, rightA) (leftB, rightB) -> do
            left <- addCotangents leftSpace leftA leftB
            right <- addCotangents rightSpace rightA rightB
            Right (left, right)
        )
        ( \scalar (left, right) -> do
            scaledLeft <- scaleCotangent leftSpace scalar left
            scaledRight <- scaleCotangent rightSpace scalar right
            Right (scaledLeft, scaledRight)
        )
        ( \(leftA, rightA) (leftB, rightB) ->
            cotangentsEquivalent leftSpace leftA leftB
                && cotangentsEquivalent rightSpace rightA rightB
        )
        (combineEqualityModes (cotangentEqualityMode leftSpace) (cotangentEqualityMode rightSpace))

combineEqualityModes :: CotangentEqualityMode -> CotangentEqualityMode -> CotangentEqualityMode
combineEqualityModes ExactCotangentEquality ExactCotangentEquality = ExactCotangentEquality
combineEqualityModes left right =
    ApproximateCotangentEquality ("product of " ++ show left ++ " and " ++ show right)
