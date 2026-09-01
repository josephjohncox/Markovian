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
    FiniteLayout,
    unitFiniteLayout,
    finiteLayout,
    productFiniteLayout,
    finiteLayoutDescription,
    finiteLayoutExtent,
    FiniteLayoutStructureError (..),
    checkedFiniteLayout,
    CotangentEqualityMode (..),
    CotangentSpace,
    cotangentSpace,
    declaredCotangentSpace,
    cotangentZero,
    addCotangents,
    scaleCotangent,
    validateCotangent,
    cotangentsEquivalent,
    cotangentEqualityMode,
    cotangentFiniteLayout,
    cotangentModuleOwner,
    sameCotangentLayout,
    compatibleCotangentSpace,
    ParametricReverseCircuit,
    reverseParameterCotangentSpace,
    reverseInputCotangentSpace,
    reverseOutputCotangentSpace,
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

import Data.Maybe (isJust)
import Numeric.Natural (Natural)

-- | Structural metadata for one finite represented layout.
data FiniteLayout
    = UnitFiniteLayout
    | AtomicFiniteLayout !String !Natural
    | ProductFiniteLayout FiniteLayout FiniteLayout
    deriving (Eq, Show)

-- | The zero-coordinate unit layout.
unitFiniteLayout :: FiniteLayout
unitFiniteLayout = UnitFiniteLayout

-- | Declare a named finite atomic layout. Names must be nonempty.
finiteLayout :: String -> Natural -> Maybe FiniteLayout
finiteLayout "" _ = Nothing
finiteLayout name extent = Just (AtomicFiniteLayout name extent)

-- | Ordered structural product; products are not flattened or reassociated.
productFiniteLayout :: FiniteLayout -> FiniteLayout -> FiniteLayout
productFiniteLayout = ProductFiniteLayout

-- | Stable structural layout description.
finiteLayoutDescription :: FiniteLayout -> String
finiteLayoutDescription UnitFiniteLayout = "unit"
finiteLayoutDescription (AtomicFiniteLayout name extent) = name ++ "[" ++ show extent ++ "]"
finiteLayoutDescription (ProductFiniteLayout left right) =
    "(" ++ finiteLayoutDescription left ++ " * " ++ finiteLayoutDescription right ++ ")"

-- | Number of represented scalar coordinates.
finiteLayoutExtent :: FiniteLayout -> Natural
finiteLayoutExtent UnitFiniteLayout = 0
finiteLayoutExtent (AtomicFiniteLayout _ extent) = extent
finiteLayoutExtent (ProductFiniteLayout left right) = finiteLayoutExtent left + finiteLayoutExtent right

{- | Exhaustion while traversing finite-layout structure.  These limits are
separate from the represented scalar extent because zero-extent products
still consume structure.
-}
data FiniteLayoutStructureError
    = FiniteLayoutNodeLimitExceeded !Natural
    | FiniteLayoutDepthLimitExceeded !Natural
    deriving (Eq, Show)

{- | Traverse and rebuild a layout under explicit structural node and depth
limits.  The returned extent, node count, and depth are computed during the
same traversal.  A node is charged before its children are inspected, so a
cyclic or infinitely deep value is rejected without an unbounded description,
equality, or extent pass.
-}
checkedFiniteLayout ::
    Natural ->
    Natural ->
    FiniteLayout ->
    Either FiniteLayoutStructureError (FiniteLayout, Natural, Natural, Natural)
checkedFiniteLayout nodeLimit depthLimit layout = do
    (checked, extent, nodes, depth, _) <- go 1 0 layout
    Right (checked, extent, nodes, depth)
  where
    go depth used current
        | used >= nodeLimit = Left (FiniteLayoutNodeLimitExceeded nodeLimit)
        | depth > depthLimit = Left (FiniteLayoutDepthLimitExceeded depthLimit)
        | otherwise =
            let charged = used + 1
             in case current of
                    UnitFiniteLayout -> Right (UnitFiniteLayout, 0, 1, depth, charged)
                    AtomicFiniteLayout name extent -> Right (AtomicFiniteLayout name extent, extent, 1, depth, charged)
                    ProductFiniteLayout left right -> do
                        (checkedLeft, leftExtent, leftNodes, leftDepth, afterLeft) <- go (depth + 1) charged left
                        (checkedRight, rightExtent, rightNodes, rightDepth, afterRight) <- go (depth + 1) afterLeft right
                        Right
                            ( ProductFiniteLayout checkedLeft checkedRight
                            , leftExtent + rightExtent
                            , 1 + leftNodes + rightNodes
                            , max depth (max leftDepth rightDepth)
                            , afterRight
                            )

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
    , spaceValidate :: cotangent -> Either error ()
    , spaceEquivalent :: cotangent -> cotangent -> Bool
    , spaceEqualityMode :: !CotangentEqualityMode
    , spaceLayout :: !(Maybe FiniteLayout)
    , spaceOwner :: !(Maybe String)
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
cotangentSpace zero add scale equivalent equalityMode =
    CotangentSpaceWitness zero add scale (const (Right ())) equivalent equalityMode Nothing Nothing

{- | Declare a finite cotangent module for prepared reverse programs.

The owner key identifies the supplied module operations. Reusing a key for
inequivalent operations is a primitive-author error; function equality cannot
be inferred by Haskell. The older 'cotangentSpace' remains available, but its
undeclared layout is rejected by program preparation.
-}
declaredCotangentSpace ::
    String ->
    FiniteLayout ->
    (cotangent -> Either error ()) ->
    cotangent ->
    (cotangent -> cotangent -> Either error cotangent) ->
    (scalar -> cotangent -> Either error cotangent) ->
    (cotangent -> cotangent -> Bool) ->
    CotangentEqualityMode ->
    Maybe (CotangentSpace error scalar cotangent)
declaredCotangentSpace "" _ _ _ _ _ _ _ = Nothing
declaredCotangentSpace owner layout validate zero add scale equivalent equalityMode =
    Just (CotangentSpaceWitness zero add scale validate equivalent equalityMode (Just layout) (Just owner))

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

-- | Validate one represented cotangent.
validateCotangent :: CotangentSpace error scalar cotangent -> cotangent -> Either error ()
validateCotangent = spaceValidate

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

-- | Read finite-layout metadata, if declared.
cotangentFiniteLayout :: CotangentSpace error scalar cotangent -> Maybe FiniteLayout
cotangentFiniteLayout = spaceLayout

-- | Read the module-owner key, if declared.
cotangentModuleOwner :: CotangentSpace error scalar cotangent -> Maybe String
cotangentModuleOwner = spaceOwner

-- | Compare represented cotangent layouts without comparing values.
sameCotangentLayout :: CotangentSpace error scalar left -> CotangentSpace error scalar right -> Bool
sameCotangentLayout left right = isJust (spaceLayout left) && spaceLayout left == spaceLayout right

{- | Compare the metadata needed to connect two cotangent modules.

This compares layout, owner key, and exact/approximate equality policy. The
owner remains responsible for the laws and callback operations behind the key.
-}
compatibleCotangentSpace :: CotangentSpace error scalar left -> CotangentSpace error scalar right -> Bool
compatibleCotangentSpace left right =
    sameCotangentLayout left right
        && spaceOwner left == spaceOwner right
        && spaceEqualityMode left == spaceEqualityMode right

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

-- | Read the parameter-cotangent witness.
reverseParameterCotangentSpace ::
    ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    CotangentSpace error scalar parameterCotangent
reverseParameterCotangentSpace (ParametricReverseCircuit space _ _ _) = space

-- | Read the input-cotangent witness.
reverseInputCotangentSpace ::
    ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    CotangentSpace error scalar inputCotangent
reverseInputCotangentSpace (ParametricReverseCircuit _ space _ _) = space

-- | Read the output-cotangent witness.
reverseOutputCotangentSpace ::
    ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    CotangentSpace error scalar outputCotangent
reverseOutputCotangentSpace (ParametricReverseCircuit _ _ space _) = space

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
    CotangentSpaceWitness
        ()
        (\() () -> Right ())
        (\_ () -> Right ())
        (\() -> Right ())
        (\() () -> True)
        ExactCotangentEquality
        (Just unitFiniteLayout)
        (Just "unit")

productCotangentSpace ::
    CotangentSpace error scalar left ->
    CotangentSpace error scalar right ->
    CotangentSpace error scalar (left, right)
productCotangentSpace leftSpace rightSpace =
    CotangentSpaceWitness
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
        (\(left, right) -> validateCotangent leftSpace left >> validateCotangent rightSpace right)
        ( \(leftA, rightA) (leftB, rightB) ->
            cotangentsEquivalent leftSpace leftA leftB
                && cotangentsEquivalent rightSpace rightA rightB
        )
        (combineEqualityModes (cotangentEqualityMode leftSpace) (cotangentEqualityMode rightSpace))
        (productFiniteLayout <$> cotangentFiniteLayout leftSpace <*> cotangentFiniteLayout rightSpace)
        ((\left right -> "(" ++ left ++ " * " ++ right ++ ")") <$> cotangentModuleOwner leftSpace <*> cotangentModuleOwner rightSpace)

combineEqualityModes :: CotangentEqualityMode -> CotangentEqualityMode -> CotangentEqualityMode
combineEqualityModes ExactCotangentEquality ExactCotangentEquality = ExactCotangentEquality
combineEqualityModes left right =
    ApproximateCotangentEquality ("product of " ++ show left ++ " and " ++ show right)
