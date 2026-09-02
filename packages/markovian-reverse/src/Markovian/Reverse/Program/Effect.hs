{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- | Effect-capable execution for the same finite reverse-program syntax.

Preparation is pure. It closes every primitive symbol, checks the same bounded
structural certificate as the pure interpreter, and retains no open resolver.
Forward, recomputation, pullback, and cotangent-addition callbacks are explicit
@m@ actions that return 'Either'. Execution is left-to-right and stops at the
first failed callback. This is an interpreter of supplied pullbacks, not an
autodifferentiator or a generic callback serialization format.
-}
module Markovian.Reverse.Program.Effect (
    EffectCotangentSpace,
    effectCotangentSpace,
    effectCotangentMetadata,
    EffectReverseEvaluation,
    effectReverseEvaluation,
    effectReversePrimalOutput,
    EffectPrimitiveRecomputation,
    effectPrimitiveRecomputation,
    EffectOwnedReversePrimitive,
    effectOwnedReversePrimitive,
    effectOwnedReversePrimitiveWithRecomputation,
    EffectReversePrimitiveResolver,
    PreparedEffectReverseProgram,
    prepareEffectReverseProgram,
    preparedEffectReverseProgramReport,
    EffectReverseRun,
    effectReverseRunOutput,
    effectReverseRunTape,
    effectReverseRunReport,
    EffectReverseTape,
    effectReverseTapeReport,
    EffectReverseExecutionReport (..),
    renderEffectReverseExecutionReport,
    EffectReverseExecutionError (..),
    specializeIdentityProgram,
    runPreparedReverseM,
    applyReverseTapeM,
) where

import Data.Functor.Identity (Identity (..))
import Markovian.Reverse (
    CotangentEqualityMode,
    CotangentSpace,
    FiniteLayout,
    addCotangents,
    applyReverseVJP,
    cotangentEqualityMode,
    cotangentFiniteLayout,
    cotangentModuleOwner,
    evaluateReverseCircuit,
    primitiveReverseCircuit,
    reverseInputCotangentSpace,
    reverseOutputCotangentSpace,
    reverseParameterCotangentSpace,
    reversePrimalOutput,
    validateCotangent,
 )
import Markovian.Reverse.Program.Internal
import Numeric.Natural (Natural)

-- | A declared cotangent space whose diagonal addition may perform effects.
data EffectCotangentSpace m error scalar cotangent
    = EffectCotangentSpace
        !(CotangentSpace error scalar cotangent)
        (cotangent -> cotangent -> m (Either error cotangent))

-- | Pair pure validation and metadata with effectful cotangent addition.
effectCotangentSpace ::
    CotangentSpace error scalar cotangent ->
    (cotangent -> cotangent -> m (Either error cotangent)) ->
    EffectCotangentSpace m error scalar cotangent
effectCotangentSpace = EffectCotangentSpace

-- | Read stable layout, owner, and equality metadata.
effectCotangentMetadata ::
    EffectCotangentSpace m error scalar cotangent ->
    (Maybe FiniteLayout, Maybe String, CotangentEqualityMode)
effectCotangentMetadata (EffectCotangentSpace space _) =
    (cotangentFiniteLayout space, cotangentModuleOwner space, cotangentEqualityMode space)

underlyingCotangent :: EffectCotangentSpace m error scalar cotangent -> CotangentSpace error scalar cotangent
underlyingCotangent (EffectCotangentSpace space _) = space

-- | One effect-produced primal output and its effectful supplied pullback.
data EffectReverseEvaluation m error parameterCotangent inputCotangent output outputCotangent
    = EffectReverseEvaluationValue
        !output
        (outputCotangent -> m (Either error (parameterCotangent, inputCotangent)))

-- | Capture an effectful pullback at one primal output.
effectReverseEvaluation ::
    output ->
    (outputCotangent -> m (Either error (parameterCotangent, inputCotangent))) ->
    EffectReverseEvaluation m error parameterCotangent inputCotangent output outputCotangent
effectReverseEvaluation = EffectReverseEvaluationValue

-- | Observe the primal output without running the pullback.
effectReversePrimalOutput :: EffectReverseEvaluation m error pc xc output yc -> output
effectReversePrimalOutput (EffectReverseEvaluationValue output _) = output

applyEffectVJP :: EffectReverseEvaluation m error pc xc output yc -> yc -> m (Either error (pc, xc))
applyEffectVJP (EffectReverseEvaluationValue _ pullback) = pullback

-- | Owner-supplied callback used only by a recomputed tape.
newtype EffectPrimitiveRecomputation m error parameter parameterCotangent input inputCotangent output outputCotangent
    = EffectPrimitiveRecomputation
        ( parameter ->
          input ->
          m (Either error (EffectReverseEvaluation m error parameterCotangent inputCotangent output outputCotangent))
        )

-- | Declare an effectful recomputation callback.
effectPrimitiveRecomputation ::
    ( parameter ->
      input ->
      m (Either error (EffectReverseEvaluation m error parameterCotangent inputCotangent output outputCotangent))
    ) ->
    EffectPrimitiveRecomputation m error parameter parameterCotangent input inputCotangent output outputCotangent
effectPrimitiveRecomputation = EffectPrimitiveRecomputation

-- | Closed effect-capable primitive definition.
data EffectOwnedReversePrimitive m error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = EffectOwnedReversePrimitive
        !String
        !String
        !ParameterOwnership
        !(FinitePrimalSpace error parameter)
        !(EffectCotangentSpace m error scalar parameterCotangent)
        !(FinitePrimalSpace error input)
        !(EffectCotangentSpace m error scalar inputCotangent)
        !(FinitePrimalSpace error output)
        !(EffectCotangentSpace m error scalar outputCotangent)
        (parameter -> input -> m (Either error (EffectReverseEvaluation m error parameterCotangent inputCotangent output outputCotangent)))
        !PrimitiveTapePolicy
        !(Maybe (EffectPrimitiveRecomputation m error parameter parameterCotangent input inputCotangent output outputCotangent))

-- | Construct a stored-pullback or explicitly non-recomputing primitive.
effectOwnedReversePrimitive ::
    String ->
    String ->
    ParameterOwnership ->
    FinitePrimalSpace error parameter ->
    EffectCotangentSpace m error scalar parameterCotangent ->
    FinitePrimalSpace error input ->
    EffectCotangentSpace m error scalar inputCotangent ->
    FinitePrimalSpace error output ->
    EffectCotangentSpace m error scalar outputCotangent ->
    (parameter -> input -> m (Either error (EffectReverseEvaluation m error parameterCotangent inputCotangent output outputCotangent))) ->
    Either ReverseDefinitionError (EffectOwnedReversePrimitive m error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
effectOwnedReversePrimitive "" _ _ _ _ _ _ _ _ _ = Left EmptyPrimitiveName
effectOwnedReversePrimitive _ "" _ _ _ _ _ _ _ _ = Left EmptyPrimitiveRevision
effectOwnedReversePrimitive name revision ownership parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent forward =
    Right (EffectOwnedReversePrimitive name revision ownership parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent forward StoreCapturedPullback Nothing)

-- | Construct a primitive whose tape reruns the declared recomputation callback.
effectOwnedReversePrimitiveWithRecomputation ::
    String ->
    String ->
    ParameterOwnership ->
    FinitePrimalSpace error parameter ->
    EffectCotangentSpace m error scalar parameterCotangent ->
    FinitePrimalSpace error input ->
    EffectCotangentSpace m error scalar inputCotangent ->
    FinitePrimalSpace error output ->
    EffectCotangentSpace m error scalar outputCotangent ->
    (parameter -> input -> m (Either error (EffectReverseEvaluation m error parameterCotangent inputCotangent output outputCotangent))) ->
    EffectPrimitiveRecomputation m error parameter parameterCotangent input inputCotangent output outputCotangent ->
    Either ReverseDefinitionError (EffectOwnedReversePrimitive m error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
effectOwnedReversePrimitiveWithRecomputation "" _ _ _ _ _ _ _ _ _ _ = Left EmptyPrimitiveName
effectOwnedReversePrimitiveWithRecomputation _ "" _ _ _ _ _ _ _ _ _ = Left EmptyPrimitiveRevision
effectOwnedReversePrimitiveWithRecomputation name revision ownership parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent forward recomputation =
    Right (EffectOwnedReversePrimitive name revision ownership parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent forward RecomputePrimitive (Just recomputation))

-- | Pure resolver for a closed primitive-symbol GADT.
type EffectReversePrimitiveResolver m primitive error scalar =
    forall parameter parameterCotangent input inputCotangent output outputCotangent.
    primitive parameter parameterCotangent input inputCotangent output outputCotangent ->
    Either ReverseDefinitionError (EffectOwnedReversePrimitive m error scalar parameter parameterCotangent input inputCotangent output outputCotangent)

data EffectNode m error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = EffectNode
        ![ReversePathStep]
        !(FinitePrimalSpace error parameter)
        !(EffectCotangentSpace m error scalar parameterCotangent)
        !(FinitePrimalSpace error input)
        !(EffectCotangentSpace m error scalar inputCotangent)
        !(FinitePrimalSpace error output)
        !(EffectCotangentSpace m error scalar outputCotangent)
        !(EffectForm m error scalar parameter parameterCotangent input inputCotangent output outputCotangent)

data EffectForm m error scalar parameter parameterCotangent input inputCotangent output outputCotangent where
    EffectPrimitive ::
        EffectOwnedReversePrimitive m error scalar p pc x xc y yc ->
        EffectForm m error scalar p pc x xc y yc
    EffectIdentity :: EffectForm m error scalar () () value cotangent value cotangent
    EffectCompose ::
        EffectNode m error scalar p pc x xc y yc ->
        EffectNode m error scalar q qc y yc z zc ->
        EffectForm m error scalar (p, q) (pc, qc) x xc z zc
    EffectTensor ::
        EffectNode m error scalar p pc x xc y yc ->
        EffectNode m error scalar q qc u uc v vc ->
        EffectForm m error scalar (p, q) (pc, qc) (x, u) (xc, uc) (y, v) (yc, vc)
    EffectPairInput ::
        EffectNode m error scalar p pc x xc y yc ->
        EffectNode m error scalar q qc x xc z zc ->
        EffectForm m error scalar (p, q) (pc, qc) x xc (y, z) (yc, zc)
    EffectShareParameter ::
        EffectNode m error scalar p pc x xc y yc ->
        EffectNode m error scalar p pc u uc v vc ->
        EffectForm m error scalar p pc (x, u) (xc, uc) (y, v) (yc, vc)

nodeEffectParameterCotangent :: EffectNode m error scalar p pc x xc y yc -> EffectCotangentSpace m error scalar pc
nodeEffectParameterCotangent (EffectNode _ _ space _ _ _ _ _) = space

nodeEffectInputCotangent :: EffectNode m error scalar p pc x xc y yc -> EffectCotangentSpace m error scalar xc
nodeEffectInputCotangent (EffectNode _ _ _ _ space _ _ _) = space

nodeEffectOutputCotangent :: EffectNode m error scalar p pc x xc y yc -> EffectCotangentSpace m error scalar yc
nodeEffectOutputCotangent (EffectNode _ _ _ _ _ _ space _) = space

-- | Opaque effect program whose symbols and complete structure are closed.
data PreparedEffectReverseProgram m primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = PreparedEffectReverseProgram
        !(EffectNode m error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
        !ReverseProgramReport

-- A typed symbol containing one already resolved definition. The pure
-- structural checker receives a circuit whose forward callback is deliberately
-- unreachable: preparation only inspects its declared spaces and ownership.
newtype StructuralSymbol m primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = StructuralSymbol (EffectOwnedReversePrimitive m error scalar parameter parameterCotangent input inputCotangent output outputCotangent)

{- | Resolve every symbol once, then run the existing bounded pure structural
checker over an execution-inert certificate.
-}
prepareEffectReverseProgram ::
    (Monad m) =>
    ReverseLimits ->
    EffectReversePrimitiveResolver m primitive error scalar ->
    ReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    Either (ReverseProgramError error) (PreparedEffectReverseProgram m primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
prepareEffectReverseProgram limits resolver program = do
    (effectNode, structuralProgram) <- resolveEffectNode resolver [] program
    structural <- prepareReverseProgram limits structuralResolver structuralProgram
    Right (PreparedEffectReverseProgram effectNode (preparedReverseProgramReport structural))

-- | Read the deterministic structural certificate produced by preparation.
preparedEffectReverseProgramReport :: PreparedEffectReverseProgram m primitive error scalar p pc x xc y yc -> ReverseProgramReport
preparedEffectReverseProgramReport (PreparedEffectReverseProgram _ report) = report

{- | Specialize the effect execution core to @Identity@. The ordinary pure
interpreter uses this conversion, so exact laws and pure/effect differentials
exercise one forward and pullback implementation.
-}
specializeIdentityProgram ::
    PreparedReverseProgram primitive error scalar p pc x xc y yc ->
    PreparedEffectReverseProgram Identity primitive error scalar p pc x xc y yc
specializeIdentityProgram (PreparedReverseProgram node report) =
    PreparedEffectReverseProgram (identityNode node) report

identityNode :: PreparedNode error scalar p pc x xc y yc -> EffectNode Identity error scalar p pc x xc y yc
identityNode (PreparedNode path _ parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent form) =
    EffectNode
        path
        parameterPrimal
        (pureEffectCotangent parameterCotangent)
        inputPrimal
        (pureEffectCotangent inputCotangent)
        outputPrimal
        (pureEffectCotangent outputCotangent)
        ( case form of
            PreparedPrimitive definition -> EffectPrimitive (identityPrimitive definition)
            PreparedIdentity -> EffectIdentity
            PreparedCompose left right -> EffectCompose (identityNode left) (identityNode right)
            PreparedTensor left right -> EffectTensor (identityNode left) (identityNode right)
            PreparedPairInput left right -> EffectPairInput (identityNode left) (identityNode right)
            PreparedShareParameter left right -> EffectShareParameter (identityNode left) (identityNode right)
        )

identityPrimitive ::
    OwnedReversePrimitive error scalar p pc x xc y yc ->
    EffectOwnedReversePrimitive Identity error scalar p pc x xc y yc
identityPrimitive (OwnedReversePrimitive name revision ownership parameterPrimal inputPrimal outputPrimal circuit policy recomputation) =
    EffectOwnedReversePrimitive
        name
        revision
        ownership
        parameterPrimal
        (pureEffectCotangent (reverseParameterCotangentSpace circuit))
        inputPrimal
        (pureEffectCotangent (reverseInputCotangentSpace circuit))
        outputPrimal
        (pureEffectCotangent (reverseOutputCotangentSpace circuit))
        (\parameter input -> Identity (fmap identityEvaluation (evaluateReverseCircuit circuit parameter input)))
        policy
        (fmap identityRecomputation recomputation)
  where
    identityEvaluation evaluation =
        effectReverseEvaluation
            (reversePrimalOutput evaluation)
            (Identity . applyReverseVJP evaluation)
    identityRecomputation (PrimitiveRecomputation recompute) =
        effectPrimitiveRecomputation $ \parameter input ->
            Identity (fmap identityEvaluation (recompute parameter input))

resolveEffectNode ::
    (Monad m) =>
    EffectReversePrimitiveResolver m primitive error scalar ->
    [ReversePathStep] ->
    ReverseProgram primitive error scalar p pc x xc y yc ->
    Either
        (ReverseProgramError error)
        ( EffectNode m error scalar p pc x xc y yc
        , ReverseProgram (StructuralSymbol m primitive error scalar) error scalar p pc x xc y yc
        )
resolveEffectNode resolver path program = case program of
    PrimitiveProgram symbol -> do
        definition@(EffectOwnedReversePrimitive _ _ _ parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent _ _ _) <-
            mapLeft (ReverseDefinitionFailure path) (resolver symbol)
        Right
            ( EffectNode path parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent (EffectPrimitive definition)
            , PrimitiveProgram (StructuralSymbol definition)
            )
    IdentityProgram primal cotangent -> do
        unit <- mapLeft (ReverseDefinitionFailure path) makeUnitCotangent
        let effectUnit = pureEffectCotangent unit
            effectValue = pureEffectCotangent cotangent
        Right (EffectNode path unitPrimalSpace effectUnit primal effectValue primal effectValue EffectIdentity, IdentityProgram primal cotangent)
    ComposeProgram first second -> do
        (leftNode, leftProgram) <- resolveEffectNode resolver (path ++ [CompositionLeft]) first
        (rightNode, rightProgram) <- resolveEffectNode resolver (path ++ [CompositionRight]) second
        parameterCotangent <- productEffectCotangent (nodeEffectParameterCotangent leftNode) (nodeEffectParameterCotangent rightNode)
        Right
            ( EffectNode path (productPrimalSpace (effectParameterPrimal leftNode) (effectParameterPrimal rightNode)) parameterCotangent (effectInputPrimal leftNode) (nodeEffectInputCotangent leftNode) (effectOutputPrimal rightNode) (nodeEffectOutputCotangent rightNode) (EffectCompose leftNode rightNode)
            , ComposeProgram leftProgram rightProgram
            )
    TensorProgram left right -> do
        (leftNode, leftProgram) <- resolveEffectNode resolver (path ++ [TensorLeft]) left
        (rightNode, rightProgram) <- resolveEffectNode resolver (path ++ [TensorRight]) right
        parameterCotangent <- productEffectCotangent (nodeEffectParameterCotangent leftNode) (nodeEffectParameterCotangent rightNode)
        inputCotangent <- productEffectCotangent (nodeEffectInputCotangent leftNode) (nodeEffectInputCotangent rightNode)
        outputCotangent <- productEffectCotangent (nodeEffectOutputCotangent leftNode) (nodeEffectOutputCotangent rightNode)
        Right
            ( EffectNode path (productPrimalSpace (effectParameterPrimal leftNode) (effectParameterPrimal rightNode)) parameterCotangent (productPrimalSpace (effectInputPrimal leftNode) (effectInputPrimal rightNode)) inputCotangent (productPrimalSpace (effectOutputPrimal leftNode) (effectOutputPrimal rightNode)) outputCotangent (EffectTensor leftNode rightNode)
            , TensorProgram leftProgram rightProgram
            )
    PairInputProgram left right -> do
        (leftNode, leftProgram) <- resolveEffectNode resolver (path ++ [InputPairLeft]) left
        (rightNode, rightProgram) <- resolveEffectNode resolver (path ++ [InputPairRight]) right
        parameterCotangent <- productEffectCotangent (nodeEffectParameterCotangent leftNode) (nodeEffectParameterCotangent rightNode)
        outputCotangent <- productEffectCotangent (nodeEffectOutputCotangent leftNode) (nodeEffectOutputCotangent rightNode)
        Right
            ( EffectNode path (productPrimalSpace (effectParameterPrimal leftNode) (effectParameterPrimal rightNode)) parameterCotangent (effectInputPrimal leftNode) (nodeEffectInputCotangent leftNode) (productPrimalSpace (effectOutputPrimal leftNode) (effectOutputPrimal rightNode)) outputCotangent (EffectPairInput leftNode rightNode)
            , PairInputProgram leftProgram rightProgram
            )
    ShareParameterProgram left right -> do
        (leftNode, leftProgram) <- resolveEffectNode resolver (path ++ [ParameterShareLeft]) left
        (rightNode, rightProgram) <- resolveEffectNode resolver (path ++ [ParameterShareRight]) right
        inputCotangent <- productEffectCotangent (nodeEffectInputCotangent leftNode) (nodeEffectInputCotangent rightNode)
        outputCotangent <- productEffectCotangent (nodeEffectOutputCotangent leftNode) (nodeEffectOutputCotangent rightNode)
        Right
            ( EffectNode path (effectParameterPrimal leftNode) (nodeEffectParameterCotangent leftNode) (productPrimalSpace (effectInputPrimal leftNode) (effectInputPrimal rightNode)) inputCotangent (productPrimalSpace (effectOutputPrimal leftNode) (effectOutputPrimal rightNode)) outputCotangent (EffectShareParameter leftNode rightNode)
            , ShareParameterProgram leftProgram rightProgram
            )

effectParameterPrimal :: EffectNode m error scalar p pc x xc y yc -> FinitePrimalSpace error p
effectParameterPrimal (EffectNode _ primal _ _ _ _ _ _) = primal

effectInputPrimal :: EffectNode m error scalar p pc x xc y yc -> FinitePrimalSpace error x
effectInputPrimal (EffectNode _ _ _ primal _ _ _ _) = primal

effectOutputPrimal :: EffectNode m error scalar p pc x xc y yc -> FinitePrimalSpace error y
effectOutputPrimal (EffectNode _ _ _ _ _ primal _ _) = primal

pureEffectCotangent :: (Applicative m) => CotangentSpace error scalar cotangent -> EffectCotangentSpace m error scalar cotangent
pureEffectCotangent space = EffectCotangentSpace space (\left right -> pure (addCotangents space left right))

productEffectCotangent ::
    (Monad m) =>
    EffectCotangentSpace m error scalar left ->
    EffectCotangentSpace m error scalar right ->
    Either (ReverseProgramError error) (EffectCotangentSpace m error scalar (left, right))
productEffectCotangent (EffectCotangentSpace leftSpace leftAdd) (EffectCotangentSpace rightSpace rightAdd) = do
    structural <- mapLeft (ReverseDefinitionFailure []) (productCotangentWitness leftSpace rightSpace)
    Right $ EffectCotangentSpace structural $ \(leftA, rightA) (leftB, rightB) -> do
        leftResult <- leftAdd leftA leftB
        case leftResult of
            Left problem -> pure (Left problem)
            Right leftValue -> do
                rightResult <- rightAdd rightA rightB
                pure (fmap (leftValue,) rightResult)

structuralResolver ::
    ReversePrimitiveResolver (StructuralSymbol m primitive error scalar) error scalar
structuralResolver (StructuralSymbol (EffectOwnedReversePrimitive name revision ownership parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent _ policy recomputation)) =
    let circuit =
            primitiveReverseCircuit
                (underlyingCotangent parameterCotangent)
                (underlyingCotangent inputCotangent)
                (underlyingCotangent outputCotangent)
                (\_ _ -> error "effect structural certificate was evaluated")
     in case (policy, recomputation) of
            (StoreCapturedPullback, _) -> ownedReversePrimitive name revision ownership parameterPrimal inputPrimal outputPrimal circuit StoreCapturedPullback
            (RecomputePrimitive, Just _) ->
                ownedReversePrimitiveWithRecomputation
                    name
                    revision
                    ownership
                    parameterPrimal
                    inputPrimal
                    outputPrimal
                    circuit
                    (primitiveRecomputation (\_ _ -> error "effect structural recomputation certificate was evaluated"))
            (RecomputePrimitive, Nothing) -> Left MissingPrimitiveRecomputation

-- | Deterministic counts of callbacks attempted by one execution call.
data EffectReverseExecutionReport = EffectReverseExecutionReport
    { effectForwardCalls :: !Natural
    , effectRecomputationCalls :: !Natural
    , effectPullbackCalls :: !Natural
    , effectCotangentAdditionCalls :: !Natural
    }
    deriving (Eq, Show)

instance Semigroup EffectReverseExecutionReport where
    EffectReverseExecutionReport a b c d <> EffectReverseExecutionReport w x y z =
        EffectReverseExecutionReport (a + w) (b + x) (c + y) (d + z)

instance Monoid EffectReverseExecutionReport where
    mempty = EffectReverseExecutionReport 0 0 0 0

-- | Stable timing-free rendering of attempted effect boundaries.
renderEffectReverseExecutionReport :: EffectReverseExecutionReport -> String
renderEffectReverseExecutionReport report =
    unlines
        [ "effect-reverse-execution-report"
        , "forward-calls: " ++ show (effectForwardCalls report)
        , "recomputation-calls: " ++ show (effectRecomputationCalls report)
        , "pullback-calls: " ++ show (effectPullbackCalls report)
        , "cotangent-addition-calls: " ++ show (effectCotangentAdditionCalls report)
        ]

-- | Execution failure paired with deterministic attempted-callback counts.
data EffectReverseExecutionError error = EffectReverseExecutionError
    { effectReverseFailure :: !(ReverseProgramError error)
    , effectReverseFailureReport :: !EffectReverseExecutionReport
    }
    deriving (Eq, Show)

-- | Opaque typed tape retained by one effectful forward run.
data EffectReverseTape m error scalar parameter parameterCotangent input inputCotangent output outputCotangent where
    EffectStoredPrimitiveTape ::
        [ReversePathStep] ->
        EffectOwnedReversePrimitive m error scalar p pc x xc y yc ->
        EffectReverseEvaluation m error pc xc y yc ->
        EffectReverseTape m error scalar p pc x xc y yc
    EffectRecomputedPrimitiveTape ::
        [ReversePathStep] ->
        EffectOwnedReversePrimitive m error scalar p pc x xc y yc ->
        p ->
        x ->
        y ->
        EffectReverseTape m error scalar p pc x xc y yc
    EffectIdentityTape ::
        [ReversePathStep] ->
        EffectCotangentSpace m error scalar cotangent ->
        EffectReverseTape m error scalar () () value cotangent value cotangent
    EffectComposeTape ::
        EffectReverseTape m error scalar p pc x xc y yc ->
        EffectReverseTape m error scalar q qc y yc z zc ->
        EffectReverseTape m error scalar (p, q) (pc, qc) x xc z zc
    EffectTensorTape ::
        EffectReverseTape m error scalar p pc x xc y yc ->
        EffectReverseTape m error scalar q qc u uc v vc ->
        EffectReverseTape m error scalar (p, q) (pc, qc) (x, u) (xc, uc) (y, v) (yc, vc)
    EffectPairInputTape ::
        [ReversePathStep] ->
        EffectCotangentSpace m error scalar xc ->
        EffectReverseTape m error scalar p pc x xc y yc ->
        EffectReverseTape m error scalar q qc x xc z zc ->
        EffectReverseTape m error scalar (p, q) (pc, qc) x xc (y, z) (yc, zc)
    EffectShareParameterTape ::
        [ReversePathStep] ->
        EffectCotangentSpace m error scalar pc ->
        EffectReverseTape m error scalar p pc x xc y yc ->
        EffectReverseTape m error scalar p pc u uc v vc ->
        EffectReverseTape m error scalar p pc (x, u) (xc, uc) (y, v) (yc, vc)

-- | Successful effectful forward output, tape, and attempted-callback report.
data EffectReverseRun m error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = EffectReverseRunValue
        !output
        !(EffectReverseTape m error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
        !EffectReverseExecutionReport

-- | Observe a successful forward output.
effectReverseRunOutput :: EffectReverseRun m error scalar p pc x xc y yc -> y
effectReverseRunOutput (EffectReverseRunValue output _ _) = output

-- | Read the region-bound tape from a successful run.
effectReverseRunTape :: EffectReverseRun m error scalar p pc x xc y yc -> EffectReverseTape m error scalar p pc x xc y yc
effectReverseRunTape (EffectReverseRunValue _ tape _) = tape

-- | Read deterministic attempted-callback counts from a successful forward run.
effectReverseRunReport :: EffectReverseRun m error scalar p pc x xc y yc -> EffectReverseExecutionReport
effectReverseRunReport (EffectReverseRunValue _ _ report) = report

-- | Count effect tape nodes using the same structural report type as pure tapes.
effectReverseTapeReport :: EffectReverseTape m error scalar p pc x xc y yc -> ReverseTapeReport
effectReverseTapeReport tape = case tape of
    EffectStoredPrimitiveTape{} -> ReverseTapeReport 1 1 0
    EffectRecomputedPrimitiveTape{} -> ReverseTapeReport 1 0 1
    EffectIdentityTape{} -> ReverseTapeReport 1 0 0
    EffectComposeTape left right -> combine (effectReverseTapeReport left) (effectReverseTapeReport right)
    EffectTensorTape left right -> combine (effectReverseTapeReport left) (effectReverseTapeReport right)
    EffectPairInputTape _ _ left right -> combine (effectReverseTapeReport left) (effectReverseTapeReport right)
    EffectShareParameterTape _ _ left right -> combine (effectReverseTapeReport left) (effectReverseTapeReport right)
  where
    combine (ReverseTapeReport an as ar) (ReverseTapeReport bn bs br) = ReverseTapeReport (1 + an + bn) (as + bs) (ar + br)

-- | Run a prepared program left-to-right.
runPreparedReverseM ::
    (Monad m) =>
    PreparedEffectReverseProgram m primitive error scalar p pc x xc y yc ->
    p ->
    x ->
    m (Either (EffectReverseExecutionError error) (EffectReverseRun m error scalar p pc x xc y yc))
runPreparedReverseM (PreparedEffectReverseProgram node _) = runEffectNode mempty node

runEffectNode ::
    (Monad m) =>
    EffectReverseExecutionReport ->
    EffectNode m error scalar p pc x xc y yc ->
    p ->
    x ->
    m (Either (EffectReverseExecutionError error) (EffectReverseRun m error scalar p pc x xc y yc))
runEffectNode report (EffectNode path parameterPrimal _ inputPrimal inputCotangent outputPrimal _ form) parameter input =
    case validatePrimal parameterPrimal parameter of
        Left problem -> pure (failure report (ReversePrimalValidationFailure path ParameterPrimalStage problem))
        Right () -> case validatePrimal inputPrimal input of
            Left problem -> pure (failure report (ReversePrimalValidationFailure path InputPrimalStage problem))
            Right () -> case form of
                EffectPrimitive definition@(EffectOwnedReversePrimitive _ _ _ _ _ _ _ _ _ forward policy _) -> do
                    let charged = report{effectForwardCalls = effectForwardCalls report + 1}
                    evaluated <- forward parameter input
                    case evaluated of
                        Left problem -> pure (failure charged (ReversePrimitiveForwardFailure path problem))
                        Right evaluation ->
                            finishOutput charged path outputPrimal $
                                EffectReverseRunValue
                                    (effectReversePrimalOutput evaluation)
                                    ( case policy of
                                        StoreCapturedPullback -> EffectStoredPrimitiveTape path definition evaluation
                                        RecomputePrimitive -> EffectRecomputedPrimitiveTape path definition parameter input (effectReversePrimalOutput evaluation)
                                    )
                                    charged
                EffectIdentity -> finishOutput report path outputPrimal (EffectReverseRunValue input (EffectIdentityTape path inputCotangent) report)
                EffectCompose first second -> do
                    firstResult <- runEffectNode report first (fst parameter) input
                    case firstResult of
                        Left problem -> pure (Left problem)
                        Right (EffectReverseRunValue middle firstTape firstReport) -> do
                            secondResult <- runEffectNode firstReport second (snd parameter) middle
                            pure $ do
                                EffectReverseRunValue output secondTape finalReport <- secondResult
                                checkOutput finalReport path outputPrimal output
                                Right (EffectReverseRunValue output (EffectComposeTape firstTape secondTape) finalReport)
                EffectTensor left right -> do
                    leftResult <- runEffectNode report left (fst parameter) (fst input)
                    case leftResult of
                        Left problem -> pure (Left problem)
                        Right (EffectReverseRunValue leftOutput leftTape leftReport) -> do
                            rightResult <- runEffectNode leftReport right (snd parameter) (snd input)
                            pure $ do
                                EffectReverseRunValue rightOutput rightTape finalReport <- rightResult
                                let output = (leftOutput, rightOutput)
                                checkOutput finalReport path outputPrimal output
                                Right (EffectReverseRunValue output (EffectTensorTape leftTape rightTape) finalReport)
                EffectPairInput left right -> do
                    leftResult <- runEffectNode report left (fst parameter) input
                    case leftResult of
                        Left problem -> pure (Left problem)
                        Right (EffectReverseRunValue leftOutput leftTape leftReport) -> do
                            rightResult <- runEffectNode leftReport right (snd parameter) input
                            pure $ do
                                EffectReverseRunValue rightOutput rightTape finalReport <- rightResult
                                let output = (leftOutput, rightOutput)
                                checkOutput finalReport path outputPrimal output
                                Right (EffectReverseRunValue output (EffectPairInputTape path (nodeEffectInputCotangent left) leftTape rightTape) finalReport)
                EffectShareParameter left right -> do
                    leftResult <- runEffectNode report left parameter (fst input)
                    case leftResult of
                        Left problem -> pure (Left problem)
                        Right (EffectReverseRunValue leftOutput leftTape leftReport) -> do
                            rightResult <- runEffectNode leftReport right parameter (snd input)
                            pure $ do
                                EffectReverseRunValue rightOutput rightTape finalReport <- rightResult
                                let output = (leftOutput, rightOutput)
                                checkOutput finalReport path outputPrimal output
                                Right (EffectReverseRunValue output (EffectShareParameterTape path (nodeEffectParameterCotangent left) leftTape rightTape) finalReport)

finishOutput :: (Applicative m) => EffectReverseExecutionReport -> [ReversePathStep] -> FinitePrimalSpace error output -> EffectReverseRun m error scalar p pc x xc output yc -> m (Either (EffectReverseExecutionError error) (EffectReverseRun m error scalar p pc x xc output yc))
finishOutput report path space run = pure $ case validatePrimal space (effectReverseRunOutput run) of
    Left problem -> failure report (ReversePrimalValidationFailure path OutputPrimalStage problem)
    Right () -> Right run

checkOutput :: EffectReverseExecutionReport -> [ReversePathStep] -> FinitePrimalSpace error output -> output -> Either (EffectReverseExecutionError error) ()
checkOutput report path space output = case validatePrimal space output of
    Left problem -> failure report (ReversePrimalValidationFailure path OutputPrimalStage problem)
    Right () -> Right ()

{- | Apply a tape. Stored and recomputed tapes are reusable while their owning
effect region remains open.
-}
applyReverseTapeM ::
    (Monad m) =>
    EffectReverseTape m error scalar p pc x xc y yc ->
    yc ->
    m (Either (EffectReverseExecutionError error) (pc, xc, EffectReverseExecutionReport))
applyReverseTapeM = applyEffectTape mempty

applyEffectTape ::
    (Monad m) =>
    EffectReverseExecutionReport ->
    EffectReverseTape m error scalar p pc x xc y yc ->
    yc ->
    m (Either (EffectReverseExecutionError error) (pc, xc, EffectReverseExecutionReport))
applyEffectTape report tape outputCotangent = case tape of
    EffectStoredPrimitiveTape path definition evaluation -> effectPrimitivePullback report path definition evaluation outputCotangent
    EffectRecomputedPrimitiveTape path definition@(EffectOwnedReversePrimitive _ _ _ _ _ _ _ outputPrimal _ _ _ recomputation) parameter input oldOutput ->
        case recomputation of
            Nothing -> pure (failure report (ReverseDefinitionFailure path MissingPrimitiveRecomputation))
            Just (EffectPrimitiveRecomputation recompute) -> do
                let charged = report{effectRecomputationCalls = effectRecomputationCalls report + 1}
                recomputed <- recompute parameter input
                case recomputed of
                    Left problem -> pure (failure charged (ReversePrimitiveRecomputationFailure path problem))
                    Right evaluation ->
                        let output = effectReversePrimalOutput evaluation
                         in case validatePrimal outputPrimal output of
                                Left problem -> pure (failure charged (ReversePrimalValidationFailure path OutputPrimalStage problem))
                                Right () ->
                                    if primalsEquivalent outputPrimal oldOutput output
                                        then effectPrimitivePullback charged path definition evaluation outputCotangent
                                        else pure (failure charged (ReverseRecomputedOutputMismatch path))
    EffectIdentityTape path space -> pure $ case validateEffectCotangent report path OutputCotangentStage space outputCotangent of
        Left problem -> Left problem
        Right () -> Right ((), outputCotangent, report)
    EffectComposeTape first second -> do
        secondResult <- applyEffectTape report second outputCotangent
        case secondResult of
            Left problem -> pure (Left problem)
            Right (secondParameter, middle, secondReport) -> do
                firstResult <- applyEffectTape secondReport first middle
                pure $ do
                    (firstParameter, inputCotangent, finalReport) <- firstResult
                    Right ((firstParameter, secondParameter), inputCotangent, finalReport)
    EffectTensorTape left right -> do
        leftResult <- applyEffectTape report left (fst outputCotangent)
        case leftResult of
            Left problem -> pure (Left problem)
            Right (leftParameter, leftInput, leftReport) -> do
                rightResult <- applyEffectTape leftReport right (snd outputCotangent)
                pure $ do
                    (rightParameter, rightInput, finalReport) <- rightResult
                    Right ((leftParameter, rightParameter), (leftInput, rightInput), finalReport)
    EffectPairInputTape path inputSpace left right -> do
        leftResult <- applyEffectTape report left (fst outputCotangent)
        case leftResult of
            Left problem -> pure (Left problem)
            Right (leftParameter, leftInput, leftReport) -> do
                rightResult <- applyEffectTape leftReport right (snd outputCotangent)
                case rightResult of
                    Left problem -> pure (Left problem)
                    Right (rightParameter, rightInput, rightReport) -> do
                        added <- addEffectCotangents rightReport path InputCotangentStage inputSpace leftInput rightInput
                        pure $ do
                            (inputCotangent, finalReport) <- added
                            Right ((leftParameter, rightParameter), inputCotangent, finalReport)
    EffectShareParameterTape path parameterSpace left right -> do
        leftResult <- applyEffectTape report left (fst outputCotangent)
        case leftResult of
            Left problem -> pure (Left problem)
            Right (leftParameter, leftInput, leftReport) -> do
                rightResult <- applyEffectTape leftReport right (snd outputCotangent)
                case rightResult of
                    Left problem -> pure (Left problem)
                    Right (rightParameter, rightInput, rightReport) -> do
                        added <- addEffectCotangents rightReport path ParameterCotangentStage parameterSpace leftParameter rightParameter
                        pure $ do
                            (parameterCotangent, finalReport) <- added
                            Right (parameterCotangent, (leftInput, rightInput), finalReport)

effectPrimitivePullback ::
    (Monad m) =>
    EffectReverseExecutionReport ->
    [ReversePathStep] ->
    EffectOwnedReversePrimitive m error scalar p pc x xc y yc ->
    EffectReverseEvaluation m error pc xc y yc ->
    yc ->
    m (Either (EffectReverseExecutionError error) (pc, xc, EffectReverseExecutionReport))
effectPrimitivePullback report path (EffectOwnedReversePrimitive _ _ _ _ parameterSpace _ inputSpace _ outputSpace _ _ _) evaluation outputCotangent =
    case validateEffectCotangent report path OutputCotangentStage outputSpace outputCotangent of
        Left problem -> pure (Left problem)
        Right () -> do
            let charged = report{effectPullbackCalls = effectPullbackCalls report + 1}
            pulled <- applyEffectVJP evaluation outputCotangent
            pure $ case pulled of
                Left problem -> failure charged (ReversePrimitivePullbackFailure path problem)
                Right (parameterCotangent, inputCotangent) -> do
                    validateEffectCotangent charged path ParameterCotangentStage parameterSpace parameterCotangent
                    validateEffectCotangent charged path InputCotangentStage inputSpace inputCotangent
                    Right (parameterCotangent, inputCotangent, charged)

addEffectCotangents ::
    (Monad m) =>
    EffectReverseExecutionReport ->
    [ReversePathStep] ->
    ReverseStage ->
    EffectCotangentSpace m error scalar cotangent ->
    cotangent ->
    cotangent ->
    m (Either (EffectReverseExecutionError error) (cotangent, EffectReverseExecutionReport))
addEffectCotangents report path stage space@(EffectCotangentSpace _ add) left right =
    case validateEffectCotangent report path stage space left >> validateEffectCotangent report path stage space right of
        Left problem -> pure (Left problem)
        Right () -> do
            let charged = report{effectCotangentAdditionCalls = effectCotangentAdditionCalls report + 1}
            result <- add left right
            pure $ case result of
                Left problem -> failure charged (ReverseCotangentAdditionFailure path stage problem)
                Right value -> do
                    validateEffectCotangent charged path stage space value
                    Right (value, charged)

validateEffectCotangent :: EffectReverseExecutionReport -> [ReversePathStep] -> ReverseStage -> EffectCotangentSpace m error scalar cotangent -> cotangent -> Either (EffectReverseExecutionError error) ()
validateEffectCotangent report path stage (EffectCotangentSpace space _) value =
    case validateCotangent space value of
        Left problem -> failure report (ReverseCotangentValidationFailure path stage problem)
        Right () -> Right ()

failure :: EffectReverseExecutionReport -> ReverseProgramError error -> Either (EffectReverseExecutionError error) value
failure report problem = Left (EffectReverseExecutionError problem report)
