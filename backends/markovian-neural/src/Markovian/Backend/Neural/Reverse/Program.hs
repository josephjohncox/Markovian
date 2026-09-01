{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- | Finite owned syntax for interpreting caller-supplied primitive VJPs.

This module is an interpreter of supplied reverse callbacks. It does not
differentiate arbitrary Haskell and does not implement loops, recursion,
stochastic differentiation, tensors, or checkpoint scheduling. Every program
is a finite acyclic tree. Preparation checks structural ownership, declared
primal and cotangent layouts, and caller-supplied limits before evaluation.
-}
module Markovian.Backend.Neural.Reverse.Program (
    FinitePrimalSpace,
    finitePrimalSpace,
    primalFiniteLayout,
    primalEqualityMode,
    validatePrimal,
    primalsEquivalent,
    samePrimalLayout,
    ParameterOwnership,
    noParameterOwnership,
    parameterOwner,
    parameterOwnershipProduct,
    parameterOwnershipDescription,
    parameterOwnerKeys,
    PrimitiveTapePolicy (..),
    PrimitiveRecomputation,
    primitiveRecomputation,
    OwnedReversePrimitive,
    ownedReversePrimitive,
    ownedReversePrimitiveWithRecomputation,
    ReversePrimitiveResolver,
    ReverseProgram,
    primitiveProgram,
    identityProgram,
    composeProgram,
    tensorProgram,
    pairInputProgram,
    shareParameterProgram,
    ReverseLimits,
    reverseLimits,
    reverseLimitsWithStructure,
    ReverseDefinitionError (..),
    ReversePathStep (..),
    ReverseStage (..),
    ReverseProgramError (..),
    PreparedReverseProgram,
    prepareReverseProgram,
    ReverseProgramReport,
    preparedReverseProgramReport,
    renderReverseProgramReport,
    ReverseRun,
    reverseRunOutput,
    reverseRunTape,
    ReverseTape,
    ReverseTapeReport (..),
    reverseTapeReport,
    runPreparedReverse,
    applyReverseTape,
) where

import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
import Markovian.Backend.Neural.Reverse (
    CotangentEqualityMode (..),
    CotangentSpace,
    FiniteLayout,
    FiniteLayoutStructureError (..),
    ParametricReverseCircuit,
    ReverseEvaluation,
    addCotangents,
    applyReverseVJP,
    checkedFiniteLayout,
    compatibleCotangentSpace,
    cotangentEqualityMode,
    cotangentFiniteLayout,
    cotangentModuleOwner,
    cotangentZero,
    cotangentsEquivalent,
    declaredCotangentSpace,
    evaluateReverseCircuit,
    finiteLayoutDescription,
    productFiniteLayout,
    reverseInputCotangentSpace,
    reverseOutputCotangentSpace,
    reverseParameterCotangentSpace,
    reversePrimalOutput,
    scaleCotangent,
    unitFiniteLayout,
    validateCotangent,
 )
import Numeric.Natural (Natural)

-- | Validation, finite layout, and observation policy for a primal value.
data FinitePrimalSpace error value
    = FinitePrimalSpaceWitness
        !FiniteLayout
        (value -> Either error ())
        (value -> value -> Bool)
        !CotangentEqualityMode

-- | Declare a finite primal space.
finitePrimalSpace ::
    FiniteLayout ->
    (value -> Either error ()) ->
    (value -> value -> Bool) ->
    CotangentEqualityMode ->
    FinitePrimalSpace error value
finitePrimalSpace = FinitePrimalSpaceWitness

-- | Read the represented primal layout.
primalFiniteLayout :: FinitePrimalSpace error value -> FiniteLayout
primalFiniteLayout (FinitePrimalSpaceWitness layout _ _ _) = layout

-- | Read the equality policy used for recomputation checks.
primalEqualityMode :: FinitePrimalSpace error value -> CotangentEqualityMode
primalEqualityMode (FinitePrimalSpaceWitness _ _ _ mode) = mode

-- | Validate one primal value.
validatePrimal :: FinitePrimalSpace error value -> value -> Either error ()
validatePrimal (FinitePrimalSpaceWitness _ validate _ _) = validate

-- | Compare primal observations under the declared exact or approximate mode.
primalsEquivalent :: FinitePrimalSpace error value -> value -> value -> Bool
primalsEquivalent (FinitePrimalSpaceWitness _ _ equivalent _) = equivalent

-- | Compare represented layouts. Equality policy remains a separate check.
samePrimalLayout :: FinitePrimalSpace error left -> FinitePrimalSpace error right -> Bool
samePrimalLayout left right = primalFiniteLayout left == primalFiniteLayout right

compatiblePrimalSpace :: FinitePrimalSpace error left -> FinitePrimalSpace error right -> Bool
compatiblePrimalSpace left right =
    samePrimalLayout left right && primalEqualityMode left == primalEqualityMode right

productPrimalSpace ::
    FinitePrimalSpace error left ->
    FinitePrimalSpace error right ->
    FinitePrimalSpace error (left, right)
productPrimalSpace left right =
    finitePrimalSpace
        (productFiniteLayout (primalFiniteLayout left) (primalFiniteLayout right))
        (\(leftValue, rightValue) -> validatePrimal left leftValue >> validatePrimal right rightValue)
        (\(leftA, rightA) (leftB, rightB) -> primalsEquivalent left leftA leftB && primalsEquivalent right rightA rightB)
        (combineModes (primalEqualityMode left) (primalEqualityMode right))

unitPrimalSpace :: FinitePrimalSpace error ()
unitPrimalSpace = finitePrimalSpace unitFiniteLayout (const (Right ())) (==) ExactCotangentEquality

-- | Structural parameter ownership. Products retain their association.
data ParameterOwnership
    = NoParameterOwnership
    | ParameterOwnerLeaf !String !FiniteLayout
    | ParameterOwnershipProduct ParameterOwnership ParameterOwnership
    deriving (Eq, Show)

-- | Ownership for the unit parameter object.
noParameterOwnership :: ParameterOwnership
noParameterOwnership = NoParameterOwnership

-- | Declare one nonempty owner key and its represented parameter layout.
parameterOwner :: String -> FiniteLayout -> Either ReverseDefinitionError ParameterOwnership
parameterOwner "" _ = Left EmptyParameterOwner
parameterOwner key layout = Right (ParameterOwnerLeaf key layout)

{- | Form an ordered ownership product. Duplicate keys are rejected later when
the product denotes independent parameters.
-}
parameterOwnershipProduct :: ParameterOwnership -> ParameterOwnership -> ParameterOwnership
parameterOwnershipProduct = ParameterOwnershipProduct

-- | Stable structural description used by deterministic reports.
parameterOwnershipDescription :: ParameterOwnership -> String
parameterOwnershipDescription NoParameterOwnership = "unit"
parameterOwnershipDescription (ParameterOwnerLeaf key layout) = key ++ ":" ++ finiteLayoutDescription layout
parameterOwnershipDescription (ParameterOwnershipProduct left right) =
    "(" ++ parameterOwnershipDescription left ++ " * " ++ parameterOwnershipDescription right ++ ")"

-- | Owner keys in structural left-to-right order.
parameterOwnerKeys :: ParameterOwnership -> [String]
parameterOwnerKeys NoParameterOwnership = []
parameterOwnerKeys (ParameterOwnerLeaf key _) = [key]
parameterOwnerKeys (ParameterOwnershipProduct left right) = parameterOwnerKeys left ++ parameterOwnerKeys right

{- | Primitive tape policy. Recomputation retains immutable parameters and
input; it is not scheduled checkpointing.
-}
data PrimitiveTapePolicy
    = StoreCapturedPullback
    | RecomputePrimitive
    deriving (Eq, Show)

{- | An owner-supplied recomputation operation distinct from the forward
primitive. It may use a different implementation, but its validated output
must agree with the retained forward output before its pullback is used.
-}
newtype PrimitiveRecomputation error parameter parameterCotangent input inputCotangent output outputCotangent
    = PrimitiveRecomputation
        ( parameter ->
          input ->
          Either error (ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent)
        )

-- | Declare the operation used only when applying a recomputed tape.
primitiveRecomputation ::
    ( parameter ->
      input ->
      Either error (ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent)
    ) ->
    PrimitiveRecomputation error parameter parameterCotangent input inputCotangent output outputCotangent
primitiveRecomputation = PrimitiveRecomputation

-- | Resolved primitive definition owned by the caller's signature.
data OwnedReversePrimitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = OwnedReversePrimitive
        !String
        !String
        !ParameterOwnership
        !(FinitePrimalSpace error parameter)
        !(FinitePrimalSpace error input)
        !(FinitePrimalSpace error output)
        !(ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
        !PrimitiveTapePolicy
        !(Maybe (PrimitiveRecomputation error parameter parameterCotangent input inputCotangent output outputCotangent))

-- | Construct one owned primitive definition.
ownedReversePrimitive ::
    String ->
    String ->
    ParameterOwnership ->
    FinitePrimalSpace error parameter ->
    FinitePrimalSpace error input ->
    FinitePrimalSpace error output ->
    ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    PrimitiveTapePolicy ->
    Either ReverseDefinitionError (OwnedReversePrimitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
ownedReversePrimitive "" _ _ _ _ _ _ _ = Left EmptyPrimitiveName
ownedReversePrimitive _ "" _ _ _ _ _ _ = Left EmptyPrimitiveRevision
ownedReversePrimitive name revision ownership parameterSpace inputSpace outputSpace circuit policy =
    Right (OwnedReversePrimitive name revision ownership parameterSpace inputSpace outputSpace circuit policy Nothing)

{- | Construct a recomputed primitive with a distinct owner-supplied
recomputation operation. Structural ownership and layout validation is
deferred to bounded program preparation.
-}
ownedReversePrimitiveWithRecomputation ::
    String ->
    String ->
    ParameterOwnership ->
    FinitePrimalSpace error parameter ->
    FinitePrimalSpace error input ->
    FinitePrimalSpace error output ->
    ParametricReverseCircuit error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    PrimitiveRecomputation error parameter parameterCotangent input inputCotangent output outputCotangent ->
    Either ReverseDefinitionError (OwnedReversePrimitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
ownedReversePrimitiveWithRecomputation "" _ _ _ _ _ _ _ = Left EmptyPrimitiveName
ownedReversePrimitiveWithRecomputation _ "" _ _ _ _ _ _ = Left EmptyPrimitiveRevision
ownedReversePrimitiveWithRecomputation name revision ownership parameterSpace inputSpace outputSpace circuit recomputation =
    Right (OwnedReversePrimitive name revision ownership parameterSpace inputSpace outputSpace circuit RecomputePrimitive (Just recomputation))

-- | A total rank-polymorphic resolver for a caller-owned primitive GADT.
type ReversePrimitiveResolver primitive error scalar =
    forall parameter parameterCotangent input inputCotangent output outputCotangent.
    primitive parameter parameterCotangent input inputCotangent output outputCotangent ->
    Either ReverseDefinitionError (OwnedReversePrimitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent)

-- | Finite acyclic reverse-program syntax. Constructors are private.
data ReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent where
    PrimitiveProgram ::
        primitive parameter parameterCotangent input inputCotangent output outputCotangent ->
        ReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    IdentityProgram ::
        FinitePrimalSpace error value ->
        CotangentSpace error scalar cotangent ->
        ReverseProgram primitive error scalar () () value cotangent value cotangent
    ComposeProgram ::
        ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseProgram primitive error scalar q qCotangent y yCotangent z zCotangent ->
        ReverseProgram primitive error scalar (p, q) (pCotangent, qCotangent) x xCotangent z zCotangent
    TensorProgram ::
        ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseProgram primitive error scalar q qCotangent u uCotangent v vCotangent ->
        ReverseProgram primitive error scalar (p, q) (pCotangent, qCotangent) (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)
    PairInputProgram ::
        ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseProgram primitive error scalar q qCotangent x xCotangent z zCotangent ->
        ReverseProgram primitive error scalar (p, q) (pCotangent, qCotangent) x xCotangent (y, z) (yCotangent, zCotangent)
    ShareParameterProgram ::
        ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseProgram primitive error scalar p pCotangent u uCotangent v vCotangent ->
        ReverseProgram primitive error scalar p pCotangent (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)

-- | Introduce one caller-signature primitive into the finite syntax.
primitiveProgram ::
    primitive parameter parameterCotangent input inputCotangent output outputCotangent ->
    ReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent
primitiveProgram = PrimitiveProgram

-- | Identity program with unit parameter ownership.
identityProgram ::
    FinitePrimalSpace error value ->
    CotangentSpace error scalar cotangent ->
    ReverseProgram primitive error scalar () () value cotangent value cotangent
identityProgram = IdentityProgram

-- | Sequential composition with an ordered independent parameter product.
composeProgram ::
    ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
    ReverseProgram primitive error scalar q qCotangent y yCotangent z zCotangent ->
    ReverseProgram primitive error scalar (p, q) (pCotangent, qCotangent) x xCotangent z zCotangent
composeProgram = ComposeProgram

-- | Parallel composition with independent parameters and inputs.
tensorProgram ::
    ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
    ReverseProgram primitive error scalar q qCotangent u uCotangent v vCotangent ->
    ReverseProgram primitive error scalar (p, q) (pCotangent, qCotangent) (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)
tensorProgram = TensorProgram

-- | Send one input to two branches and add their input cotangents.
pairInputProgram ::
    ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
    ReverseProgram primitive error scalar q qCotangent x xCotangent z zCotangent ->
    ReverseProgram primitive error scalar (p, q) (pCotangent, qCotangent) x xCotangent (y, z) (yCotangent, zCotangent)
pairInputProgram = PairInputProgram

-- | Use one owned parameter tree in two branches and add its cotangents.
shareParameterProgram ::
    ReverseProgram primitive error scalar p pCotangent x xCotangent y yCotangent ->
    ReverseProgram primitive error scalar p pCotangent u uCotangent v vCotangent ->
    ReverseProgram primitive error scalar p pCotangent (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)
shareParameterProgram = ShareParameterProgram

{- | Preparation limits. Node and primitive limits are totals; depth is
one-based; owner count is unique by key; layout limits are maximum extents.
-}
data ReverseLimits = ReverseLimits !Natural !Natural !Natural !Natural !Natural !Natural !Natural !Natural
    deriving (Eq, Show)

{- | Set node, primitive, depth, owner, primal-extent, and cotangent-extent
limits. This compatibility form also uses the program node and depth limits as
the independent layout/ownership structural limits.
-}
reverseLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> ReverseLimits
reverseLimits nodes primitives depth owners primalExtent cotangentExtent =
    ReverseLimits nodes primitives depth owners primalExtent cotangentExtent nodes depth

-- | Set all limits, including separate layout/ownership node and depth limits.
reverseLimitsWithStructure :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> ReverseLimits
reverseLimitsWithStructure = ReverseLimits

-- | Definition errors independent of primitive callback failures.
data ReverseDefinitionError
    = EmptyPrimitiveName
    | EmptyPrimitiveRevision
    | EmptyParameterOwner
    | PrimitiveOwnershipLayoutMismatch
    | DuplicateOwnerInsidePrimitive
    | MissingPrimitiveRecomputation
    | UndeclaredCotangentLayout
    | UndeclaredCotangentOwner
    deriving (Eq, Show)

-- | Deterministic path component in the syntax tree.
data ReversePathStep
    = CompositionLeft
    | CompositionRight
    | TensorLeft
    | TensorRight
    | InputPairLeft
    | InputPairRight
    | ParameterShareLeft
    | ParameterShareRight
    deriving (Eq, Show)

-- | Callback or validation stage.
data ReverseStage
    = ParameterPrimalStage
    | InputPrimalStage
    | OutputPrimalStage
    | ParameterCotangentStage
    | InputCotangentStage
    | OutputCotangentStage
    deriving (Eq, Show)

-- | Structured preparation and execution errors.
data ReverseProgramError error
    = ReverseDefinitionFailure ![ReversePathStep] !ReverseDefinitionError
    | ReverseNodeLimitExceeded ![ReversePathStep] !Natural
    | ReversePrimitiveLimitExceeded ![ReversePathStep] !Natural
    | ReverseDepthLimitExceeded ![ReversePathStep] !Natural
    | ReverseOwnerLimitExceeded ![ReversePathStep] !Natural
    | ReverseLayoutNodeLimitExceeded ![ReversePathStep] !Natural
    | ReverseLayoutDepthLimitExceeded ![ReversePathStep] !Natural
    | ReverseOwnershipNodeLimitExceeded ![ReversePathStep] !Natural
    | ReverseOwnershipDepthLimitExceeded ![ReversePathStep] !Natural
    | ReversePrimalLayoutLimitExceeded ![ReversePathStep] !FiniteLayout !Natural
    | ReverseCotangentLayoutLimitExceeded ![ReversePathStep] !FiniteLayout !Natural
    | ReversePrimalLayoutMismatch ![ReversePathStep] !FiniteLayout !FiniteLayout
    | ReverseCotangentLayoutMismatch ![ReversePathStep] !String !String
    | DuplicateIndependentOwner ![ReversePathStep] !String
    | MismatchedSharedOwnership ![ReversePathStep] !String !String
    | ReversePrimalValidationFailure ![ReversePathStep] !ReverseStage !error
    | ReverseCotangentValidationFailure ![ReversePathStep] !ReverseStage !error
    | ReversePrimitiveForwardFailure ![ReversePathStep] !error
    | ReversePrimitivePullbackFailure ![ReversePathStep] !error
    | ReversePrimitiveRecomputationFailure ![ReversePathStep] !error
    | ReverseRecomputedOutputMismatch ![ReversePathStep]
    | ReverseCotangentAdditionFailure ![ReversePathStep] !ReverseStage !error
    deriving (Eq, Show)

-- | Opaque syntax that has passed complete bounded structural preparation.
data PreparedReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = PreparedReverseProgram
        !(PreparedNode error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
        !ReverseProgramReport

data PreparedNode error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = PreparedNode
        ![ReversePathStep]
        !ParameterOwnership
        !(FinitePrimalSpace error parameter)
        !(CotangentSpace error scalar parameterCotangent)
        !(FinitePrimalSpace error input)
        !(CotangentSpace error scalar inputCotangent)
        !(FinitePrimalSpace error output)
        !(CotangentSpace error scalar outputCotangent)
        !(PreparedForm error scalar parameter parameterCotangent input inputCotangent output outputCotangent)

data PreparedForm error scalar parameter parameterCotangent input inputCotangent output outputCotangent where
    PreparedPrimitive ::
        OwnedReversePrimitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
        PreparedForm error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    PreparedIdentity :: PreparedForm error scalar () () value cotangent value cotangent
    PreparedCompose ::
        PreparedNode error scalar p pCotangent x xCotangent y yCotangent ->
        PreparedNode error scalar q qCotangent y yCotangent z zCotangent ->
        PreparedForm error scalar (p, q) (pCotangent, qCotangent) x xCotangent z zCotangent
    PreparedTensor ::
        PreparedNode error scalar p pCotangent x xCotangent y yCotangent ->
        PreparedNode error scalar q qCotangent u uCotangent v vCotangent ->
        PreparedForm error scalar (p, q) (pCotangent, qCotangent) (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)
    PreparedPairInput ::
        PreparedNode error scalar p pCotangent x xCotangent y yCotangent ->
        PreparedNode error scalar q qCotangent x xCotangent z zCotangent ->
        PreparedForm error scalar (p, q) (pCotangent, qCotangent) x xCotangent (y, z) (yCotangent, zCotangent)
    PreparedShareParameter ::
        PreparedNode error scalar p pCotangent x xCotangent y yCotangent ->
        PreparedNode error scalar p pCotangent u uCotangent v vCotangent ->
        PreparedForm error scalar p pCotangent (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)

nodeOwnership :: PreparedNode error scalar p pc x xc y yc -> ParameterOwnership
nodeOwnership (PreparedNode _ ownership _ _ _ _ _ _ _) = ownership

nodeParameterPrimal :: PreparedNode error scalar p pc x xc y yc -> FinitePrimalSpace error p
nodeParameterPrimal (PreparedNode _ _ space _ _ _ _ _ _) = space

nodeParameterCotangent :: PreparedNode error scalar p pc x xc y yc -> CotangentSpace error scalar pc
nodeParameterCotangent (PreparedNode _ _ _ space _ _ _ _ _) = space

nodeInputPrimal :: PreparedNode error scalar p pc x xc y yc -> FinitePrimalSpace error x
nodeInputPrimal (PreparedNode _ _ _ _ space _ _ _ _) = space

nodeInputCotangent :: PreparedNode error scalar p pc x xc y yc -> CotangentSpace error scalar xc
nodeInputCotangent (PreparedNode _ _ _ _ _ space _ _ _) = space

nodeOutputPrimal :: PreparedNode error scalar p pc x xc y yc -> FinitePrimalSpace error y
nodeOutputPrimal (PreparedNode _ _ _ _ _ _ space _ _) = space

nodeOutputCotangent :: PreparedNode error scalar p pc x xc y yc -> CotangentSpace error scalar yc
nodeOutputCotangent (PreparedNode _ _ _ _ _ _ _ space _) = space

-- Report inventory.
data PrimitiveUse = PrimitiveUse !String !String !PrimitiveTapePolicy !Natural
    deriving (Eq, Show)

-- | Opaque deterministic structural inventory for a prepared program.
data ReverseProgramReport
    = ReverseProgramReport
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        ![String]
        !ParameterOwnership
        !Natural
        !Natural
        ![PrimitiveUse]
    deriving (Eq, Show)

-- | Read the deterministic report retained by a prepared program.
preparedReverseProgramReport :: PreparedReverseProgram primitive error scalar p pc x xc y yc -> ReverseProgramReport
preparedReverseProgramReport (PreparedReverseProgram _ report) = report

-- | Render a stable timing-free semantic report.
renderReverseProgramReport :: ReverseProgramReport -> String
renderReverseProgramReport (ReverseProgramReport nodes primitives depth identities compositions tensors pairs shares stored recomputed owners ownership primalExtent cotangentExtent uses) =
    unlines
        [ "reverse-program-report"
        , "nodes: " ++ show nodes
        , "primitives: " ++ show primitives
        , "maximum-depth: " ++ show depth
        , "identity/composition/tensor/input-share/parameter-share: " ++ slash [identities, compositions, tensors, pairs, shares]
        , "primitive-tapes stored/recomputed: " ++ slash [stored, recomputed]
        , "owners: " ++ show owners
        , "ownership-tree: " ++ parameterOwnershipDescription ownership
        , "maximum-primal-extent: " ++ show primalExtent
        , "maximum-cotangent-extent: " ++ show cotangentExtent
        , "primitive-uses: " ++ show (map renderUse uses)
        ]
  where
    slash = joinWith "/" . map show
    renderUse (PrimitiveUse name revision policy count) = name ++ "@" ++ revision ++ ":" ++ show policy ++ ":" ++ show count

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [value] = value
joinWith separator (value : values) = value ++ separator ++ joinWith separator values

data Cursor = Cursor
    { cursorNodes :: !Natural
    , cursorPrimitives :: !Natural
    , cursorDepth :: !Natural
    , cursorIdentities :: !Natural
    , cursorCompositions :: !Natural
    , cursorTensors :: !Natural
    , cursorPairs :: !Natural
    , cursorShares :: !Natural
    , cursorStored :: !Natural
    , cursorRecomputed :: !Natural
    , cursorOwners :: ![String]
    , cursorPrimalExtent :: !Natural
    , cursorCotangentExtent :: !Natural
    , cursorUses :: ![PrimitiveUse]
    }

emptyCursor :: Cursor
emptyCursor = Cursor 0 0 0 0 0 0 0 0 0 0 [] 0 0 []

-- | Resolve and validate a complete program. Failure returns no prepared tree.
prepareReverseProgram ::
    ReverseLimits ->
    ReversePrimitiveResolver primitive error scalar ->
    ReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    Either (ReverseProgramError error) (PreparedReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
prepareReverseProgram limits resolver program = do
    (prepared, cursor) <- prepareNode limits resolver [] 1 emptyCursor program
    let report =
            ReverseProgramReport
                (cursorNodes cursor)
                (cursorPrimitives cursor)
                (cursorDepth cursor)
                (cursorIdentities cursor)
                (cursorCompositions cursor)
                (cursorTensors cursor)
                (cursorPairs cursor)
                (cursorShares cursor)
                (cursorStored cursor)
                (cursorRecomputed cursor)
                (cursorOwners cursor)
                (nodeOwnership prepared)
                (cursorPrimalExtent cursor)
                (cursorCotangentExtent cursor)
                (cursorUses cursor)
    Right (PreparedReverseProgram prepared report)

prepareNode ::
    ReverseLimits ->
    ReversePrimitiveResolver primitive error scalar ->
    [ReversePathStep] ->
    Natural ->
    Cursor ->
    ReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    Either (ReverseProgramError error) (PreparedNode error scalar parameter parameterCotangent input inputCotangent output outputCotangent, Cursor)
prepareNode limits resolver path depth cursor program = do
    charged <- chargeNode limits path depth cursor
    case program of
        PrimitiveProgram primitive -> do
            definition@(OwnedReversePrimitive name revision ownership parameterPrimal inputPrimal outputPrimal circuit policy recomputation) <-
                mapLeft (ReverseDefinitionFailure path) (resolver primitive)
            when (policy == RecomputePrimitive && isNothing recomputation) $
                Left (ReverseDefinitionFailure path MissingPrimitiveRecomputation)
            (ownershipLayoutChecked, ownerKeys) <- checkOwnership limits path ownership
            parameterLayout <- checkOneLayout limits path (primalFiniteLayout parameterPrimal)
            when (ownershipLayoutChecked /= parameterLayout) $
                Left (ReverseDefinitionFailure path PrimitiveOwnershipLayoutMismatch)
            when (hasDuplicate ownerKeys) $
                Left (ReverseDefinitionFailure path DuplicateOwnerInsidePrimitive)
            withPrimitive <- chargePrimitive limits path name revision policy ownerKeys charged
            checked <- checkNodeSpaces limits path withPrimitive [primalFiniteLayout parameterPrimal, primalFiniteLayout inputPrimal, primalFiniteLayout outputPrimal] [cotangentMetadata (reverseParameterCotangentSpace circuit), cotangentMetadata (reverseInputCotangentSpace circuit), cotangentMetadata (reverseOutputCotangentSpace circuit)]
            Right
                ( PreparedNode path ownership parameterPrimal (reverseParameterCotangentSpace circuit) inputPrimal (reverseInputCotangentSpace circuit) outputPrimal (reverseOutputCotangentSpace circuit) (PreparedPrimitive definition)
                , checked
                )
        IdentityProgram primal cotangent -> do
            unitCotangent <- mapLeft (ReverseDefinitionFailure path) makeUnitCotangent
            checked <- checkNodeSpaces limits path charged{cursorIdentities = cursorIdentities charged + 1} [unitFiniteLayout, primalFiniteLayout primal] [cotangentMetadata unitCotangent, cotangentMetadata cotangent]
            Right (PreparedNode path NoParameterOwnership unitPrimalSpace unitCotangent primal cotangent primal cotangent PreparedIdentity, checked)
        ComposeProgram first second -> do
            (preparedFirst, afterFirst) <- prepareNode limits resolver (path ++ [CompositionLeft]) (depth + 1) charged first
            (preparedSecond, afterSecond) <- prepareNode limits resolver (path ++ [CompositionRight]) (depth + 1) afterFirst second
            ensurePrimalMatch path (nodeOutputPrimal preparedFirst) (nodeInputPrimal preparedSecond)
            ensureCotangentMatch path (nodeOutputCotangent preparedFirst) (nodeInputCotangent preparedSecond)
            ownership <- independentOwnership path (nodeOwnership preparedFirst) (nodeOwnership preparedSecond)
            _ <- checkOwnership limits path ownership
            parameterCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeParameterCotangent preparedFirst) (nodeParameterCotangent preparedSecond))
            let parameterPrimal = productPrimalSpace (nodeParameterPrimal preparedFirst) (nodeParameterPrimal preparedSecond)
                next = afterSecond{cursorCompositions = cursorCompositions afterSecond + 1}
            checked <- checkNodeSpaces limits path next [primalFiniteLayout parameterPrimal] [cotangentMetadata parameterCotangent]
            Right (PreparedNode path ownership parameterPrimal parameterCotangent (nodeInputPrimal preparedFirst) (nodeInputCotangent preparedFirst) (nodeOutputPrimal preparedSecond) (nodeOutputCotangent preparedSecond) (PreparedCompose preparedFirst preparedSecond), checked)
        TensorProgram left right -> do
            (preparedLeft, afterLeft) <- prepareNode limits resolver (path ++ [TensorLeft]) (depth + 1) charged left
            (preparedRight, afterRight) <- prepareNode limits resolver (path ++ [TensorRight]) (depth + 1) afterLeft right
            ownership <- independentOwnership path (nodeOwnership preparedLeft) (nodeOwnership preparedRight)
            _ <- checkOwnership limits path ownership
            parameterCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeParameterCotangent preparedLeft) (nodeParameterCotangent preparedRight))
            inputCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeInputCotangent preparedLeft) (nodeInputCotangent preparedRight))
            outputCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeOutputCotangent preparedLeft) (nodeOutputCotangent preparedRight))
            let parameterPrimal = productPrimalSpace (nodeParameterPrimal preparedLeft) (nodeParameterPrimal preparedRight)
                inputPrimal = productPrimalSpace (nodeInputPrimal preparedLeft) (nodeInputPrimal preparedRight)
                outputPrimal = productPrimalSpace (nodeOutputPrimal preparedLeft) (nodeOutputPrimal preparedRight)
                next = afterRight{cursorTensors = cursorTensors afterRight + 1}
            checked <- checkNodeSpaces limits path next [primalFiniteLayout parameterPrimal, primalFiniteLayout inputPrimal, primalFiniteLayout outputPrimal] [cotangentMetadata parameterCotangent, cotangentMetadata inputCotangent, cotangentMetadata outputCotangent]
            Right (PreparedNode path ownership parameterPrimal parameterCotangent inputPrimal inputCotangent outputPrimal outputCotangent (PreparedTensor preparedLeft preparedRight), checked)
        PairInputProgram left right -> do
            (preparedLeft, afterLeft) <- prepareNode limits resolver (path ++ [InputPairLeft]) (depth + 1) charged left
            (preparedRight, afterRight) <- prepareNode limits resolver (path ++ [InputPairRight]) (depth + 1) afterLeft right
            ensurePrimalMatch path (nodeInputPrimal preparedLeft) (nodeInputPrimal preparedRight)
            ensureCotangentMatch path (nodeInputCotangent preparedLeft) (nodeInputCotangent preparedRight)
            ownership <- independentOwnership path (nodeOwnership preparedLeft) (nodeOwnership preparedRight)
            _ <- checkOwnership limits path ownership
            parameterCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeParameterCotangent preparedLeft) (nodeParameterCotangent preparedRight))
            outputCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeOutputCotangent preparedLeft) (nodeOutputCotangent preparedRight))
            let parameterPrimal = productPrimalSpace (nodeParameterPrimal preparedLeft) (nodeParameterPrimal preparedRight)
                outputPrimal = productPrimalSpace (nodeOutputPrimal preparedLeft) (nodeOutputPrimal preparedRight)
                next = afterRight{cursorPairs = cursorPairs afterRight + 1}
            checked <- checkNodeSpaces limits path next [primalFiniteLayout parameterPrimal, primalFiniteLayout outputPrimal] [cotangentMetadata parameterCotangent, cotangentMetadata outputCotangent]
            Right (PreparedNode path ownership parameterPrimal parameterCotangent (nodeInputPrimal preparedLeft) (nodeInputCotangent preparedLeft) outputPrimal outputCotangent (PreparedPairInput preparedLeft preparedRight), checked)
        ShareParameterProgram left right -> do
            (preparedLeft, afterLeft) <- prepareNode limits resolver (path ++ [ParameterShareLeft]) (depth + 1) charged left
            (preparedRight, afterRight) <- prepareNode limits resolver (path ++ [ParameterShareRight]) (depth + 1) afterLeft right
            ensurePrimalMatch path (nodeParameterPrimal preparedLeft) (nodeParameterPrimal preparedRight)
            ensureCotangentMatch path (nodeParameterCotangent preparedLeft) (nodeParameterCotangent preparedRight)
            when (nodeOwnership preparedLeft /= nodeOwnership preparedRight) $
                Left (MismatchedSharedOwnership path (parameterOwnershipDescription (nodeOwnership preparedLeft)) (parameterOwnershipDescription (nodeOwnership preparedRight)))
            inputCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeInputCotangent preparedLeft) (nodeInputCotangent preparedRight))
            outputCotangent <- mapLeft (ReverseDefinitionFailure path) (productCotangentWitness (nodeOutputCotangent preparedLeft) (nodeOutputCotangent preparedRight))
            let inputPrimal = productPrimalSpace (nodeInputPrimal preparedLeft) (nodeInputPrimal preparedRight)
                outputPrimal = productPrimalSpace (nodeOutputPrimal preparedLeft) (nodeOutputPrimal preparedRight)
                next = afterRight{cursorShares = cursorShares afterRight + 1}
            checked <- checkNodeSpaces limits path next [primalFiniteLayout inputPrimal, primalFiniteLayout outputPrimal] [cotangentMetadata inputCotangent, cotangentMetadata outputCotangent]
            Right (PreparedNode path (nodeOwnership preparedLeft) (nodeParameterPrimal preparedLeft) (nodeParameterCotangent preparedLeft) inputPrimal inputCotangent outputPrimal outputCotangent (PreparedShareParameter preparedLeft preparedRight), checked)

chargeNode :: ReverseLimits -> [ReversePathStep] -> Natural -> Cursor -> Either (ReverseProgramError error) Cursor
chargeNode (ReverseLimits nodeLimit _ depthLimit _ _ _ _ _) path depth cursor
    | cursorNodes cursor + 1 > nodeLimit = Left (ReverseNodeLimitExceeded path nodeLimit)
    | depth > depthLimit = Left (ReverseDepthLimitExceeded path depthLimit)
    | otherwise = Right cursor{cursorNodes = cursorNodes cursor + 1, cursorDepth = max depth (cursorDepth cursor)}

chargePrimitive :: ReverseLimits -> [ReversePathStep] -> String -> String -> PrimitiveTapePolicy -> [String] -> Cursor -> Either (ReverseProgramError error) Cursor
chargePrimitive (ReverseLimits _ primitiveLimit _ ownerLimit _ _ _ _) path name revision policy newOwnerKeys cursor
    | cursorPrimitives cursor + 1 > primitiveLimit = Left (ReversePrimitiveLimitExceeded path primitiveLimit)
    | fromIntegral (length owners) > ownerLimit = Left (ReverseOwnerLimitExceeded path ownerLimit)
    | otherwise =
        Right
            cursor
                { cursorPrimitives = cursorPrimitives cursor + 1
                , cursorStored = cursorStored cursor + if policy == StoreCapturedPullback then 1 else 0
                , cursorRecomputed = cursorRecomputed cursor + if policy == RecomputePrimitive then 1 else 0
                , cursorOwners = owners
                , cursorUses = addPrimitiveUse name revision policy (cursorUses cursor)
                }
  where
    owners = foldl addUnique (cursorOwners cursor) newOwnerKeys

addUnique :: (Eq value) => [value] -> value -> [value]
addUnique values value = if value `elem` values then values else values ++ [value]

addPrimitiveUse :: String -> String -> PrimitiveTapePolicy -> [PrimitiveUse] -> [PrimitiveUse]
addPrimitiveUse name revision policy [] = [PrimitiveUse name revision policy 1]
addPrimitiveUse name revision policy (entry@(PrimitiveUse oldName oldRevision oldPolicy count) : entries)
    | (name, revision, policy) == (oldName, oldRevision, oldPolicy) = PrimitiveUse oldName oldRevision oldPolicy (count + 1) : entries
    | otherwise = entry : addPrimitiveUse name revision policy entries

type CotangentMetadata = (Maybe FiniteLayout, Maybe String)

cotangentMetadata :: CotangentSpace error scalar cotangent -> CotangentMetadata
cotangentMetadata space = (cotangentFiniteLayout space, cotangentModuleOwner space)

checkNodeSpaces :: ReverseLimits -> [ReversePathStep] -> Cursor -> [FiniteLayout] -> [CotangentMetadata] -> Either (ReverseProgramError failure) Cursor
checkNodeSpaces limits@(ReverseLimits _ _ _ _ primalLimit cotangentLimit _ _) path cursor primalLayouts cotangentSpaces = do
    cotangentLayouts <- traverse declaredLayout cotangentSpaces
    checkedPrimals <- traverse (checkOneLayoutWithExtent limits path) primalLayouts
    checkedCotangents <- traverse (checkOneLayoutWithExtent limits path) cotangentLayouts
    case firstOver primalLimit checkedPrimals of
        Just (layout, _) -> Left (ReversePrimalLayoutLimitExceeded path layout primalLimit)
        Nothing -> pure ()
    case firstOver cotangentLimit checkedCotangents of
        Just (layout, _) -> Left (ReverseCotangentLayoutLimitExceeded path layout cotangentLimit)
        Nothing -> pure ()
    let primalExtent = maximumOrZero (map snd checkedPrimals)
        cotangentExtent = maximumOrZero (map snd checkedCotangents)
    Right cursor{cursorPrimalExtent = max primalExtent (cursorPrimalExtent cursor), cursorCotangentExtent = max cotangentExtent (cursorCotangentExtent cursor)}
  where
    declaredLayout metadata = case metadata of
        (Nothing, _) -> Left (ReverseDefinitionFailure path UndeclaredCotangentLayout)
        (_, Nothing) -> Left (ReverseDefinitionFailure path UndeclaredCotangentOwner)
        (Just layout, Just _) -> Right layout
    firstOver limit = firstMatching ((> limit) . snd)

checkOneLayout :: ReverseLimits -> [ReversePathStep] -> FiniteLayout -> Either (ReverseProgramError failure) FiniteLayout
checkOneLayout limits path layout = fst <$> checkOneLayoutWithExtent limits path layout

checkOneLayoutWithExtent :: ReverseLimits -> [ReversePathStep] -> FiniteLayout -> Either (ReverseProgramError failure) (FiniteLayout, Natural)
checkOneLayoutWithExtent (ReverseLimits _ _ _ _ _ _ structureNodes structureDepth) path layout =
    case checkedFiniteLayout structureNodes structureDepth layout of
        Left (FiniteLayoutNodeLimitExceeded limit) -> Left (ReverseLayoutNodeLimitExceeded path limit)
        Left (FiniteLayoutDepthLimitExceeded limit) -> Left (ReverseLayoutDepthLimitExceeded path limit)
        Right (checked, extent, _, _) -> Right (checked, extent)

{- | Validate ownership structure and every leaf layout before any unbounded
key enumeration, description, equality, or layout conversion occurs.
-}
checkOwnership :: ReverseLimits -> [ReversePathStep] -> ParameterOwnership -> Either (ReverseProgramError failure) (FiniteLayout, [String])
checkOwnership limits@(ReverseLimits _ _ _ _ _ _ structureNodes structureDepth) path ownership = do
    (layout, keys, _, _) <- go 1 0 ownership
    Right (layout, keys)
  where
    go depth used current
        | used >= structureNodes = Left (ReverseOwnershipNodeLimitExceeded path structureNodes)
        | depth > structureDepth = Left (ReverseOwnershipDepthLimitExceeded path structureDepth)
        | otherwise = case current of
            NoParameterOwnership -> Right (unitFiniteLayout, [], used + 1, depth)
            ParameterOwnerLeaf key layout -> do
                checkedLayout <- checkOneLayout limits path layout
                Right (checkedLayout, [key], used + 1, depth)
            ParameterOwnershipProduct left right -> do
                (leftLayout, leftKeys, afterLeft, leftDepth) <- go (depth + 1) (used + 1) left
                (rightLayout, rightKeys, afterRight, rightDepth) <- go (depth + 1) afterLeft right
                Right (productFiniteLayout leftLayout rightLayout, leftKeys ++ rightKeys, afterRight, max leftDepth rightDepth)

firstMatching :: (value -> Bool) -> [value] -> Maybe value
firstMatching _ [] = Nothing
firstMatching predicate (value : values) = if predicate value then Just value else firstMatching predicate values

maximumOrZero :: [Natural] -> Natural
maximumOrZero = foldl max 0

ensurePrimalMatch :: [ReversePathStep] -> FinitePrimalSpace error left -> FinitePrimalSpace error right -> Either (ReverseProgramError failure) ()
ensurePrimalMatch path left right
    | compatiblePrimalSpace left right = Right ()
    | otherwise = Left (ReversePrimalLayoutMismatch path (primalFiniteLayout left) (primalFiniteLayout right))

ensureCotangentMatch :: [ReversePathStep] -> CotangentSpace error scalar left -> CotangentSpace error scalar right -> Either (ReverseProgramError failure) ()
ensureCotangentMatch path left right
    | compatibleCotangentSpace left right = Right ()
    | otherwise = Left (ReverseCotangentLayoutMismatch path (describeCotangent left) (describeCotangent right))

describeCotangent :: CotangentSpace error scalar cotangent -> String
describeCotangent space = show (cotangentModuleOwner space, fmap finiteLayoutDescription (cotangentFiniteLayout space), cotangentEqualityMode space)

independentOwnership :: [ReversePathStep] -> ParameterOwnership -> ParameterOwnership -> Either (ReverseProgramError error) ParameterOwnership
independentOwnership path left right = case firstDuplicate (parameterOwnerKeys left ++ parameterOwnerKeys right) of
    Just duplicate -> Left (DuplicateIndependentOwner path duplicate)
    Nothing -> Right (ParameterOwnershipProduct left right)

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate = go []
  where
    go _ [] = Nothing
    go seen (value : values)
        | value `elem` seen = Just value
        | otherwise = go (value : seen) values

hasDuplicate :: (Eq value) => [value] -> Bool
hasDuplicate = isJust . firstDuplicate

productCotangentWitness :: CotangentSpace error scalar left -> CotangentSpace error scalar right -> Either ReverseDefinitionError (CotangentSpace error scalar (left, right))
productCotangentWitness left right = case (cotangentFiniteLayout left, cotangentFiniteLayout right, cotangentModuleOwner left, cotangentModuleOwner right) of
    (Just leftLayout, Just rightLayout, Just leftOwner, Just rightOwner) ->
        maybe (Left UndeclaredCotangentOwner) Right $
            declaredCotangentSpace
                ("(" ++ leftOwner ++ " * " ++ rightOwner ++ ")")
                (productFiniteLayout leftLayout rightLayout)
                (\(leftValue, rightValue) -> validateCotangent left leftValue >> validateCotangent right rightValue)
                (cotangentZero left, cotangentZero right)
                (\(leftA, rightA) (leftB, rightB) -> (,) <$> addCotangents left leftA leftB <*> addCotangents right rightA rightB)
                (\scalar (leftValue, rightValue) -> (,) <$> scaleCotangent left scalar leftValue <*> scaleCotangent right scalar rightValue)
                (\(leftA, rightA) (leftB, rightB) -> cotangentsEquivalent left leftA leftB && cotangentsEquivalent right rightA rightB)
                (combineModes (cotangentEqualityMode left) (cotangentEqualityMode right))
    (Nothing, _, _, _) -> Left UndeclaredCotangentLayout
    (_, Nothing, _, _) -> Left UndeclaredCotangentLayout
    (_, _, Nothing, _) -> Left UndeclaredCotangentOwner
    (_, _, _, Nothing) -> Left UndeclaredCotangentOwner

makeUnitCotangent :: Either ReverseDefinitionError (CotangentSpace error scalar ())
makeUnitCotangent =
    maybe (Left UndeclaredCotangentOwner) Right $
        declaredCotangentSpace "unit" unitFiniteLayout (const (Right ())) () (\() () -> Right ()) (\_ () -> Right ()) (==) ExactCotangentEquality

combineModes :: CotangentEqualityMode -> CotangentEqualityMode -> CotangentEqualityMode
combineModes ExactCotangentEquality ExactCotangentEquality = ExactCotangentEquality
combineModes left right = ApproximateCotangentEquality ("product of " ++ show left ++ " and " ++ show right)

mapLeft :: (left -> newLeft) -> Either left right -> Either newLeft right
mapLeft function result = case result of
    Left value -> Left (function value)
    Right value -> Right value

-- | Opaque typed self-contained reverse tape for exactly one forward run.
data ReverseTape error scalar parameter parameterCotangent input inputCotangent output outputCotangent where
    StoredPrimitiveTape ::
        [ReversePathStep] ->
        OwnedReversePrimitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
        ReverseEvaluation error parameterCotangent inputCotangent output outputCotangent ->
        ReverseTape error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    RecomputedPrimitiveTape ::
        [ReversePathStep] ->
        OwnedReversePrimitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
        parameter ->
        input ->
        output ->
        ReverseTape error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    IdentityTape ::
        [ReversePathStep] ->
        CotangentSpace error scalar cotangent ->
        ReverseTape error scalar () () value cotangent value cotangent
    ComposeTape ::
        ReverseTape error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseTape error scalar q qCotangent y yCotangent z zCotangent ->
        ReverseTape error scalar (p, q) (pCotangent, qCotangent) x xCotangent z zCotangent
    TensorTape ::
        ReverseTape error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseTape error scalar q qCotangent u uCotangent v vCotangent ->
        ReverseTape error scalar (p, q) (pCotangent, qCotangent) (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)
    PairInputTape ::
        [ReversePathStep] ->
        CotangentSpace error scalar xCotangent ->
        ReverseTape error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseTape error scalar q qCotangent x xCotangent z zCotangent ->
        ReverseTape error scalar (p, q) (pCotangent, qCotangent) x xCotangent (y, z) (yCotangent, zCotangent)
    ShareParameterTape ::
        [ReversePathStep] ->
        CotangentSpace error scalar pCotangent ->
        ReverseTape error scalar p pCotangent x xCotangent y yCotangent ->
        ReverseTape error scalar p pCotangent u uCotangent v vCotangent ->
        ReverseTape error scalar p pCotangent (x, u) (xCotangent, uCotangent) (y, v) (yCotangent, vCotangent)

-- | Forward result and its self-contained tape.
data ReverseRun error scalar parameter parameterCotangent input inputCotangent output outputCotangent
    = ReverseRunValue
        !output
        !(ReverseTape error scalar parameter parameterCotangent input inputCotangent output outputCotangent)

-- | Observe the checked primal output of a forward run.
reverseRunOutput :: ReverseRun error scalar p pc x xc y yc -> y
reverseRunOutput (ReverseRunValue output _) = output

-- | Read the opaque self-contained tape produced by a forward run.
reverseRunTape :: ReverseRun error scalar p pc x xc y yc -> ReverseTape error scalar p pc x xc y yc
reverseRunTape (ReverseRunValue _ tape) = tape

-- | Structural tape counts; no byte or timing estimate is implied.
data ReverseTapeReport = ReverseTapeReport
    { reverseTapeNodes :: !Natural
    , reverseStoredPrimitiveCount :: !Natural
    , reverseRecomputedPrimitiveCount :: !Natural
    }
    deriving (Eq, Show)

-- | Count tape nodes and primitive policies without estimating bytes.
reverseTapeReport :: ReverseTape error scalar p pc x xc y yc -> ReverseTapeReport
reverseTapeReport tape = case tape of
    StoredPrimitiveTape{} -> ReverseTapeReport 1 1 0
    RecomputedPrimitiveTape{} -> ReverseTapeReport 1 0 1
    IdentityTape{} -> ReverseTapeReport 1 0 0
    ComposeTape left right -> combineTapeReports (reverseTapeReport left) (reverseTapeReport right)
    TensorTape left right -> combineTapeReports (reverseTapeReport left) (reverseTapeReport right)
    PairInputTape _ _ left right -> combineTapeReports (reverseTapeReport left) (reverseTapeReport right)
    ShareParameterTape _ _ left right -> combineTapeReports (reverseTapeReport left) (reverseTapeReport right)

combineTapeReports :: ReverseTapeReport -> ReverseTapeReport -> ReverseTapeReport
combineTapeReports (ReverseTapeReport leftNodes leftStored leftRecomputed) (ReverseTapeReport rightNodes rightStored rightRecomputed) =
    ReverseTapeReport (1 + leftNodes + rightNodes) (leftStored + rightStored) (leftRecomputed + rightRecomputed)

-- | Run a fully prepared program left-to-right and retain an explicit tape.
runPreparedReverse ::
    PreparedReverseProgram primitive error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    parameter ->
    input ->
    Either (ReverseProgramError error) (ReverseRun error scalar parameter parameterCotangent input inputCotangent output outputCotangent)
runPreparedReverse (PreparedReverseProgram node _) = runNode node

runNode :: PreparedNode error scalar p pc x xc y yc -> p -> x -> Either (ReverseProgramError error) (ReverseRun error scalar p pc x xc y yc)
runNode (PreparedNode path _ parameterSpace _ inputSpace inputCotangentSpace outputSpace _ form) parameter input = do
    mapLeft (ReversePrimalValidationFailure path ParameterPrimalStage) (validatePrimal parameterSpace parameter)
    mapLeft (ReversePrimalValidationFailure path InputPrimalStage) (validatePrimal inputSpace input)
    run <- case form of
        PreparedPrimitive definition@(OwnedReversePrimitive _ _ _ _ _ _ circuit policy _) -> do
            evaluation <- mapLeft (ReversePrimitiveForwardFailure path) (evaluateReverseCircuit circuit parameter input)
            let output = reversePrimalOutput evaluation
            mapLeft (ReversePrimalValidationFailure path OutputPrimalStage) (validatePrimal outputSpace output)
            let tape = case policy of
                    StoreCapturedPullback -> StoredPrimitiveTape path definition evaluation
                    RecomputePrimitive -> RecomputedPrimitiveTape path definition parameter input output
            Right (ReverseRunValue output tape)
        PreparedIdentity -> Right (ReverseRunValue input (IdentityTape path inputCotangentSpace))
        PreparedCompose first second -> do
            ReverseRunValue middle firstTape <- runNode first (fst parameter) input
            ReverseRunValue output secondTape <- runNode second (snd parameter) middle
            Right (ReverseRunValue output (ComposeTape firstTape secondTape))
        PreparedTensor left right -> do
            ReverseRunValue leftOutput leftTape <- runNode left (fst parameter) (fst input)
            ReverseRunValue rightOutput rightTape <- runNode right (snd parameter) (snd input)
            Right (ReverseRunValue (leftOutput, rightOutput) (TensorTape leftTape rightTape))
        PreparedPairInput left right -> do
            ReverseRunValue leftOutput leftTape <- runNode left (fst parameter) input
            ReverseRunValue rightOutput rightTape <- runNode right (snd parameter) input
            Right (ReverseRunValue (leftOutput, rightOutput) (PairInputTape path (nodeInputCotangent left) leftTape rightTape))
        PreparedShareParameter left right -> do
            ReverseRunValue leftOutput leftTape <- runNode left parameter (fst input)
            ReverseRunValue rightOutput rightTape <- runNode right parameter (snd input)
            Right (ReverseRunValue (leftOutput, rightOutput) (ShareParameterTape path (nodeParameterCotangent left) leftTape rightTape))
    mapLeft (ReversePrimalValidationFailure path OutputPrimalStage) (validatePrimal outputSpace (reverseRunOutput run))
    Right run

{- | Apply a self-contained tape. No program argument exists, so a tape cannot
be paired with different syntax.
-}
applyReverseTape ::
    ReverseTape error scalar parameter parameterCotangent input inputCotangent output outputCotangent ->
    outputCotangent ->
    Either (ReverseProgramError error) (parameterCotangent, inputCotangent)
applyReverseTape tape outputCotangent = case tape of
    StoredPrimitiveTape path definition evaluation -> primitivePullback path definition evaluation outputCotangent
    RecomputedPrimitiveTape path definition@(OwnedReversePrimitive _ _ _ _ _ outputSpace _ _ recomputation) parameter input oldOutput ->
        case recomputation of
            Nothing -> Left (ReverseDefinitionFailure path MissingPrimitiveRecomputation)
            Just (PrimitiveRecomputation recompute) -> do
                evaluation <- mapLeft (ReversePrimitiveRecomputationFailure path) (recompute parameter input)
                let newOutput = reversePrimalOutput evaluation
                mapLeft (ReversePrimalValidationFailure path OutputPrimalStage) (validatePrimal outputSpace newOutput)
                if primalsEquivalent outputSpace oldOutput newOutput
                    then primitivePullback path definition evaluation outputCotangent
                    else Left (ReverseRecomputedOutputMismatch path)
    IdentityTape path space -> do
        mapLeft (ReverseCotangentValidationFailure path OutputCotangentStage) (validateCotangent space outputCotangent)
        Right ((), outputCotangent)
    ComposeTape first second -> do
        (secondParameterCotangent, middleCotangent) <- applyReverseTape second outputCotangent
        (firstParameterCotangent, inputCotangent) <- applyReverseTape first middleCotangent
        Right ((firstParameterCotangent, secondParameterCotangent), inputCotangent)
    TensorTape left right -> do
        (leftParameterCotangent, leftInputCotangent) <- applyReverseTape left (fst outputCotangent)
        (rightParameterCotangent, rightInputCotangent) <- applyReverseTape right (snd outputCotangent)
        Right ((leftParameterCotangent, rightParameterCotangent), (leftInputCotangent, rightInputCotangent))
    PairInputTape path inputSpace left right -> do
        (leftParameterCotangent, leftInputCotangent) <- applyReverseTape left (fst outputCotangent)
        (rightParameterCotangent, rightInputCotangent) <- applyReverseTape right (snd outputCotangent)
        inputCotangent <- checkedAddition path InputCotangentStage inputSpace leftInputCotangent rightInputCotangent
        Right ((leftParameterCotangent, rightParameterCotangent), inputCotangent)
    ShareParameterTape path parameterSpace left right -> do
        (leftParameterCotangent, leftInputCotangent) <- applyReverseTape left (fst outputCotangent)
        (rightParameterCotangent, rightInputCotangent) <- applyReverseTape right (snd outputCotangent)
        parameterCotangent <- checkedAddition path ParameterCotangentStage parameterSpace leftParameterCotangent rightParameterCotangent
        Right (parameterCotangent, (leftInputCotangent, rightInputCotangent))

primitivePullback ::
    [ReversePathStep] ->
    OwnedReversePrimitive error scalar p pc x xc y yc ->
    ReverseEvaluation error pc xc y yc ->
    yc ->
    Either (ReverseProgramError error) (pc, xc)
primitivePullback path (OwnedReversePrimitive _ _ _ _ _ _ circuit _ _) evaluation outputCotangent = do
    mapLeft (ReverseCotangentValidationFailure path OutputCotangentStage) (validateCotangent (reverseOutputCotangentSpace circuit) outputCotangent)
    (parameterCotangent, inputCotangent) <- mapLeft (ReversePrimitivePullbackFailure path) (applyReverseVJP evaluation outputCotangent)
    mapLeft (ReverseCotangentValidationFailure path ParameterCotangentStage) (validateCotangent (reverseParameterCotangentSpace circuit) parameterCotangent)
    mapLeft (ReverseCotangentValidationFailure path InputCotangentStage) (validateCotangent (reverseInputCotangentSpace circuit) inputCotangent)
    Right (parameterCotangent, inputCotangent)

checkedAddition ::
    [ReversePathStep] -> ReverseStage -> CotangentSpace error scalar cotangent -> cotangent -> cotangent -> Either (ReverseProgramError error) cotangent
checkedAddition path stage space left right = do
    mapLeft (ReverseCotangentValidationFailure path stage) (validateCotangent space left)
    mapLeft (ReverseCotangentValidationFailure path stage) (validateCotangent space right)
    result <- mapLeft (ReverseCotangentAdditionFailure path stage) (addCotangents space left right)
    mapLeft (ReverseCotangentValidationFailure path stage) (validateCotangent space result)
    Right result
