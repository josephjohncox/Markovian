{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Structured cospans of finite typed hypergraphs and their double-category
cell fragment.

Horizontal composition is pushout gluing along discrete boundaries. Tensor is
disjoint union. Boundary reversal swaps cospan legs only; it is unrelated to
matrix transpose and prior-indexed Bayesian inversion.
-}
module Markovian.Open.StructuredCospan (
    OpenSystem,
    OpenSystemError (..),
    openSystem,
    openInput,
    openOutput,
    openApex,
    openInputLeg,
    openOutputLeg,
    identityOpenSystem,
    composeOpenSystem,
    tensorOpenSystem,
    reverseOpenBoundary,
    sameOpenSystemLayout,
    OpenSystemCell,
    openSystemCell,
    cellSource,
    cellTarget,
    cellInputMap,
    cellOutputMap,
    cellApexMap,
    identityOpenSystemCell,
    verticalComposeOpenSystemCell,
    horizontalComposeOpenSystemCell,
    tensorOpenSystemCell,
) where

import Data.Bifunctor (bimap)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void (Void)
import Markovian.Open.Hypergraph
import Markovian.Open.Interface
import Markovian.Open.Pushout

-- | Opaque structured cospan @L input -> apex <- L output@.
type role OpenSystem nominal nominal nominal nominal nominal nominal

data OpenSystem sort input output vertex edge label where
    UnsafeOpenSystem ::
        !(Interface sort input) ->
        !(Interface sort output) ->
        !(TypedHypergraph sort vertex edge label) ->
        !(InterfaceMap sort input vertex) ->
        !(InterfaceMap sort output vertex) ->
        OpenSystem sort input output vertex edge label

-- | Open-system, gluing, or cell validation failure.
data OpenSystemError
    = OpenInputLegSourceMismatch
    | OpenOutputLegSourceMismatch
    | OpenInputLegApexMismatch
    | OpenOutputLegApexMismatch
    | OpenSequentialBoundaryMismatch
    | OpenInterfaceMapError !InterfaceMapError
    | OpenPushoutError !PushoutError
    | OpenHypergraphError !HypergraphError
    | OpenHypergraphMapError !HypergraphMapError
    | OpenCellSourceGraphMismatch
    | OpenCellTargetGraphMismatch
    | OpenCellInputBoundaryMismatch
    | OpenCellOutputBoundaryMismatch
    | OpenCellInputSquareMismatch
    | OpenCellOutputSquareMismatch
    | OpenCellHorizontalBoundaryMismatch
    | OpenInternalInvariantFailure
    deriving (Eq, Show)

{- | Validate a structured cospan. Legs are total and type preserving but need
not be injective.
-}
openSystem ::
    Interface sort input ->
    Interface sort output ->
    TypedHypergraph sort vertex edge label ->
    InterfaceMap sort input vertex ->
    InterfaceMap sort output vertex ->
    Either OpenSystemError (OpenSystem sort input output vertex edge label)
openSystem input output apex inputLeg outputLeg
    | not (sameInterfaceLayout input (interfaceMapSource inputLeg)) =
        Left OpenInputLegSourceMismatch
    | not (sameInterfaceLayout output (interfaceMapSource outputLeg)) =
        Left OpenOutputLegSourceMismatch
    | not (sameInterfaceLayout (hypergraphVertices apex) (interfaceMapTarget inputLeg)) =
        Left OpenInputLegApexMismatch
    | not (sameInterfaceLayout (hypergraphVertices apex) (interfaceMapTarget outputLeg)) =
        Left OpenOutputLegApexMismatch
    | otherwise = Right (UnsafeOpenSystem input output apex inputLeg outputLeg)

-- | Read the input interface.
openInput :: OpenSystem sort input output vertex edge label -> Interface sort input
openInput (UnsafeOpenSystem input _ _ _ _) = input

-- | Read the output interface.
openOutput :: OpenSystem sort input output vertex edge label -> Interface sort output
openOutput (UnsafeOpenSystem _ output _ _ _) = output

-- | Read the apex hypergraph.
openApex ::
    OpenSystem sort input output vertex edge label ->
    TypedHypergraph sort vertex edge label
openApex (UnsafeOpenSystem _ _ apex _ _) = apex

-- | Read the input leg.
openInputLeg ::
    OpenSystem sort input output vertex edge label ->
    InterfaceMap sort input vertex
openInputLeg (UnsafeOpenSystem _ _ _ inputLeg _) = inputLeg

-- | Read the output leg.
openOutputLeg ::
    OpenSystem sort input output vertex edge label ->
    InterfaceMap sort output vertex
openOutputLeg (UnsafeOpenSystem _ _ _ _ outputLeg) = outputLeg

-- | Horizontal identity cospan on one interface.
identityOpenSystem ::
    (Eq sort, Eq port, Eq label) =>
    Interface sort port ->
    OpenSystem sort port port port Void label
identityOpenSystem object =
    UnsafeOpenSystem object object apex identityLeg identityLeg
  where
    apex = discreteHypergraph object
    identityLeg = identityInterfaceMap object

-- | Pushout composition of horizontal arrows.
composeOpenSystem ::
    ( Eq sort
    , Eq middle
    , Eq leftVertex
    , Eq rightVertex
    , Eq leftEdge
    , Eq rightEdge
    , Eq label
    ) =>
    OpenSystem sort input middle leftVertex leftEdge label ->
    OpenSystem sort middle output rightVertex rightEdge label ->
    Either
        OpenSystemError
        ( OpenSystem
            sort
            input
            output
            (PushoutPoint leftVertex rightVertex)
            (Either leftEdge rightEdge)
            label
        )
composeOpenSystem left right = fst <$> composeOpenSystemDetailed left right

-- | Disjoint-union tensor of horizontal arrows.
tensorOpenSystem ::
    OpenSystem sort leftInput leftOutput leftVertex leftEdge label ->
    OpenSystem sort rightInput rightOutput rightVertex rightEdge label ->
    OpenSystem
        sort
        (Either leftInput rightInput)
        (Either leftOutput rightOutput)
        (Either leftVertex rightVertex)
        (Either leftEdge rightEdge)
        label
tensorOpenSystem left right =
    UnsafeOpenSystem
        (tensorInterface (openInput left) (openInput right))
        (tensorInterface (openOutput left) (openOutput right))
        (tensorHypergraph (openApex left) (openApex right))
        (tensorInterfaceMap (openInputLeg left) (openInputLeg right))
        (tensorInterfaceMap (openOutputLeg left) (openOutputLeg right))

{- | Reverse only the structured-cospan boundary orientation.

Hyperedges and labels are unchanged. This operation is not matrix conjugate
transpose, Bayesian inversion, or a common @Dagger@ instance.
-}
reverseOpenBoundary ::
    OpenSystem sort input output vertex edge label ->
    OpenSystem sort output input vertex edge label
reverseOpenBoundary (UnsafeOpenSystem input output apex inputLeg outputLeg) =
    UnsafeOpenSystem output input apex outputLeg inputLeg

-- | Compare complete represented cospan layout.
sameOpenSystemLayout ::
    (Eq input, Eq output, Eq vertex) =>
    OpenSystem sort input output vertex edge label ->
    OpenSystem sort input output vertex edge label ->
    Bool
sameOpenSystemLayout left right =
    sameInterfaceLayout (openInput left) (openInput right)
        && sameInterfaceLayout (openOutput left) (openOutput right)
        && sameHypergraphLayout (openApex left) (openApex right)
        && interfaceMapEntries (openInputLeg left) == interfaceMapEntries (openInputLeg right)
        && interfaceMapEntries (openOutputLeg left) == interfaceMapEntries (openOutputLeg right)

{- | A square in the structured-cospan double fragment.

The two vertical arrows are interface maps and the apex map preserves the typed
hypergraph structure. Both boundary squares commute exactly.
-}
type role OpenSystemCell nominal nominal nominal nominal nominal nominal nominal nominal nominal nominal

data
    OpenSystemCell
        sort
        sourceInput
        sourceOutput
        targetInput
        targetOutput
        sourceVertex
        sourceEdge
        targetVertex
        targetEdge
        label
    where
    UnsafeOpenSystemCell ::
        !(OpenSystem sort sourceInput sourceOutput sourceVertex sourceEdge label) ->
        !(OpenSystem sort targetInput targetOutput targetVertex targetEdge label) ->
        !(InterfaceMap sort sourceInput targetInput) ->
        !(InterfaceMap sort sourceOutput targetOutput) ->
        !(HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label) ->
        OpenSystemCell
            sort
            sourceInput
            sourceOutput
            targetInput
            targetOutput
            sourceVertex
            sourceEdge
            targetVertex
            targetEdge
            label

-- | Validate a commuting open-system square.
openSystemCell ::
    (Eq targetVertex) =>
    OpenSystem sort sourceInput sourceOutput sourceVertex sourceEdge label ->
    OpenSystem sort targetInput targetOutput targetVertex targetEdge label ->
    InterfaceMap sort sourceInput targetInput ->
    InterfaceMap sort sourceOutput targetOutput ->
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    Either
        OpenSystemError
        ( OpenSystemCell
            sort
            sourceInput
            sourceOutput
            targetInput
            targetOutput
            sourceVertex
            sourceEdge
            targetVertex
            targetEdge
            label
        )
openSystemCell source target inputMap outputMap apexMap
    | not (sameHypergraphLayout (openApex source) (hypergraphMapSource apexMap)) =
        Left OpenCellSourceGraphMismatch
    | not (sameHypergraphLayout (openApex target) (hypergraphMapTarget apexMap)) =
        Left OpenCellTargetGraphMismatch
    | not (sameInterfaceLayout (openInput source) (interfaceMapSource inputMap))
        || not (sameInterfaceLayout (openInput target) (interfaceMapTarget inputMap)) =
        Left OpenCellInputBoundaryMismatch
    | not (sameInterfaceLayout (openOutput source) (interfaceMapSource outputMap))
        || not (sameInterfaceLayout (openOutput target) (interfaceMapTarget outputMap)) =
        Left OpenCellOutputBoundaryMismatch
    | not (commutes (openInputLeg source) inputMap (openInputLeg target) apexMap) =
        Left OpenCellInputSquareMismatch
    | not (commutes (openOutputLeg source) outputMap (openOutputLeg target) apexMap) =
        Left OpenCellOutputSquareMismatch
    | otherwise = Right (UnsafeOpenSystemCell source target inputMap outputMap apexMap)

-- | Read the source horizontal arrow.
cellSource ::
    OpenSystemCell sort sourceInput sourceOutput targetInput targetOutput sourceVertex sourceEdge targetVertex targetEdge label ->
    OpenSystem sort sourceInput sourceOutput sourceVertex sourceEdge label
cellSource (UnsafeOpenSystemCell source _ _ _ _) = source

-- | Read the target horizontal arrow.
cellTarget ::
    OpenSystemCell sort sourceInput sourceOutput targetInput targetOutput sourceVertex sourceEdge targetVertex targetEdge label ->
    OpenSystem sort targetInput targetOutput targetVertex targetEdge label
cellTarget (UnsafeOpenSystemCell _ target _ _ _) = target

-- | Read the left vertical boundary.
cellInputMap ::
    OpenSystemCell sort sourceInput sourceOutput targetInput targetOutput sourceVertex sourceEdge targetVertex targetEdge label ->
    InterfaceMap sort sourceInput targetInput
cellInputMap (UnsafeOpenSystemCell _ _ inputMap _ _) = inputMap

-- | Read the right vertical boundary.
cellOutputMap ::
    OpenSystemCell sort sourceInput sourceOutput targetInput targetOutput sourceVertex sourceEdge targetVertex targetEdge label ->
    InterfaceMap sort sourceOutput targetOutput
cellOutputMap (UnsafeOpenSystemCell _ _ _ outputMap _) = outputMap

-- | Read the structure-preserving apex map.
cellApexMap ::
    OpenSystemCell sort sourceInput sourceOutput targetInput targetOutput sourceVertex sourceEdge targetVertex targetEdge label ->
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label
cellApexMap (UnsafeOpenSystemCell _ _ _ _ apexMap) = apexMap

-- | Identity 2-cell.
identityOpenSystemCell ::
    OpenSystem sort input output vertex edge label ->
    OpenSystemCell sort input output input output vertex edge vertex edge label
identityOpenSystemCell system =
    UnsafeOpenSystemCell
        system
        system
        (identityInterfaceMap (openInput system))
        (identityInterfaceMap (openOutput system))
        (identityHypergraphMap (openApex system))

-- | Vertical composition of commuting squares.
verticalComposeOpenSystemCell ::
    (Eq targetVertex) =>
    OpenSystemCell sort firstInput firstOutput middleInput middleOutput firstVertex firstEdge middleVertex middleEdge label ->
    OpenSystemCell sort middleInput middleOutput targetInput targetOutput middleVertex middleEdge targetVertex targetEdge label ->
    Either
        OpenSystemError
        ( OpenSystemCell
            sort
            firstInput
            firstOutput
            targetInput
            targetOutput
            firstVertex
            firstEdge
            targetVertex
            targetEdge
            label
        )
verticalComposeOpenSystemCell first second = do
    inputMap <- mapInterfaceError (composeInterfaceMap (cellInputMap first) (cellInputMap second))
    outputMap <- mapInterfaceError (composeInterfaceMap (cellOutputMap first) (cellOutputMap second))
    apexMap <- mapHypergraphMapError (composeHypergraphMap (cellApexMap first) (cellApexMap second))
    openSystemCell (cellSource first) (cellTarget second) inputMap outputMap apexMap

-- | Horizontal composition of squares, induced by the two finite pushouts.
horizontalComposeOpenSystemCell ::
    ( Eq sort
    , Eq middleSource
    , Eq middleTarget
    , Eq firstSourceVertex
    , Eq secondSourceVertex
    , Eq firstTargetVertex
    , Eq secondTargetVertex
    , Eq firstSourceEdge
    , Eq secondSourceEdge
    , Eq firstTargetEdge
    , Eq secondTargetEdge
    , Eq label
    ) =>
    OpenSystemCell
        sort
        sourceInput
        middleSource
        targetInput
        middleTarget
        firstSourceVertex
        firstSourceEdge
        firstTargetVertex
        firstTargetEdge
        label ->
    OpenSystemCell
        sort
        middleSource
        sourceOutput
        middleTarget
        targetOutput
        secondSourceVertex
        secondSourceEdge
        secondTargetVertex
        secondTargetEdge
        label ->
    Either
        OpenSystemError
        ( OpenSystemCell
            sort
            sourceInput
            sourceOutput
            targetInput
            targetOutput
            (PushoutPoint firstSourceVertex secondSourceVertex)
            (Either firstSourceEdge secondSourceEdge)
            (PushoutPoint firstTargetVertex secondTargetVertex)
            (Either firstTargetEdge secondTargetEdge)
            label
        )
horizontalComposeOpenSystemCell first second
    | not (sameInterfaceMap (cellOutputMap first) (cellInputMap second)) =
        Left OpenCellHorizontalBoundaryMismatch
    | otherwise = do
        (sourceComposite, sourcePushout) <- composeOpenSystemDetailed (cellSource first) (cellSource second)
        (targetComposite, targetPushout) <- composeOpenSystemDetailed (cellTarget first) (cellTarget second)
        vertexEntries <-
            traverse
                (inducedVertexEntry targetPushout (cellApexMap first) (cellApexMap second))
                (pushoutClasses sourcePushout)
        let edgeEntries =
                map (bimap Left Left) (hypergraphEdgeEntries (cellApexMap first))
                    ++ map (bimap Right Right) (hypergraphEdgeEntries (cellApexMap second))
        apexMap <-
            mapHypergraphMapError
                ( hypergraphMap
                    (openApex sourceComposite)
                    (openApex targetComposite)
                    vertexEntries
                    edgeEntries
                )
        openSystemCell
            sourceComposite
            targetComposite
            (cellInputMap first)
            (cellOutputMap second)
            apexMap

-- | Tensor of squares.
tensorOpenSystemCell ::
    OpenSystemCell sort leftSourceInput leftSourceOutput leftTargetInput leftTargetOutput leftSourceVertex leftSourceEdge leftTargetVertex leftTargetEdge label ->
    OpenSystemCell sort rightSourceInput rightSourceOutput rightTargetInput rightTargetOutput rightSourceVertex rightSourceEdge rightTargetVertex rightTargetEdge label ->
    OpenSystemCell
        sort
        (Either leftSourceInput rightSourceInput)
        (Either leftSourceOutput rightSourceOutput)
        (Either leftTargetInput rightTargetInput)
        (Either leftTargetOutput rightTargetOutput)
        (Either leftSourceVertex rightSourceVertex)
        (Either leftSourceEdge rightSourceEdge)
        (Either leftTargetVertex rightTargetVertex)
        (Either leftTargetEdge rightTargetEdge)
        label
tensorOpenSystemCell left right =
    UnsafeOpenSystemCell
        (tensorOpenSystem (cellSource left) (cellSource right))
        (tensorOpenSystem (cellTarget left) (cellTarget right))
        (tensorInterfaceMap (cellInputMap left) (cellInputMap right))
        (tensorInterfaceMap (cellOutputMap left) (cellOutputMap right))
        (tensorHypergraphMap (cellApexMap left) (cellApexMap right))

composeOpenSystemDetailed ::
    ( Eq sort
    , Eq middle
    , Eq leftVertex
    , Eq rightVertex
    , Eq leftEdge
    , Eq rightEdge
    , Eq label
    ) =>
    OpenSystem sort input middle leftVertex leftEdge label ->
    OpenSystem sort middle output rightVertex rightEdge label ->
    Either
        OpenSystemError
        ( OpenSystem
            sort
            input
            output
            (PushoutPoint leftVertex rightVertex)
            (Either leftEdge rightEdge)
            label
        , FinitePushout sort middle leftVertex rightVertex
        )
composeOpenSystemDetailed left right
    | not (sameInterface (openOutput left) (openInput right)) =
        Left OpenSequentialBoundaryMismatch
    | otherwise = do
        rightCommonLeg <- reindexCommonLeg (openOutput left) (openInputLeg right)
        pushout <- mapPushoutError (finitePushout (openOutputLeg left) rightCommonLeg)
        apex <- gluedApex pushout (openApex left) (openApex right)
        inputLeg <-
            mapInterfaceError
                (composeInterfaceMap (openInputLeg left) (pushoutLeftInjection pushout))
        outputLeg <-
            mapInterfaceError
                (composeInterfaceMap (openOutputLeg right) (pushoutRightInjection pushout))
        composite <- openSystem (openInput left) (openOutput right) apex inputLeg outputLeg
        Right (composite, pushout)

reindexCommonLeg ::
    Interface sort common ->
    InterfaceMap sort common rightVertex ->
    Either OpenSystemError (InterfaceMap sort common rightVertex)
reindexCommonLeg common rightLeg = do
    entries <- traverse entryFor (interfaceEntries common)
    mapInterfaceError (interfaceMap common (interfaceMapTarget rightLeg) entries)
  where
    entryFor (port, _) =
        case interfaceMapImage rightLeg port of
            Nothing -> Left OpenInternalInvariantFailure
            Just vertex -> Right (port, vertex)

gluedApex ::
    (Eq sort, Eq leftVertex, Eq rightVertex, Eq leftEdge, Eq rightEdge, Eq label) =>
    FinitePushout sort common leftVertex rightVertex ->
    TypedHypergraph sort leftVertex leftEdge label ->
    TypedHypergraph sort rightVertex rightEdge label ->
    Either
        OpenSystemError
        ( TypedHypergraph
            sort
            (PushoutPoint leftVertex rightVertex)
            (Either leftEdge rightEdge)
            label
        )
gluedApex pushout left right = do
    leftEdges <- traverse (mapGluedEdge Left (pushoutLeftInjection pushout)) (hypergraphEdges left)
    rightEdges <- traverse (mapGluedEdge Right (pushoutRightInjection pushout)) (hypergraphEdges right)
    mapHypergraphError (typedHypergraph (pushoutInterface pushout) (leftEdges ++ rightEdges))

mapGluedEdge ::
    (edge -> combinedEdge) ->
    InterfaceMap sort vertex targetVertex ->
    (edge, TypedHyperedge sort label vertex) ->
    Either
        OpenSystemError
        (combinedEdge, label, [(targetVertex, sort)], [(targetVertex, sort)])
mapGluedEdge injectEdge vertexMap (edge, typedEdge) = do
    inputs <- traverse (mapTypedPort vertexMap) (hyperedgeInputs typedEdge)
    outputs <- traverse (mapTypedPort vertexMap) (hyperedgeOutputs typedEdge)
    Right (injectEdge edge, hyperedgeLabel typedEdge, inputs, outputs)

mapTypedPort ::
    InterfaceMap sort vertex targetVertex ->
    (vertex, sort) ->
    Either OpenSystemError (targetVertex, sort)
mapTypedPort vertexMap (vertex, portType) =
    case interfaceMapImage vertexMap vertex of
        Nothing -> Left OpenInternalInvariantFailure
        Just targetVertex -> Right (targetVertex, portType)

commutes ::
    (Eq targetVertex) =>
    InterfaceMap sort boundary sourceVertex ->
    InterfaceMap sort boundary targetBoundary ->
    InterfaceMap sort targetBoundary targetVertex ->
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    Bool
commutes sourceLeg vertical targetLeg apexMap =
    all commutesAt (interfaceEntries (interfaceMapSource sourceLeg))
  where
    commutesAt (port, _) =
        case (interfaceMapImage sourceLeg port, interfaceMapImage vertical port) of
            (Just sourceVertex, Just targetPort) ->
                case (hypergraphVertexImage apexMap sourceVertex, interfaceMapImage targetLeg targetPort) of
                    (Just mappedVertex, Just legVertex) -> mappedVertex == legVertex
                    _ -> False
            _ -> False

inducedVertexEntry ::
    (Eq targetLeft, Eq targetRight) =>
    FinitePushout sort common targetLeft targetRight ->
    HypergraphMap sort sourceLeft sourceLeftEdge targetLeft targetLeftEdge label ->
    HypergraphMap sort sourceRight sourceRightEdge targetRight targetRightEdge label ->
    PushoutPoint sourceLeft sourceRight ->
    Either
        OpenSystemError
        (PushoutPoint sourceLeft sourceRight, PushoutPoint targetLeft targetRight)
inducedVertexEntry targetPushout leftMap rightMap sourcePoint = do
    targetPoints <- traverse mapMember (NonEmpty.toList (pushoutPointMembers sourcePoint))
    case targetPoints of
        [] -> Left OpenInternalInvariantFailure
        first : remaining
            | all (== first) remaining -> Right (sourcePoint, first)
            | otherwise -> Left OpenInternalInvariantFailure
  where
    mapMember (Left sourceVertex) =
        case hypergraphVertexImage leftMap sourceVertex >>= pushoutPointForLeft targetPushout of
            Nothing -> Left OpenInternalInvariantFailure
            Just targetPoint -> Right targetPoint
    mapMember (Right sourceVertex) =
        case hypergraphVertexImage rightMap sourceVertex >>= pushoutPointForRight targetPushout of
            Nothing -> Left OpenInternalInvariantFailure
            Just targetPoint -> Right targetPoint

mapInterfaceError :: Either InterfaceMapError value -> Either OpenSystemError value
mapInterfaceError = either (Left . OpenInterfaceMapError) Right

mapPushoutError :: Either PushoutError value -> Either OpenSystemError value
mapPushoutError = either (Left . OpenPushoutError) Right

mapHypergraphError :: Either HypergraphError value -> Either OpenSystemError value
mapHypergraphError = either (Left . OpenHypergraphError) Right

mapHypergraphMapError :: Either HypergraphMapError value -> Either OpenSystemError value
mapHypergraphMapError = either (Left . OpenHypergraphMapError) Right
