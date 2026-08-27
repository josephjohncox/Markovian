{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Finite typed directed hypergraphs and structure-preserving maps.

Each hyperedge has an identity, a label, and ordered typed input and output
ports. Maps preserve vertex types, edge labels, port order, and incidence.
-}
module Markovian.Open.Hypergraph (
    TypedHyperedge,
    hyperedgeLabel,
    hyperedgeInputs,
    hyperedgeOutputs,
    TypedHypergraph,
    HypergraphError (..),
    typedHypergraph,
    hypergraphVertices,
    hypergraphEdges,
    hypergraphEdgeIds,
    hypergraphEdge,
    discreteHypergraph,
    discreteHypergraphMap,
    tensorHypergraph,
    sameHypergraphLayout,
    HypergraphMap,
    HypergraphMapError (..),
    hypergraphMap,
    hypergraphMapSource,
    hypergraphMapTarget,
    hypergraphVertexEntries,
    hypergraphEdgeEntries,
    hypergraphVertexImage,
    hypergraphEdgeImage,
    identityHypergraphMap,
    composeHypergraphMap,
    tensorHypergraphMap,
) where

import Data.Bifunctor qualified as Bifunctor
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Open.Interface

-- | One labelled directed hyperedge with ordered typed ports.
data TypedHyperedge sort label vertex
    = TypedHyperedge !label ![(vertex, sort)] ![(vertex, sort)]
    deriving (Eq, Show)

-- | Read an edge label.
hyperedgeLabel :: TypedHyperedge sort label vertex -> label
hyperedgeLabel (TypedHyperedge label _ _) = label

-- | Read ordered typed input ports.
hyperedgeInputs :: TypedHyperedge sort label vertex -> [(vertex, sort)]
hyperedgeInputs (TypedHyperedge _ inputs _) = inputs

-- | Read ordered typed output ports.
hyperedgeOutputs :: TypedHyperedge sort label vertex -> [(vertex, sort)]
hyperedgeOutputs (TypedHyperedge _ _ outputs) = outputs

-- | Opaque finite typed directed hypergraph.
type role TypedHypergraph nominal nominal nominal nominal

data TypedHypergraph sort vertex edge label where
    UnsafeTypedHypergraph ::
        (Eq sort, Eq vertex, Eq edge, Eq label) =>
        !(Interface sort vertex) ->
        !(FiniteSet edge) ->
        ![(edge, TypedHyperedge sort label vertex)] ->
        TypedHypergraph sort vertex edge label

-- | Hypergraph validation failure.
data HypergraphError
    = DuplicateHyperedgeId !Int
    | HyperedgeInputOutsideVertices !Int !Int
    | HyperedgeOutputOutsideVertices !Int !Int
    | HyperedgeInputTypeMismatch !Int !Int
    | HyperedgeOutputTypeMismatch !Int !Int
    deriving (Eq, Show)

{- | Construct a finite typed hypergraph. Edge tuples contain identity, label,
ordered typed inputs, and ordered typed outputs.
-}
typedHypergraph ::
    (Eq sort, Eq vertex, Eq edge, Eq label) =>
    Interface sort vertex ->
    [(edge, label, [(vertex, sort)], [(vertex, sort)])] ->
    Either HypergraphError (TypedHypergraph sort vertex edge label)
typedHypergraph vertices entries = do
    edgeSet <-
        case finiteSet (map edgeId entries) of
            Left _ -> Left (DuplicateHyperedgeId (firstDuplicateIndex (map edgeId entries)))
            Right represented -> Right represented
    traverse_Indexed validateEdge entries
    Right
        ( UnsafeTypedHypergraph
            vertices
            edgeSet
            [ (edge, TypedHyperedge label inputs outputs)
            | (edge, label, inputs, outputs) <- entries
            ]
        )
  where
    edgeId (edge, _, _, _) = edge
    validateEdge edgeIndex (_, _, inputs, outputs) = do
        validatePorts edgeIndex True 0 inputs
        validatePorts edgeIndex False 0 outputs
    validatePorts _ _ _ [] = Right ()
    validatePorts edgeIndex isInput portIndex ((vertex, declaredType) : remaining) =
        case interfacePortType vertices vertex of
            Nothing ->
                Left
                    ( if isInput
                        then HyperedgeInputOutsideVertices edgeIndex portIndex
                        else HyperedgeOutputOutsideVertices edgeIndex portIndex
                    )
            Just actualType
                | actualType /= declaredType ->
                    Left
                        ( if isInput
                            then HyperedgeInputTypeMismatch edgeIndex portIndex
                            else HyperedgeOutputTypeMismatch edgeIndex portIndex
                        )
                | otherwise -> validatePorts edgeIndex isInput (portIndex + 1) remaining
    traverse_Indexed function = traverse_ (uncurry function) . zip [0 ..]

-- | Read typed vertices.
hypergraphVertices :: TypedHypergraph sort vertex edge label -> Interface sort vertex
hypergraphVertices (UnsafeTypedHypergraph vertices _ _) = vertices

-- | Read edges in represented edge layout order.
hypergraphEdges ::
    TypedHypergraph sort vertex edge label ->
    [(edge, TypedHyperedge sort label vertex)]
hypergraphEdges (UnsafeTypedHypergraph _ _ edges) = edges

-- | Read represented edge identities.
hypergraphEdgeIds :: TypedHypergraph sort vertex edge label -> FiniteSet edge
hypergraphEdgeIds (UnsafeTypedHypergraph _ edges _) = edges

-- | Look up one represented edge.
hypergraphEdge ::
    TypedHypergraph sort vertex edge label ->
    edge ->
    Maybe (TypedHyperedge sort label vertex)
hypergraphEdge (UnsafeTypedHypergraph _ _ edges) edge = lookup edge edges

-- | Discrete-boundary functor on objects.
discreteHypergraph ::
    (Eq sort, Eq vertex, Eq edge, Eq label) =>
    Interface sort vertex ->
    TypedHypergraph sort vertex edge label
discreteHypergraph vertices =
    UnsafeTypedHypergraph vertices (UnsafeFiniteSet []) []

-- | Discrete-boundary functor on vertical arrows.
discreteHypergraphMap ::
    (Eq sort, Eq source, Eq target, Eq edge, Eq label) =>
    InterfaceMap sort source target ->
    HypergraphMap sort source edge target edge label
discreteHypergraphMap vertical =
    UnsafeHypergraphMap
        (discreteHypergraph (interfaceMapSource vertical))
        (discreteHypergraph (interfaceMapTarget vertical))
        (interfaceMapEntries vertical)
        []

-- | Disjoint-union tensor of hypergraphs.
tensorHypergraph ::
    TypedHypergraph sort leftVertex leftEdge label ->
    TypedHypergraph sort rightVertex rightEdge label ->
    TypedHypergraph
        sort
        (Either leftVertex rightVertex)
        (Either leftEdge rightEdge)
        label
tensorHypergraph
    (UnsafeTypedHypergraph leftVertices _ leftEdges)
    (UnsafeTypedHypergraph rightVertices _ rightEdges) =
        UnsafeTypedHypergraph vertices edgeIds edges
      where
        vertices = tensorInterface leftVertices rightVertices
        edges =
            [ (Left edge, mapEdge Left typedEdge)
            | (edge, typedEdge) <- leftEdges
            ]
                ++ [ (Right edge, mapEdge Right typedEdge)
                   | (edge, typedEdge) <- rightEdges
                   ]
        edgeIds = UnsafeFiniteSet (map fst edges)

-- | Compare complete represented graph layout.
sameHypergraphLayout ::
    TypedHypergraph sort vertex edge label ->
    TypedHypergraph sort vertex edge label ->
    Bool
sameHypergraphLayout
    (UnsafeTypedHypergraph leftVertices leftIds leftEdges)
    (UnsafeTypedHypergraph rightVertices rightIds rightEdges) =
        sameInterfaceLayout leftVertices rightVertices
            && sameFiniteSetLayout leftIds rightIds
            && leftEdges == rightEdges

-- | Opaque type- and incidence-preserving hypergraph map.
type role HypergraphMap nominal nominal nominal nominal nominal nominal

data HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label where
    UnsafeHypergraphMap ::
        ( Eq sort
        , Eq sourceVertex
        , Eq sourceEdge
        , Eq targetVertex
        , Eq targetEdge
        , Eq label
        ) =>
        !(TypedHypergraph sort sourceVertex sourceEdge label) ->
        !(TypedHypergraph sort targetVertex targetEdge label) ->
        ![(sourceVertex, targetVertex)] ->
        ![(sourceEdge, targetEdge)] ->
        HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label

-- | Hypergraph-map validation failure.
data HypergraphMapError
    = HypergraphVertexMapDomainMismatch
    | HypergraphEdgeMapDomainMismatch
    | HypergraphVertexOutsideTarget !Int
    | HypergraphEdgeOutsideTarget !Int
    | HypergraphVertexTypeMismatch !Int
    | HypergraphEdgeLabelMismatch !Int
    | HypergraphInputIncidenceMismatch !Int
    | HypergraphOutputIncidenceMismatch !Int
    | HypergraphMapCompositionObjectMismatch
    | HypergraphMapInternalInvariantFailure
    deriving (Eq, Show)

-- | Validate a total structure-preserving hypergraph map.
hypergraphMap ::
    TypedHypergraph sort sourceVertex sourceEdge label ->
    TypedHypergraph sort targetVertex targetEdge label ->
    [(sourceVertex, targetVertex)] ->
    [(sourceEdge, targetEdge)] ->
    Either
        HypergraphMapError
        (HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label)
hypergraphMap source@UnsafeTypedHypergraph{} target@UnsafeTypedHypergraph{} vertexEntries edgeEntries
    | not (validDomain (finiteSetValues (interfacePorts (hypergraphVertices source))) vertexEntries) =
        Left HypergraphVertexMapDomainMismatch
    | not (validDomain (finiteSetValues (hypergraphEdgeIds source)) edgeEntries) =
        Left HypergraphEdgeMapDomainMismatch
    | otherwise = do
        traverse_ (uncurry validateVertex) (zip [0 ..] vertexEntries)
        traverse_ (uncurry validateEdge) (zip [0 ..] edgeEntries)
        Right (UnsafeHypergraphMap source target vertexEntries edgeEntries)
  where
    validateVertex index (sourceVertex, targetVertex) =
        case interfacePortType (hypergraphVertices target) targetVertex of
            Nothing -> Left (HypergraphVertexOutsideTarget index)
            Just targetType
                | interfacePortType (hypergraphVertices source) sourceVertex /= Just targetType ->
                    Left (HypergraphVertexTypeMismatch index)
                | otherwise -> Right ()
    validateEdge index (sourceEdge, targetEdge) =
        case (hypergraphEdge source sourceEdge, hypergraphEdge target targetEdge) of
            (_, Nothing) -> Left (HypergraphEdgeOutsideTarget index)
            (Nothing, _) -> Left HypergraphMapInternalInvariantFailure
            (Just sourceSpec, Just targetSpec)
                | hyperedgeLabel sourceSpec /= hyperedgeLabel targetSpec ->
                    Left (HypergraphEdgeLabelMismatch index)
                | mapPorts vertexEntries (hyperedgeInputs sourceSpec) /= Just (hyperedgeInputs targetSpec) ->
                    Left (HypergraphInputIncidenceMismatch index)
                | mapPorts vertexEntries (hyperedgeOutputs sourceSpec) /= Just (hyperedgeOutputs targetSpec) ->
                    Left (HypergraphOutputIncidenceMismatch index)
                | otherwise -> Right ()

-- | Read the source hypergraph.
hypergraphMapSource ::
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    TypedHypergraph sort sourceVertex sourceEdge label
hypergraphMapSource (UnsafeHypergraphMap source _ _ _) = source

-- | Read the target hypergraph.
hypergraphMapTarget ::
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    TypedHypergraph sort targetVertex targetEdge label
hypergraphMapTarget (UnsafeHypergraphMap _ target _ _) = target

-- | Read the vertex-map table.
hypergraphVertexEntries ::
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    [(sourceVertex, targetVertex)]
hypergraphVertexEntries (UnsafeHypergraphMap _ _ entries _) = entries

-- | Read the edge-map table.
hypergraphEdgeEntries ::
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    [(sourceEdge, targetEdge)]
hypergraphEdgeEntries (UnsafeHypergraphMap _ _ _ entries) = entries

-- | Apply the vertex map.
hypergraphVertexImage ::
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    sourceVertex ->
    Maybe targetVertex
hypergraphVertexImage (UnsafeHypergraphMap _ _ entries _) = flip lookup entries

-- | Apply the edge map.
hypergraphEdgeImage ::
    HypergraphMap sort sourceVertex sourceEdge targetVertex targetEdge label ->
    sourceEdge ->
    Maybe targetEdge
hypergraphEdgeImage (UnsafeHypergraphMap _ _ _ entries) = flip lookup entries

-- | Identity hypergraph map.
identityHypergraphMap ::
    TypedHypergraph sort vertex edge label ->
    HypergraphMap sort vertex edge vertex edge label
identityHypergraphMap graph@UnsafeTypedHypergraph{} =
    UnsafeHypergraphMap
        graph
        graph
        [(vertex, vertex) | vertex <- finiteSetValues (interfacePorts (hypergraphVertices graph))]
        [(edge, edge) | edge <- finiteSetValues (hypergraphEdgeIds graph)]

-- | Checked vertical composition of hypergraph maps.
composeHypergraphMap ::
    HypergraphMap sort firstVertex firstEdge middleVertex middleEdge label ->
    HypergraphMap sort middleVertex middleEdge targetVertex targetEdge label ->
    Either
        HypergraphMapError
        (HypergraphMap sort firstVertex firstEdge targetVertex targetEdge label)
composeHypergraphMap first second
    | not (sameHypergraphLayout (hypergraphMapTarget first) (hypergraphMapSource second)) =
        Left HypergraphMapCompositionObjectMismatch
    | otherwise = do
        vertices <- traverse composeVertex (hypergraphVertexEntries first)
        edges <- traverse composeEdge (hypergraphEdgeEntries first)
        hypergraphMap (hypergraphMapSource first) (hypergraphMapTarget second) vertices edges
  where
    composeVertex (sourceVertex, middleVertex) =
        case hypergraphVertexImage second middleVertex of
            Nothing -> Left HypergraphMapInternalInvariantFailure
            Just targetVertex -> Right (sourceVertex, targetVertex)
    composeEdge (sourceEdge, middleEdge) =
        case hypergraphEdgeImage second middleEdge of
            Nothing -> Left HypergraphMapInternalInvariantFailure
            Just targetEdge -> Right (sourceEdge, targetEdge)

-- | Tensor of hypergraph maps.
tensorHypergraphMap ::
    HypergraphMap sort leftSourceVertex leftSourceEdge leftTargetVertex leftTargetEdge label ->
    HypergraphMap sort rightSourceVertex rightSourceEdge rightTargetVertex rightTargetEdge label ->
    HypergraphMap
        sort
        (Either leftSourceVertex rightSourceVertex)
        (Either leftSourceEdge rightSourceEdge)
        (Either leftTargetVertex rightTargetVertex)
        (Either leftTargetEdge rightTargetEdge)
        label
tensorHypergraphMap left@UnsafeHypergraphMap{} right@UnsafeHypergraphMap{} =
    UnsafeHypergraphMap
        (tensorHypergraph (hypergraphMapSource left) (hypergraphMapSource right))
        (tensorHypergraph (hypergraphMapTarget left) (hypergraphMapTarget right))
        (map (Bifunctor.bimap Left Left) (hypergraphVertexEntries left) ++ map (Bifunctor.bimap Right Right) (hypergraphVertexEntries right))
        (map (Bifunctor.bimap Left Left) (hypergraphEdgeEntries left) ++ map (Bifunctor.bimap Right Right) (hypergraphEdgeEntries right))

mapEdge ::
    (sourceVertex -> targetVertex) ->
    TypedHyperedge sort label sourceVertex ->
    TypedHyperedge sort label targetVertex
mapEdge function (TypedHyperedge label inputs outputs) =
    TypedHyperedge label (map (Bifunctor.first function) inputs) (map (Bifunctor.first function) outputs)

mapPorts ::
    (Eq sourceVertex) =>
    [(sourceVertex, targetVertex)] ->
    [(sourceVertex, sort)] ->
    Maybe [(targetVertex, sort)]
mapPorts entries = traverse mapPort
  where
    mapPort (sourceVertex, portType) =
        case lookup sourceVertex entries of
            Nothing -> Nothing
            Just targetVertex -> Just (targetVertex, portType)

validDomain :: (Eq source) => [source] -> [(source, target)] -> Bool
validDomain represented entries =
    length represented == length entries
        && all (\value -> count value == 1) represented
        && all (\(value, _) -> value `elem` represented) entries
  where
    count requested = length [() | (value, _) <- entries, value == requested]

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ function = foldr (\value remaining -> function value >> remaining) (Right ())

firstDuplicateIndex :: (Eq value) => [value] -> Int
firstDuplicateIndex = go 0 []
  where
    go _ _ [] = 0
    go index seen (value : remaining)
        | value `elem` seen = index
        | otherwise = go (index + 1) (seen ++ [value]) remaining
