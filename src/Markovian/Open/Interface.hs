{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Validated finite typed interfaces and total type-preserving maps.

Interfaces may be empty. Port identity and port type are stored as finite
tables; no type classifier function is retained.
-}
module Markovian.Open.Interface (
    Interface,
    InterfaceError (..),
    interface,
    interfacePorts,
    interfaceEntries,
    interfaceCardinality,
    interfacePortType,
    sameInterface,
    sameInterfaceLayout,
    tensorInterface,
    InterfaceMap,
    InterfaceMapError (..),
    interfaceMap,
    interfaceMapSource,
    interfaceMapTarget,
    interfaceMapEntries,
    interfaceMapImage,
    sameInterfaceMap,
    identityInterfaceMap,
    composeInterfaceMap,
    tensorInterfaceMap,
) where

import Data.Bifunctor (first)
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))

-- | An opaque finite table of typed ports.
type role Interface nominal nominal

data Interface sort port where
    UnsafeInterface ::
        (Eq sort, Eq port) =>
        !(FiniteSet port) ->
        ![(port, sort)] ->
        Interface sort port

-- | Interface validation failure.
newtype InterfaceError
    = DuplicateInterfacePort Int
    deriving (Eq, Show)

-- | Construct a typed interface. Entry order is the represented port layout.
interface ::
    (Eq sort, Eq port) =>
    [(port, sort)] ->
    Either InterfaceError (Interface sort port)
interface entries =
    case finiteSet (map fst entries) of
        Left _ -> Left (DuplicateInterfacePort (firstDuplicateIndex (map fst entries)))
        Right ports -> Right (UnsafeInterface ports entries)

-- | Read represented ports in layout order.
interfacePorts :: Interface sort port -> FiniteSet port
interfacePorts (UnsafeInterface ports _) = ports

-- | Read typed entries in layout order.
interfaceEntries :: Interface sort port -> [(port, sort)]
interfaceEntries (UnsafeInterface _ entries) = entries

-- | Read the number of represented ports.
interfaceCardinality :: Interface sort port -> Int
interfaceCardinality = finiteSetCardinality . interfacePorts

-- | Look up a represented port's type.
interfacePortType :: Interface sort port -> port -> Maybe sort
interfacePortType (UnsafeInterface _ entries) port = lookup port entries

-- | Compare typed port support without considering layout order.
sameInterface :: Interface sort port -> Interface sort port -> Bool
sameInterface left@(UnsafeInterface _ _) right@(UnsafeInterface _ _) =
    sameFiniteSet (interfacePorts left) (interfacePorts right)
        && all
            (\(port, portType) -> interfacePortType right port == Just portType)
            (interfaceEntries left)

-- | Compare typed port support and layout.
sameInterfaceLayout :: Interface sort port -> Interface sort port -> Bool
sameInterfaceLayout left@(UnsafeInterface _ _) right@(UnsafeInterface _ _) =
    interfaceEntries left == interfaceEntries right

-- | Disjoint-union tensor of typed interfaces.
tensorInterface ::
    Interface sort left ->
    Interface sort right ->
    Interface sort (Either left right)
tensorInterface (UnsafeInterface _ left) (UnsafeInterface _ right) =
    UnsafeInterface
        (UnsafeFiniteSet (map (Left . fst) left ++ map (Right . fst) right))
        (map (first Left) left ++ map (first Right) right)

-- | An opaque total type-preserving finite interface map.
type role InterfaceMap nominal nominal nominal

data InterfaceMap sort source target where
    UnsafeInterfaceMap ::
        (Eq sort, Eq source, Eq target) =>
        !(Interface sort source) ->
        !(Interface sort target) ->
        ![(source, target)] ->
        InterfaceMap sort source target

-- | Interface-map validation or composition failure.
data InterfaceMapError
    = InterfaceMapDomainMismatch
    | InterfaceMapTargetOutsideInterface !Int
    | InterfaceMapTypeMismatch !Int
    | InterfaceMapCompositionObjectMismatch
    | InterfaceMapInternalInvariantFailure
    deriving (Eq, Show)

-- | Validate a total type-preserving map table.
interfaceMap ::
    Interface sort source ->
    Interface sort target ->
    [(source, target)] ->
    Either InterfaceMapError (InterfaceMap sort source target)
interfaceMap source@(UnsafeInterface _ _) target@(UnsafeInterface _ _) entries
    | not (validDomain (finiteSetValues (interfacePorts source)) entries) =
        Left InterfaceMapDomainMismatch
    | otherwise = do
        validateEntries 0 entries
        canonicalEntries <- traverse canonicalEntry (finiteSetValues (interfacePorts source))
        Right (UnsafeInterfaceMap source target canonicalEntries)
  where
    validateEntries _ [] = Right ()
    validateEntries index ((sourcePort, targetPort) : remaining) =
        case interfacePortType target targetPort of
            Nothing -> Left (InterfaceMapTargetOutsideInterface index)
            Just targetType
                | interfacePortType source sourcePort /= Just targetType ->
                    Left (InterfaceMapTypeMismatch index)
                | otherwise -> validateEntries (index + 1) remaining
    canonicalEntry sourcePort =
        case lookup sourcePort entries of
            Nothing -> Left InterfaceMapInternalInvariantFailure
            Just targetPort -> Right (sourcePort, targetPort)

-- | Read the source interface.
interfaceMapSource :: InterfaceMap sort source target -> Interface sort source
interfaceMapSource (UnsafeInterfaceMap source _ _) = source

-- | Read the target interface.
interfaceMapTarget :: InterfaceMap sort source target -> Interface sort target
interfaceMapTarget (UnsafeInterfaceMap _ target _) = target

-- | Read the total map table in source layout order.
interfaceMapEntries :: InterfaceMap sort source target -> [(source, target)]
interfaceMapEntries (UnsafeInterfaceMap _ _ entries) = entries

-- | Apply a map to one represented source port.
interfaceMapImage :: InterfaceMap sort source target -> source -> Maybe target
interfaceMapImage (UnsafeInterfaceMap _ _ entries) sourcePort = lookup sourcePort entries

-- | Compare total maps extensionally, ignoring source and target layout order.
sameInterfaceMap ::
    InterfaceMap sort source target ->
    InterfaceMap sort source target ->
    Bool
sameInterfaceMap left@UnsafeInterfaceMap{} right@UnsafeInterfaceMap{} =
    sameInterface (interfaceMapSource left) (interfaceMapSource right)
        && sameInterface (interfaceMapTarget left) (interfaceMapTarget right)
        && all
            (\(sourcePort, _) -> interfaceMapImage left sourcePort == interfaceMapImage right sourcePort)
            (interfaceEntries (interfaceMapSource left))

-- | Identity vertical arrow.
identityInterfaceMap :: Interface sort port -> InterfaceMap sort port port
identityInterfaceMap object@(UnsafeInterface _ _) =
    UnsafeInterfaceMap object object [(port, port) | port <- finiteSetValues (interfacePorts object)]

-- | Checked left-to-right vertical composition.
composeInterfaceMap ::
    InterfaceMap sort source middle ->
    InterfaceMap sort middle target ->
    Either InterfaceMapError (InterfaceMap sort source target)
composeInterfaceMap firstMap@UnsafeInterfaceMap{} second@UnsafeInterfaceMap{}
    | not (sameInterface (interfaceMapTarget firstMap) (interfaceMapSource second)) =
        Left InterfaceMapCompositionObjectMismatch
    | otherwise = do
        entries <- traverse composeEntry (interfaceMapEntries firstMap)
        interfaceMap (interfaceMapSource firstMap) (interfaceMapTarget second) entries
  where
    composeEntry (sourcePort, middlePort) =
        case interfaceMapImage second middlePort of
            Nothing -> Left InterfaceMapInternalInvariantFailure
            Just targetPort -> Right (sourcePort, targetPort)

-- | Tensor of vertical arrows.
tensorInterfaceMap ::
    InterfaceMap sort leftSource leftTarget ->
    InterfaceMap sort rightSource rightTarget ->
    InterfaceMap
        sort
        (Either leftSource rightSource)
        (Either leftTarget rightTarget)
tensorInterfaceMap left@UnsafeInterfaceMap{} right@UnsafeInterfaceMap{} =
    UnsafeInterfaceMap source target entries
  where
    source = tensorInterface (interfaceMapSource left) (interfaceMapSource right)
    target = tensorInterface (interfaceMapTarget left) (interfaceMapTarget right)
    entries =
        [ (Left sourcePort, Left targetPort)
        | (sourcePort, targetPort) <- interfaceMapEntries left
        ]
            ++ [ (Right sourcePort, Right targetPort)
               | (sourcePort, targetPort) <- interfaceMapEntries right
               ]

validDomain :: (Eq source) => [source] -> [(source, target)] -> Bool
validDomain ports entries =
    length ports == length entries
        && all (\port -> count port == 1) ports
        && all (\(port, _) -> port `elem` ports) entries
  where
    count requested = length [() | (port, _) <- entries, port == requested]

firstDuplicateIndex :: (Eq value) => [value] -> Int
firstDuplicateIndex = go 0 []
  where
    go _ _ [] = 0
    go index seen (value : remaining)
        | value `elem` seen = index
        | otherwise = go (index + 1) (seen ++ [value]) remaining
