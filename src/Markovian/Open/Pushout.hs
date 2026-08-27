{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Total finite pushouts of typed interface maps.

The carrier is an explicit quotient of a disjoint union. Quotient classes and
both canonical injections remain observable witnesses.
-}
module Markovian.Open.Pushout (
    PushoutPoint,
    pushoutPointMembers,
    FinitePushout,
    PushoutError (..),
    finitePushout,
    pushoutInterface,
    pushoutLeftInjection,
    pushoutRightInjection,
    pushoutClasses,
    pushoutPointForLeft,
    pushoutPointForRight,
    factorPushout,
) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Open.Interface

-- | One explicit equivalence class in the pushout quotient.
type role PushoutPoint nominal nominal

newtype PushoutPoint left right
    = UnsafePushoutPoint (NonEmpty (Either left right))
    deriving (Eq, Show)

-- | Read class members in canonical left-then-right carrier order.
pushoutPointMembers :: PushoutPoint left right -> NonEmpty (Either left right)
pushoutPointMembers (UnsafePushoutPoint members) = members

-- | Pushout object with explicit quotient and injection witnesses.
type role FinitePushout nominal nominal nominal nominal

data FinitePushout sort (common :: Type) left right where
    FinitePushout ::
        !(Interface sort (PushoutPoint left right)) ->
        !(InterfaceMap sort left (PushoutPoint left right)) ->
        !(InterfaceMap sort right (PushoutPoint left right)) ->
        ![PushoutPoint left right] ->
        FinitePushout sort common left right

-- | Pushout construction failure.
data PushoutError
    = PushoutSpanSourceMismatch
    | PushoutTypeConflict !Int
    | PushoutLeftInjectionFailure
    | PushoutRightInjectionFailure
    | PushoutCoconeSourceMismatch
    | PushoutCoconeTargetMismatch
    | PushoutCoconeDoesNotRespectQuotient !Int
    | PushoutInternalInvariantFailure
    deriving (Eq, Show)

-- | Construct the finite typed pushout of two maps with common source.
finitePushout ::
    (Eq sort, Eq common, Eq left, Eq right) =>
    InterfaceMap sort common left ->
    InterfaceMap sort common right ->
    Either PushoutError (FinitePushout sort common left right)
finitePushout leftMap rightMap
    | not (sameInterface (interfaceMapSource leftMap) (interfaceMapSource rightMap)) =
        Left PushoutSpanSourceMismatch
    | otherwise = do
        relations <- traverse relationFor (interfaceEntries (interfaceMapSource leftMap))
        let carrier =
                map (Left . fst) (interfaceEntries leftObject)
                    ++ map (Right . fst) (interfaceEntries rightObject)
            classes = map UnsafePushoutPoint (equivalenceClasses carrier relations)
        classEntries <- traverse (uncurry classEntry) (zip [0 ..] classes)
        quotient <-
            case interface classEntries of
                Left _ -> Left PushoutInternalInvariantFailure
                Right represented -> Right represented
        leftInjectionEntries <- traverse (leftEntry classes) (interfaceEntries leftObject)
        rightInjectionEntries <- traverse (rightEntry classes) (interfaceEntries rightObject)
        leftInjection <-
            case interfaceMap leftObject quotient leftInjectionEntries of
                Left _ -> Left PushoutLeftInjectionFailure
                Right injection -> Right injection
        rightInjection <-
            case interfaceMap rightObject quotient rightInjectionEntries of
                Left _ -> Left PushoutRightInjectionFailure
                Right injection -> Right injection
        Right (FinitePushout quotient leftInjection rightInjection classes)
  where
    leftObject = interfaceMapTarget leftMap
    rightObject = interfaceMapTarget rightMap
    relationFor (common, _) =
        case (interfaceMapImage leftMap common, interfaceMapImage rightMap common) of
            (Just left, Just right) -> Right (Left left, Right right)
            _ -> Left PushoutInternalInvariantFailure
    classEntry index point = do
        memberTypes <- traverse memberType (NonEmpty.toList (pushoutPointMembers point))
        case memberTypes of
            [] -> Left PushoutInternalInvariantFailure
            firstType : remaining
                | all (== firstType) remaining -> Right (point, firstType)
                | otherwise -> Left (PushoutTypeConflict index)
    memberType (Left left) =
        case interfacePortType leftObject left of
            Nothing -> Left PushoutInternalInvariantFailure
            Just portType -> Right portType
    memberType (Right right) =
        case interfacePortType rightObject right of
            Nothing -> Left PushoutInternalInvariantFailure
            Just portType -> Right portType
    leftEntry classes (left, _) =
        case findClass (Left left) classes of
            Nothing -> Left PushoutLeftInjectionFailure
            Just point -> Right (left, point)
    rightEntry classes (right, _) =
        case findClass (Right right) classes of
            Nothing -> Left PushoutRightInjectionFailure
            Just point -> Right (right, point)

-- | Read the quotient interface.
pushoutInterface :: FinitePushout sort common left right -> Interface sort (PushoutPoint left right)
pushoutInterface (FinitePushout object _ _ _) = object

-- | Read the canonical left injection.
pushoutLeftInjection ::
    FinitePushout sort common left right ->
    InterfaceMap sort left (PushoutPoint left right)
pushoutLeftInjection (FinitePushout _ injection _ _) = injection

-- | Read the canonical right injection.
pushoutRightInjection ::
    FinitePushout sort common left right ->
    InterfaceMap sort right (PushoutPoint left right)
pushoutRightInjection (FinitePushout _ _ injection _) = injection

-- | Read explicit quotient classes.
pushoutClasses :: FinitePushout sort common left right -> [PushoutPoint left right]
pushoutClasses (FinitePushout _ _ _ classes) = classes

-- | Find the quotient image of one left point.
pushoutPointForLeft ::
    FinitePushout sort common left right ->
    left ->
    Maybe (PushoutPoint left right)
pushoutPointForLeft pushout = interfaceMapImage (pushoutLeftInjection pushout)

-- | Find the quotient image of one right point.
pushoutPointForRight ::
    FinitePushout sort common left right ->
    right ->
    Maybe (PushoutPoint left right)
pushoutPointForRight pushout = interfaceMapImage (pushoutRightInjection pushout)

{- | Factor a compatible cocone through the explicit quotient. Compatibility is
checked on every quotient class, which also covers noninjective span legs.
-}
factorPushout ::
    (Eq sort, Eq left, Eq right, Eq target) =>
    FinitePushout sort common left right ->
    InterfaceMap sort left target ->
    InterfaceMap sort right target ->
    Either
        PushoutError
        (InterfaceMap sort (PushoutPoint left right) target)
factorPushout pushout leftMap rightMap
    | not (sameInterface (interfaceMapSource leftMap) (interfaceMapSource (pushoutLeftInjection pushout)))
        || not (sameInterface (interfaceMapSource rightMap) (interfaceMapSource (pushoutRightInjection pushout))) =
        Left PushoutCoconeSourceMismatch
    | not (sameInterface (interfaceMapTarget leftMap) (interfaceMapTarget rightMap)) =
        Left PushoutCoconeTargetMismatch
    | otherwise = do
        entries <- traverse (uncurry factorClass) (zip [0 ..] (pushoutClasses pushout))
        case interfaceMap (pushoutInterface pushout) (interfaceMapTarget leftMap) entries of
            Left _ -> Left PushoutInternalInvariantFailure
            Right factor -> Right factor
  where
    factorClass index point = do
        images <- traverse memberImage (NonEmpty.toList (pushoutPointMembers point))
        case images of
            [] -> Left PushoutInternalInvariantFailure
            firstImage : remaining
                | all (== firstImage) remaining -> Right (point, firstImage)
                | otherwise -> Left (PushoutCoconeDoesNotRespectQuotient index)
    memberImage (Left left) =
        case interfaceMapImage leftMap left of
            Nothing -> Left PushoutInternalInvariantFailure
            Just target -> Right target
    memberImage (Right right) =
        case interfaceMapImage rightMap right of
            Nothing -> Left PushoutInternalInvariantFailure
            Just target -> Right target

equivalenceClasses ::
    (Eq value) =>
    [value] ->
    [(value, value)] ->
    [NonEmpty value]
equivalenceClasses carrier relations = go carrier
  where
    go [] = []
    go (seed : remaining) =
        let connected = closure [seed]
            members = filter (`elem` connected) carrier
            rest = filter (`notElem` connected) remaining
         in case members of
                first : others -> (first :| others) : go rest
                [] -> go rest
    closure current =
        let expanded = foldl addRelated current relations
         in if length expanded == length current then current else closure expanded
    addRelated current (left, right)
        | left `elem` current && right `notElem` current = current ++ [right]
        | right `elem` current && left `notElem` current = current ++ [left]
        | otherwise = current

findClass ::
    (Eq left, Eq right) =>
    Either left right ->
    [PushoutPoint left right] ->
    Maybe (PushoutPoint left right)
findClass _ [] = Nothing
findClass member (point : remaining)
    | member `elem` pushoutPointMembers point = Just point
    | otherwise = findClass member remaining
