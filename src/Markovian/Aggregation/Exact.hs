{-# LANGUAGE RoleAnnotations #-}

{- | Bounded supplied-partition congruence for one closed exact policy.

The checked table preserves declared state observations, terminal/payoff timing,
and the joint immediate-reward/next-block law. It does not discover partitions
or preserve alternative policies. Compilation and policy closure precede this
operation and have separate budgets. See @docs/plans/EL-05-AGGREGATION.md@ for
semantic admission charges and deterministic failure precedence.
-}
module Markovian.Aggregation.Exact (
    AggregationLimits (..),
    AggregationDimension (..),
    AggregationError (..),
    AggregationAccounting (..),
    AggregationDifference (..),
    AggregationWitness,
    CheckedAggregation,
    AggregationResult (..),
    checkFixedPolicyAggregation,
    aggregationOriginalLayout,
    aggregationBlockLayout,
    aggregationMapping,
    aggregationInitialBlock,
    aggregationRows,
    aggregationWitnessStates,
    aggregationWitnessBlock,
    aggregationWitnessDifference,
) where

import Control.Monad (foldM, forM_, unless)
import Data.List.NonEmpty qualified as NE
import Data.Ratio (denominator, numerator)
import Markovian.Category.Finite.Set (FiniteSet, finiteSetValues)
import Markovian.Compile.Exact
import Markovian.Probability.Exact (exactOutcomes, exactProbability)
import Markovian.Reward.Exact (ExactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- | Independent hard caps. Zero is admitted; machine-sized caps are required.
data AggregationLimits = AggregationLimits
    { maximumAggregationStates :: !Natural
    , maximumAggregationBlocks :: !Natural
    , maximumAggregationSupport :: !Natural
    , maximumAggregationWork :: !Natural
    , maximumAggregationRationalBits :: !Natural
    }
    deriving (Eq, Show)

-- | Admission dimensions, in limit-validation order.
data AggregationDimension
    = AggregationStates
    | AggregationBlocks
    | AggregationSupport
    | AggregationWork
    | AggregationRationalBits
    deriving (Eq, Ord, Show)

-- | Atomic admission failures; no partial table, witness or ledger escapes.
data AggregationError state block
    = AggregationLimitExceeded !AggregationDimension !Natural !Natural
    | AggregationMachineOverflow !AggregationDimension
    | AggregationEmptyBlocks
    | AggregationDuplicatePartitionState !state
    | AggregationUnknownPartitionState !state
    | AggregationUnknownBlock !block
    | AggregationMissingPartitionState !state
    | AggregationUnusedBlock !block
    | AggregationDuplicateObservationState !state
    | AggregationUnknownObservationState !state
    | AggregationMissingObservationState !state
    deriving (Eq, Show)

-- | Counts cover all original rows, including on a distinguished result.
data AggregationAccounting = AggregationAccounting
    { aggregationStateCount :: !Natural
    , aggregationBlockCount :: !Natural
    , aggregationRawSupportCount :: !Natural
    , aggregationCanonicalSupportCount :: !Natural
    , aggregationWorkCharged :: !Natural
    , aggregationMaximumObservedBits :: !Natural
    }
    deriving (Eq, Show)

{- | Plain difference data; only an opaque witness certifies its provenance.
Fields are ordered representative first, member second. True means terminal.
-}
data AggregationDifference block observation
    = AggregationObservationDifference !observation !observation
    | AggregationTerminalDifference !Bool !Bool
    | AggregationPayoffDifference !ExactReward !ExactReward
    | AggregationJointMassDifference !ExactReward !block !Rational !Rational
    deriving (Eq, Show)

-- | A checked first difference after complete admission and canonicalization.
type role AggregationWitness nominal nominal nominal

data AggregationWitness state block observation
    = AggregationWitness
        !(state, state)
        !block
        !(AggregationDifference block observation)

-- | A quotient and its owned original layout/mapping. No recompilation occurs.
type role CheckedAggregation nominal nominal nominal

data CheckedAggregation state block observation
    = CheckedAggregation
        !(FiniteStateIndex state)
        !(FiniteSet block)
        ![(state, block)]
        !block
        ![(block, observation, Either ExactReward [(ExactReward, block, Rational)])]

-- | Both successful outcomes carry the same operation-wide accounting.
type role AggregationResult nominal nominal nominal

data AggregationResult state block observation
    = AggregationEquivalent !(CheckedAggregation state block observation)
    | AggregationDistinguished !(AggregationWitness state block observation)

-- | Stored original layout, not a reconstructed or reordered index.
aggregationOriginalLayout :: CheckedAggregation state block observation -> FiniteStateIndex state
aggregationOriginalLayout (CheckedAggregation x _ _ _ _) = x

-- | Stored block layout, in supplied order.
aggregationBlockLayout :: CheckedAggregation state block observation -> FiniteSet block
aggregationBlockLayout (CheckedAggregation _ x _ _ _) = x

-- | Total mapping in original state order.
aggregationMapping :: CheckedAggregation state block observation -> [(state, block)]
aggregationMapping (CheckedAggregation _ _ x _ _) = x

-- | Block containing the compiled original initial state.
aggregationInitialBlock :: CheckedAggregation state block observation -> block
aggregationInitialBlock (CheckedAggregation _ _ _ x _) = x

-- | Representative rows; atoms are ordered by reward then block ordinal.
aggregationRows :: CheckedAggregation state block observation -> [(block, observation, Either ExactReward [(ExactReward, block, Rational)])]
aggregationRows (CheckedAggregation _ _ _ _ x) = x

-- | Representative and distinguished member, in that order.
aggregationWitnessStates :: AggregationWitness state block observation -> (state, state)
aggregationWitnessStates (AggregationWitness x _ _) = x

-- | Declared block containing both states.
aggregationWitnessBlock :: AggregationWitness state block observation -> block
aggregationWitnessBlock (AggregationWitness _ x _) = x

-- | Stored, literally revalidated difference; no new arithmetic.
aggregationWitnessDifference :: AggregationWitness state block observation -> AggregationDifference block observation
aggregationWitnessDifference (AggregationWitness _ _ x) = x

-- Private state transformer keeps every phase under one atomic meter.
newtype Meter state block a = Meter
    {runMeter :: AggregationLimits -> AggregationAccounting -> Either (AggregationError state block) (a, AggregationAccounting)}
instance Functor (Meter state block) where
    fmap f action = Meter $ \limits account -> do
        (x, next) <- runMeter action limits account
        pure (f x, next)
instance Applicative (Meter state block) where
    pure x = Meter $ \_ account -> Right (x, account)
    ff <*> xx = do f <- ff; f <$> xx
instance Monad (Meter state block) where
    action >>= next = Meter $ \limits account -> do
        (x, updated) <- runMeter action limits account
        runMeter (next x) limits updated

cap :: AggregationLimits -> AggregationDimension -> Natural
cap l AggregationStates = maximumAggregationStates l
cap l AggregationBlocks = maximumAggregationBlocks l
cap l AggregationSupport = maximumAggregationSupport l
cap l AggregationWork = maximumAggregationWork l
cap l AggregationRationalBits = maximumAggregationRationalBits l

bounded :: AggregationLimits -> AggregationDimension -> Natural -> Either (AggregationError s b) ()
bounded limits dimension actual =
    unless (actual <= cap limits dimension) (Left (AggregationLimitExceeded dimension (cap limits dimension) (min actual (cap limits dimension + 1))))

count :: AggregationLimits -> AggregationDimension -> Natural -> [a] -> Either (AggregationError s b) Natural
count limits dimension = go
  where
    go n [] = Right n
    go n (_ : xs) = bounded limits dimension (n + 1) >> go (n + 1) xs

charge :: Natural -> Meter s b ()
charge amount = Meter $ \limits account -> do
    let next = min (maximumAggregationWork limits + 1) (aggregationWorkCharged account + amount)
    bounded limits AggregationWork next
    pure ((), account{aggregationWorkCharged = next})

bits :: Integer -> Natural
bits = go 1 . abs
  where
    go n x
        | x < 2 = n
        | otherwise = go (n + 1) (x `quot` 2)

observe :: Rational -> Meter s b ()
observe value = do
    charge 1
    Meter $ \limits account -> do
        let size = max (bits (numerator value)) (bits (denominator value))
        bounded limits AggregationRationalBits size
        pure ((), account{aggregationMaximumObservedBits = max size (aggregationMaximumObservedBits account)})

addMass :: Rational -> Rational -> Meter s b Rational
addMass x y = do
    charge 1
    -- Observation of an arithmetic result uses the arithmetic unit itself.
    Meter $ \limits account -> do
        let z = x + y
            size = max (bits (numerator z)) (bits (denominator z))
        bounded limits AggregationRationalBits size
        pure (z, account{aggregationMaximumObservedBits = max size (aggregationMaximumObservedBits account)})

-- Every intermediate is saturated, including product prefixes before a zero.
structuralWork :: Natural -> Natural -> Natural -> Natural -> Natural
structuralWork limit n b k =
    foldl
        plus
        0
        [times (times 4 n) n, times (times 2 n) b, n, b, times k (plus (plus n b) 1)]
  where
    plus x y = min (limit + 1) (x + y)
    times x y
        | x > limit || y > limit = limit + 1
        | otherwise = min (limit + 1) (x * y)

{- | Check a supplied total partition and observation labeling. The first
difference is in block/member order, not in supplied association-list order.
Terminal payoff precedes horizon truncation in the preserved semantics.
-}
checkFixedPolicyAggregation ::
    (Eq state, Eq block, Eq observation) =>
    AggregationLimits ->
    CompiledExactMRP state ->
    FiniteSet block ->
    [(state, block)] ->
    [(state, observation)] ->
    Either (AggregationError state block) (AggregationResult state block observation, AggregationAccounting)
checkFixedPolicyAggregation limits model blocks partition observations = do
    forM_ [AggregationStates, AggregationBlocks, AggregationSupport, AggregationWork, AggregationRationalBits] $ \dimension ->
        unless (cap limits dimension <= fromIntegral (maxBound :: Int)) (Left (AggregationMachineOverflow dimension))
    n <- count limits AggregationStates 0 entries
    b <- count limits AggregationBlocks 0 blockValues
    _ <- count limits AggregationStates 0 partition
    _ <- count limits AggregationStates 0 observations
    unless (b > 0) (Left AggregationEmptyBlocks)
    k <- foldM (\total (_, row) -> count limits AggregationSupport total (raw row)) 0 entries
    let w0 = structuralWork (maximumAggregationWork limits) n b k
    bounded limits AggregationWork w0
    validatePartition [] partition
    mapping <- traverse (\s -> maybe (Left (AggregationMissingPartitionState s)) (Right . (s,)) (lookup s partition)) states
    forM_ blockValues $ \v -> unless (v `elem` map snd mapping) (Left (AggregationUnusedBlock v))
    validateObservations [] observations
    labels <- traverse (\s -> maybe (Left (AggregationMissingObservationState s)) Right (lookup s observations)) states
    -- Coverage checks also produce canonical lists: zip, rather than repeated
    -- equality joins, keeps collection within the frozen structural reservation.
    let indexedBlocks = [(i, v) | ((i, _), (_, v)) <- zip entries mapping]
        initial = requireOwned (lookup (compiledMRPInitialState model) indexedBlocks)
        ordinal v = requireOwned (lookup v (zip blockValues [0 :: Natural ..]))
        inputRows = [(s, v, o, row) | ((_, row), (s, v), o) <- zip3 entries mapping labels]
    runMeter
        ( do
            forM_ entries $ \(_, row) -> case row of
                CompiledMRPTerminalState _ payoff -> observe (exactRewardValue payoff)
                CompiledMRPContinuingState _ _ -> forM_ (raw row) $ \(outcome, mass) -> do
                    observe (exactRewardValue (compiledTransitionReward outcome))
                    observe mass
            observe 0
            rows <- traverse (canonicalRow indexedBlocks ordinal) inputRows
            let groups = [(v, requireOwned (NE.nonEmpty [row | row@(_, target, _, _) <- rows, v == target])) | v <- blockValues]
            difference <- compareBlocks groups
            case difference of
                Just witness -> pure (AggregationDistinguished witness)
                Nothing -> do
                    forM_ blockValues (const (charge 1))
                    forM_ mapping (const (charge 1))
                    let quotientRows =
                            [ (v, o, fmap (map (\(r, _, target, mass) -> (r, target, mass))) law)
                            | (v, group) <- groups
                            , let (_, _, o, law) = NE.head group
                            ]
                    pure (AggregationEquivalent (CheckedAggregation (compiledMRPStateIndex model) blocks mapping initial quotientRows))
        )
        limits
        (AggregationAccounting n b k 0 w0 0)
  where
    entries = NE.toList (compiledMRPStateEntries model)
    states = map (compiledMRPSourceState . snd) entries
    blockValues = finiteSetValues blocks
    raw (CompiledMRPTerminalState _ _) = []
    raw (CompiledMRPContinuingState _ law) = [(x, exactProbability p) | (x, p) <- NE.toList (exactOutcomes law)]
    validatePartition _ [] = Right ()
    validatePartition seen ((s, v) : rest)
        | s `elem` seen = Left (AggregationDuplicatePartitionState s)
        | s `notElem` states = Left (AggregationUnknownPartitionState s)
        | v `notElem` blockValues = Left (AggregationUnknownBlock v)
        | otherwise = validatePartition (s : seen) rest
    validateObservations _ [] = Right ()
    validateObservations seen ((s, _) : rest)
        | s `elem` seen = Left (AggregationDuplicateObservationState s)
        | s `notElem` states = Left (AggregationUnknownObservationState s)
        | otherwise = validateObservations (s : seen) rest
    canonicalRow indexedBlocks ordinal (s, v, o, row) = do
        law <- case row of
            CompiledMRPTerminalState _ payoff -> pure (Left payoff)
            CompiledMRPContinuingState _ _ -> do
                atoms <-
                    foldM
                        ( \acc (outcome, mass) ->
                            let target = requireOwned (lookup (compiledSuccessorState outcome) indexedBlocks)
                             in insertAtom (compiledTransitionReward outcome, ordinal target, target, mass) acc
                        )
                        []
                        (raw row)
                forM_ atoms $ \(_, _, _, mass) -> observe mass
                Meter $ \ls account -> do
                    total <- count ls AggregationSupport (aggregationCanonicalSupportCount account) atoms
                    pure (Right atoms, account{aggregationCanonicalSupportCount = total})
        pure (s, v, o, law)

-- Only used for facts established by total coverage and opaque compiled indexes.
requireOwned :: Maybe a -> a
requireOwned (Just x) = x
requireOwned Nothing = error "Markovian.Aggregation.Exact: violated owned compiled layout invariant"

type Atom block = (ExactReward, Natural, block, Rational)
type Row state block observation = (state, block, observation, Either ExactReward [Atom block])

key :: Atom block -> (ExactReward, Natural)
key (r, ordinal, _, _) = (r, ordinal)

insertAtom :: Atom b -> [Atom b] -> Meter s b [Atom b]
insertAtom atom [] = charge 1 >> pure [atom]
insertAtom atom@(r, i, b, p) (candidate@(_, _, _, q) : rest) = do
    charge 1
    case compare (key atom) (key candidate) of
        LT -> charge 1 >> pure (atom : candidate : rest)
        EQ -> do total <- addMass q p; pure ((r, i, b, total) : rest)
        GT -> (candidate :) <$> insertAtom atom rest

compareBlocks :: (Eq o) => [(b, NE.NonEmpty (Row s b o))] -> Meter s b (Maybe (AggregationWitness s b o))
compareBlocks [] = pure Nothing
compareBlocks ((b, group) : groups) = do
    result <- members (NE.head group) (NE.tail group)
    case result of
        Just witness -> pure (Just witness)
        Nothing -> compareBlocks groups
  where
    members _ [] = pure Nothing
    members rep@(s, _, _, _) (row@(t, _, _, _) : rest) = do
        difference <- compareRows rep row
        case difference of
            Nothing -> members rep rest
            Just d -> do
                charge 1
                case d of
                    AggregationPayoffDifference x y -> observe (exactRewardValue x) >> observe (exactRewardValue y)
                    AggregationJointMassDifference r _ p q -> observe (exactRewardValue r) >> observe p >> observe q
                    _ -> pure ()
                charge 1
                -- Literal revalidation on the retained differing fields.
                let different = case d of
                        AggregationObservationDifference x y -> x /= y
                        AggregationTerminalDifference x y -> x /= y
                        AggregationPayoffDifference x y -> x /= y
                        AggregationJointMassDifference _ _ x y -> x /= y
                if different
                    then pure (Just (AggregationWitness (s, t) b d))
                    else error "Markovian.Aggregation.Exact: violated retained difference invariant"

compareRows :: (Eq o) => Row s b o -> Row s b o -> Meter s b (Maybe (AggregationDifference b o))
compareRows (_, _, x, left) (_, _, y, right) = do
    charge 1
    if x /= y
        then pure (Just (AggregationObservationDifference x y))
        else do
            charge 1
            case (left, right) of
                (Left _, Right _) -> pure (Just (AggregationTerminalDifference True False))
                (Right _, Left _) -> pure (Just (AggregationTerminalDifference False True))
                (Left p, Left q) -> do
                    charge 1
                    pure (if p == q then Nothing else Just (AggregationPayoffDifference p q))
                (Right p, Right q) -> compareAtoms p q

compareAtoms :: [Atom b] -> [Atom b] -> Meter s b (Maybe (AggregationDifference b o))
compareAtoms [] [] = pure Nothing
compareAtoms ((r, _, b, p) : _) [] = charge 2 >> pure (Just (AggregationJointMassDifference r b p 0))
compareAtoms [] ((r, _, b, q) : _) = charge 2 >> pure (Just (AggregationJointMassDifference r b 0 q))
compareAtoms (a@(r, _, b, p) : as) (z@(t, _, c, q) : zs) = do
    charge 1
    let ordering = compare (key a) (key z)
    charge 1
    case ordering of
        LT -> pure (Just (AggregationJointMassDifference r b p 0))
        GT -> pure (Just (AggregationJointMassDifference t c 0 q))
        EQ
            | p /= q -> pure (Just (AggregationJointMassDifference r b p q))
            | otherwise -> compareAtoms as zs
