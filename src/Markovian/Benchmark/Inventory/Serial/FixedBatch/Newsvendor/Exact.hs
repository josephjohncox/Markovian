{-# LANGUAGE TupleSections #-}

{- | Exact finite stationary subsystem calculations for fixed-batch reorder levels.

This module implements equations (9)--(14) and the weak/strict discrete
inequalities (17)--(21) from Doğru, van Houtum, and de Kok, BETA Working Paper
134 (2005), pages 10--14.  All sums use the conditioned finite demand law.
Search is restricted to explicit finite @R1@ and @R2@ layouts with declared
Cartesian-product semantics; no unbounded minimizer or continuous-demand
equality is returned.
-}
module Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact (
    ShortfallPair (..),
    ShortfallLaw,
    shortfallOutcomes,
    shortfallExpectedB1,
    shortfallExpectedB0,
    shortfallNoStockoutProbability,
    shortfallB1ZeroProbability,
    subsystem1Shortfalls,
    subsystem2Shortfalls,
    subsystem1Cost,
    subsystem2Cost,
    subsystem1ForwardDifference,
    subsystem2ForwardDifference,
    NewsvendorInequality (..),
    subsystem1Inequality,
    subsystem2Inequality,
    NewsvendorDomain,
    newsvendorDomain,
    newsvendorR1Layout,
    newsvendorR2Layout,
    newsvendorDomainContains,
    NewsvendorSolution,
    solveNewsvendorGrid,
    newsvendorSolutionParameters,
    newsvendorSolutionDomain,
    newsvendorSelectedLevels,
    newsvendorSubsystem1Cost,
    newsvendorSubsystem2Cost,
    newsvendorSubsystem1Difference,
    newsvendorSubsystem2Difference,
    newsvendorSubsystem1Inequality,
    newsvendorSubsystem2Inequality,
    newsvendorWeakLowerR1,
    newsvendorStrictUpperR1,
    newsvendorWeakLowerR2,
    newsvendorStrictUpperR2,
    newsvendorDomainBinds,
    newsvendorCheckedTerms,
) where

import Control.Monad (replicateM)
import Data.List (foldl', sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isNothing)
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact
import Numeric.Natural (Natural)

-- | Joint subsystem shortfalls @(B1,B0)@; @B2=0@ is implicit.
data ShortfallPair = ShortfallPair
    { shortfallB1 :: !Natural
    , shortfallB0 :: !Natural
    }
    deriving (Eq, Ord, Show)

newtype ShortfallLaw = ShortfallLaw (NonEmpty (ShortfallPair, Rational))
    deriving (Eq, Show)

shortfallOutcomes :: ShortfallLaw -> NonEmpty (ShortfallPair, Rational)
shortfallOutcomes (ShortfallLaw outcomes) = outcomes

shortfallExpectedB1 :: ShortfallLaw -> Rational
shortfallExpectedB1 law = sum [fromIntegral (shortfallB1 pair) * mass | (pair, mass) <- NonEmpty.toList (shortfallOutcomes law)]

shortfallExpectedB0 :: ShortfallLaw -> Rational
shortfallExpectedB0 law = sum [fromIntegral (shortfallB0 pair) * mass | (pair, mass) <- NonEmpty.toList (shortfallOutcomes law)]

shortfallNoStockoutProbability :: ShortfallLaw -> Rational
shortfallNoStockoutProbability law = sum [mass | (pair, mass) <- NonEmpty.toList (shortfallOutcomes law), shortfallB0 pair == 0]

shortfallB1ZeroProbability :: ShortfallLaw -> Rational
shortfallB1ZeroProbability law = sum [mass | (pair, mass) <- NonEmpty.toList (shortfallOutcomes law), shortfallB1 pair == 0]

-- | Equations (10) and (12) for the isolated stage-1 subsystem.
subsystem1Shortfalls :: FixedBatchParameters -> Integer -> Either FixedBatchError ShortfallLaw
subsystem1Shortfalls parameters r1 = do
    preflight parameters terms
    makeLaw
        [ (ShortfallPair 0 (positivePart (toInteger demand - r1 - toInteger uniform)), demandMass / fromIntegral q1)
        | (demand, demandMass) <- demandSupport
        , uniform <- [1 .. q1]
        ]
  where
    demandSupport = demandValues parameters
    q1 = fixedBatchQ1 parameters
    terms = fromIntegral (length demandSupport) * q1

-- | Equations (10)--(12) for the isolated two-stage subsystem.
subsystem2Shortfalls :: FixedBatchParameters -> ReorderLevels -> Either FixedBatchError ShortfallLaw
subsystem2Shortfalls parameters levels = do
    let demandCount = fromIntegral (length (demandValues parameters))
        count = demandCount ^ (fixedBatchSupplierLeadTime parameters + 1) * (fixedBatchQ2 parameters `div` fixedBatchQ1 parameters) * fixedBatchQ1 parameters
    preflight parameters count
    leadDemand <- convolvedDemand parameters (fixedBatchSupplierLeadTime parameters)
    subsystem2ShortfallsFromLead parameters leadDemand levels

subsystem2ShortfallsFromLead :: FixedBatchParameters -> NonEmpty (Natural, Rational) -> ReorderLevels -> Either FixedBatchError ShortfallLaw
subsystem2ShortfallsFromLead parameters leadDemand levels =
    makeLaw
        [ ( ShortfallPair b1 (positivePart (toInteger b1 + toInteger d1 - fixedBatchR1 levels - toInteger uniform))
          , leadMass * d1Mass / fromIntegral ratio / fromIntegral q1
          )
        | (leadSum, leadMass) <- NonEmpty.toList leadDemand
        , (d1, d1Mass) <- demand
        , z <- [0 .. ratio - 1]
        , let b1 = positivePart (toInteger leadSum - (fixedBatchR2 levels - fixedBatchR1 levels) - toInteger (z * q1))
        , uniform <- [1 .. q1]
        ]
  where
    demand = demandValues parameters
    q1 = fixedBatchQ1 parameters
    ratio = fixedBatchQ2 parameters `div` q1

-- | Equation (9) for @i=1@ with @L1=0@.
subsystem1Cost :: FixedBatchParameters -> Integer -> Either FixedBatchError Rational
subsystem1Cost parameters r1 = do
    law <- subsystem1Shortfalls parameters r1
    Right (cost1FromLaw parameters r1 law)

cost1FromLaw :: FixedBatchParameters -> Integer -> ShortfallLaw -> Rational
cost1FromLaw parameters r1 law =
    h1 * echelonTerm + (penalty + h1 + h2) * shortfallExpectedB0 law
  where
    h1 = fixedBatchStage1HoldingCost parameters
    h2 = fixedBatchStage2HoldingCost parameters
    penalty = fixedBatchBacklogCost parameters
    q1 = fixedBatchQ1 parameters
    mean = demandMean parameters
    echelonTerm = fromInteger r1 + (fromIntegral q1 + 1) / 2 - mean

-- | Equation (9) for @i=2@ with @L1=0@ and configured positive @L2@.
subsystem2Cost :: FixedBatchParameters -> ReorderLevels -> Either FixedBatchError Rational
subsystem2Cost parameters levels = do
    law <- subsystem2Shortfalls parameters levels
    Right (cost2FromLaw parameters levels law)

cost2FromLaw :: FixedBatchParameters -> ReorderLevels -> ShortfallLaw -> Rational
cost2FromLaw parameters levels law =
    h1 * stage1Term + h2 * stage2Term + (penalty + h1 + h2) * shortfallExpectedB0 law
  where
    h1 = fixedBatchStage1HoldingCost parameters
    h2 = fixedBatchStage2HoldingCost parameters
    penalty = fixedBatchBacklogCost parameters
    q1 = fixedBatchQ1 parameters
    q2 = fixedBatchQ2 parameters
    mean = demandMean parameters
    stage1Term = fromInteger (fixedBatchR1 levels) - shortfallExpectedB1 law + (fromIntegral q1 + 1) / 2 - mean
    stage2Term =
        fromInteger (fixedBatchR2 levels)
            + (fromIntegral q2 + 1) / 2
            - fromIntegral (fixedBatchSupplierLeadTime parameters + 1) * mean

subsystem1ForwardDifference :: FixedBatchParameters -> Integer -> Either FixedBatchError Rational
subsystem1ForwardDifference parameters r1 = (-) <$> subsystem1Cost parameters (r1 + 1) <*> subsystem1Cost parameters r1

subsystem2ForwardDifference :: FixedBatchParameters -> ReorderLevels -> Either FixedBatchError Rational
subsystem2ForwardDifference parameters levels =
    (-)
        <$> subsystem2Cost parameters levels{fixedBatchR2 = fixedBatchR2 levels + 1}
        <*> subsystem2Cost parameters levels

-- | Both equivalent sides of Theorem 1's discrete weak/strict test.
data NewsvendorInequality = NewsvendorInequality
    { newsvendorNoStockoutProbability :: !Rational
    , newsvendorRightHandSide :: !Rational
    , newsvendorForwardDifference :: !Rational
    , newsvendorWeakSatisfied :: !Bool
    , newsvendorStrictSatisfied :: !Bool
    }
    deriving (Eq, Show)

subsystem1Inequality :: FixedBatchParameters -> Integer -> Either FixedBatchError NewsvendorInequality
subsystem1Inequality parameters r1 = do
    law <- subsystem1Shortfalls parameters r1
    difference <- subsystem1ForwardDifference parameters r1
    let h1 = fixedBatchStage1HoldingCost parameters
        h2 = fixedBatchStage2HoldingCost parameters
        penalty = fixedBatchBacklogCost parameters
        rhs = (penalty + h2) / (penalty + h1 + h2)
    makeInequality law rhs difference

subsystem2Inequality :: FixedBatchParameters -> ReorderLevels -> Either FixedBatchError NewsvendorInequality
subsystem2Inequality parameters levels = do
    law <- subsystem2Shortfalls parameters levels
    c1 <- subsystem1ForwardDifference parameters (fixedBatchR1 levels)
    difference <- subsystem2ForwardDifference parameters levels
    let h1 = fixedBatchStage1HoldingCost parameters
        h2 = fixedBatchStage2HoldingCost parameters
        penalty = fixedBatchBacklogCost parameters
        rhs = penalty / (penalty + h1 + h2) + shortfallB1ZeroProbability law * c1 / (penalty + h1 + h2)
    makeInequality law rhs difference

makeInequality :: ShortfallLaw -> Rational -> Rational -> Either FixedBatchError NewsvendorInequality
makeInequality law rhs difference
    | (difference >= 0) /= (probability >= rhs) = Left (FixedBatchModelMismatch "weak newsvendor inequality and forward difference disagree")
    | (difference > 0) /= (probability > rhs) = Left (FixedBatchModelMismatch "strict newsvendor inequality and forward difference disagree")
    | otherwise =
        Right
            NewsvendorInequality
                { newsvendorNoStockoutProbability = probability
                , newsvendorRightHandSide = rhs
                , newsvendorForwardDifference = difference
                , newsvendorWeakSatisfied = probability >= rhs
                , newsvendorStrictSatisfied = probability > rhs
                }
  where
    probability = shortfallNoStockoutProbability law

{- | Explicit Cartesian stationary search domain.  The two layouts are kept
separate so a sparse set of execution-policy pairs cannot silently acquire
unreported pair candidates by projection and recombination.
-}
data NewsvendorDomain = NewsvendorDomain
    { newsvendorR1Layout :: !(NonEmpty Integer)
    , newsvendorR2Layout :: !(NonEmpty Integer)
    }
    deriving (Eq, Show)

{- | Validate and canonicalize the two stationary reorder-level axes.  Their
Cartesian cardinality is charged against the finite grid budget before the
solver constructs any shortfall law.
-}
newsvendorDomain :: FixedBatchParameters -> NonEmpty Integer -> NonEmpty Integer -> Either FixedBatchError NewsvendorDomain
newsvendorDomain parameters rawR1 rawR2 = do
    r1Values <- boundedAxis "R1" rawR1
    r2Values <- boundedAxis "R2" rawR2
    let cardinality = fromIntegral (length r1Values) * fromIntegral (length r2Values)
    if cardinality > fixedBatchGridBudget parameters
        then Left (FixedBatchGridBudgetExceeded cardinality)
        else
            Right
                NewsvendorDomain
                    { newsvendorR1Layout = NonEmpty.fromList r1Values
                    , newsvendorR2Layout = NonEmpty.fromList r2Values
                    }
  where
    boundedAxis label values = do
        consumed <- consumeAtMost (fixedBatchGridBudget parameters) (NonEmpty.toList values)
        let ordered = sort consumed
        case firstDuplicateValue ordered of
            Just duplicate -> Left (FixedBatchModelMismatch ("duplicate stationary " ++ label ++ " level " ++ show duplicate))
            Nothing -> Right ordered

-- | Membership in the represented Cartesian stationary domain.
newsvendorDomainContains :: NewsvendorDomain -> ReorderLevels -> Bool
newsvendorDomainContains domain levels =
    fixedBatchR1 levels `elem` NonEmpty.toList (newsvendorR1Layout domain)
        && fixedBatchR2 levels `elem` NonEmpty.toList (newsvendorR2Layout domain)

{- | Sequential finite-domain stationary subsystem evidence.  Parameters and
the actual Cartesian domain are retained as provenance for report checks.
-}
data NewsvendorSolution = NewsvendorSolution
    { newsvendorSolutionParameters :: !FixedBatchParameters
    , newsvendorSolutionDomain :: !NewsvendorDomain
    , newsvendorSelectedLevels :: !ReorderLevels
    , newsvendorSubsystem1Cost :: !Rational
    , newsvendorSubsystem2Cost :: !Rational
    , newsvendorSubsystem1Difference :: !Rational
    , newsvendorSubsystem2Difference :: !Rational
    , newsvendorSubsystem1Inequality :: !NewsvendorInequality
    , newsvendorSubsystem2Inequality :: !NewsvendorInequality
    , newsvendorWeakLowerR1 :: !(Maybe Integer)
    , newsvendorStrictUpperR1 :: !(Maybe Integer)
    , newsvendorWeakLowerR2 :: !(Maybe Integer)
    , newsvendorStrictUpperR2 :: !(Maybe Integer)
    , newsvendorDomainBinds :: !Bool
    , newsvendorCheckedTerms :: !Natural
    }
    deriving (Eq, Show)

{- | Solve on the represented Cartesian domain.  Each distinct required
shortfall law is built once.  The stored work count is the exact number of
raw finite-law terms generated: stage-1 laws, one lead-demand convolution,
and stage-2 laws.  Rejection occurs before any of those lists is built.
-}
solveNewsvendorGrid :: FixedBatchParameters -> NewsvendorDomain -> Either FixedBatchError NewsvendorSolution
solveNewsvendorGrid parameters domain = do
    let r1Values = NonEmpty.toList (newsvendorR1Layout domain)
        r2Values = NonEmpty.toList (newsvendorR2Layout domain)
        r1LawKeys = unique (r1Values ++ fmap (+ 1) r1Values)
        r2LawKeys = unique (r2Values ++ fmap (+ 1) r2Values)
        demandCount = fromIntegral (length (demandValues parameters))
        stage1Terms = demandCount * fixedBatchQ1 parameters
        leadTerms = demandCount ^ fixedBatchSupplierLeadTime parameters
        -- The conditioned geometric support is every integer from zero through
        -- demandCap, so its L-period sum has exactly L*demandCap+1 outcomes.
        leadSupportCount = fixedBatchSupplierLeadTime parameters * fixedBatchDemandCap parameters + 1
        stage2Terms = leadSupportCount * demandCount * (fixedBatchQ2 parameters `div` fixedBatchQ1 parameters) * fixedBatchQ1 parameters
        checked =
            fromIntegral (length r1LawKeys) * stage1Terms
                + leadTerms
                + fromIntegral (length r2LawKeys) * stage2Terms
    if checked > fixedBatchSolverWorkBudget parameters
        then Left (FixedBatchSolverWorkBudgetExceeded checked)
        else Right ()
    preflight parameters stage1Terms
    preflight parameters leadTerms
    preflight parameters stage2Terms
    stage1Laws <- traverse (\r1 -> fmap (r1,) (subsystem1Shortfalls parameters r1)) r1LawKeys
    let stage1CostAt r1 = cost1FromLaw parameters r1 <$> lookupLaw "stage-1" r1 stage1Laws
        stage1DifferenceAt r1 = (-) <$> stage1CostAt (r1 + 1) <*> stage1CostAt r1
        stage1InequalityAt r1 = do
            law <- lookupLaw "stage-1" r1 stage1Laws
            difference <- stage1DifferenceAt r1
            let rhs =
                    (fixedBatchBacklogCost parameters + fixedBatchStage2HoldingCost parameters)
                        / (fixedBatchBacklogCost parameters + fixedBatchStage1HoldingCost parameters + fixedBatchStage2HoldingCost parameters)
            makeInequality law rhs difference
    stage1Costs <- traverse (\r1 -> fmap (r1,) (stage1CostAt r1)) r1Values
    (selectedR1, selectedC1) <- minimumValue stage1Costs
    leadDemand <- convolvedDemand parameters (fixedBatchSupplierLeadTime parameters)
    stage2Laws <-
        traverse
            ( \r2 ->
                fmap
                    (r2,)
                    (subsystem2ShortfallsFromLead parameters leadDemand (ReorderLevels selectedR1 r2))
            )
            r2LawKeys
    let stage2CostAt r2 = cost2FromLaw parameters (ReorderLevels selectedR1 r2) <$> lookupLaw "stage-2" r2 stage2Laws
        stage2DifferenceAt r2 = (-) <$> stage2CostAt (r2 + 1) <*> stage2CostAt r2
        stage2InequalityAt c1 r2 = do
            law <- lookupLaw "stage-2" r2 stage2Laws
            difference <- stage2DifferenceAt r2
            let totalCost = fixedBatchBacklogCost parameters + fixedBatchStage1HoldingCost parameters + fixedBatchStage2HoldingCost parameters
                rhs = fixedBatchBacklogCost parameters / totalCost + shortfallB1ZeroProbability law * c1 / totalCost
            makeInequality law rhs difference
        pairWithLevels r2 cost = (ReorderLevels selectedR1 r2, cost)
    stage2Costs <- traverse (\r2 -> fmap (pairWithLevels r2) (stage2CostAt r2)) r2Values
    (selectedLevels, selectedC2) <- minimumValue stage2Costs
    c1 <- stage1DifferenceAt selectedR1
    c2 <- stage2DifferenceAt (fixedBatchR2 selectedLevels)
    inequality1 <- stage1InequalityAt selectedR1
    inequality2 <- stage2InequalityAt c1 (fixedBatchR2 selectedLevels)
    tests1 <- traverse stage1InequalityAt r1Values
    tests2 <- traverse (stage2InequalityAt c1) r2Values
    let weakR1 = firstSatisfying newsvendorWeakSatisfied r1Values tests1
        strictR1 = firstSatisfying newsvendorStrictSatisfied r1Values tests1
        weakR2 = firstSatisfying newsvendorWeakSatisfied r2Values tests2
        strictR2 = firstSatisfying newsvendorStrictSatisfied r2Values tests2
        boundary =
            selectedR1 == minimum r1Values
                || selectedR1 == maximum r1Values
                || fixedBatchR2 selectedLevels == minimum r2Values
                || fixedBatchR2 selectedLevels == maximum r2Values
                || isNothing weakR1
                || isNothing strictR1
                || isNothing weakR2
                || isNothing strictR2
    if not (newsvendorDomainContains domain selectedLevels)
        then Left (FixedBatchModelMismatch "stationary selection is outside its represented Cartesian domain")
        else
            Right
                NewsvendorSolution
                    { newsvendorSolutionParameters = parameters
                    , newsvendorSolutionDomain = domain
                    , newsvendorSelectedLevels = selectedLevels
                    , newsvendorSubsystem1Cost = selectedC1
                    , newsvendorSubsystem2Cost = selectedC2
                    , newsvendorSubsystem1Difference = c1
                    , newsvendorSubsystem2Difference = c2
                    , newsvendorSubsystem1Inequality = inequality1
                    , newsvendorSubsystem2Inequality = inequality2
                    , newsvendorWeakLowerR1 = weakR1
                    , newsvendorStrictUpperR1 = strictR1
                    , newsvendorWeakLowerR2 = weakR2
                    , newsvendorStrictUpperR2 = strictR2
                    , newsvendorDomainBinds = boundary
                    , newsvendorCheckedTerms = checked
                    }

minimumValue :: (Ord key) => [(key, Rational)] -> Either FixedBatchError (key, Rational)
minimumValue [] = Left FixedBatchEmptyGrid
minimumValue (first : remaining) = Right (foldl' choose first remaining)
  where
    choose selected candidate
        | snd candidate < snd selected = candidate
        | otherwise = selected

firstSatisfying :: (NewsvendorInequality -> Bool) -> [Integer] -> [NewsvendorInequality] -> Maybe Integer
firstSatisfying predicate values tests = case [value | (value, result) <- zip values tests, predicate result] of
    value : _ -> Just value
    [] -> Nothing

convolvedDemand :: FixedBatchParameters -> Natural -> Either FixedBatchError (NonEmpty (Natural, Rational))
convolvedDemand parameters periods = do
    let demand = demandValues parameters
        terms = fromIntegral (length demand) ^ periods
    preflight parameters terms
    makeScalarLaw
        [ (sum (fmap fst path), product (fmap snd path))
        | path <- replicateM (fromIntegral periods) demand
        ]

makeLaw :: [(ShortfallPair, Rational)] -> Either FixedBatchError ShortfallLaw
makeLaw raw = case NonEmpty.nonEmpty (aggregate raw) of
    Nothing -> Left (FixedBatchModelMismatch "empty shortfall law")
    Just outcomes
        | sum (fmap snd (NonEmpty.toList outcomes)) /= 1 -> Left (FixedBatchModelMismatch "shortfall law is not normalized")
        | otherwise -> Right (ShortfallLaw outcomes)

makeScalarLaw :: [(Natural, Rational)] -> Either FixedBatchError (NonEmpty (Natural, Rational))
makeScalarLaw raw = case NonEmpty.nonEmpty (aggregate raw) of
    Nothing -> Left (FixedBatchModelMismatch "empty convolved demand")
    Just outcomes
        | sum (fmap snd (NonEmpty.toList outcomes)) /= 1 -> Left (FixedBatchModelMismatch "convolved demand is not normalized")
        | otherwise -> Right outcomes

aggregate :: (Ord value) => [(value, Rational)] -> [(value, Rational)]
aggregate = combine . sort
  where
    combine [] = []
    combine ((value, mass) : remaining) = gather value mass remaining
    gather value mass [] = [(value, mass)]
    gather value mass ((nextValue, nextMass) : remaining)
        | value == nextValue = gather value (mass + nextMass) remaining
        | otherwise = (value, mass) : gather nextValue nextMass remaining

preflight :: FixedBatchParameters -> Natural -> Either FixedBatchError ()
preflight parameters terms
    | terms > fixedBatchConvolutionBudget parameters = Left (FixedBatchConvolutionBudgetExceeded terms)
    | otherwise = Right ()

demandValues :: FixedBatchParameters -> [(Natural, Rational)]
demandValues = NonEmpty.toList . fixedBatchDemandOutcomes . conditionedGeometricDemand . fixedBatchDemandCap

demandMean :: FixedBatchParameters -> Rational
demandMean parameters = sum [fromIntegral demand * mass | (demand, mass) <- demandValues parameters]

positivePart :: Integer -> Natural
positivePart = fromInteger . max 0

lookupLaw :: (Eq key, Show key) => String -> key -> [(key, value)] -> Either FixedBatchError value
lookupLaw label key laws =
    maybe
        (Left (FixedBatchModelMismatch ("memoized " ++ label ++ " law missing for " ++ show key)))
        Right
        (lookup key laws)

consumeAtMost :: Natural -> [value] -> Either FixedBatchError [value]
consumeAtMost limit = go limit []
  where
    go 0 consumed [] = Right (reverse consumed)
    go 0 _ (_ : _) = Left (FixedBatchGridBudgetExceeded (limit + 1))
    go _ consumed [] = Right (reverse consumed)
    go remaining consumed (value : values) = go (remaining - 1) (value : consumed) values

firstDuplicateValue :: (Eq value) => [value] -> Maybe value
firstDuplicateValue (left : right : remaining)
    | left == right = Just right
    | otherwise = firstDuplicateValue (right : remaining)
firstDuplicateValue _ = Nothing

unique :: (Ord value) => [value] -> [value]
unique = removeDuplicates . sort
  where
    removeDuplicates [] = []
    removeDuplicates (first : remaining) = first : go first remaining
    go _ [] = []
    go previous (value : values)
        | previous == value = go previous values
        | otherwise = value : go value values
