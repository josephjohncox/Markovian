{- | Independent exact checks for the finite Clark--Scarf specialization.

The module compares an @ExactMDP@ traversal, a direct finite-sum transcription
of equation (14), and the equations (15), (20), (21), and (26) decomposition
from Clark and Scarf (1960), pages 482--484.  Agreement is evidence only for the
validated finite lattice and explicit bounds supplied to this computation.
-}
module Markovian.Benchmark.Inventory.ClarkScarf1960.Oracle.Exact (
    ClarkScarfStateCheck (..),
    ClarkScarfSolution,
    solveClarkScarf,
    clarkScarfSolutionFixture,
    clarkScarfStateChecks,
    clarkScarfInitialCost,
    clarkScarfInitialDecomposedPolicyCost,
    clarkScarfInitialRegret,
    clarkScarfMaximumDifferential,
    clarkScarfOrderCapSelected,
    clarkScarfTargetCapSelected,
    clarkScarfCheckedWork,
    clarkScarfEquation15Cost,
    clarkScarfEquation15Target,
    clarkScarfEquation21OpportunityLoss,
    clarkScarfEquation26Cost,
    ClarkScarfStability (..),
    compareClarkScarfBounds,
) where

import Control.Monad (foldM)
import Data.List (foldl', sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact
import Markovian.MDP.Exact (
    ExactDecision (..),
    exactSuccessorState,
    exactTransitionReward,
    inspectExactMDP,
    stepExactMDP,
 )
import Markovian.Objective.Exact (exactDiscountValue)
import Markovian.Probability.Exact (exactOutcomes, exactProbability)
import Markovian.Reward.Exact (exactRewardValue)
import Numeric.Natural (Natural)

-- | Literal results retained for every represented reachable state.
data ClarkScarfStateCheck = ClarkScarfStateCheck
    { clarkScarfCheckedState :: !ClarkScarfState
    , clarkScarfGenericMDPCost :: !Rational
    , clarkScarfEquation14Cost :: !Rational
    , clarkScarfDecomposedCost :: !Rational
    , clarkScarfDecomposedPolicyCost :: !Rational
    , clarkScarfEquation14Action :: !(Maybe ClarkScarfAction)
    , clarkScarfDecomposedAction :: !(Maybe ClarkScarfAction)
    }
    deriving (Eq, Show)

-- | Opaque completed three-path comparison.
data ClarkScarfSolution = ClarkScarfSolution
    { clarkScarfSolutionFixture :: !ClarkScarfFixture
    , clarkScarfStateChecks :: !(NonEmpty.NonEmpty ClarkScarfStateCheck)
    , solutionInitialCost :: !Rational
    , solutionPolicyCost :: !Rational
    , solutionMaximumDifferential :: !Rational
    , solutionOrderCapSelected :: !Bool
    , solutionTargetCapSelected :: !Bool
    , clarkScarfCheckedWork :: !Natural
    }

instance Show ClarkScarfSolution where
    show solution =
        "ClarkScarfSolution {initialCost = "
            ++ show (clarkScarfInitialCost solution)
            ++ ", regret = "
            ++ show (clarkScarfInitialRegret solution)
            ++ ", maximumDifferential = "
            ++ show (clarkScarfMaximumDifferential solution)
            ++ "}"

clarkScarfInitialCost :: ClarkScarfSolution -> Rational
clarkScarfInitialCost = solutionInitialCost

clarkScarfInitialDecomposedPolicyCost :: ClarkScarfSolution -> Rational
clarkScarfInitialDecomposedPolicyCost = solutionPolicyCost

clarkScarfInitialRegret :: ClarkScarfSolution -> Rational
clarkScarfInitialRegret solution =
    clarkScarfInitialDecomposedPolicyCost solution - clarkScarfInitialCost solution

clarkScarfMaximumDifferential :: ClarkScarfSolution -> Rational
clarkScarfMaximumDifferential = solutionMaximumDifferential

clarkScarfOrderCapSelected :: ClarkScarfSolution -> Bool
clarkScarfOrderCapSelected = solutionOrderCapSelected

clarkScarfTargetCapSelected :: ClarkScarfSolution -> Bool
clarkScarfTargetCapSelected = solutionTargetCapSelected

-- | Run all three paths. Budget rejection occurs before semantic traversal.
solveClarkScarf :: ClarkScarfFixture -> Either ClarkScarfError ClarkScarfSolution
solveClarkScarf fixture = do
    let parameters = clarkScarfFixtureParameters fixture
        demandCount = fromIntegral (length (clarkScarfDemandOutcomes (clarkScarfFixtureDemand fixture)))
        stateCount = fromIntegral (length (clarkScarfReachableStates fixture))
        horizon = clarkScarfHorizon parameters
        maximumDemand = maximum (fmap fst (NonEmpty.toList (clarkScarfDemandOutcomes (clarkScarfFixtureDemand fixture))))
        -- Every isolated recursion starts from either a represented lower-echelon
        -- inventory position or a represented echelon-two stock.  A recursive
        -- position is @target-demand@, so after at most @horizon@ descents it is
        -- no smaller than the represented minimum minus @horizon*maximumDemand@.
        -- This retains the current inventory position that the old target-cap-
        -- only estimate omitted (for example, x1=-10000 with targetCap=0).
        representedMinimumPosition =
            minimum
                [ min
                    (clarkScarfDownstreamNetStock state + toInteger (clarkScarfDownstreamInTransit state))
                    (clarkScarfEchelonTwoStock state)
                | state <- NonEmpty.toList (clarkScarfReachableStates fixture)
                ]
        isolatedMinimumPosition = representedMinimumPosition - toInteger horizon * toInteger maximumDemand
        maximumIsolatedTargets =
            fromInteger
                (max 1 (clarkScarfIsolatedTargetCap parameters - isolatedMinimumPosition + 1))
        isolatedBranch = maximumIsolatedTargets * demandCount
        upstreamBranch = (clarkScarfOrderCap parameters + 1) * demandCount
        isolatedTree = boundedTreeSize horizon isolatedBranch
        upstreamTree = boundedTreeSize horizon upstreamBranch
        conservativeWork =
            4 * clarkScarfStateActionCount fixture * demandCount
                + 8 * stateCount * isolatedTree * upstreamTree
    if conservativeWork > clarkScarfSolverWorkBudget parameters
        then Left (ClarkScarfSolverWorkBudgetExceeded conservativeWork)
        else Right ()
    generic <- genericCosts fixture
    direct <- equation14Costs fixture
    policy <- decomposedPolicyCosts fixture
    let states = sortOn clarkScarfPeriodsRemaining (NonEmpty.toList (clarkScarfReachableStates fixture))
    checks <- traverse (makeCheck parameters fixture generic direct policy) states
    nonemptyChecks <- maybe (Left (ClarkScarfMissingSuccessor (clarkScarfInitialState fixture))) Right (NonEmpty.nonEmpty checks)
    let differentials =
            concatMap
                ( \check ->
                    [ abs (clarkScarfGenericMDPCost check - clarkScarfEquation14Cost check)
                    , abs (clarkScarfEquation14Cost check - clarkScarfDecomposedCost check)
                    , abs (clarkScarfEquation14Cost check - clarkScarfDecomposedPolicyCost check)
                    ]
                )
                checks
        maximumDifference = maximum (0 : differentials)
        initial = clarkScarfInitialState fixture
    initialCheck <- maybe (Left (ClarkScarfMissingSuccessor initial)) Right (findCheck initial checks)
    if maximumDifference /= 0
        then Left (ClarkScarfModelError ("exact oracle paths disagree; maximum difference=" ++ show maximumDifference))
        else
            Right
                ClarkScarfSolution
                    { clarkScarfSolutionFixture = fixture
                    , clarkScarfStateChecks = nonemptyChecks
                    , solutionInitialCost = clarkScarfEquation14Cost initialCheck
                    , solutionPolicyCost = clarkScarfDecomposedPolicyCost initialCheck
                    , solutionMaximumDifferential = maximumDifference
                    , solutionOrderCapSelected = any (actionAtOrderCap parameters . clarkScarfDecomposedAction) checks
                    , solutionTargetCapSelected = any (targetAtCap parameters fixture) checks
                    , clarkScarfCheckedWork = conservativeWork
                    }

makeCheck ::
    ClarkScarfParameters ->
    ClarkScarfFixture ->
    [(ClarkScarfState, Rational)] ->
    [(ClarkScarfState, (Rational, Maybe ClarkScarfAction))] ->
    [(ClarkScarfState, (Rational, Maybe ClarkScarfAction))] ->
    ClarkScarfState ->
    Either ClarkScarfError ClarkScarfStateCheck
makeCheck parameters fixture generic direct policy state = do
    genericCost <- requireLookup state generic
    (directCost, directAction) <- requireLookup state direct
    (policyCost, policyAction) <- requireLookup state policy
    let decomposed =
            clarkScarfEquation15Cost parameters (clarkScarfFixtureDemand fixture) (clarkScarfPeriodsRemaining state) (clarkScarfDownstreamNetStock state) (clarkScarfDownstreamInTransit state)
                + clarkScarfEquation26Cost parameters (clarkScarfFixtureDemand fixture) (clarkScarfPeriodsRemaining state) (clarkScarfEchelonTwoStock state)
    Right
        ClarkScarfStateCheck
            { clarkScarfCheckedState = state
            , clarkScarfGenericMDPCost = genericCost
            , clarkScarfEquation14Cost = directCost
            , clarkScarfDecomposedCost = decomposed
            , clarkScarfDecomposedPolicyCost = policyCost
            , clarkScarfEquation14Action = directAction
            , clarkScarfDecomposedAction = policyAction
            }

-- Generic MDP path: inspect actions and consume paired reward/successor outcomes.
genericCosts :: ClarkScarfFixture -> Either ClarkScarfError [(ClarkScarfState, Rational)]
genericCosts fixture = foldM evaluate [] ordered
  where
    model = clarkScarfMDP fixture
    alpha = exactDiscountValue (clarkScarfDiscount (clarkScarfFixtureParameters fixture))
    ordered = sortOn clarkScarfPeriodsRemaining (NonEmpty.toList (clarkScarfReachableStates fixture))
    evaluate values state =
        case inspectExactMDP model state of
            Left err -> Left (ClarkScarfModelError (show err))
            Right (ExactTerminalDecision payoff) -> Right ((state, negate (exactRewardValue payoff)) : values)
            Right (ExactActionDecision actions) -> do
                costs <- traverse (actionCost values state) (NonEmpty.toList actions)
                Right ((state, minimum costs) : values)
    actionCost values state selected =
        case stepExactMDP model state selected of
            Left err -> Left (ClarkScarfModelError (show err))
            Right distribution ->
                fmap
                    sum
                    ( traverse
                        ( \(outcome, probability) -> do
                            future <- requireLookup (exactSuccessorState outcome) values
                            Right
                                ( exactProbability probability
                                    * (negate (exactRewardValue (exactTransitionReward outcome)) + alpha * future)
                                )
                        )
                        (NonEmpty.toList (exactOutcomes distribution))
                    )

-- Direct equation-(14) path: use the source equation and finite sums directly.
equation14Costs :: ClarkScarfFixture -> Either ClarkScarfError [(ClarkScarfState, (Rational, Maybe ClarkScarfAction))]
equation14Costs fixture = foldM evaluate [] ordered
  where
    parameters = clarkScarfFixtureParameters fixture
    demand = clarkScarfFixtureDemand fixture
    alpha = exactDiscountValue (clarkScarfDiscount parameters)
    ordered = sortOn clarkScarfPeriodsRemaining (NonEmpty.toList (clarkScarfReachableStates fixture))
    evaluate values state
        | clarkScarfPeriodsRemaining state == 0 = Right ((state, (0, Nothing)) : values)
        | otherwise = do
            actions <- clarkScarfActions parameters state
            alternatives <- traverse (actionCost values state) actions
            case alternatives of
                [] -> Left (ClarkScarfNoAction state)
                first : remaining ->
                    let (cost, action) = foldl' chooseMinimum first remaining
                     in Right ((state, (cost, Just action)) : values)
    actionCost values state action = do
        cost <-
            fmap
                sum
                ( traverse
                    ( \(demandValue, mass) -> do
                        (successor, immediate) <- clarkScarfTransition parameters state action demandValue
                        (future, _) <- requireLookup successor values
                        Right (mass * (immediate + alpha * future))
                    )
                    (NonEmpty.toList (clarkScarfDemandOutcomes demand))
                )
        Right (cost, action)

-- | Equation (15), with integrals replaced by exact sums and an explicit target cap.
clarkScarfEquation15Cost :: ClarkScarfParameters -> ClarkScarfDemand -> Natural -> Integer -> Natural -> Rational
clarkScarfEquation15Cost parameters demand periods x1 w1
    | periods == 0 = 0
    | otherwise = fst (isolatedMinimum parameters demand periods x1 w1)

-- | The minimizing post-dispatch target in the bounded equation (15).
clarkScarfEquation15Target :: ClarkScarfParameters -> ClarkScarfDemand -> Natural -> Integer -> Natural -> Integer
clarkScarfEquation15Target parameters demand periods x1 w1
    | periods == 0 = x1 + toInteger w1
    | otherwise = snd (isolatedMinimum parameters demand periods x1 w1)

isolatedMinimum :: ClarkScarfParameters -> ClarkScarfDemand -> Natural -> Integer -> Natural -> (Rational, Integer)
isolatedMinimum parameters demand periods x1 w1 =
    foldl' chooseMinimum first remaining
  where
    position = x1 + toInteger w1
    upper = max position (clarkScarfIsolatedTargetCap parameters)
    targets = [position .. upper]
    alternatives = fmap (\target -> (isolatedAt target, target)) targets
    first = (isolatedAt position, position)
    remaining = drop 1 alternatives
    alpha = exactDiscountValue (clarkScarfDiscount parameters)
    immediate = clarkScarfExpectedNaturalCost demand (clarkScarfDownstreamHoldingCost parameters) (clarkScarfDownstreamShortageCost parameters) x1
    isolatedAt target =
        clarkScarfTransportUnitCost parameters * fromInteger (target - position)
            + immediate
            + alpha
                * sum
                    [ mass
                        * clarkScarfEquation15Cost
                            parameters
                            demand
                            (periods - 1)
                            (x1 + toInteger w1 - toInteger demandValue)
                            (fromInteger (target - position))
                    | (demandValue, mass) <- NonEmpty.toList (clarkScarfDemandOutcomes demand)
                    ]

-- | Equations (20)--(21): the lower-echelon loss caused by stock limitation.
clarkScarfEquation21OpportunityLoss ::
    ClarkScarfParameters -> ClarkScarfDemand -> Natural -> Integer -> Natural -> Integer -> Either ClarkScarfError Rational
clarkScarfEquation21OpportunityLoss parameters demand periods x1 w1 x2
    | x2 < x1 + toInteger w1 =
        Left
            ( ClarkScarfInvalidState
                (ClarkScarfState periods x1 w1 x2)
            )
    | otherwise = Right (opportunityLossValue parameters demand periods x1 w1 x2)

opportunityLossValue ::
    ClarkScarfParameters -> ClarkScarfDemand -> Natural -> Integer -> Natural -> Integer -> Rational
opportunityLossValue parameters demand periods x1 w1 x2
    | periods == 0 = 0
    | x2 >= unconstrainedTarget = 0
    | otherwise = forcedCost - optimalCost
  where
    position = x1 + toInteger w1
    unconstrainedTarget = clarkScarfEquation15Target parameters demand periods x1 w1
    optimalCost = clarkScarfEquation15Cost parameters demand periods x1 w1
    alpha = exactDiscountValue (clarkScarfDiscount parameters)
    forcedTransit = x2 - position
    forcedCost =
        clarkScarfTransportUnitCost parameters * fromInteger forcedTransit
            + clarkScarfExpectedNaturalCost demand (clarkScarfDownstreamHoldingCost parameters) (clarkScarfDownstreamShortageCost parameters) x1
            + alpha
                * sum
                    [ mass
                        * clarkScarfEquation15Cost
                            parameters
                            demand
                            (periods - 1)
                            (x1 + toInteger w1 - toInteger demandValue)
                            (fromInteger forcedTransit)
                    | (demandValue, mass) <- NonEmpty.toList (clarkScarfDemandOutcomes demand)
                    ]

-- | Equation (26), using equation (21)'s opportunity loss and bounded orders.
clarkScarfEquation26Cost :: ClarkScarfParameters -> ClarkScarfDemand -> Natural -> Integer -> Rational
clarkScarfEquation26Cost parameters demand periods x2
    | periods == 0 = 0
    | otherwise = fst (equation26Minimum parameters demand periods x2)

equation26Minimum :: ClarkScarfParameters -> ClarkScarfDemand -> Natural -> Integer -> (Rational, Natural)
equation26Minimum parameters demand periods x2 = foldl' chooseMinimum first remaining
  where
    first = (costAt 0, 0)
    remaining = fmap (\order -> (costAt order, order)) [1 .. clarkScarfOrderCap parameters]
    alpha = exactDiscountValue (clarkScarfDiscount parameters)
    natural = clarkScarfExpectedNaturalCost demand (clarkScarfUpstreamHoldingCost parameters) (clarkScarfUpstreamShortageCost parameters) x2
    opportunity = opportunityLossValue parameters demand periods x2 0 x2
    costAt order =
        externalOrderCost parameters order
            + natural
            + opportunity
            + alpha
                * sum
                    [ mass * clarkScarfEquation26Cost parameters demand (periods - 1) (x2 + toInteger order - toInteger demandValue)
                    | (demandValue, mass) <- NonEmpty.toList (clarkScarfDemandOutcomes demand)
                    ]

-- Evaluate the policy reconstructed from equations (15) and (26) in the joint model.
decomposedPolicyCosts :: ClarkScarfFixture -> Either ClarkScarfError [(ClarkScarfState, (Rational, Maybe ClarkScarfAction))]
decomposedPolicyCosts fixture = foldM evaluate [] ordered
  where
    parameters = clarkScarfFixtureParameters fixture
    demand = clarkScarfFixtureDemand fixture
    alpha = exactDiscountValue (clarkScarfDiscount parameters)
    ordered = sortOn clarkScarfPeriodsRemaining (NonEmpty.toList (clarkScarfReachableStates fixture))
    evaluate values state
        | clarkScarfPeriodsRemaining state == 0 = Right ((state, (0, Nothing)) : values)
        | otherwise = do
            let periods = clarkScarfPeriodsRemaining state
                isolatedTarget = clarkScarfEquation15Target parameters demand periods (clarkScarfDownstreamNetStock state) (clarkScarfDownstreamInTransit state)
                dispatchTarget = min (clarkScarfEchelonTwoStock state) isolatedTarget
                order = snd (equation26Minimum parameters demand periods (clarkScarfEchelonTwoStock state))
                action = ClarkScarfAction order dispatchTarget
            contributions <-
                traverse
                    ( \(demandValue, mass) -> do
                        (successor, immediate) <- clarkScarfTransition parameters state action demandValue
                        (future, _) <- requireLookup successor values
                        Right (mass * (immediate + alpha * future))
                    )
                    (NonEmpty.toList (clarkScarfDemandOutcomes demand))
            Right ((state, (sum contributions, Just action)) : values)

externalOrderCost :: ClarkScarfParameters -> Natural -> Rational
externalOrderCost parameters order
    | order == 0 = 0
    | otherwise =
        clarkScarfExternalSetupCost parameters
            + clarkScarfExternalUnitCost parameters * fromIntegral order

chooseMinimum :: (Ord cost, Ord choice) => (cost, choice) -> (cost, choice) -> (cost, choice)
chooseMinimum selected candidate
    | candidate < selected = candidate
    | otherwise = selected

requireLookup :: (Eq key) => key -> [(key, value)] -> Either ClarkScarfError value
requireLookup key values = maybe (Left (ClarkScarfModelError "bounded dynamic program missed a predecessor value")) Right (lookup key values)

findCheck :: ClarkScarfState -> [ClarkScarfStateCheck] -> Maybe ClarkScarfStateCheck
findCheck state = go
  where
    go [] = Nothing
    go (check : remaining)
        | clarkScarfCheckedState check == state = Just check
        | otherwise = go remaining

actionAtOrderCap :: ClarkScarfParameters -> Maybe ClarkScarfAction -> Bool
actionAtOrderCap parameters (Just action) =
    clarkScarfOrderCap parameters > 0
        && clarkScarfExternalOrder action == clarkScarfOrderCap parameters
actionAtOrderCap _ Nothing = False

targetAtCap :: ClarkScarfParameters -> ClarkScarfFixture -> ClarkScarfStateCheck -> Bool
targetAtCap parameters fixture check =
    let state = clarkScarfCheckedState check
     in clarkScarfPeriodsRemaining state > 0
            && clarkScarfEquation15Target
                parameters
                (clarkScarfFixtureDemand fixture)
                (clarkScarfPeriodsRemaining state)
                (clarkScarfDownstreamNetStock state)
                (clarkScarfDownstreamInTransit state)
                == clarkScarfIsolatedTargetCap parameters

-- | Exact primary-versus-widened diagnostics; not an unbounded proof.
data ClarkScarfStability = ClarkScarfStability
    { clarkScarfBoundsStable :: !Bool
    , clarkScarfInitialCostChanged :: !Bool
    , clarkScarfWidenedOrderCapSelected :: !Bool
    , clarkScarfWidenedTargetCapSelected :: !Bool
    }
    deriving (Eq, Show)

compareClarkScarfBounds :: ClarkScarfSolution -> ClarkScarfSolution -> Either ClarkScarfError ClarkScarfStability
compareClarkScarfBounds primary widened = do
    let primaryFixture = clarkScarfSolutionFixture primary
        widenedFixture = clarkScarfSolutionFixture widened
        primaryParameters = clarkScarfFixtureParameters primaryFixture
        widenedParameters = clarkScarfFixtureParameters widenedFixture
    if clarkScarfInitialState primaryFixture /= clarkScarfInitialState widenedFixture
        then Left (ClarkScarfComparisonMismatch "initial state")
        else Right ()
    if clarkScarfFixtureDemand primaryFixture /= clarkScarfFixtureDemand widenedFixture
        then Left (ClarkScarfComparisonMismatch "demand law")
        else Right ()
    if sourceCoefficients primaryParameters /= sourceCoefficients widenedParameters
        then Left (ClarkScarfComparisonMismatch "source coefficients")
        else Right ()
    if clarkScarfOrderCap widenedParameters <= clarkScarfOrderCap primaryParameters
        || clarkScarfIsolatedTargetCap widenedParameters <= clarkScarfIsolatedTargetCap primaryParameters
        then Left ClarkScarfBoundsNotWidened
        else
            let changed = clarkScarfInitialCost primary /= clarkScarfInitialCost widened
                orderSelected = clarkScarfOrderCapSelected widened
                targetSelected = clarkScarfTargetCapSelected widened
             in Right
                    ClarkScarfStability
                        { clarkScarfBoundsStable = not changed && not orderSelected && not targetSelected
                        , clarkScarfInitialCostChanged = changed
                        , clarkScarfWidenedOrderCapSelected = orderSelected
                        , clarkScarfWidenedTargetCapSelected = targetSelected
                        }

boundedTreeSize :: Natural -> Natural -> Natural
boundedTreeSize depth branch = sum [branch ^ power | power <- [0 .. depth]]

sourceCoefficients :: ClarkScarfParameters -> (Natural, Rational, Rational, Rational, Rational, Rational, Rational, Rational, Rational)
sourceCoefficients parameters =
    ( clarkScarfHorizon parameters
    , exactDiscountValue (clarkScarfDiscount parameters)
    , clarkScarfExternalSetupCost parameters
    , clarkScarfExternalUnitCost parameters
    , clarkScarfTransportUnitCost parameters
    , clarkScarfDownstreamHoldingCost parameters
    , clarkScarfDownstreamShortageCost parameters
    , clarkScarfUpstreamHoldingCost parameters
    , clarkScarfUpstreamShortageCost parameters
    )
