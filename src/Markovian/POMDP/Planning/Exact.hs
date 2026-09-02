-- | Exact finite-horizon planning over exact finite beliefs.
module Markovian.POMDP.Planning.Exact (
    ExactBeliefPolicy,
    exactBeliefPolicy,
    exactBeliefPolicyActions,
    ExactBeliefPlanningError (..),
    expectedExactBeliefReturn,
    expectedExactBeliefReturnFrom,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Action (ActionId)
import Markovian.Horizon (horizonValue)
import Markovian.Kernel.Exact (ExactKernel, ExactKernelError, runExactKernel)
import Markovian.MDP.Exact (
    ExactDecision (..),
    ExactModelError,
    ExactStateStatus (..),
    exactMDPStateStatus,
    exactSuccessorState,
    exactTransitionReward,
    inspectExactMDP,
    stepExactMDP,
 )
import Markovian.Objective.Exact (
    ExactFiniteObjective,
    exactDiscountValue,
    exactObjectiveDiscount,
    exactObjectiveHorizon,
 )
import Markovian.POMDP.Exact (
    ExactBelief,
    ExactFilteringError,
    ExactPOMDP,
    conditionExactBelief,
    exactBeliefOutcomes,
    exactInitialBelief,
    exactObservationDistribution,
    exactPOMDPModel,
    predictExactBelief,
 )
import Markovian.Policy.Exact (ExactPolicyError (..), validateExactPolicySupport)
import Markovian.Probability.Exact (
    ExactFiniteDist,
    exactOutcomes,
    exactProbability,
 )
import Markovian.Reward.Exact (ExactReward, exactReward, exactRewardValue)

-- | A policy kernel over exact finite beliefs.
newtype ExactBeliefPolicy state action = ExactBeliefPolicy (ExactKernel (ExactBelief state) (ActionId action))

-- | Construct an exact belief policy.
exactBeliefPolicy ::
    ExactKernel (ExactBelief state) (ActionId action) ->
    ExactBeliefPolicy state action
exactBeliefPolicy = ExactBeliefPolicy

-- | Read one belief's exact action distribution.
exactBeliefPolicyActions ::
    ExactBeliefPolicy state action ->
    ExactBelief state ->
    Either ExactKernelError (ExactFiniteDist (ActionId action))
exactBeliefPolicyActions (ExactBeliefPolicy selected) = runExactKernel selected

-- | Exact finite-horizon belief-planning failures.
data ExactBeliefPlanningError state action observation
    = ExactBeliefPlanningModelError !state !(ExactModelError action)
    | ExactBeliefPlanningPolicyError !(ExactPolicyError action)
    | ExactBeliefPlanningFilteringError !(ExactFilteringError state action observation)
    | MixedBeliefTermination
    | NoCommonBeliefAction
    deriving (Eq, Show)

-- | Evaluate from a POMDP's initial exact belief.
expectedExactBeliefReturn ::
    (Eq state, Eq action, Eq observation) =>
    ExactFiniteObjective ->
    ExactPOMDP state action observation ->
    ExactBeliefPolicy state action ->
    Either (ExactBeliefPlanningError state action observation) ExactReward
expectedExactBeliefReturn objective pomdp selectedPolicy =
    expectedExactBeliefReturnFrom objective pomdp selectedPolicy (exactInitialBelief pomdp)

{- | Evaluate an exact belief policy by bounded belief-state recursion.

An action must be available in every continuing latent state with positive
belief mass. A belief mixing terminal and continuing states is rejected.
-}
expectedExactBeliefReturnFrom ::
    (Eq state, Eq action, Eq observation) =>
    ExactFiniteObjective ->
    ExactPOMDP state action observation ->
    ExactBeliefPolicy state action ->
    ExactBelief state ->
    Either (ExactBeliefPlanningError state action observation) ExactReward
expectedExactBeliefReturnFrom objective pomdp selectedPolicy initial =
    exactReward <$> go (horizonValue (exactObjectiveHorizon objective)) initial
  where
    model = exactPOMDPModel pomdp
    discount = exactDiscountValue (exactObjectiveDiscount objective)

    go remaining belief =
        case classifyTermination belief of
            BeliefTerminal payoff -> Right payoff
            BeliefMixed -> Left MixedBeliefTermination
            BeliefContinuing
                | remaining == 0 -> Right 0
                | otherwise -> do
                    available <- commonAvailable belief
                    selected <-
                        mapPolicyError
                            (either (Left . ExactPolicyKernelError) Right (exactBeliefPolicyActions selectedPolicy belief))
                    mapPolicyError (validateExactPolicySupport available selected)
                    contributions <-
                        traverse
                            (actionContribution (remaining - 1) belief)
                            (NonEmpty.toList (exactOutcomes selected))
                    Right (sum contributions)

    actionContribution remaining belief (selectedAction, actionMass) = do
        predicted <- mapFilteringError (predictExactBelief pomdp selectedAction belief)
        branches <- primitiveBranches belief selectedAction
        values <-
            traverse
                (branchContribution remaining predicted selectedAction)
                branches
        Right (exactProbability actionMass * sum values)

    branchContribution remaining predicted selectedAction (reward, observed, branchMass) = do
        posterior <-
            mapFilteringError
                (conditionExactBelief pomdp selectedAction observed predicted)
        future <- go remaining posterior
        Right
            ( branchMass
                * (exactRewardValue reward + discount * future)
            )

    primitiveBranches belief selectedAction =
        fmap
            concat
            ( traverse
                stateBranches
                (NonEmpty.toList (exactBeliefOutcomes belief))
            )
      where
        stateBranches (state, beliefMass) = do
            transition <- mapModelError state (stepExactMDP model state selectedAction)
            Right
                [ ( exactTransitionReward outcome
                  , observed
                  , exactProbability beliefMass
                        * exactProbability transitionMass
                        * exactProbability observationMass
                  )
                | (outcome, transitionMass) <- NonEmpty.toList (exactOutcomes transition)
                , (observed, observationMass) <-
                    NonEmpty.toList
                        ( exactOutcomes
                            ( exactObservationDistribution
                                pomdp
                                selectedAction
                                (exactSuccessorState outcome)
                            )
                        )
                ]

    classifyTermination belief =
        let statuses =
                [ (mass, exactMDPStateStatus model state)
                | (state, mass) <- NonEmpty.toList (exactBeliefOutcomes belief)
                ]
            terminal = [(mass, payoff) | (mass, ExactTerminal payoff) <- statuses]
            continuing = [() | (_, ExactContinuing) <- statuses]
         in case (terminal, continuing) of
                ([], _ : _) -> BeliefContinuing
                (_ : _, []) ->
                    BeliefTerminal
                        ( sum
                            [ exactProbability mass * exactRewardValue payoff
                            | (mass, payoff) <- terminal
                            ]
                        )
                _ -> BeliefMixed

    commonAvailable belief = do
        availableByState <-
            traverse
                inspectAvailable
                (NonEmpty.toList (exactBeliefOutcomes belief))
        case availableByState of
            [] -> Left NoCommonBeliefAction
            firstAvailable : remainingAvailable ->
                case commonActions firstAvailable remainingAvailable of
                    [] -> Left NoCommonBeliefAction
                    first : rest -> Right (first :| rest)

    inspectAvailable (state, _) = do
        decision <- mapModelError state (inspectExactMDP model state)
        case decision of
            ExactTerminalDecision _ -> Left MixedBeliefTermination
            ExactActionDecision available -> Right available

commonActions :: (Eq action) => NonEmpty (ActionId action) -> [NonEmpty (ActionId action)] -> [ActionId action]
commonActions first = foldl intersect (NonEmpty.toList first)
  where
    intersect current available = filter (`elem` available) current

data BeliefTermination
    = BeliefTerminal !Rational
    | BeliefContinuing
    | BeliefMixed

mapModelError ::
    state ->
    Either (ExactModelError action) value ->
    Either (ExactBeliefPlanningError state action observation) value
mapModelError state = either (Left . ExactBeliefPlanningModelError state) Right

mapPolicyError ::
    Either (ExactPolicyError action) value ->
    Either (ExactBeliefPlanningError state action observation) value
mapPolicyError = either (Left . ExactBeliefPlanningPolicyError) Right

mapFilteringError ::
    Either (ExactFilteringError state action observation) value ->
    Either (ExactBeliefPlanningError state action observation) value
mapFilteringError = either (Left . ExactBeliefPlanningFilteringError) Right
