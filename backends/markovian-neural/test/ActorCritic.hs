module ActorCritic (tests) where

import Markovian.Backend.Neural (
    ActionMask,
    ActorCriticSuccessor (..),
    ActorCriticTransition (..),
    ActorCriticUpdate (..),
    linearPolicyParameters,
    linearPolicySelectedLogProbability,
    linearValueParameters,
    mkActionMask,
    mkActorCriticConfig,
    mkLinearCategoricalPolicy,
    mkLinearValueFunction,
    updateActorCritic,
 )
import TestSupport (
    assert,
    assertClose,
    assertVectorClose,
    assertVectorCloseWith,
    centralDifference,
    requireRight,
 )

tests :: IO ()
tests = do
    terminalWorkedUpdate
    continuingTargetAndDetachment
    finiteDifferenceGradients
    failureChecks
    putStrLn "PASS: actor-critic reference updates"

terminalWorkedUpdate :: IO ()
terminalWorkedUpdate = do
    config <- requireRight "terminal actor-critic config" (mkActorCriticConfig 0.5 0.1 0.2)
    policy <- requireRight "terminal actor policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    valueFunction <- requireRight "terminal critic" (mkLinearValueFunction 1 [2])
    mask <- requireRight "terminal actor mask" (mkActionMask 2 [0, 1])
    let transition = ActorCriticTransition [3] mask 0 1 (ActorCriticTerminal 4)
    update <- requireRight "terminal actor-critic update" (updateActorCritic config policy valueFunction transition)
    assertClose "terminal target" 1e-14 3 (actorCriticTarget update)
    assertClose "current value" 1e-14 6 (actorCriticValue update)
    assertClose "terminal TD error" 1e-14 (-3) (actorCriticTDError update)
    assertVectorClose "terminal actor gradient" 1e-14 [-4.5, 4.5] (actorCriticActorGradient update)
    assertVectorClose "terminal critic gradient" 1e-14 [-9] (actorCriticCriticGradient update)
    assertVectorClose "terminal actor update" 1e-14 [-0.45, 0.45] (linearPolicyParameters (actorCriticUpdatedPolicy update))
    assertVectorClose "terminal critic update" 1e-14 [0.2] (linearValueParameters (actorCriticUpdatedValueFunction update))

continuingTargetAndDetachment :: IO ()
continuingTargetAndDetachment = do
    config <- requireRight "continuing config" (mkActorCriticConfig 0.5 0 0)
    policy <- requireRight "continuing policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    valueFunction <- requireRight "continuing critic" (mkLinearValueFunction 1 [2])
    mask <- requireRight "continuing actor mask" (mkActionMask 2 [0, 1])
    let transition = ActorCriticTransition [3] mask 1 1 (ActorCriticContinuing [4])
    update <- requireRight "continuing update" (updateActorCritic config policy valueFunction transition)
    assertClose "continuing detached target" 1e-14 5 (actorCriticTarget update)
    assertClose "continuing TD error" 1e-14 (-1) (actorCriticTDError update)
    assertVectorClose "continuing critic gradient" 1e-14 [-3] (actorCriticCriticGradient update)

finiteDifferenceGradients :: IO ()
finiteDifferenceGradients = do
    config <- requireRight "finite-difference actor-critic config" (mkActorCriticConfig 0.7 0 0)
    mask <- requireRight "finite-difference actor mask" (mkActionMask 2 [0, 1])
    let policyParameters = [0.2, -0.1, -0.4, 0.3]
        valueParameters = [0.5, -0.2]
        features = [1.2, -0.7]
        transition = ActorCriticTransition features mask 1 0.4 (ActorCriticContinuing [-0.3, 0.8])
    policy <- requireRight "finite-difference actor" (mkLinearCategoricalPolicy 2 2 policyParameters)
    valueFunction <- requireRight "finite-difference critic" (mkLinearValueFunction 2 valueParameters)
    update <- requireRight "finite-difference actor-critic update" (updateActorCritic config policy valueFunction transition)
    let detachedDelta = actorCriticTDError update
        detachedTarget = actorCriticTarget update
    numericalActor <-
        traverse
            ( \index ->
                centralDifference
                    1e-6
                    (\point -> actorObjective (replace index point policyParameters) features mask detachedDelta)
                    (policyParameters !! index)
            )
            [0 .. length policyParameters - 1]
    assertVectorCloseWith "actor-critic actor finite difference" 2e-10 4e-8 (actorCriticActorGradient update) numericalActor
    numericalCritic <-
        traverse
            ( \index ->
                centralDifference
                    1e-6
                    (\point -> pure (criticObjective (replace index point valueParameters) features detachedTarget))
                    (valueParameters !! index)
            )
            [0 .. length valueParameters - 1]
    assertVectorCloseWith "actor-critic critic finite difference" 2e-10 4e-8 (actorCriticCriticGradient update) numericalCritic

failureChecks :: IO ()
failureChecks = do
    config <- requireRight "failure config" (mkActorCriticConfig 0.9 0.1 0.1)
    policy <- requireRight "failure policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    valueFunction <- requireRight "failure critic" (mkLinearValueFunction 1 [0])
    fullMask <- requireRight "failure actor mask" (mkActionMask 2 [0, 1])
    onlyFirst <- requireRight "restricted actor mask" (mkActionMask 2 [0])
    case updateActorCritic config policy valueFunction (ActorCriticTransition [1] fullMask 2 0 (ActorCriticTerminal 0)) of
        Left _ -> assertVectorClose "failed actor update changed source" 0 [0, 0] (linearPolicyParameters policy)
        Right _ -> assert "out-of-range actor action succeeded" False
    case updateActorCritic config policy valueFunction (ActorCriticTransition [1] onlyFirst 1 0 (ActorCriticTerminal 0)) of
        Left _ -> pure ()
        Right _ -> assert "masked actor action succeeded" False
    case updateActorCritic config policy valueFunction (ActorCriticTransition [1] onlyFirst 0 (0 / 0) (ActorCriticTerminal 0)) of
        Left _ -> pure ()
        Right _ -> assert "non-finite actor-critic reward succeeded" False
    case mkActorCriticConfig 1.1 0.1 0.1 of
        Left _ -> pure ()
        Right _ -> assert "invalid actor-critic discount succeeded" False
    overflowConfig <- requireRight "actor-critic overflow config" (mkActorCriticConfig 1 0 0)
    let largest = 1.7976931348623157e308
    case updateActorCritic overflowConfig policy valueFunction (ActorCriticTransition [1] onlyFirst 0 largest (ActorCriticTerminal largest)) of
        Left _ -> pure ()
        Right _ -> assert "overflowing actor-critic target succeeded" False

actorObjective :: [Double] -> [Double] -> ActionMask -> Double -> IO Double
actorObjective parameters features mask detachedDelta = do
    policy <- requireRight "actor objective policy" (mkLinearCategoricalPolicy 2 2 parameters)
    logProbability <- requireRight "actor objective log probability" (linearPolicySelectedLogProbability policy features mask 1)
    pure (detachedDelta * logProbability)

criticObjective :: [Double] -> [Double] -> Double -> Double
criticObjective parameters features detachedTarget =
    let value = sum (zipWith (*) parameters features)
        difference = detachedTarget - value
     in -(0.5 * difference * difference)

replace :: Int -> Double -> [Double] -> [Double]
replace selected replacement =
    zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]
