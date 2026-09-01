module Reinforce (tests) where

import Markovian.Backend.Neural (
    EpisodeBoundary (..),
    ReinforceError (..),
    ReinforceStep (..),
    ReinforceUpdate (..),
    evaluateLinearValue,
    linearPolicyParameters,
    linearPolicySelectedLogProbability,
    linearValueParameters,
    mkActionMask,
    mkLinearCategoricalPolicy,
    mkLinearValueFunction,
    mkReinforceConfig,
    reinforceReturnToGo,
    updateReinforce,
 )
import TestSupport (
    assert,
    assertVectorClose,
    assertVectorCloseWith,
    centralDifference,
    requireRight,
 )

tests :: IO ()
tests = do
    workedUpdate
    finiteDifferenceGradients
    boundaryAndFailureChecks
    putStrLn "PASS: REINFORCE reference updates"

workedUpdate :: IO ()
workedUpdate = do
    config <- requireRight "REINFORCE config" (mkReinforceConfig 2 0.5 0.1 0.2)
    policy <- requireRight "REINFORCE policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    baseline <- requireRight "REINFORCE baseline" (mkLinearValueFunction 1 [0.5])
    mask <- requireRight "REINFORCE mask" (mkActionMask 2 [0, 1])
    let steps =
            [ ReinforceStep [1] mask 0 1
            , ReinforceStep [2] mask 1 2
            ]
    update <- requireRight "REINFORCE update" (updateReinforce config policy (Just baseline) steps (TerminalBoundary 4))
    assertVectorClose "return-to-go" 1e-14 [3, 4] (reinforceReturns update)
    assertVectorClose "detached advantages" 1e-14 [2.5, 3] (reinforceAdvantages update)
    assertVectorClose "discounted actor gradient" 1e-14 [-0.25, 0.25] (reinforceActorGradient update)
    case reinforceBaselineGradient update of
        Just gradient -> assertVectorClose "baseline gradient" 1e-14 [8.5] gradient
        Nothing -> assert "baseline gradient missing" False
    assertVectorClose
        "actor update"
        1e-14
        [-0.025, 0.025]
        (linearPolicyParameters (reinforceUpdatedPolicy update))
    case reinforceUpdatedBaseline update of
        Just updated -> assertVectorClose "baseline update" 1e-14 [2.2] (linearValueParameters updated)
        Nothing -> assert "updated baseline missing" False

finiteDifferenceGradients :: IO ()
finiteDifferenceGradients = do
    config <- requireRight "finite-difference config" (mkReinforceConfig 2 0.9 0 0)
    mask <- requireRight "finite-difference mask" (mkActionMask 2 [0, 1])
    let parameters = [0.2, -0.1, -0.3, 0.4]
        baselineParameters = [0.25, -0.5]
        steps =
            [ ReinforceStep [1, 0.5] mask 0 0.7
            , ReinforceStep [-0.25, 1] mask 1 (-0.2)
            ]
        boundary = TruncatedBoundary 0.6
    policy <- requireRight "finite-difference policy" (mkLinearCategoricalPolicy 2 2 parameters)
    baseline <- requireRight "finite-difference baseline" (mkLinearValueFunction 2 baselineParameters)
    update <- requireRight "finite-difference update" (updateReinforce config policy (Just baseline) steps boundary)
    let returns = reinforceReturns update
        advantages = reinforceAdvantages update
    numericalActor <-
        traverse
            ( \index ->
                centralDifference
                    1e-6
                    (\point -> actorObjective (replace index point parameters) 0.9 steps returns)
                    (parameters !! index)
            )
            [0 .. length parameters - 1]
    assertVectorCloseWith "REINFORCE actor finite difference" 2e-10 4e-8 (reinforceActorGradient update) numericalActor
    numericalBaseline <-
        traverse
            ( \index ->
                centralDifference
                    1e-6
                    (\point -> baselineObjective (replace index point baselineParameters) steps returns)
                    (baselineParameters !! index)
            )
            [0 .. length baselineParameters - 1]
    case reinforceBaselineGradient update of
        Just analytic -> assertVectorCloseWith "REINFORCE baseline finite difference" 2e-10 4e-8 analytic numericalBaseline
        Nothing -> assert "finite-difference baseline gradient missing" False
    fastBaselineConfig <- requireRight "detachment config" (mkReinforceConfig 2 0.9 0 100)
    fastBaselineUpdate <- requireRight "detachment update" (updateReinforce fastBaselineConfig policy (Just baseline) steps boundary)
    assert
        "simultaneous baseline update changed actor gradient"
        (reinforceActorGradient fastBaselineUpdate == reinforceActorGradient update)
    assert "advantage count changed" (length advantages == length steps)

boundaryAndFailureChecks :: IO ()
boundaryAndFailureChecks = do
    config <- requireRight "boundary config" (mkReinforceConfig 1 0.5 0.1 0.1)
    policy <- requireRight "boundary policy" (mkLinearCategoricalPolicy 2 1 [0, 0])
    mask <- requireRight "boundary mask" (mkActionMask 2 [0, 1])
    truncatedReturn <-
        requireRight "truncated return" (reinforceReturnToGo config [1] (TruncatedBoundary 4))
    assertVectorClose "truncated bootstrap return" 1e-14 [3] truncatedReturn
    case reinforceReturnToGo config [1] TruncatedBoundaryWithoutBootstrap of
        Left MissingTruncatedBootstrap -> pure ()
        result -> assert ("missing bootstrap was accepted: " ++ show result) False
    case reinforceReturnToGo config [1, 2] (TerminalBoundary 0) of
        Left (ReinforceStepLimitExceeded 1 2) -> pure ()
        result -> assert ("step limit was not enforced: " ++ show result) False
    case updateReinforce config policy Nothing [ReinforceStep [1] mask 2 0] (TerminalBoundary 0) of
        Left _ -> assertVectorClose "failed update changed source policy" 0 [0, 0] (linearPolicyParameters policy)
        Right _ -> assert "invalid action update succeeded" False
    case reinforceReturnToGo config [1 / 0] (TerminalBoundary 0) of
        Left _ -> pure ()
        Right _ -> assert "non-finite reward was accepted" False
    overflowConfig <- requireRight "overflow config" (mkReinforceConfig 1 1 0 0)
    case reinforceReturnToGo overflowConfig [1.7976931348623157e308] (TerminalBoundary 1.7976931348623157e308) of
        Left _ -> pure ()
        Right _ -> assert "overflowing return was accepted" False

actorObjective :: [Double] -> Double -> [ReinforceStep] -> [Double] -> IO Double
actorObjective parameters discount steps returns = do
    policy <- requireRight "actor objective policy" (mkLinearCategoricalPolicy 2 2 parameters)
    -- The advantages are fixed from the pre-update baseline snapshot.
    baseline <- requireRight "actor objective baseline" (mkLinearValueFunction 2 [0.25, -0.5])
    terms <-
        sequence
            [ do
                baselineValue <- requireRight "actor objective baseline value" (evaluateLinearValue baseline (reinforceFeatures step))
                logProbability <-
                    requireRight
                        "actor objective log probability"
                        ( linearPolicySelectedLogProbability
                            policy
                            (reinforceFeatures step)
                            (reinforceActionMask step)
                            (reinforceAction step)
                        )
                pure (discountPower * (returnValue - baselineValue) * logProbability)
            | (discountPower, step, returnValue) <- zip3 (iterate (* discount) 1) steps returns
            ]
    pure (sum terms)

baselineObjective :: [Double] -> [ReinforceStep] -> [Double] -> IO Double
baselineObjective parameters steps returns = do
    baseline <- requireRight "baseline objective value function" (mkLinearValueFunction 2 parameters)
    terms <-
        sequence
            [ do
                value <- requireRight "baseline objective value" (evaluateLinearValue baseline (reinforceFeatures step))
                pure (-(0.5 * (returnValue - value) * (returnValue - value)))
            | (step, returnValue) <- zip steps returns
            ]
    pure (sum terms)

replace :: Int -> Double -> [Double] -> [Double]
replace selected replacement =
    zipWith (\index value -> if index == selected then replacement else value) [0 :: Int ..]
