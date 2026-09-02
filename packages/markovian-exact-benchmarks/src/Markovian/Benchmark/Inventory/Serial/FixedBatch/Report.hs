-- | Deterministic report for bounded execution and separate stationary evidence.
module Markovian.Benchmark.Inventory.Serial.FixedBatch.Report (
    FixedBatchReport,
    fixedBatchReport,
    renderFixedBatchReport,
    fixedBatchBenchmarkWarmupCount,
    fixedBatchBenchmarkSampleCount,
) where

import Data.List (intercalate, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact
import Markovian.Category.Finite.Set (finiteSetCardinality)
import Numeric.Natural (Natural)

-- | Return the fixed batch benchmark warmup count.
fixedBatchBenchmarkWarmupCount :: Natural
fixedBatchBenchmarkWarmupCount = 1

-- | Return the fixed batch benchmark sample count.
fixedBatchBenchmarkSampleCount :: Natural
fixedBatchBenchmarkSampleCount = 20

-- | Deterministic fixed batch report.
newtype FixedBatchReport = FixedBatchReport [String]
    deriving (Eq, Show)

-- | Build a report only after checked action/grid and demand-cap comparisons.
fixedBatchReport :: FixedBatchSolution -> FixedBatchSolution -> FixedBatchSolution -> NewsvendorSolution -> NewsvendorSolution -> FixedBatchSolution -> NewsvendorSolution -> Either FixedBatchError FixedBatchReport
fixedBatchReport primary widened demandWidened stationary demandStationary separationFinite separationStationary = do
    stability <- compareFixedBatchBounds primary widened
    demandDiagnostic <- compareFixedBatchDemandCaps primary demandWidened
    validateStationaryProvenance "primary" primary stationary
    validateStationaryProvenance "demand-widened" demandWidened demandStationary
    validateStationaryProvenance "separation" separationFinite separationStationary
    if fixedBatchSelectedLevels separationFinite /= ReorderLevels 0 (-4)
        || newsvendorSelectedLevels separationStationary /= ReorderLevels 0 1
        then Left (FixedBatchModelMismatch "checked finite/stationary separation fixture")
        else Right ()
    let fixture = fixedBatchSolutionFixture primary
        parameters = fixedBatchFixtureParameters fixture
        demand = fixedBatchFixtureDemand fixture
        widenedParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture widened)
        demandWidenedParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture demandWidened)
        separationParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture separationFinite)
    Right
        ( FixedBatchReport
            [ "Fixed-batch (R,nQ) inventory evidence: bounded execution and stationary calculations."
            , "primary source: M. K. Doğru, G. J. van Houtum, and A. G. de Kok, Newsboy Characterizations for the Optimal Reorder Levels of Multi-Echelon Inventory Systems with Fixed Batch Sizes, BETA Working Paper 134, 22 February 2005"
            , "journal source: Doğru, van Houtum, and de Kok, Operations Research Letters 36(5), 2008, 551-556, DOI 10.1016/j.orl.2008.06.003"
            , "supporting sources: Chen, Management Science 44(12), 1998, S221-S234, DOI 10.1287/mnsc.44.12.S221; Chen, Operations Research 48(3), 2000, 376-389, DOI 10.1287/opre.48.3.376.12427; Puterman, Markov Decision Processes, 1994, Chapter 4"
            , "source crosswalk: model and event order section 2 pages 4-5; stationary cost equations (3) and (9) pages 7 and 10; shortfall equations (10)-(12) pages 10-11; forward differences equations (13)-(14) pages 11-12; discrete weak/strict inequalities (17)-(21) pages 13-14; Q=1 relation S=R+1 page 15; continuous equality (22) page 16"
            , "source assumptions: serial N-stage periodic review, integer-ratio batches, iid nonnegative discrete demand with positive mean and Pr(D=1)>0, deterministic lead times, complete backlog, positive echelon holding and backlog costs, centralized infinite-horizon average-cost control"
            , "implemented execution fragment: two stages, L1=0, configurable positive L2, exact finite horizon, conditioned finite geometric demand"
            , "scope boundary: Chen's infinite-horizon echelon-stock (R,nQ) optimality result is not transferred to the finite-horizon oracle"
            , "scope boundary: no generic N-stage, unbounded-demand accuracy, average-reward convergence, continuous-demand equality, or unbounded minimizer claim"
            , "repository-authored fixture: parameters and numeric results below are not a published table"
            , "event timing: choose physical release q1 and external order q2; receive the known due stage-2 batch; release q1; observe one demand; assess end-of-period cost paired with the same successor"
            , "action constraints: q1 is a Q1 multiple with 0<=q1<=x2+a0; q2 is a Q2 multiple with at most externalBatchCap batches; Q2=n*Q1"
            , "positions: IP1=x1; IP2=x1+x2+sum(pipeline); order(y,R,Q)=0 for y>R and Q*(1+(R-y) div Q) otherwise"
            , "successor: x2'=x2+a0-q1; x1'=x1+q1-D; pipeline'=tail(pipeline)++[q2]"
            , "cost: h2*x2'+(h1+h2)*max(x1',0)+p*max(-x1',0)"
            , "terminal convention: no action and zero payoff after the final transition; pipeline has no terminal salvage"
            , "boundary behavior: complete reachable closure, no state clamping, no redirected successor"
            , "primary parameters: " ++ renderParameters parameters
            , "widened action/grid parameters: " ++ renderParameters widenedParameters
            , "demand-widened parameters: " ++ renderParameters demandWidenedParameters
            , "initial state: " ++ show (fixedBatchInitialState fixture)
            , "conditioned demand: " ++ renderDemand demand
            , "retained one-period mass: " ++ rational (fixedBatchRetainedMass demand)
            , "omitted one-period mass: " ++ rational (fixedBatchOmittedMass demand)
            , "path exceedance probability: " ++ rational (fixedBatchPathExceedanceMass (fixedBatchHorizon parameters) demand)
            , "truncation boundary: omitted mass and widened-demand deltas are not value-error bounds"
            , "primary state layout cardinality: " ++ show (length (fixedBatchReachableStates fixture))
            , "primary action layout cardinality: " ++ show (finiteSetCardinality (fixedBatchActionLayout fixture))
            , "primary state-action count: " ++ show (fixedBatchStateActionCount fixture)
            , "primary reorder grid: " ++ show (NonEmpty.toList (reorderGridCandidates (fixedBatchSolutionGrid primary)))
            , "finite-horizon oracle return: " ++ rational (fixedBatchOracleReturn primary)
            , "finite-horizon selected constant policy: " ++ show (fixedBatchSelectedLevels primary)
            , "finite-horizon selected-policy return: " ++ rational (fixedBatchPolicyReturn primary)
            , "finite-horizon policy regret: " ++ rational (fixedBatchPolicyRegret primary)
            , "oracle external cap selected: " ++ yesNo (fixedBatchOracleExternalCapSelected primary)
            , "policy external cap clipped: " ++ yesNo (fixedBatchPolicyExternalCapClipped primary)
            , "policy physical release clipped: " ++ yesNo (fixedBatchPolicyReleaseClipped primary)
            , "primary grid boundary selected: " ++ yesNo (fixedBatchGridBinds primary)
            , "checked finite-horizon solver work: " ++ show (fixedBatchCheckedSolverWork primary)
            , "action/grid widening stable and nonbinding: " ++ yesNo (fixedBatchBoundsStable stability)
            , "widening detail: oracleChanged=" ++ yesNo (fixedBatchOracleReturnChanged stability) ++ ", policyChanged=" ++ yesNo (fixedBatchPolicyReturnChanged stability) ++ ", levelsChanged=" ++ yesNo (fixedBatchSelectedLevelsChanged stability) ++ ", widenedCapSelected=" ++ yesNo (fixedBatchWidenedExternalCapSelected stability) ++ ", widenedGridBinds=" ++ yesNo (fixedBatchWidenedGridBinds stability)
            , "widening boundary: finite diagnostics are not proof of an unbounded optimum"
            , "demand-cap oracle-return delta: " ++ rational (fixedBatchDemandOracleDelta demandDiagnostic)
            , "demand-cap policy-return delta: " ++ rational (fixedBatchDemandPolicyDelta demandDiagnostic)
            , "demand-cap regret delta: " ++ rational (fixedBatchDemandRegretDelta demandDiagnostic)
            , "demand-cap selected levels changed: " ++ yesNo (fixedBatchDemandSelectedLevelsChanged demandDiagnostic)
            , "demand-cap omitted mass change: " ++ rational (fixedBatchDemandPrimaryOmittedMass demandDiagnostic) ++ " -> " ++ rational (fixedBatchDemandWidenedOmittedMass demandDiagnostic)
            , "--- separate stationary newsvendor section ---"
            , "stationary scope: exact finite sums for equations (9)-(14) and discrete inequalities (17)-(21), searched only on the reported Cartesian domain"
            , "stationary Cartesian domain: " ++ renderDomain (newsvendorSolutionDomain stationary)
            , "stationary selected levels: " ++ show (newsvendorSelectedLevels stationary)
            , "stationary subsystem C1: " ++ rational (newsvendorSubsystem1Cost stationary)
            , "stationary subsystem C2: " ++ rational (newsvendorSubsystem2Cost stationary)
            , "stationary forward differences: c1=" ++ rational (newsvendorSubsystem1Difference stationary) ++ ", c2=" ++ rational (newsvendorSubsystem2Difference stationary)
            , "stationary subsystem-1 inequality: " ++ renderInequality (newsvendorSubsystem1Inequality stationary)
            , "stationary subsystem-2 inequality: " ++ renderInequality (newsvendorSubsystem2Inequality stationary)
            , "stationary weak/strict witnesses: R1=" ++ show (newsvendorWeakLowerR1 stationary, newsvendorStrictUpperR1 stationary) ++ ", R2=" ++ show (newsvendorWeakLowerR2 stationary, newsvendorStrictUpperR2 stationary)
            , "stationary domain boundary witness: " ++ yesNo (newsvendorDomainBinds stationary)
            , "stationary checked generated finite-law terms: " ++ show (newsvendorCheckedTerms stationary)
            , "demand-widened stationary Cartesian domain: " ++ renderDomain (newsvendorSolutionDomain demandStationary)
            , "demand-widened stationary selected levels: " ++ show (newsvendorSelectedLevels demandStationary)
            , "demand-widened stationary C2: " ++ rational (newsvendorSubsystem2Cost demandStationary)
            , "demand-cap stationary C2 delta: " ++ rational (newsvendorSubsystem2Cost demandStationary - newsvendorSubsystem2Cost stationary)
            , "demand-widened stationary subsystem-1 inequality: " ++ renderInequality (newsvendorSubsystem1Inequality demandStationary)
            , "demand-widened stationary subsystem-2 inequality: " ++ renderInequality (newsvendorSubsystem2Inequality demandStationary)
            , "demand-widened stationary weak/strict witnesses: R1=" ++ show (newsvendorWeakLowerR1 demandStationary, newsvendorStrictUpperR1 demandStationary) ++ ", R2=" ++ show (newsvendorWeakLowerR2 demandStationary, newsvendorStrictUpperR2 demandStationary)
            , "--- checked finite/stationary separation witness ---"
            , "separation parameters: " ++ renderParameters separationParameters
            , "separation finite-policy grid axes: " ++ renderGridAxes (fixedBatchSolutionGrid separationFinite)
            , "separation stationary Cartesian domain: " ++ renderDomain (newsvendorSolutionDomain separationStationary)
            , "separation witness: finite-horizon selected levels " ++ show (fixedBatchSelectedLevels separationFinite) ++ "; stationary selected levels " ++ show (newsvendorSelectedLevels separationStationary)
            , "stationary boundary: Theorem 1 concerns the source stationary model; this finite conditioned-law calculation does not establish the continuous equality in Theorem 2"
            ]
        )

-- | Render the fixed batch report.
renderFixedBatchReport :: FixedBatchReport -> String
renderFixedBatchReport (FixedBatchReport reportLines) = unlines reportLines

validateStationaryProvenance :: String -> FixedBatchSolution -> NewsvendorSolution -> Either FixedBatchError ()
validateStationaryProvenance label execution stationary = do
    let executionParameters = fixedBatchFixtureParameters (fixedBatchSolutionFixture execution)
        stationaryParameters = newsvendorSolutionParameters stationary
        expectedR1 = uniqueSorted (fmap fixedBatchR1 candidates)
        expectedR2 = uniqueSorted (fmap fixedBatchR2 candidates)
        domain = newsvendorSolutionDomain stationary
    if stationaryParameters /= executionParameters
        then Left (FixedBatchModelMismatch (label ++ " stationary parameters do not match execution provenance"))
        else Right ()
    if NonEmpty.toList (newsvendorR1Layout domain) /= expectedR1
        || NonEmpty.toList (newsvendorR2Layout domain) /= expectedR2
        then Left (FixedBatchModelMismatch (label ++ " stationary search domain does not match execution grid axes"))
        else Right ()
    if newsvendorDomainContains domain (newsvendorSelectedLevels stationary)
        then Right ()
        else Left (FixedBatchModelMismatch (label ++ " stationary selection is outside its represented domain"))
  where
    candidates = NonEmpty.toList (reorderGridCandidates (fixedBatchSolutionGrid execution))

renderDomain :: NewsvendorDomain -> String
renderDomain domain =
    "R1="
        ++ show (NonEmpty.toList (newsvendorR1Layout domain))
        ++ ", R2="
        ++ show (NonEmpty.toList (newsvendorR2Layout domain))
        ++ ", semantics=R1×R2"

renderGridAxes :: ReorderGrid -> String
renderGridAxes grid =
    let candidates = NonEmpty.toList (reorderGridCandidates grid)
     in "R1="
            ++ show (uniqueSorted (fmap fixedBatchR1 candidates))
            ++ ", R2="
            ++ show (uniqueSorted (fmap fixedBatchR2 candidates))

uniqueSorted :: (Ord value) => [value] -> [value]
uniqueSorted = removeDuplicates . sort
  where
    removeDuplicates [] = []
    removeDuplicates (first : remaining) = first : go first remaining
    go _ [] = []
    go previous (value : values)
        | previous == value = go previous values
        | otherwise = value : go value values

renderParameters :: FixedBatchParameters -> String
renderParameters parameters =
    intercalate
        ", "
        [ "horizon=" ++ show (fixedBatchHorizon parameters)
        , "L1=0"
        , "L2=" ++ show (fixedBatchSupplierLeadTime parameters)
        , "Q1=" ++ show (fixedBatchQ1 parameters)
        , "Q2=" ++ show (fixedBatchQ2 parameters)
        , "externalBatchCap=" ++ show (fixedBatchExternalBatchCap parameters)
        , "demandCap=" ++ show (fixedBatchDemandCap parameters)
        , "h1=" ++ rational (fixedBatchStage1HoldingCost parameters)
        , "h2=" ++ rational (fixedBatchStage2HoldingCost parameters)
        , "p=" ++ rational (fixedBatchBacklogCost parameters)
        , "stateBudget=" ++ show (fixedBatchStateBudget parameters)
        , "stateActionBudget=" ++ show (fixedBatchStateActionBudget parameters)
        , "solverWorkBudget=" ++ show (fixedBatchSolverWorkBudget parameters)
        , "gridBudget=" ++ show (fixedBatchGridBudget parameters)
        , "convolutionBudget=" ++ show (fixedBatchConvolutionBudget parameters)
        ]

renderDemand :: FixedBatchDemand -> String
renderDemand demand = intercalate ", " [show value ++ ":" ++ rational mass | (value, mass) <- NonEmpty.toList (fixedBatchDemandOutcomes demand)]

renderInequality :: NewsvendorInequality -> String
renderInequality witness =
    "Pr(B0=0)="
        ++ rational (newsvendorNoStockoutProbability witness)
        ++ ", rhs="
        ++ rational (newsvendorRightHandSide witness)
        ++ ", weak="
        ++ yesNo (newsvendorWeakSatisfied witness)
        ++ ", strict="
        ++ yesNo (newsvendorStrictSatisfied witness)

rational :: Rational -> String
rational value = show (numerator value) ++ "/" ++ show (denominator value)

yesNo :: Bool -> String
yesNo True = "yes"
yesNo False = "no"
