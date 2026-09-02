-- | Deterministic evidence for the finite Clark--Scarf 1960 specialization.
module Markovian.Benchmark.Inventory.ClarkScarf1960.Report (
    ClarkScarfReport,
    clarkScarfReport,
    renderClarkScarfReport,
    clarkScarfBenchmarkWarmupCount,
    clarkScarfBenchmarkSampleCount,
) where

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact
import Markovian.Benchmark.Inventory.ClarkScarf1960.Oracle.Exact
import Markovian.Objective.Exact (exactDiscountValue)
import Numeric.Natural (Natural)

-- | Return the clark scarf benchmark warmup count.
clarkScarfBenchmarkWarmupCount :: Natural
clarkScarfBenchmarkWarmupCount = 1

-- | Return the clark scarf benchmark sample count.
clarkScarfBenchmarkSampleCount :: Natural
clarkScarfBenchmarkSampleCount = 20

-- | Deterministic clark scarf report.
newtype ClarkScarfReport = ClarkScarfReport [String]
    deriving (Eq, Show)

-- | Construct a report only after checked bound widening.
clarkScarfReport :: ClarkScarfSolution -> ClarkScarfSolution -> Either ClarkScarfError ClarkScarfReport
clarkScarfReport primary widened = do
    stability <- compareClarkScarfBounds primary widened
    let fixture = clarkScarfSolutionFixture primary
        widenedFixture = clarkScarfSolutionFixture widened
        parameters = clarkScarfFixtureParameters fixture
        widenedParameters = clarkScarfFixtureParameters widenedFixture
        demand = clarkScarfFixtureDemand fixture
    Right
        ( ClarkScarfReport
            [ "Clark–Scarf (1960), Section III, finite lattice specialization."
            , "primary source: Andrew J. Clark and Herbert Scarf, Management Science 6(4), 1960, 475–490, DOI 10.1287/mnsc.6.4.475"
            , "inspected scan SHA-256: b64d82098b47dffa7cc4b87a4bbc6c833bb90295ccbede0a1897c8af44956239"
            , "source crosswalk: timing and state pages 481–482; joint recursion equation (14) page 482; isolated recursion equation (15) page 482; decomposition equations (20), (21), and (26) pages 483–484"
            , "source assumptions represented: beginning-of-period action; independent period demand; complete backlog; two-period downstream and one-period upstream natural lead times; echelon-stock natural costs; linear internal transport; optional highest-echelon setup cost"
            , "repository deductions: integer lattice; finite rational demand sum; external-order cap; isolated-target cap; finite reachable closure; explicit state, state-action, and solver-work budgets"
            , "scope boundary: not a published numeric reproduction; the paper supplies no numeric oracle"
            , "scope boundary: no continuous-demand, average-cost, branching, generic multi-echelon, or unbounded-optimality claim"
            , "boundary behavior: no state clamping and no redirected successor"
            , "event timing: observe (x1,w1,x2); choose external order z and dispatch target y; charge action and beginning-stock natural costs; observe one demand; pair its realized shortage cost with successor (x1+w1-D,y-x1-w1,x2+z-D)"
            , "state invariant: x2-x1-w1 >= 0"
            , "equation (1) finite form: h*max(x,0)+p*max(D-x,0)"
            , "equation (14) finite form: minimize c(z)+c1*(y-x1-w1)+L1(x1)+L2(x2)+alpha*E[C(n-1,x1+w1-D,y-x1-w1,x2+z-D)]"
            , "primary parameters: " ++ renderParameters parameters
            , "widened parameters: " ++ renderParameters widenedParameters
            , "initial state: " ++ show (clarkScarfInitialState fixture)
            , "conditioned demand: " ++ renderDemand demand
            , "retained one-period mass: " ++ rational (clarkScarfRetainedDemandMass demand)
            , "omitted one-period mass: " ++ rational (clarkScarfOmittedDemandMass demand)
            , "horizon exceedance mass (not a value-error bound): " ++ rational (clarkScarfHorizonExceededMass (clarkScarfHorizon parameters) demand)
            , "three exact paths: generic ExactMDP backward induction; direct equation (14); equations (15)/(20)/(21)/(26) decomposition"
            , "primary initial exact cost: " ++ rational (clarkScarfInitialCost primary)
            , "decomposed policy cost in joint model: " ++ rational (clarkScarfInitialDecomposedPolicyCost primary)
            , "decomposed policy exact regret: " ++ rational (clarkScarfInitialRegret primary)
            , "maximum exact path differential: " ++ rational (clarkScarfMaximumDifferential primary)
            , "primary layout counts: states=" ++ show (length (clarkScarfReachableStates fixture)) ++ ", state-actions=" ++ show (clarkScarfStateActionCount fixture)
            , "primary checked work bound: " ++ show (clarkScarfCheckedWork primary)
            , "primary order cap selected: " ++ yesNo (clarkScarfOrderCapSelected primary)
            , "primary isolated-target cap selected: " ++ yesNo (clarkScarfTargetCapSelected primary)
            , "widened initial exact cost: " ++ rational (clarkScarfInitialCost widened)
            , "widened layout counts: states=" ++ show (length (clarkScarfReachableStates widenedFixture)) ++ ", state-actions=" ++ show (clarkScarfStateActionCount widenedFixture)
            , "widened order cap selected: " ++ yesNo (clarkScarfOrderCapSelected widened)
            , "widened isolated-target cap selected: " ++ yesNo (clarkScarfTargetCapSelected widened)
            , "widened-bound diagnostic: " ++ if clarkScarfBoundsStable stability then "stable and nonbinding" else "not stable or a widened cap is selected"
            , "diagnostic boundary: widening is finite evidence, not an unbounded proof"
            ]
        )

-- | Render the clark scarf report.
renderClarkScarfReport :: ClarkScarfReport -> String
renderClarkScarfReport (ClarkScarfReport lines') = unlines lines'

renderParameters :: ClarkScarfParameters -> String
renderParameters parameters =
    intercalate
        ", "
        [ "horizon=" ++ show (clarkScarfHorizon parameters)
        , "discount=" ++ rational (exactDiscountValue (clarkScarfDiscount parameters))
        , "orderCap=" ++ show (clarkScarfOrderCap parameters)
        , "isolatedTargetCap=" ++ show (clarkScarfIsolatedTargetCap parameters)
        , "K=" ++ rational (clarkScarfExternalSetupCost parameters)
        , "c=" ++ rational (clarkScarfExternalUnitCost parameters)
        , "c1=" ++ rational (clarkScarfTransportUnitCost parameters)
        , "h1=" ++ rational (clarkScarfDownstreamHoldingCost parameters)
        , "p1=" ++ rational (clarkScarfDownstreamShortageCost parameters)
        , "h2=" ++ rational (clarkScarfUpstreamHoldingCost parameters)
        , "p2=" ++ rational (clarkScarfUpstreamShortageCost parameters)
        , "stateBudget=" ++ show (clarkScarfStateBudget parameters)
        , "stateActionBudget=" ++ show (clarkScarfStateActionBudget parameters)
        , "solverWorkBudget=" ++ show (clarkScarfSolverWorkBudget parameters)
        ]

renderDemand :: ClarkScarfDemand -> String
renderDemand demand =
    intercalate
        ", "
        [show value ++ ":" ++ rational mass | (value, mass) <- NonEmpty.toList (clarkScarfDemandOutcomes demand)]

rational :: Rational -> String
rational value = show (numerator value) ++ "/" ++ show (denominator value)

yesNo :: Bool -> String
yesNo True = "yes"
yesNo False = "no"
