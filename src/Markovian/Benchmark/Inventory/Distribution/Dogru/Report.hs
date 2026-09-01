-- | Deterministic evidence report for the bounded Doğru adaptation.
module Markovian.Benchmark.Inventory.Distribution.Dogru.Report (
    DogruReport,
    dogruReport,
    renderDogruReport,
    dogruBenchmarkWarmupCount,
    dogruBenchmarkSampleCount,
) where

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Markovian.Benchmark.Inventory.Distribution.Dogru.Exact
import Numeric.Natural (Natural)

dogruBenchmarkWarmupCount :: Natural
dogruBenchmarkWarmupCount = 1

dogruBenchmarkSampleCount :: Natural
dogruBenchmarkSampleCount = 20

newtype DogruReport = DogruReport [String]
    deriving (Eq, Show)

-- | Construct a semantic report only after strict two-dimensional widening.
dogruReport :: DogruSolution -> DogruSolution -> Either DogruError DogruReport
dogruReport primary widened = do
    stability <- compareDogruBounds primary widened
    let fixture = dogruSolutionFixture primary
        widenedFixture = dogruSolutionFixture widened
        parameters = dogruFixtureParameters fixture
        widenedParameters = dogruFixtureParameters widenedFixture
    Right
        ( DogruReport
            [ "Doğru (2006), Chapters 2–4, two-retailer finite-horizon adaptation."
            , "primary source: Mustafa Kemal Doğru, Optimal Control of One-Warehouse Multi-Retailer Systems: An Assessment of the Balance Assumption, 2006, DOI 10.6100/IR601558"
            , "source crosswalk: balance relaxation equations (2.3)–(2.5), printed page 24, and equations (2.6)–(2.7), printed page 26; balanced policy Theorem 2.9, page 30; physicalized LB heuristic section 3.3.4, page 53; physical timing and equations (4.5)–(4.7), pages 82–87; average-cost objective equation (4.11), page 92; Table 4.1, page 95; Table 4.3, page 98; scenario-2 discussion and base stocks, pages 112–114"
            , "implemented fragment: N=2, l0=1, l1=l2=0, exact finite horizon"
            , "scope boundary: finite-horizon adaptation, not reproduction of published average costs"
            , "scope boundary: no average-cost convergence, unrestricted return, continuous demand, or generic multi-retailer optimality claim"
            , "coordinate map: w=I0-IP1-IP2, hence source physical capacity is s1+s2<=w+o"
            , "physical actions: q>=0, s1>=0, s2>=0, s1+s2<=w+o"
            , "balance-relaxed actions: q>=0, signed s1 and s2, s1+s2<=w+o, repository lower bound si>=-returnCap"
            , "event timing: observe state and known due order o; choose supplier order q and shipments s1,s2; receive the due supplier order and zero-lead retailer shipments; observe independent demands; incur end-period cost paired with its successor"
            , "successor: w'=w+o-s1-s2, o'=q, i1'=i1+s1-d1, i2'=i2+s2-d2"
            , "cost: h0*w'+(h0+h1)*max(i1',0)+p1*max(-i1',0)+(h0+h2)*max(i2',0)+p2*max(-i2',0)"
            , "boundary behavior: complete reachable closure, no state clamping, no redirected successor"
            , "terminal convention: no order when one period remains; no terminal salvage"
            , "scenario 2 source label: cv_i=2 (label only, not the exact coefficient of variation of the displayed finite law)"
            , "source coefficients: h0=9/10, h1=h2=1/10, p1=p2=4, y0*=y1*=y2*=3"
            , "independent marginal demand: " ++ renderDemand
            , "exact marginal mean: " ++ rational dogruDemandMean
            , "exact marginal second moment: " ++ rational dogruDemandSecondMoment
            , "exact marginal variance: " ++ rational dogruDemandVariance
            , "omitted demand mass: " ++ rational dogruOmittedDemandMass
            , "primary parameters: " ++ renderParameters parameters
            , "widened parameters: " ++ renderParameters widenedParameters
            , "initial state: " ++ show (dogruInitialState fixture)
            , "bounded relaxed oracle C_R: " ++ rational (dogruRelaxedOracleCost primary)
            , "bounded physical oracle C_P: " ++ rational (dogruPhysicalOracleCost primary)
            , "relaxed balanced base-stock policy cost: " ++ rational (dogruBalancedPolicyCost primary)
            , "physicalized LB heuristic C_H: " ++ rational (dogruPhysicalizedLBHeuristicCost primary)
            , "relaxation error C_P-C_R: " ++ rational (dogruRelaxationError primary)
            , "policy regret C_H-C_P: " ++ rational (dogruPolicyRegret primary)
            , "total gap C_H-C_R: " ++ rational (dogruTotalGap primary)
            , "source-compatible bounded epsilon*=100*(C_P-C_R)/C_R: " ++ rational (dogruRelativeRelaxationError primary) ++ "%"
            , "source-compatible bounded epsilon=100*(C_H-C_R)/C_R: " ++ rational (dogruRelativeTotalGap primary) ++ "%"
            , "separate policy regret percentage=100*(C_H-C_P)/C_P: " ++ rational (dogruRelativePolicyRegret primary) ++ "%"
            , "primary layouts: physical states=" ++ show (length (dogruPhysicalReachableStates fixture)) ++ ", relaxed states=" ++ show (length (dogruRelaxedReachableStates fixture)) ++ ", physical state-actions=" ++ show (dogruPhysicalStateActionCount fixture) ++ ", relaxed state-actions=" ++ show (dogruRelaxedStateActionCount fixture)
            , "primary checked solver work: " ++ show (dogruCheckedSolverWork primary)
            , "primary order cap selected: " ++ yesNo (dogruOrderCapSelected primary)
            , "primary relaxed-return cap selected: " ++ yesNo (dogruReturnCapSelected primary)
            , "widened exact costs: C_R=" ++ rational (dogruRelaxedOracleCost widened) ++ ", C_P=" ++ rational (dogruPhysicalOracleCost widened) ++ ", C_H=" ++ rational (dogruPhysicalizedLBHeuristicCost widened)
            , "widened layouts: physical states=" ++ show (length (dogruPhysicalReachableStates widenedFixture)) ++ ", relaxed states=" ++ show (length (dogruRelaxedReachableStates widenedFixture)) ++ ", physical state-actions=" ++ show (dogruPhysicalStateActionCount widenedFixture) ++ ", relaxed state-actions=" ++ show (dogruRelaxedStateActionCount widenedFixture)
            , "widened order cap selected: " ++ yesNo (dogruWidenedOrderCapSelected stability)
            , "widened relaxed-return cap selected: " ++ yesNo (dogruWidenedReturnCapSelected stability)
            , "widened-bound diagnostic: " ++ if dogruBoundsStable stability then "stable and nonbinding" else "not stable or a widened cap is selected"
            , "diagnostic boundary: finite widening is evidence, not proof of an unbounded relaxed or physical optimum"
            ]
        )

renderDogruReport :: DogruReport -> String
renderDogruReport (DogruReport reportLines) = unlines reportLines

renderParameters :: DogruParameters -> String
renderParameters parameters =
    intercalate
        ", "
        [ "horizon=" ++ show (dogruHorizon parameters)
        , "orderCap=" ++ show (dogruOrderCap parameters)
        , "returnCap=" ++ show (dogruReturnCap parameters)
        , "h0=" ++ rational (dogruWarehouseHoldingCost parameters)
        , "h1=h2=" ++ rational (dogruRetailerHoldingCost parameters)
        , "p1=p2=" ++ rational (dogruRetailerPenaltyCost parameters)
        , "stateBudget=" ++ show (dogruStateBudget parameters)
        , "stateActionBudget=" ++ show (dogruStateActionBudget parameters)
        , "solverWorkBudget=" ++ show (dogruSolverWorkBudget parameters)
        ]

renderDemand :: String
renderDemand =
    intercalate ", " [show demand ++ ":" ++ rational mass | (demand, mass) <- NonEmpty.toList dogruDemandOutcomes]

rational :: Rational -> String
rational value = show (numerator value) ++ "/" ++ show (denominator value)

yesNo :: Bool -> String
yesNo True = "yes"
yesNo False = "no"
