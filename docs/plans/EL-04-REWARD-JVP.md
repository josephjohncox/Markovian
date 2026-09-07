# EL-04 — Fixed-topology event-reward feedback JVP

**Status:** Proposed

Frozen declarations and algorithm passed independent contract review. Implemented with local fixture evidence in `test/FeedbackRewardJVP.hs`; independent implementation review remains required. Not accepted or released. Extends only the exact root's `Markovian.Feedback.Value.Exact`, using `Markovian.Feedback.Internal`. D-078 remains Proposed. No numerical, autodiff, continuous, or new package edge.

## Parameterized semantics and admitted directions

Reuse the consumer and timing of `closeAffineFeedback`: a normalized channel from the canonical source layout `Left inputs ++ Right loops` into a finite ordered `FeedbackEvent loop output` layout. Each event pays immediately; either its loop value or its exit continuation is discounted once. The strict contraction discount is fixed, `0 <= gamma < 1`. Infinite non-exit paths contribute discounted rewards and zero exit-continuation coefficient.

The new operation admits **event-reward directions only**, supplied as a signed Rational matrix on exactly the channel's source and event layouts. At source s and event e the real family changes the reward to `r(e)+t*h(s,e)` while keeping its probability, route, event *slot*, and all layouts fixed. This is differentiation of a real affine family at rational data, not differentiation of Haskell Rational evaluation or constructor failures. Distinct slots stay distinct even if their perturbed rewards coincide. Zero-probability slots may carry any finite rational h and contribute zero. All directions are two-sided lawful: rewards have no positivity constraint, and no probability, support, discount, policy, argmax, continuation-payoff, or topology change is permitted.

Let B,D be external/internal loop-mass matrices, C,E exit-mass matrices, and mX,mU expected immediate rewards. Base equations are `AU=mU+gamma*D*AU`, `AX=mX+gamma*B*AU`, `KU=gamma*E+gamma*D*KU`, `KX=gamma*C+gamma*B*KU`. Set `hm(s)=sum_e P(s,e)*h(s,e)`. Then `dAU=hmU+gamma*D*dAU`, `dAX=hmX+gamma*B*dAU`, and dKU=dKX=0. Validate these four differentiated equations literally; compute and validate the four base equations in the same operation. No general implicit-autodiff or full Jacobian claim.

## Complete new exported declarations

Existing public `FeedbackLimits`, `FeedbackAccounting`, `AffineFeedbackError`, `AffineFeedbackCoefficients`, `CheckedAffineFeedback`, `LoopLayout`, `FeedbackEvent`, `ExactContractionDiscount`, finite sets, raw `Matrix`, and normalized `StochasticMatrix` are reused. Raw Matrix is from `Markovian.Category.Matrix`; NonNegativeRational is the existing opaque exact scalar. No hidden type appears in a public signature.

```haskell
data AffineRewardJVPError loop output
  = AffineRewardJVPBaseError !(AffineFeedbackError loop output)
  | AffineRewardJVPDirectionSourceLayoutMismatch
  | AffineRewardJVPDirectionEventLayoutMismatch
  | AffineRewardJVPInternalConstantEquationFailure !loop
  | AffineRewardJVPExternalConstantEquationFailure !Int
  | AffineRewardJVPInternalContinuationEquationFailure !loop !output
  | AffineRewardJVPExternalContinuationEquationFailure !Int !output
  deriving (Eq, Show)

data AffineRewardJVPReport owner = AffineRewardJVPReport
  { affineRewardJVPOwner :: !owner
  , affineRewardJVPMatrixCells :: !Natural
  , affineRewardJVPGraphWork :: !Natural
  , affineRewardJVPValidatedEquations :: !Natural
  , affineRewardJVPAccounting :: !FeedbackAccounting
  } deriving (Eq, Show)

type role CheckedAffineRewardJVP nominal nominal nominal nominal
data CheckedAffineRewardJVP owner input loop output -- opaque

closeAffineFeedbackRewardJVP :: (Eq input, Eq output)
  => FeedbackLimits -> ExactContractionDiscount
  -> FiniteSet input -> LoopLayout owner loop -> FiniteSet output
  -> StochasticMatrix NonNegativeRational
       (Either input loop) (FeedbackEvent loop output)
  -> Matrix Rational (Either input loop) (FeedbackEvent loop output)
  -> Either (AffineRewardJVPError loop output)
       (CheckedAffineRewardJVP owner input loop output)
affineRewardJVPBase :: CheckedAffineRewardJVP owner input loop output
  -> CheckedAffineFeedback owner input loop output
affineRewardJVPExternal :: CheckedAffineRewardJVP owner input loop output
  -> AffineFeedbackCoefficients input output
affineRewardJVPInternal :: CheckedAffineRewardJVP owner input loop output
  -> AffineFeedbackCoefficients loop output
affineRewardJVPReport :: CheckedAffineRewardJVP owner input loop output
  -> AffineRewardJVPReport owner
```

`affineRewardJVPExternal/Internal` contain derivative A and zero derivative K, not new normalized channels. Their existing coefficient accessors are the only point queries; absent labels return Nothing. A raw direction is checked in the operation, never treated as an opaque reusable proof. The result owns its base and derivatives together, so the caller cannot apply a direction witness to another channel. `LoopLayout` provides the Eq loop dictionary and owner. Exact ordered layout checks include full event labels/rewards, not merely dimensions or route support. No relabeling by `coerce`; retain all existing nominal roles and add compile-fail tests for the new result.

## Single preflight and ledger

Construction of supplied matrices and discount is prior work; the JVP re-admits their complete contents under its own limits. Do not call `closeAffineFeedback` and reset its meter. Extract private metered base construction and Gaussian routines, preserving all existing public base-operation behavior/goldens. Base and derivative calls share one `FeedbackMeter` and one cumulative arithmetic allowance.

Let x,u,y,e denote input, loop, output and event counts; S=x+u, T=u+y. In the following precedence, every sum/product is checked using existing machine-saturating feedback helpers before allocation:

1. Source, loop, output, then event count limits (events use `maximumFeedbackTraceOutcomes`, as in the base operation). No ticks are executed; tick limit is inapplicable.
2. Matrix-cell reservation `2*S*e + S*(u+y) + 2*S*(1+y) + u*(u+y+2)`. This covers input channel/direction cells, aggregated route matrices, base and derivative A/K arrays, and one reused augmented solve workspace (base u by u+y+1, derivative u by u+1). The limit measures this stated conservative semantic cell plan, not exact live heap cells; temporary arithmetic nodes are metered separately. Include zero derivative K arrays even when values are known zero.
3. Graph-work reservation `G + S + e + 4*S*e`, where `G=e*T+S+e+4*S*e+S*e*T` is the existing affine base bound. The added work pays for direction source/event layout comparisons, row-major extraction, rational observation, and reward-direction aggregation. The reserve is charged once and is independent of probability sparsity.
4. Check base event targets in event order, then base canonical source layout; wrap errors in `AffineRewardJVPBaseError`. Check direction source layout and then direction event layout exactly, returning their dedicated mismatches. No arithmetic or solve precedes these checks.
5. Begin one rational ledger: observe gamma, base event rewards, base matrix row-major, then direction matrix row-major. Rational-size failures wrap `AffineFeedbackLimitError` in the base-error constructor. Input observations consume no arithmetic units but all contribute to input maxima.
6. Run existing base aggregation, Gaussian solve, external derivation, and four base-equation checks in their existing order, without resetting the ledger. Retain base coefficient observations as usual. The base report inside the new result describes this base prefix only; the JVP report is the authoritative operation-wide account and must not be obtained by adding reports from independent public calls.
7. Form hmX then hmU: for every source/event cell, including zero mass, multiply mass*h then add to a running zero. Two arithmetic units per cell, with every result checked. Solve `(I-gamma*D)*dAU=hmU` by the base deterministic first-nonzero-pivot Gaussian algorithm with one RHS; do not shortcut zero h or zero gamma. Empty u uses its existing empty-system case. Derive dAX in input/loop order (scale gamma*B, multiply by dAU, add into hmX). Allocate zero derivative K arrays according to preflight.
8. Check differentiated equations in external A, internal A, external K, internal K order and retained source/output order. Recompute their RHS using the same multiply/add order as the corresponding base validations, including zero products for dK. Zero direct exit RHS is not a reward or discount tangent. Compare each scalar literally; mismatch returns the named failure. `affineRewardJVPValidatedEquations=8` counts four base and four derivative *families*, including vacuous families.
9. Observe retained derivative A/K values and return one result and aggregate report. Every arithmetic operation charges one unit before calculating and checking the reduced result. Pivot observations and row/pivot branch visits follow the existing Gaussian ledger; their discarded maxima count. Input/retained observations cost no arithmetic units. Work overflow precedes the corresponding rational failure. No partially checked result or partial ledger escapes Left.

All rational limits use existing feedback bit definitions and phase maxima. Gamma, masses, signed directions, pivots, intermediate cancellation, and zero K are subject to the same cap. A singular solve is impossible for lawful strict-discount data mathematically but retains the existing defensive singular-system failure. No differentiation of that failure is claimed.

## Consumer evidence

The API lesson differentiates the retry's reward amplitude: `V=a/(1-gamma*p)`, hence at a=1 and gamma=p=1/2, V=4/3 and dV/da=4/3. A **separately labeled symbolic probability-direction exercise**, outside this API, derives `dV/dp=a*gamma/(1-gamma*p)^2=8/9`. Use an independent dual-number finite-unrolling oracle to illustrate convergence of derivatives without claiming equality to the fixed point at finite horizon. Include multiple sources/events sharing routes with different rewards/directions, zero mass, zero loops/outputs where a normalized channel exists, zero direction, gamma=0, gamma approaching but not equal to 1, negative rewards, layout reorder rejection, and nominal-role failures. Freeze exact and one-below cell/graph/arithmetic/rational goldens, discarded growth and combined-invalid precedence. Update current exports/docs/archives only at implementation time.
