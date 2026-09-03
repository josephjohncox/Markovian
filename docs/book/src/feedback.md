# Checked finite feedback

Markovian supports three explicit exact fragments. It does not expose a universal stochastic trace.

## Why raw trace is not stochastic feedback

For a raw matrix `F : X × U -> Y × U`, `traceMatrix` contracts the repeated index:

\\[
\operatorname{Tr}^{U}(F)(x,y)=\sum_{u\in U}F((x,u),(y,u)).
\\]

This is valid semiring matrix algebra. It does not preserve row normalization. If `X` and `Y` are singleton and `F` is the identity on a two-value `U`, the only output entry is `2`. `StochasticMatrix` therefore has no trace operation.

The feedback APIs are also distinct from matrix dagger, Bayesian inversion, payoff pullback, reverse differentiation, strategic duality, and disintegration.

## Delayed feedback

`Markovian.Feedback.Delay.Exact` represents a seed and a one-tick body:

\\[
S_0\sim A(-\mid x),\qquad
(Y_t,S_{t+1})\sim B(-\mid x,S_t).
\\]

The external input is held constant. `observeDelayedTrace` returns the complete joint trace law. Horizon zero returns the seeded initial state and no body steps. `observeDelayedFinal` requires a positive `FeedbackTicks` witness and returns the joint final `(output,state)` law. It does not silently marginalize the state.

This is a finite causal state machine. It does not solve `s = f(s)`. The Boolean `not` function has no instantaneous fixed point, while identity has two. Finite fuel does not repair either uniqueness or existence.

## Proper first exit

`Markovian.Feedback.Channel.Exact` accepts a normalized routing channel

\\[
K:X+U\longrightarrow Y+U,
\qquad
K=\begin{bmatrix}A&B\\C&D\end{bmatrix}.
\\]

Every represented internal state must have a positive-mass path to an exit. The check includes internal states that external inputs cannot reach. The implementation solves

\\[
H=C+DH=(I-D)^{-1}C,
\qquad
F=A+BH.
\\]

It then checks the first equation and exact row normalization. If `D` is nilpotent, the report records the least checked index. Otherwise it records the finite-state witness

\\[
\beta=\lVert D^{|U|}\rVert_\infty<1.
\\]

The exact rational solve does not use approximate iteration. A unit self-loop is rejected because it never exits. A half-loop and half-exit is accepted for its marginal exit channel. Reachability uses a counted reverse queue. The implementation computes the finite matrix-power sequence once; the same sequence supplies nilpotence or the `D^|U|` witness.

## Timed feedback

`Markovian.Feedback.Timed.Exact` puts one exact reward on each microstep and returns the joint value

```haskell
TimedFeedbackExit
  { timedFeedbackReward   :: ExactReward
  , timedFeedbackDuration :: Natural
  , timedFeedbackOutput   :: output
  }
```

For duration `d`, the accumulated reward is

\\[
G=r_0+\gamma r_1+\cdots+\gamma^{d-1}r_{d-1}.
\\]

An outer dynamic model must use `G + gamma^d V(output)`. Terminal payoff remains outside this operation and is applied once by the outer model. Observation and successor can be carried together in `output`.

Timed closure requires nilpotence. A half-loop and half-exit has a valid marginal exit law but durations `1,2,3,...`. Reward support depends on the event rewards and `gamma`: for example, unit rewards with `gamma = 0` collapse to one accumulated value, while `gamma = 1` gives unbounded accumulated rewards. Infinite duration support alone is enough to exceed this finite result type, so every such cycle is rejected.

## Bounds and reports

`FeedbackLimits` bounds source, loop, and output cardinality. It also bounds cells, graph work, arithmetic work, trace outcomes, and rational size. The interpreters check represented dimensions before result layout construction.

A single operation-wide meter charges each rational operation. Delayed and timed interpreters also charge each branch before they inspect it or descend. Zero-mass branches consume work. A one-below work limit stops the operation at the first excess charge.

`FeedbackAccounting` reports exact charged work and these rational maxima:

- input values;
- matrix-power intermediates;
- Gaussian-elimination intermediates;
- delayed path intermediates;
- timed path intermediates;
- other checked intermediates;
- retained results;
- all observed values.

The phase maxima include discarded products, sums, pivots, quotients, path probabilities, reward accumulations, and discount powers. Input and retained-result observations do not consume arithmetic work. A phase that the operation does not use has a zero maximum.

The interpreters check rational size at each observation. A limit failure returns no channel, witness, result, or partial report. This atomic result rule does not claim transactional heap rollback.

Reports contain deterministic counts and witnesses only. The separate benchmark reports one excluded warmup and twenty raw timing samples. Timing is local reproducibility data, not a complexity or production-performance claim. D-069 is `Accepted` for these three checked feedback fragments.

## Evidence and nonclaims

`test/FeedbackExact.hs` checks equations, normalization, timing, correlations, limits, and rejected cycles. It includes an independent acyclic path enumerator and multi-output absorption.

The accounting fixtures use separate operation-count and path oracles. They force discarded maxima above retained maxima for Gaussian, delayed, and timed execution. Each interpreter has exact and one-below work and rational limits. `test/golden/feedback-accounting.txt` fixes the complete first-exit accounting report. The root source archive contains and runs this evidence in an isolated GHC 9.8.4 project. `scripts/check-feedback-boundary` checks constructor opacity and nominal endpoint roles.

This subsystem does not establish:

- universal trace or arbitrary feedback;
- a guarded Conway category;
- arbitrary cyclic circuit or open-system semantics;
- stationary-distribution selection;
- finite support for rewardful cyclic transience;
- continuous disintegration;
- tensor, device, or numerical correctness;
- release readiness.

See [Law catalogue](laws-and-boundaries.md), [Public module map](api-map.md), and [References](references.md).
