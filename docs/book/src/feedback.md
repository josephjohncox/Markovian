# Checked finite feedback

Markovian supports three accepted exact fragments and one proposed strict-discount value fragment. It does not expose a universal stochastic trace.

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

## Strict-discount affine value coefficients

`Markovian.Feedback.Value.Exact` accepts the same normalized joint reward-and-route events as timed feedback, but it returns no path law. It requires an `ExactContractionDiscount`, so `0 <= gamma < 1`. For first-exit time `tau`, including `tau = infinity`, the additive functional is

\\[
G=\\sum_{t=0}^{\\tau}\\gamma^t r_t,
\\qquad
V(x)=\\mathbb{E}_x[G]+\\sum_y\\mathbb{E}_x[\\gamma^{\\tau+1}1_{Y_\\tau=y}]v(y).
\\]

The continuation term is zero on `tau = infinity`. Finite state and event sets make rewards bounded, so the strict-discount reward sum exists even for a closed internal class.

The implementation keeps reward and route in each normalized event while it aggregates expected one-event rewards `m`, internal routing `D`, and exits `E`. One private bounded multi-right-hand-side elimination solves

\\[
\\alpha=m_U+\\gamma D\\alpha,
\\qquad
Q=\\gamma E_U+\\gamma DQ.
\\]

It then derives

\\[
A=m_X+\\gamma B\\alpha,
\\qquad
K=\\gamma E_X+\\gamma BQ.
\\]

Construction checks these four equations literally over `Rational`. Public code receives only opaque nominal external and internal `AffineFeedbackCoefficients`. It can observe one `A` or `K` coefficient by a typed label. There is no continuation evaluator and no normalized output channel.

A closed unit-reward loop at `gamma=1/2` has `A=2` and `K=0`. A half-exit, half-loop unit-reward channel has `A=4/3` and `K=1/3`. This fragment is implementation evidence for D-078; D-078 remains `Proposed`.

## Bounds and reports

`FeedbackLimits` bounds source, loop, and output cardinality. It also bounds cells, graph work, arithmetic work, trace outcomes or affine event count, and rational size. The interpreters check represented dimensions before result layout construction. Affine feedback reads the checked channel in row-major order instead of repeatedly calling linear labelled lookup. For `S=|X|+|U|`, `E=|events|`, and `T=|U|+|Y|`, its conservative graph bound is `E*T + S + E + 4*S*E + S*E*T`. This covers event validation, layout comparison, row extraction and observation, reward aggregation, and every event scan for every continuation or exit target.

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

`test/FeedbackExact.hs` checks equations, normalization, timing, correlations, limits, and rejected cycles. It includes an independent acyclic path enumerator and multi-output absorption. `test/FeedbackValueExact.hs` checks strict-discount hand solutions, infinite and partial exit, literal equations, a nilpotent timed differential, malformed channels, fixed exact and one-below ledgers, adversarial unmatched-event scans, and combined-invalid failure precedence. Its independent two-loop, two-output finite oracle records `N=4`, preflights a fixed 180-operation plan, and checks exact and one-below horizon, work, and rational limits.

The accounting fixtures use separate operation-count and path oracles. They force discarded maxima above retained maxima for Gaussian, delayed, and timed execution. Each accepted D-069 interpreter has exact and one-below work and rational limits. `test/golden/feedback-accounting.txt` fixes the complete first-exit accounting report. The affine proposal adds `test/golden/affine-feedback-accounting.txt`, including fixed work, graph, phase, and rational maxima; its one-below rational fixture fails on a discarded Gaussian difference. The root source archive contains and runs the accepted D-069 evidence in an isolated GHC 9.8.4 project. `scripts/check-feedback-boundary` checks constructor opacity and nominal endpoint roles. D-078 evidence is in this proposal worktree, not the immutable `v2026.9.3.0` release; package versions remain unchanged under the task invariant.

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
