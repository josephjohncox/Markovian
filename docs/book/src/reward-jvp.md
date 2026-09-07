# Reward sensitivity is not probability sensitivity

**Prerequisites:** [feedback timing](feedback.md), [orientation](orientation.md),
and elementary differentiation of a geometric series.
**Outcomes:** construct a lawful reward direction, interpret a checked directional
result, and distinguish a finite unrolling from the solved fixed point.
**Next:** [autodiff and lowering](autodiff-lowering.md) for a separate closed
language and [capabilities](capabilities.md) for availability boundaries.

## The retry experiment

An event pays reward amplitude a immediately, then either retries with probability
p or exits with probability 1-p. The loop or exit continuation is discounted once.
With exit payoff zero, strict discount gives the real parameterized semantics

\\[
V= a+\gamma p V=\frac{a}{1-\gamma p},\qquad
\frac{\partial V}{\partial a}=\frac{1}{1-\gamma p}.
\\]

At a=1 and gamma=p=1/2, **V=4/3 and the reward derivative is 4/3**.
`closeAffineFeedbackRewardJVP` implements this reward direction: supply a raw
`Matrix Rational` of ones on exactly the event channel's ordered source and event
layouts. Read `affineRewardJVPExternal` for derivative A and derivative K (zero).
`affineRewardJVPBase` owns the base coefficients alongside this derivative.

**Tempting claim:** this API also gives 8/9 by changing p. **Counterexample:**
its matrix is a reward direction, not a probability direction. The following is a
**separate symbolic exercise, outside the API**:

\\[
\frac{\partial V}{\partial p}=\frac{a\gamma}{(1-\gamma p)^2}=\frac89
\quad\text{at }a=1,\ \gamma=p=\frac12.
\\]

**Exercise:** derive that probability derivative by differentiating the fixed-point
equation. **Hint:** both p and V depend on p. **Checked answer:**
`dV/dp = gamma*V + gamma*p*dV/dp`; substituting V=4/3 gives 8/9.
The runnable fixture evaluates this separate symbolic expression exactly; it does
not pass a probability tangent to the API or claim probability-tangent admission.

## Four events are a different function

Truncate after four events and set the remaining continuation to zero. Independent
event-by-event dual-number unrolling gives V4=85/64 and dV4/da=85/64, **not** 4/3.
The four-event symbolic probability derivative is 27/32, **not** 8/9. Differentiating
a finite computation and differentiating the solved system are distinct operations.
For this retry, the reward derivative error after n events is
`(gamma*p)^n/(1-gamma*p)`; for n=4 it is 1/192. The tests also check general
strict-discount bounds on small signed, multi-source fixtures, without asserting
finite equality or claiming convergence at gamma=1.

**Exercise:** what happens to both derivatives when gamma=0?
**Hint:** only the immediate reward survives. **Checked answer:** dV/da=1 and
symbolic dV/dp=0. Zero discount is admitted; discount 1 is not.

## Exact scope and admission

The real family is `r(e)+t*h(s,e)`, evaluated at rational data. Probabilities,
discount, routes, slot identity and layouts stay fixed. Directions may be signed;
zero-probability slots contribute zero but are still observed and charged. Slots
remain distinct even if their perturbed rewards coincide. This does not
differentiate Haskell Rational operations, arbitrary callbacks, failed admission,
continuation payoffs, policies, argmax, topology or support. It is a directional
JVP, not a full Jacobian or a general autodiff system. Reward and successor remain
in their joint event; terminal and horizon conventions are unchanged.

One preflight reserves channel/direction cells, route aggregates, base/derivative
coefficient arrays and the reused solve workspace. A single cumulative rational
ledger then covers all inputs, the base solve and four base equation families,
the derivative solve and four differentiated equation families, and retained
results. Derivative K arrays are zero but their equations still execute zero
products. No zero-direction or zero-discount shortcut changes the work plan.
The JVP report is authoritative; the contained base report describes only the
base prefix. It is not a separately budgeted public base call.

For the retry fixture the conservative plan is **24 cells, 52 graph visits,
78 arithmetic units, and four maximum rational bits**. All exact limits pass;
one below any applicable limit fails atomically. Tests exhaust every arithmetic
prefix, include discarded Gaussian and derivative cancellation growth, and check
layout/failure order. Private probes compare machine-saturating reservations
against an independent Integer formula. Compile-fail gates protect constructor
opacity and all four nominal roles. See the [frozen EL-04 contract](../../plans/EL-04-REWARD-JVP.md)
for the complete deterministic operation order. EL-04 and D-078 remain Proposed;
this is unreleased implementation evidence, not release or acceptance.

## Run and inspect the independent evidence

```sh
cabal test Markovian-test --project-file=cabal.project.ci --test-show-details=direct
"$(cabal list-bin Markovian-test --project-file=cabal.project.ci)" --reward-jvp
python3 scripts/check-learning --run
bash scripts/check-feedback-boundary
```

Displayed stdout is generated by execution, not hand-entered:

```text
{{#include ../../learning/reward-jvp-output.txt}}
```

The test module below is also the runnable lesson context, dispatched by
`test/Main.hs`. Its small closed-form determinant oracle does not call production
Gaussian elimination. Its dual unrolling follows individual reward/route events,
not production aggregation or derivative code. The broad regression tests run in
the normal suite; the lesson entry point checks the displayed retry answers.

```haskell
{{#include ../../../test/FeedbackRewardJVP.hs}}
```
