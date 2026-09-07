# When can two states share a block?

**Prerequisites:** [your first MDP](first-mdp.md), [joint-law laboratory C and timing laboratory D](law-laboratory.md), and [orientation](orientation.md).
**Outcomes:** supply a partition of one closed policy, read its owned quotient,
and explain a concrete failed merge without confusing means with laws.
**Next:** inspect the [capability boundaries](capabilities.md) and the
[frozen EL-05 contract](../../plans/EL-05-AGGREGATION.md).

## A successful merge

The runnable fixture compiles an exact MDP and closes its single action with
`compileExactMDP` and `closeCompiledExactPolicy`. The original state layout is
`[20,30,10,11]`. States 20 and 30 both observe 5 and continue: with probability
1/2 they pay 0 and stay in the live block, and with probability 1/2 they pay 2
and reach a terminal state. States 10 and 11 both observe 9 and stop with
payoff 7. State 20's reward-2 branch is represented by two duplicate atoms of
mass 1/4; state 30 uses one atom of mass 1/2. Raw layout equality is not required.

Supply block layout `[4,8]`, mapping `[(20,8),(30,8),(10,4),(11,4)]`, and the
observations to `checkFixedPolicyAggregation`. `AggregationEquivalent` owns the
original index, explicit original-order mapping, initial block 8, and quotient
rows. Block 4 is terminal; block 8 has the exact joint atoms `(0,8,1/2)` and
`(2,4,1/2)`. Block order remains `[4,8]`; its representatives are 10 and 20.
Reordered complete association lists are accepted and canonicalized.

With discount 1/2, the live values at horizons 0, 1 and 2 are **0, 11/4 and
55/16**. A terminal value at horizon zero is still **7**. On a transition the
immediate reward is paid first, the correlated successor is reached, and its
future is discounted once. Terminal payoff precedes the horizon check; a
continuing state stopped by horizon receives no invented payoff.

**Exercise:** derive the horizon-one live value. **Hint:** the terminal payoff
is future value even on the last permitted transition. **Checked answer:**
`(1/2)*0 + (1/2)*(2+(1/2)*7) = 11/4`. For horizon two the self-loop adds
`(1/4)*(11/4)`, giving 55/16. These are finite-horizon values, not a fixed point.

## A failed merge with the same expected reward

**Tempting claim:** equal expected reward and successor blocks suffice.
State 0 pays 0 or 2 with equal probability; state 1 always pays 1. Both reach
terminal state 2 with payoff zero. Both have mean reward 1 and the same successor.
Nevertheless, the checker returns `AggregationDistinguished`: representative 0
and member 1 disagree on joint atom `(reward 0, block 1)`, with masses **1/2 and
0**. A single observed reward distinguishes them. The opaque witness certifies
admission, canonicalization, and a literally rechecked difference; a freely
constructed `AggregationDifference` value alone certifies nothing.

**Exercise:** why are equal separate reward and successor marginals also
insufficient? **Hint:** pair rewards 0 and 2 with two differently observed terminal
blocks in opposite orders. **Checked answer:** both marginals are fair, but the
probability of reward 0 followed by the first terminal observation is 1/2 in one
state and zero in the other. The test fixture enumerates those traces independently.
Tests also reject terminal-kind, terminal-payoff and initial-observation mismatches.
Observations are seen initially and at every reached state, including the state
at the horizon or terminal stop; no hidden observation likelihood is inferred.

## Admission is part of the API

The checker admits supplied finite data, not callbacks. Every state must occur
exactly once in the partition and observation lists; every declared block must
be used. Malformed and infinite list spines fail under bounded admission before
unbounded equality scans. Lawful terminating `Eq` instances are assumed; semantic
work counts do not bound arbitrary computation inside a user-defined `Eq`.

One cumulative meter covers structural reservation, input rationals, duplicate
mass additions, canonical rows, representative comparisons, and either witness
validation or quotient/mapping retention. Both outcomes are atomic; `Left` returns
no partial table, witness or accounting. Exact arithmetic remains separate from
floating computation. For the successful fixture, the full golden ledger is
**4 states, 2 blocks, 5 raw atoms, 4 original canonical atoms, 162 work units,
and 3 maximum rational bits**. All exact caps pass. Every smaller work prefix
fails, including retention and witness construction in the failure fixtures.
The tests cover discarded duplicate-addition rational growth and work-before-bit
precedence. Compilation and policy closure are separately budgeted prior work;
this meter is semantic admission, not a measured execution-time estimate.

This is a sufficient congruence for **one fixed policy** and the declared
reward/observation/stop traces and finite-horizon returns. Actions and policy
alternatives are not preserved observations. No automatic partition discovery,
optimal-policy preservation, general MDP/POMDP abstraction, or performance claim
follows. EL-05 and D-077–D-085 remain Proposed; this is unreleased implementation
evidence, not release or decision acceptance.

## Run and inspect

```sh
cabal test Markovian-test --project-file=cabal.project.ci --test-show-details=direct
"$(cabal list-bin Markovian-test --project-file=cabal.project.ci)" --aggregation
python3 scripts/check-learning --run
bash scripts/check-aggregation-boundary
```

Displayed stdout is generated by the executed lesson:

```text
{{#include ../../learning/aggregation-output.txt}}
```

The runnable context below is dispatched by `test/Main.hs`. Tests compare the
existing original-model evaluator with separate recursion over public quotient
rows at six horizons and three discounts. A source-fixture path enumerator and
a separate quotient path enumerator retain reward sequences, every observation,
and terminal versus horizon stops; their laws are compared by exhaustive equality
classes, not by reusing production canonicalization. The lesson asserts the
values and witness before displaying them.

```haskell
{{#include ../../../test/AggregationExact.hs}}
```
