# Law laboratory

**Prerequisites:** follow one of the [three routes](learning-routes.md), read
[orientation](orientation.md), and build with the pinned toolchain.
**Outcomes:** refute six tempting identifications below and compare resource
admission in the linked seventh experiment with small checked computations,
state each experiment's hypotheses, and read a failure witness rather than
silently extending a theorem. **Next:** [laws and proof boundaries](laws-and-boundaries.md).

## Run, predict, inspect

From the repository root:

```sh
python3 scripts/check-learning --run
python3 scripts/test_learning.py
```

The first command compiles every registered fragment context and runs `Sample`
and both laboratory modules through existing Cabal components. It rejects stale
shown output. To run just the laboratory after building:

```sh
"$(cabal list-bin Markovian-test --project-file=cabal.project.ci)" --learning
"$(cabal list-bin markovian-tensor-test --project-file=cabal.project.ci)" --learning
```

All numerical answers below are asserted before being printed. Full fixtures
are included at the end, not substituted by a second demonstration semantics.
The displayed output is generated from those successful executions.

## A: Shared randomness and determinism

**Tempting claim:** drawing once and copying is the same as executing a draw twice.
**Hypotheses:** a fair Boolean kernel from the singleton object; product order
`(False,False), (False,True), (True,False), (True,True)`; exact rational mass.

`labA` compares `coin` followed by `copyStochastic` with copying the singleton
and executing `tensorStochastic coin coin`. The first produces masses
`[1/2,0,0,1/2]`; the second produces `[1/4,1/4,1/4,1/4]`.
The off-diagonal events are concrete distinguishing witnesses.

**Exercise:** does the same equation fail for Boolean negation?
**Hint:** negation introduces no random draw. Follow one input through both sides.
**Checked answer:** it holds. `labA` constructs `deterministicFromFunction bits bits not`,
embeds that witness, and checks the copy equation. This fixture is an instance
of the proof-carrying deterministic law, not a universal stochastic copy law.

## B: Transpose is not Bayesian inversion

**Tempting claim:** reverse a channel by transposing its matrix.
**Hypotheses:** source order `[False,True]`, observation order `[0,1,2]`, channel
rows `[1,0,0]` and `[1/2,1/2,0]`, prior `[1/4,3/4]`.

`labB` computes the transpose, calls `condition`, and constructs `bayesianInverse`.
The first transpose row is `[1,1/2]`, whose sum is 3/2, not one. The evidence for
observation 0 is `1/4 + (3/4)*(1/2) = 5/8`, so its posterior is `[2/5,3/5]`.
The checked inverse rows are `[[2/5,3/5],[0,1]]`, in supported observation
order `[0,1]`; it does not manufacture an observation-2 row.

**Exercise:** what happens if you condition on observation 2?
**Hint:** compute the denominator before dividing.
**Checked answer:** `Left (ZeroEvidence 2)`. `labB` pattern-matches that exact
constructor. A label outside the target is a different boundary; see the
independent `testBayesianConstruction` fixture in `test/BayesianExact.hs`.

## C: Reward and successor correlation

**Tempting claim:** separate reward and successor marginals preserve the model.
**Hypotheses:** one continuing state Live, terminal Done with payoff 4, one action,
horizon 1, discount 1/2. The joint outcomes are `(reward 2, Done)` and
`(reward 0, Live)`, each of mass 1/2. Compare their product marginals: four
outcomes with mass 1/4 each.

`labC` constructs both with `exactFiniteDist` and evaluates their actual MDPs.
Both additive expected returns are 2: expected immediate reward 1 plus discounted
expected terminal payoff 1. That equality does **not** preserve the joint law.
The observable `reward * indicator(successor == Done)` has expectations 1 and
1/2 respectively. The exact trace supports have sizes 2 and 4.

**Exercise:** can this one-step additive expected return detect correlation loss?
**Hint:** linearity separates the two marginal expectations, but not their product.
**Checked answer:** no; `[2,2]` is asserted by the evaluator fixture. The cross
moment and the reward-labelled traces distinguish the models. This does not
assert that arbitrary multi-step policies or nonlinear return observables agree.

## D: Terminal and horizon timing

**Tempting claim:** zero remaining transitions always implies zero return.
**Hypotheses:** Live goes to Done with reward 2, Done has terminal payoff 7,
discount 1/2. Compare horizon zero at Live, horizon zero at Done, and horizon one
at Live.

`labD` calls `expectedExactReturn` and `exactTraceDistribution`. The returns are
`[0,7,11/2]`. From Live, the zero-step trace has `HorizonStop` and no transition;
the one-step trace has `TerminalStop 7` and exactly one realized transition.
Terminal status is checked before the horizon boundary, and the reached payoff
is included once, discounted by that transition.

**Exercise:** does the zero-step continuing trace contain a selected action?
**Hint:** inspect `traceSteps` rather than inferring a step from the policy value.
**Checked answer:** no: the checked step counts are `[[0],[1]]`. A policy object
existing in memory is not an action-selection event.

## E: Exact equality and floating reassociation

**Tempting claim:** associativity of real addition licenses reassociating F64 code.
**Hypotheses:** `a=10^16`, `b=-10^16`, `c=1`, scalar finite F64 tensors. Each addition
is a distinct checked `add` primitive, with no compiler reassociation across it.
The exact comparison uses Haskell Rational arithmetic, not an unlawful Double
instance of the exact semiring classes.

The tensor API gives `(a+b)+c = 1` and `a+(b+c) = 0`. Rational gives 1 on both
sides. All intermediates here are finite; nonfinite-result rejection is a
separate boundary. No tolerance-based equality or algebraic reassociation claim
is inferred from these integer-looking payloads.

**Exercise:** which intermediate loses the unit?
**Hint:** the spacing of representable numbers near `10^16` exceeds one.
**Checked answer:** `b+c` rounds back to b on this F64 fixture; that intermediate
is asserted directly. The checked final pair is `([1.0],[0.0])`. Changing
operation order changes the experiment.

## F: Logical coordinates, storage and derivatives

**Tempting claim:** sharing storage makes a view and its base interchangeable inputs.
**Hypotheses:** a 2-by-3 base with row-major values `[1,2,3,4,5,6]`; its 3-by-2
transpose view; cotangent seed `[1,2,3,4,5,6]` in **view** coordinate order.

The view has logical values `[1,4,2,5,3,6]` and shares the base storage. Its
`contiguousCopy` has the same logical values but fresh storage. The fixture
calls `multiplyWithTape view view` and applies the binary tape with that seed.
Because the two primitive inputs are the same logical variable, their two
cotangents are added. The resulting primitive-on-view gradient is
`[2,16,12,40,30,72]`, namely `2 * view * seed` coordinatewise.

**Exercise:** is this already the gradient in base order?
**Hint:** pull back the transpose map as well; under the Euclidean pairing its
adjoint transposes the cotangent back.
**Checked answer:** no. Explicitly applying `transposeFinite2D` to the logical
gradient gives base order `[2,12,30,16,40,72]`. This is an explicit chain-rule
composition, **not** a claim that the primitive tape recorded the view-to-base
map. The fixture independently perturbs all six base coordinates in a literal
weighted-square objective. Central differences with step 1/1024 agree exactly
for these dyadic quadratic computations. That exactness is fixture-specific,
not a general finite-difference theorem.

`viewReverseEquivalenceTests` in `packages/markovian-tensor/test/Main.hs` also
checks materialize-first equivalence and independent coordinate differences
for add, multiply, matmul and tanh. Materialize-first comparison alone would
be a circular oracle; the independent formulas and perturbations supply the
additional evidence. No CUDA execution or arbitrary view autodiff is claimed.

## G: Resource admission and association

The [seventh laboratory](resource-admission.md) checks the 100-source-unit versus
one-hypothetical-executor-unit example under budget ten, ordered discarded
rational failures, and an association-sensitive admission contrast using the
real checked-bind API. Its two-account experiment is explicitly a model, not a
cache implementation; its public binds retain their separate per-call budgets.

## Checked output

Exact fixtures (Haskell prints rational numerator and denominator with `%`):

```text
{{#include ../../learning/exact-output.txt}}
```

Host tensor fixtures:

```text
{{#include ../../learning/tensor-output.txt}}
```

## Complete runnable source contexts

A–D are executed by `Markovian-test --learning`; `main` dispatch is in `test/Main.hs`.

```haskell
{{#include ../../../test/LawLaboratory.hs}}
```

E–F are executed by `markovian-tensor-test --learning`; they also run in the normal
suite. All payloads and tapes stay inside a checked session.

```haskell
{{#include ../../../packages/markovian-tensor/test/TensorLawLaboratory.hs}}
```

## Evidence boundary

The small exact references are literal finite enumeration and hand-computed
masses, not calls back into production evaluation to manufacture expected values.
Related independent references remain `test/AlgebraicFoundation.hs`,
`test/BayesianExact.hs`, `test/ExactControl.hs` and the tensor test module above.
These are regression experiments under stated hypotheses, not proofs of all
possible inputs. The separately linked EL-06 accounting model checks its stated
source-admission relation; no checked-bind Monad or universal optimizer law follows.
