# G: Equal results do not imply equal admission

**Prerequisites:** [orientation](orientation.md), checked `Either` sequencing, and
[finite probability](probability-kernels.md). **Outcomes:** distinguish exact
denotation, source admission, first failure and executor cost; predict a
resource-sensitive association counterexample. **Next:** [circuit rewrite
certificates](circuits.md) and [laws and boundaries](laws-and-boundaries.md).

**Tempting claim:** if a cache gives the same answer faster, it preserves every
work-budget failure. This is false for actual-work admission. D-085 instead
selects **source-semantic admission**, with separately bounded executor work.
D-085 remains **Proposed**; this lesson is an **accounting-model experiment, not
a cache implementation**, benchmark or optimizer certificate.

## Run and predict

```sh
cabal build Markovian-test --project-file=cabal.project.ci
"$(cabal list-bin Markovian-test --project-file=cabal.project.ci)" --resource-admission
python3 scripts/check-learning --run
```

The normal root test suite also runs this lesson and its boundary tests. Source
CI and archive-only CI discover it through `docs/learning/fences.json`; the
learning gate compiles the complete source below and compares executed stdout.
Every displayed answer is asserted, not copied from an unchecked calculation.

## Two accounts, one source

**Hypotheses:** the closed `Hundred` program writes the exact value 1 in each of
100 ordered steps. Its scalar denotation is its last value, 1. `runModel` executes
that same source validation for all three path labels. It does **not** look up,
construct or retain any matrix/cache entry. The scalar is a tiny accounting
example, not an implementation of the retained-circuit interpreter.

Declared executor costs are 100 for reference execution, 102 for construction
(100 source-execution units plus two infrastructure units), and **one hypothetical
unit inclusive of lookup/replay** for a hit. These are assumed model units,
not Haskell instruction counts, time or allocation. Real trace replay costs work;
a real implementation has not demonstrated one-unit replay of this source.

| Source budget | Executor budget | Reference | Hypothetical hit |
| --- | --- | --- | --- |
| 10 | sufficient | source failure at charge 11 | same source failure at 11 |
| 99 | sufficient | source failure at charge 100 | same source failure at 100 |
| 100 | sufficient | value 1, semantic 100, executor 100 | value 1, semantic 100, executor 1 |
| 100 | 10 | infrastructure failure, saturated sentinel 11 | value 1, semantic 100, executor 1 |

The final row is **not** equal failure behavior: executor admission differs.
With actual-executor-work admission alone, ten would allow only the hypothetical
hit. That is the rejected alternative, not the selected source policy.
Infrastructure preflight precedes source validation; when it fails, there is no
source result or partial account. The model's exact structural, bit and failure
orders are [frozen separately](../../plans/EL-06-RESOURCE-ADMISSION.md).

**Exercise:** what does construction followed by one hit cost in one request?
**Hint:** do not charge construction twice as both build and first use; do not
reset either account on the hit. **Checked answer:** semantic 200, executor 103.
Semantic 199 fails at charge 200; executor 102 fails with sentinel 103. The model
also reserves 200 request trace slots, not an actual cache's resident size.

## Why a warm answer cannot bypass validation

The proposed key includes explicit interpreter identity, source circuit identity,
both **ordered** endpoint layouts, and every semantic limit. Changed limits
**miss**. The model checks this with a closed key-equality test; it implements
neither lookup nor identity/purity certification. Tokens stand for trusted,
frozen identities; equal labels alone cannot establish equal interpreters.

A real entry successfully built with budget 100 would miss at budget ten. Under
the success-only retention policy an entry for this source cannot have been
built at ten. The hypothetical-hit label in the first row tests the required
admission relation even if a hit were offered; it is not evidence that such an
entry exists. No failed or partial table is retained by this experiment.

Only pure, deterministic, terminating primitive callbacks with stable identity,
limits and replayable semantic evidence could be admitted to a future cache.
Hidden state, IO, opaque internal costs and unrepresented limit-dependent
behavior are excluded. The model uses a closed instruction set, **no callbacks**.
A primitive-refusal instruction verifies its position relative to work failures;
it is not a cached failure or evidence for arbitrary callback replay.

## Discarded values and first failures

Compare the ordered write traces `[1/256, 0, 0]` and `[0, 1/256, 0]`. Each ends
at zero, uses three semantic units and has maximum denominator size nine bits.
`CancelEarly` and `CancelLate` overwrite a temporary; they do not perform a
floating cancellation or optimize away arithmetic.

**Exercise:** use semantic work limit one and denominator limit eight. Which
failure comes first? **Hint:** charge each write **before** inspecting its
rational. **Checked answer:** the first trace fails on denominator size nine at
write one. The second fails on work charge two before inspecting that rational.
Thus final value, total work and even maximum rational size cannot reconstruct
first-failure order. Checking only the retained zero would miss both discarded
values. `DiscardBoth` also tests nine-bit numerator/denominator boundaries and
numerator-before-denominator precedence. Tests check primitive refusal before
later work, later refusal after exhausted work, and same-step work precedence.

## Association with the actual checked-bind API

This part calls **`bindExactFiniteDistChecked`**, not the model. Let `m` be a fair
draw from `[0,1]`, `f x` a fresh fair draw from `[x,x+1]`, and `g y` the certain
value `3*y`. When admitted, both association orders return the exact ordered
entries `[(0,1/4),(3,1/4),(3,1/4),(6,1/4)]`; the duplicate 3 is retained.
These literal masses are the independent four-path enumeration.

For `(m bind f) bind g`, the two public calls cost **12 and 16** units. For
`m bind (x -> f x bind g)`, the outer call costs **12**, and each inner call
costs **8**. A call charges one outer traversal, one continuation invocation,
and two units per result entry (traversal and multiplication). Report fields
therefore independently reconstruct `2*outer + 2*result`.

**Exercise:** give every public call work limit 12, support limit four, numerator
limit one and denominator limit three. Are both associations admitted?
**Checked answer:** no. The left second call fails at work 13; the right outer
and both inner calls succeed. With the left second limit raised to 16, the
literal result entries agree. The fixture executes both branch receipts too.

These are **separate public operations with local limits**, as documented by
checked bind. Inner work is not silently included in an outer report. Both
whole expressions happen to total 28 units here, but this is not an operation-wide
optimizer meter. `runModel`, by contrast, threads one cumulative account across
all source occurrences. The lesson neither changes checked bind nor constructs
a fresh-budget wrapper and calls it cumulative. Association changes source
syntax and admission, so D-085 may not silently reassociate it. No unrestricted
`Monad`, universal optimization law or performance claim follows.

## Checked output

Rationals print numerator and denominator with `%`. `executorReserved` is the
separate **declared reservation**, never measured execution work. `Left` carries
only the first failure, with no partial values or account.

```text
{{#include ../../learning/resource-admission-output.txt}}
```

## Complete runnable source context

The private model types do not add a public API. `test/Main.hs` dispatches the
lesson and runs all five named resource checks in the ordinary root suite.

```haskell
{{#include ../../../test/ResourceAdmission.hs}}
```

This checks a bounded accounting relation and one real-API counterexample.
D-085's future exact matrix equality, opaque owned cache entries, interpreter
certification, actual trace compression and time/allocation measurements remain
unimplemented and require their own review. No package or dependency edge is
added.
