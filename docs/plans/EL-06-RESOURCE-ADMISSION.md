# EL-06 — Source-semantic admission policy

**Status:** Proposed

EL-01 freezes this policy for later EL-06 teaching and D-085 design. The checked experiment and full cache signatures/implementation remain unimplemented. D-085 remains Proposed. No new package edges or publication authority follow.

## Four distinct contracts

1. **Denotation:** successful retained and reference paths return literally equal exact matrices on identical endpoint layouts under one frozen pure primitive interpreter.
2. **Admission:** the semantic work budget measures the uncached source algorithm's ordered charges and validations, not the cost of cache lookup. A hit replays those charges cumulatively. Rational admission includes discarded intermediates, not just final matrix bits. Source syntax association and primitive semantic limits are part of this contract.
3. **Error precedence:** source structural preflight, then source ordered work/rational/primitive validations have the same semantic first failure when both executors are admitted. Cache infrastructure has a separate error class and must not masquerade as a source failure. Infrastructure preflight (cache entry/cell/trace size and executor bounds) happens first; if it fails, no source result is claimed. Thus equality is a conditional relation on admitted executors, not unconditional equality of all Left values.
4. **Execution cost:** cache allocation, construction, table lookup and source-validation replay have separately bounded executor counters and measured time/allocation. These may differ from reference execution. Semantic work charged remains equal. A replay costs real executor work; do not call it a free validation. A bound that cannot cover the retained trace rejects before allocating or replaying it.

## The 100/1/10 example

Assume the source algorithm requires 100 semantic work units and the hypothetical hit costs one executor unit, with sufficient executor/storage allowance. Under a semantic work budget of ten, **both paths reject** at the same source charge boundary; the cache cannot admit the source. Under semantic budget 100 both can succeed. A ten-unit *executor* budget could reject reference execution and admit the hypothetical hit, but that is an explicitly different infrastructure outcome. One executor unit is illustrative, not a claim about the cost of validating a real retained trace. An actual-work admission policy would admit only the hit under a ten-unit work budget and would violate the chosen equal-semantic-admission promise.

## Construction, identity and evidence

A cache key is explicit interpreter identity, retained circuit identity, exact endpoint layouts, and **all** semantic limits. Never compare Haskell closures or infer equality from labels alone. With this first contract, changed limits miss rather than reuse evidence across an unreviewed admission boundary. Cache construction and repeated calls use one request-wide semantic account and a separate request-wide executor account; neither resets between local circuits. Per-key construction is charged as that occurrence's source execution, not charged twice as both build and first use. Later hits incur that source trace again plus their own executor work.

Only successful complete exact tables and their ordered validation traces are retained. Construction failure returns no partial entry. Do not cache arbitrary failures. Replay must reproduce each source semantic check at its original position, including primitive failures if a cumulative limit causes one, and the first exceeded cumulative budget. Scalar maxima and total charge alone cannot preserve order-dependent failure precedence; a compact trace is allowed only after proving that it preserves this relation. Trace storage and replay must themselves be bounded. A warm table cannot suppress an earlier source validation error.

The selected interpreter must supply pure, deterministic, terminating callbacks, stable under its explicit identity and the same limits; it must expose enough checked semantic evidence to replay their source contract. Callbacks with hidden state, nondeterminism, IO, opaque unaccounted internal costs, or limit-dependent behavior not represented in that evidence are outside the admissible first implementation. No extensional test can establish arbitrary closure purity or identity. The retained-circuit consumer remains `Markovian.Open.Acyclic.Circuit.Exact`; successful sharing means reuse of exact denotation, not shared stochastic execution of different edge occurrences.

## Remaining EL-06 evidence

Implement a bounded executable model of source and executor accounts with the 100/1/10 case, exact/one-below boundaries, a changed-limit miss, and a discarded-rational/order counterexample to aggregate-only replay. Generate and check its displayed results. This may reuse checked-bind association fixtures to separate equal admitted denotation from differing association-sensitive admission; it must not claim an unrestricted Monad instance. Freeze D-085's concrete cache and interpreter-evidence signatures only when its consumer can satisfy this source contract. None of those future cache surfaces are introduced by EL-01.
