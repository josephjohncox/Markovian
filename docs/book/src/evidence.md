# Evidence records

The repository keeps durable command evidence for hardware-specific and proof-boundary claims.

## D-061 package-boundary evidence

[D-061 implementation evidence for 2026-09-02](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/D061-2026-09-02.md) records the root topology checks, package and component counts, exact bind boundaries, source-archive ownership, and the unpacked 14-package test result. It is historical local implementation evidence. It does not accept D-061 or replace hosted release gates.

[Integration and documentation repair evidence](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/INTEGRATION-DOCS-REPAIR.md) records the checked 16-package graph, 18-suite and 11-benchmark manifest, GHC 9.8.4 suite run, bounded archive validation, one isolated GPU dependency-closure run, and the gates that remain open. It does not accept a decision or establish release readiness.

[Tensor allocation and SafeTensors repair evidence](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/TENSOR-SAFETENSORS-REPAIR.md) records deterministic allocation-fault, malformed-corpus, raw-IEEE, canonical-byte, boundary, and two-package unpacked-archive results for the dirty 16-package overlay. It does not accept D-072 or D-073 or replace complete release evidence.

## CUDA evidence

[CUDA evidence for 2026-08-26](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/CUDA-2026-08-26.md) records:

- The revision and host.
- The CUDA toolkit and driver.
- Differential test output.
- Transfer-inclusive benchmark samples.
- The warmup and sample policy.
- PTX regeneration commands.
- Historical measurements with explicit labels.

The measurement proves one local execution. It is not a general GPU performance claim.

## Acyclic open-system evidence

[S6 acyclic open-system evidence for 2026-08-27](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/S6-ACYCLIC-OPEN-2026-08-27.md) records:

- The supported semantic scope.
- Package and compiler checks.
- Exact law fixtures.
- Rejection fixtures.
- Differential comparisons.
- Compile-fail proof boundaries.
- Deferred research claims.

## Clark--Scarf finite-specialization evidence

`test/ClarkScarf1960.hs` checks the page/equation-crosswalked finite specialization against three exact paths: generic exact-MDP backward induction, direct equation (14), and equations (15), (20), (21), and (26). It also evaluates the decomposed policy in the joint model and requires zero exact regret. The deterministic golden report records the inspected scan checksum, source assumptions, repository finite assumptions, retained and omitted mass, explicit budgets, and cap-widening diagnostics.

The paper supplies no numeric oracle. This is equation-level differential evidence for one finite lattice, not a published-value reproduction, continuous approximation result, average-cost theorem, or unbounded proof.

## Fixed-batch finite and stationary evidence

`test/FixedBatchRnQ.hs` checks the BETA Working Paper 134 event order, physical integer-ratio batches, exact supplier delay, same-demand cost/successor outcomes, conditioned-demand mass, complete finite layouts, independent finite-horizon oracle and fixed-policy recursions, exact subsystem shortfalls, forward differences, weak and strict discrete inequalities, unit batches, and all computation budgets. It includes a finite-horizon-versus-stationary counterexample. The golden report keeps bounded execution and stationary newsvendor results in separate sections and labels widening and omitted mass as diagnostics rather than error bounds.

`fixed-batch-rnq-bench` excludes one warm-up and measures twenty complete runs. It requires every run to reproduce the same semantic report. Timing is local reproducibility evidence only; there is no speedup or convergence claim.

## Finite open-game evidence

`test/FiniteOpenGames.hs` checks bounded total tables and function spaces, concrete optic formulas, owner-disjoint sequential and tensor composition, exact rational ties, pure equilibria for prisoner's dilemma and coordination, no pure equilibrium for matching pennies, and a sequential non-credible-threat counterexample. It exhausts every represented two-player `2 x 2` payoff table over `{0,1}` and compares the result with independent unilateral-deviation enumeration. A separate counterexample verifies that observational equality cannot ignore best-response membership. Golden reports contain deterministic counts and exact utilities with no timing data.

This evidence applies only to represented finite pure contextual equilibria. It supplies no equilibrium-existence, mixed, correlated, repeated, stochastic, Bayesian, continuous, or subgame-perfect result.

Use an evidence record when a result depends on hardware, a benchmark method, or a large verification campaign.
