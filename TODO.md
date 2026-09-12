# Open development work

Next: complete D-083 solver verification. Its implementation and public/private
fixtures exist; the remaining evidence is listed below.

D-077 through D-081 are Accepted within their unreleased scopes. EL-03,
EL-04, and EL-05 are implemented but remain Proposed. See the
[capability inventory](docs/book/src/capabilities.md) for status and
[decision log](docs/DECISIONS.md) for completed work.

<a id="all-nine-execution-checklist"></a>

## Open work

### D-083: CE and CCE solvers (next)

`Markovian.Game.Correlated.Exact` implements the
[CE/CCE contract](docs/plans/D083-CE-CCE-SOLVERS.md). D-083 remains Proposed.
The accepted placement adds no package or dependency edge.

- [ ] Instrument the constructor/checker's ordered Rational intermediates and
  compare them with the shadow sequence (§11.8). `testCheckerRowSequenceAgreement`
  compares returned rows but misses discarded intermediates.
  `correlationSolveCheckerCoveredRationalBits` currently comes from the shadow,
  without an actual checker measurement.
- [ ] Complete redundant-row, degeneracy, rank-deficiency, inconsistency, and
  tiny-vertex controls (§11.5). Compare production builders and witness
  inequalities with independent references, including private infeasible
  traversal and public budget exhaustion.
- [x] Observe candidate-count exhaustion before matrix construction and elimination,
  and exact/one-below final successor reservations in the production search
  loop (§11.8).
- [ ] Complete remaining atomic-failure, report-length, and competing-failure
  checks (§11.8).
- [ ] Complete source-loop, reservation, and strictness evidence.
- [ ] Obtain independent review of placement, solver behavior, accounting, and
  evidence; record any acceptance in a separate reviewed decision update.

A completed public search without a witness is an invariant failure. These
solvers make no Nash or unrestricted equilibrium-solving claim.

### D-082: CUDA multiply-chain graphs

D-082 is Proposed and unimplemented. D-077 and D-081 prerequisites are
satisfied; they provide no graph or deployed-revision evidence.

- [ ] Freeze and review graph signatures, admitted nodes/views, ownership,
  schedule, cleanup, ledgers, and failure precedence.
- [ ] Implement the reviewed matrix-input/view/multiply DAG and its declared VJPs.
- [ ] Test graph, transfer, payload, work, and launch limits; cleanup; and every
  forward/VJP coordinate against independent dyadic and CPU references.
- [ ] Obtain independent review and same-session hardware/sanitizer evidence
  under the [GPU evidence policy](docs/WORKFLOWS.md#gpu-deployment-evidence).
  Hardware workflows require separate authorization.
- [ ] Record any acceptance in a separate reviewed decision update. Deployment
  evidence must identify the deployed revision.

### D-084: reference DQN trainer

D-084 is Proposed and unimplemented. The existing checked batch update is the
implementation basis.

- [ ] Freeze and review signatures, fuel, replay/checkpoint ownership, event
  order, terminal/truncation rules, RNG advancement, resumption, and failures.
- [ ] Implement the reviewed resumable trainer.
- [ ] Test split-run equality, masks, replay warming/eviction/order, target
  schedules, standard/Double-DQN, callback failures, atomic updates, and
  exact/one-below limits.
- [ ] Obtain independent review with semantic step evidence and separately
  scoped timing results; record any acceptance in a reviewed decision update.

Production, convergence, device, and distributed-training claims remain excluded.

### D-085: retained-circuit trace cache

D-085 is Proposed. The [EL-06 resource policy](docs/plans/EL-06-RESOURCE-ADMISSION.md)
and lesson are implemented; the concrete cache is not.

- [ ] Freeze and review table/cache/interpreter/trace signatures, key identity,
  ordered validation replay, dual ledgers, and failure precedence.
- [ ] Implement the named acyclic retained-circuit consumer. Preserve full
  source charges and separately bounded executor costs.
- [ ] Test discarded-intermediate checks, source failure order, key separation,
  changed-limit misses, no partial entries, exact uncached equality, and
  exact/one-below limits.
- [ ] Obtain independent review using the same workload for cached and uncached
  execution. Record raw time, allocation, hit, and table-size measurements;
  EL-06's hypothetical hit cost is insufficient.
- [ ] Record any acceptance in a separate reviewed decision update.

Universal normalization and optimization claims remain excluded.

## Requirements for new work

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
