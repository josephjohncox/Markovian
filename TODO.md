# Open development work

<a id="all-nine-execution-checklist"></a>

## Open work

The recorded backlog is complete. D-082 through D-085 and EL-03 through EL-06
are Accepted within their documented unreleased scopes. The
[CUDA graph contract](docs/plans/D082-CUDA-GRAPHS.md#protected-validation--2026-09-14)
records the verified hardware evidence and its exact tested revision.

See the [capability inventory](docs/book/src/capabilities.md) and
[decision log](docs/DECISIONS.md) for scope and evidence.

## Requirements for new work

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
