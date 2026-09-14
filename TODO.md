# Open development work

<a id="all-nine-execution-checklist"></a>

## Open work

All recorded implementations are present. [D-082 CUDA graphs](docs/plans/D082-CUDA-GRAPHS.md)
still require protected same-session hardware/sanitizer evidence and a reviewed
acceptance update. The trainer, retained cache, and EL-03 through EL-06 have
passed independent review and are Accepted within their unreleased scopes.

See the [capability inventory](docs/book/src/capabilities.md) and
[decision log](docs/DECISIONS.md) for scope and evidence.

## Requirements for new work

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
