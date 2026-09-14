# Open development work

<a id="all-nine-execution-checklist"></a>

## Open work

The remaining implementations are present and unreleased. Acceptance is pending
final integration and evidence review:

- [D-082 CUDA graphs](docs/plans/D082-CUDA-GRAPHS.md): CPU review passed;
  same-session hardware and sanitizer validation remains.
- [D-084 reference trainer](docs/plans/D084-DQN-TRAINER.md): independent review
  and final publication-boundary checks are in progress.
- [D-085 retained circuit cache](docs/plans/D085-CIRCUIT-CACHE.md): independent
  review passed; archive and integration checks remain.
- EL-03 through EL-06: independent review supports the documented bounded
  scopes; a reviewed status update remains.

See the [capability inventory](docs/book/src/capabilities.md) for current status
and the [decision log](docs/DECISIONS.md) for accepted work.

## Requirements for new work

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
