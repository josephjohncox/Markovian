# Markovian

Markovian is an experimental Haskell library for finite stochastic models,
exact evaluation, and bounded interpreters. It represents action choices
separately from probabilistic outcomes and preserves joint reward/successor
distributions.

The root package uses exact rational arithmetic and depends only on `base`.
Optional packages provide floating-point models, sampling, learning,
differentiation, tensors, and CUDA execution.

## Example

```sh
scripts/bootstrap-tools
direnv allow .
cabal run Sample --project-file=cabal.project.ci
```

The sample evaluates one transition with reward `2`, discount `1/2`, and
terminal payoff `7`. The expected return is `11/2`.

Bootstrap installs project-local tools. See the
[installation guide](docs/book/src/installation.md) for prerequisites, editor
setup, and builds without CUDA.

## Release status

The [published release](https://github.com/josephjohncox/Markovian/releases/tag/v2026.9.3.0)
is `2026.9.3.0`. Development packages retain that version while new APIs remain
unreleased; the [checked capability inventory](docs/book/src/capabilities.md)
distinguishes availability from decision status.

Accepted, unreleased additions include strict-discount affine feedback,
joint-affine substitution, polynomial quotation with cumulative compilation
budgets, and host-F64 affine views. The CE/CCE solvers are implemented and
unreleased; D-083 remains Proposed pending verification. CUDA multiply-chain
graphs remain unimplemented.

See the [release notes](RELEASE-NOTES.md), [migration guide](MIGRATION.md), and
[open work](TODO.md). Releases use coordinated UTC CalVer `YYYY.M.D.N` and
matching `vYYYY.M.D.N` tags.

## Present API surfaces

The [API map](docs/book/src/api-map.md) lists public modules. Select optional
packages according to the computation you need:

| Package | Purpose |
| --- | --- |
| `Markovian` | Exact finite distributions, MRP/MDP models, policies, Bellman solvers, beliefs, matrices, Bayesian inversion, circuits, finite games, and checked feedback. |
| `markovian-exact-benchmarks` | Bounded inventory fixtures and reproducible exact benchmarks. |
| `markovian-numerical` | Validated floating-point probabilities, distributions, and model values. |
| `markovian-sampling` | Seeded finite-support sampling and traces with explicit generator state. |
| `markovian-learning` | Tabular TD updates and bounded, resumable episodic runners. |
| `markovian-dense-exact` | Dense CPU interpretation of the exact circuit IR. |
| `markovian-continuous` | Exact algebraic continuous-kernel and event fragments, including joint-affine substitution. |
| `markovian-continuous-numerical` | Numerical companions to the restricted continuous fragments. |
| `markovian-reverse` | Bounded reverse programs built from supplied primitive VJPs. |
| `markovian-autodiff` | Differentiation of a closed polynomial/`tanh` language and exact-polynomial quotation. |
| `markovian-tensor` | Immutable host-F64 tensors, ownership, checked affine views, and primitive pullbacks. |
| `markovian-tensor-reverse` | Host tensor adapter for the bounded reverse interpreter. |
| `markovian-safetensors` | Metadata-free F64 serialization under a pinned SafeTensors profile. |
| `markovian-neural` | Numerical neural primitives and reference update algorithms. |
| `markovian-neural-bridge` | Checked exact action layouts and neural masks. |
| `markovian-gpu` | Positive-size F64 matrix multiplication and its VJP under the declared `sm_121` profile. |

The [architecture](docs/ARCHITECTURE.md) defines package boundaries, equality
contracts, ownership, and supported operations. The library does not provide
arbitrary-Haskell autodiff, general tensor/device graphs, unrestricted
equilibrium solving, or production training.

CUDA is disabled by default. See the
[GPU package](backends/markovian-gpu/README.md) for enabled builds, profile
checks, and the evidence required for hardware claims.

## Documentation

Read [the Markovian Book](https://josephjohncox.github.io/Markovian/) for concepts,
examples, laws, and counterexamples. Start with
[model construction](docs/book/src/model-semantics.md),
[exact evaluation](docs/book/src/exact-evaluation.md), or the
[learning guide](docs/book/src/sampling-learning.md). Haddock documents exact signatures
and error values.

Build the book locally:

```sh
scripts/install-doc-tools
scripts/check-book
```

Open `docs/book/build/index.html` after the build succeeds. The book's
[law catalogue](docs/book/src/laws-and-boundaries.md) and
[references](docs/book/src/references.md) cover the mathematical foundations.

## Inventory benchmark

```sh
cabal bench inventory-control-bench --project-file=cabal.project.ci
cabal bench clark-scarf-1960-bench --project-file=cabal.project.ci
cabal bench dogru-inventory-bench --project-file=cabal.project.ci
cabal bench fixed-batch-rnq-bench --project-file=cabal.project.ci
```

Each executable excludes one warmup and measures twenty complete runs with
identical semantic reports. The [inventory](docs/book/src/inventory-control.md)
and [fixed-batch](docs/book/src/fixed-batch-inventory.md) chapters state the
finite-model assumptions, independent oracles, and limits of widening and
truncation diagnostics. These are bounded model results, not published numeric
reproductions or general convergence results.

## Verification

Development uses GHC 9.14.1 and Cabal 3.18.1.0, pinned in
[`toolchain.env`](toolchain.env). GHC 9.8.4 builds standalone ancillary tools
only. HLS setup and its limitations are documented in the
[installation guide](docs/book/src/installation.md).

```sh
cabal build all --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
python3 scripts/check-learning --run
scripts/check-book
```

[CI](.github/workflows/ci.yml) defines the full checks for all 16 packages,
18 test suites, and 11 benchmarks, including source archives, compile-failure
boundaries, formatting, and Haddock. [Contributor workflows](docs/WORKFLOWS.md)
explain which checks apply to a change. CUDA compilation and protected hardware
execution have separate workflows.

## Project documents

- [Open work](TODO.md): priorities and remaining verification.
- [Repository context](docs/CONTEXT.md): current status and invariants.
- [Architecture](docs/ARCHITECTURE.md): contracts and package boundaries.
- [Decisions](docs/DECISIONS.md): technical decisions and acceptance records.
- [Changelog](CHANGELOG.md): released behavior.
- [Release checklist](RELEASE-CHECKLIST.md): preparation and publication checks.

Release preparation validates archives, checksums, SBOMs, and provenance. It
does not publish packages or create tags; publication requires explicit approval.
