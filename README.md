# Markovian

Markovian is an experimental Haskell package for finite stochastic kernels, Markov reward processes, Markov decision processes, policies, and bounded interpreters.

The package is greenfield and unreleased. It makes no compatibility promise. Incorrect interfaces are removed rather than retained behind shims.

## Implemented semantics

- opaque validated floating probabilities, weights, finite distributions, rewards, discounts, and horizons;
- separate rational reference values for literal equality and law tests;
- one-layer floating and exact stochastic kernels;
- MRP and MDP interfaces with action IDs distinct from transition outcomes;
- validated policy closure that preserves joint reward and successor distributions;
- exact finite-horizon expected-return evaluation with explicit model, policy, horizon, and discount;
- explicit seeded finite-support sampling with returned generator state;
- action-labeled traces with terminal and horizon stop reasons;
- exact trace enumeration whose expected return matches direct evaluation;
- validated finite state and action compilation with unindexed-successor rejection;
- exact finite-horizon dynamic programming and discounted Bellman policy evaluation;
- validated tabular Q-values, constant schedules, and pure terminal-aware updates;
- seeded bounded epsilon-greedy episodic Q-learning with deterministic traces;
- canonical exact finite beliefs with post-transition prediction and conditioning;
- exact bounded belief-state policy evaluation with mixed-termination rejection;
- duplicate-free finite sets, including empty sets, plus nonempty finite-object refinements;
- lawful semiring, involution, exact positivity, and convex scalar contracts with an opaque nonnegative rational implementation;
- opaque source-by-target semiring matrices with checked indexing, composition, tensor, biproduct, transpose, conjugate transpose, compact structure, and trace;
- exact normalized stochastic matrices, proof-carrying deterministic matrices, and exact convex mixtures;
- exact priors, positive supports, pushforward, joints, conditioning, and support-restricted Bayesian inversion;
- checked Bayesian channels with explicit prior flow and prior-indexed almost-sure equality;
- raw purity-indexed stochastic-circuit syntax with explicit sharing, fanout, structural maps, and exact convex choice;
- first-order deterministic categorical compilation from finite quoted tables;
- finite typed hypergraphs, explicit quotient pushouts, structured cospans, and commuting open-system cells;
- directed circuit-decorated open topology with no invented reverse or graph black-box denotation;
- a finite symmetric monoidal Markov IR with explicit object witnesses, full-tensor copy, fanout, symmetry, associators, and unitors;
- standard probability-monad, Kleisli `Category`, `Arrow`, and `ArrowChoice` instances;
- dense rational CPU lowering with denotational differential tests;
- structured model, policy, sampling, compilation, solver, arithmetic, normalization, and conditioning errors.

Raw matrices can use empty objects. The vacuous empty-to-empty stochastic arrow is also valid, but a stochastic arrow from a nonempty source to an empty target is not. Normalized states, distributions, priors, and other probability-bearing finite objects remain nonempty. Both finite-witness modules export `sameFiniteLayout` as the canonical layout comparison. `sameFiniteSetLayout` and `sameFiniteObjectLayout` remain descriptive aliases. `matrixEquivalent` is labelled extensional equality; `sameMatrixLayout` compares the represented witnesses and row layout. Stochastic matrices deliberately have no transpose, dagger, compact, trace, or raw-addition API because those operations do not generally preserve normalization. Nominal roles protect stochastic, deterministic, and convex proofs from `coerce`. Copy-naturality reasoning requires the proof-carrying deterministic refinement.

Bayesian inversion is prior-indexed and maps positive output support to positive input support. It does not fill zero-evidence rows and is not matrix conjugate transpose. `BayesianChannel` composition checks its middle prior and has no plain `Category` or dagger instance. Exact POMDP filtering delegates to the same pushforward and conditioning algebra.

Circuit purity records provenance. Only deterministic syntax can use copy-naturality optimization. `shareCircuit` performs one stochastic execution and copies its result; `fanoutCircuit` performs conditionally independent branch executions. Exact circuit interpretation and dense CPU lowering share one nonnegative-rational matrix denotation. Floating, CUDA, and neural backends require an explicit approximation relation and do not inherit exact-law claims.

The deterministic compiler supports identity, composition, products, pairing, projections, and finite quoted tables. It does not compile arbitrary Haskell functions or provide stochastic cartesian closure.

Open systems use structured cospans of finite typed directed hypergraphs. Sequential composition is an explicit finite pushout; tensor is disjoint union. Binary quotient members have canonical left-then-right order, and cocones compare interfaces by typed support rather than layout. Higher cells are commuting squares with type-, label-, order-, and incidence-preserving apex maps. Boundary reversal swaps cospan legs only and retains the original directed circuit state orientation. It is separate from matrix conjugate transpose and Bayesian inversion.

`OpenCircuit` attaches one directed circuit decoration. Only that decoration has exact stochastic denotation. Internal graph labels, cycles, and feedback are not black-boxed, and no continuous-time open-Markov theorem is claimed for MDPs.

The semantic core depends only on `base`. GPU runtimes and neural contracts remain outside it in separate packages:

- `backends/markovian-gpu` provides an optional CUDA 13 driver backend, CPU/GPU differential tests, and a transfer-inclusive benchmark;
- `backends/markovian-neural` provides stable-softmax normalization, analytic Jacobian, score-function estimator, and approximation contracts without selecting a tensor framework.

The CUDA package flag is disabled by default so ordinary builds require no GPU toolkit. On a CUDA host, run:

```sh
cabal test markovian-gpu-test --project-file=cabal.project -fcuda --test-show-details=direct
cabal bench markovian-gpu-bench --project-file=cabal.project -fcuda
```

The 2026-08-26 repair ran both commands on an NVIDIA GB10. The host used driver 580.173.02 and compute capability 12.1. The worktree was based on commit `22796e4fb1998729eeed075fb47d31ef4f35b5a6`.

The differential test passed. The benchmark used a 256-by-256 identity matrix, `Double` values, 20 runs, and no random seed. It measured a transfer-inclusive mean of `267.236742 ms` and a maximum error of `0.000e0`.

The original P6 record at `faa5bd4` measured `295.110287 ms`. The prior PR verification for `22796e4` measured `265.395672 ms`. These variable local measurements are not general performance claims.

CUDA 13.0 `nvcc` V13.0.88 was available at `/usr/local/cuda/bin/nvcc`. This command reproduced the committed PTX files exactly:

```sh
backends/markovian-gpu/scripts/build-ptx
```

## Example

```sh
scripts/bootstrap-tools
direnv allow .
cabal run Sample --project-file=cabal.project.ci
```

The sample evaluates one exact transition with reward `2`, discount `1/2`, and terminal payoff `7`. Its expected return is `11/2`.

## Verification

The package tests GHC 9.4.8 and 9.8.4. The required gates are:

```bash
for dir in . backends/markovian-gpu backends/markovian-neural; do
  (cd "$dir" && cabal check)
done
cabal build all --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
set -o pipefail
cabal haddock all \
  --project-file=cabal.project.ci \
  --haddock-all \
  --haddock-hyperlink-source 2>&1 | tee haddock.log
! grep -nE '(^|[[:space:]])Warning:' haddock.log
cabal build all --project-file=cabal.project.ci --prefer-oldest
cabal test all --project-file=cabal.project.ci --prefer-oldest
hlint src backends/*/src
find src app test backends -type f -name '*.hs' -print0 \
  | sort -z \
  | xargs -0 fourmolu --mode check
bash -n \
  scripts/bootstrap-tools \
  scripts/check-refinement-roles \
  scripts/check-circuit-purity \
  backends/markovian-gpu/scripts/build-ptx
scripts/check-refinement-roles
scripts/check-circuit-purity
cabal-fmt --check Markovian.cabal \
  backends/markovian-gpu/markovian-gpu.cabal \
  backends/markovian-neural/markovian-neural.cabal
```

Fourmolu 0.20 does not parse the repository's three LaTeX-style literate Haskell files. CI excludes only those `.lhs` files from Fourmolu; GHC and HLint still check them. CI also builds and tests the unpacked source distribution.

## Project documents

- [TODO.md](TODO.md) is the prioritized implementation plan.
- [docs/CONTEXT.md](docs/CONTEXT.md) is the current repository state.
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) defines semantic contracts and boundaries.
- [docs/DECISIONS.md](docs/DECISIONS.md) records accepted decisions and proof obligations.
- [docs/WORKFLOWS.md](docs/WORKFLOWS.md) defines required evidence and change procedures.
- [CHANGELOG.md](CHANGELOG.md) records unreleased user-visible changes.

Read `docs/CONTEXT.md` before changing semantic code.
