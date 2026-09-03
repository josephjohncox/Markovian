# Markovian

Markovian is an experimental Haskell package for finite stochastic kernels, Markov reward processes, Markov decision processes, policies, and bounded interpreters.

Markovian `2026.9.3.0` is the first release. It uses coordinated UTC CalVer and the matching Git tag `v2026.9.3.0`. Incorrect interfaces were removed before this release instead of being retained behind compatibility shims.

Capability labels in this repository have these meanings:

- **Accepted:** the stated bounded capability and its complete evidence matrix passed.
- **Partial:** a restricted capability has executable fixtures, but its contract or evidence matrix is incomplete.
- **Blocked:** a safety or semantic prerequisite fails or has no executable evidence.
- **Out of scope:** no support claim exists for convergence, production training, general tensor semantics, arbitrary-Haskell autodiff, or neural devices.

D-053 through D-076 are accepted only for their stated finite, bounded scopes. Mixed games, closed-language autodiff, checked feedback, restricted continuous probability, host F64 tensors, the metadata-free F64 SafeTensors profile, and the GB10 CUDA matrix/VJP fragment have complete release evidence. Their explicit nonclaims remain in force.

## Release status

Release `2026.9.3.0` was prepared from a clean immutable revision with complete hosted compiler, lower-bound, archive, documentation, CUDA compile-only, protected hardware, four-tool Compute Sanitizer, checksum, SBOM, and provenance evidence.

The repository has bounded, non-publishing release preparation tools. They check metadata, public module snapshots, source archives, checksums, SPDX 2.3 SBOMs, and archive-only consumers.

Run the static release checks with:

```sh
bash scripts/check-release-metadata
bash scripts/check-release-policy
python3 scripts/test_release_tool.py
```

Run full preparation only from a clean immutable revision. Read [the release checklist](RELEASE-CHECKLIST.md), [migration guide](MIGRATION.md), and [release notes](RELEASE-NOTES.md).

Preparation does not upload packages or create tags or releases. External publication requires explicit user approval. Coordinated releases use UTC CalVer `YYYY.M.D.N`; tags use the matching `vYYYY.M.D.N` form.

## Documentation

[The Markovian Book](docs/book/src/introduction.md) is the user and contributor guide. It covers model construction, exact evaluation, the [bounded serial-inventory benchmark](docs/book/src/inventory-control.md), the [fixed-batch execution and newsvendor evidence](docs/book/src/fixed-batch-inventory.md), learning, POMDPs, matrices, Bayesian inference, circuits, open systems, finite interaction protocols, [finite open games](docs/book/src/finite-open-games.md), and optional backends. Foundation chapters explain the [algebra](docs/book/src/algebra-primer.md), [category theory](docs/book/src/category-primer.md), [measure theory](docs/book/src/measure-theory-primer.md), [categorical probability](docs/book/src/categorical-probability.md), [information theory](docs/book/src/information-theory.md), [categorical structure of learning](docs/book/src/categorical-learning.md), and [polarity, push-pull duality, and game semantics](docs/book/src/polarity-and-games.md) behind those APIs. The book also gives an equation-level [law catalogue](docs/book/src/laws-and-boundaries.md), [derived mathematical insights](docs/book/src/categorical-insights.md), and an annotated [bibliography with guided reading routes](docs/book/src/references.md).

Build the searchable HTML book with the pinned documentation tool:

```sh
scripts/install-doc-tools
scripts/check-book
```

Open `docs/book/build/index.html` after the build succeeds. Haddock remains the API-signature reference.

The public book is <https://josephjohncox.github.io/Markovian/>. [Pages run 33126170927](https://github.com/josephjohncox/Markovian/actions/runs/33126170927) deployed merge commit `1268191a025c22fd9b995a1025d9ca810ff43451` from `main`.

## Present API surfaces

- opaque validated floating probabilities, weights, finite distributions, rewards, discounts, and horizons;
- separate rational reference values for literal equality and law tests;
- one-layer floating and exact stochastic kernels;
- MRP and MDP interfaces with action IDs distinct from transition outcomes;
- validated policy closure that preserves joint reward and successor distributions;
- exact finite-horizon expected-return evaluation with explicit model, policy, horizon, and discount;
- explicit seeded finite-support sampling with returned generator state;
- action-labeled traces with terminal and horizon stop reasons;
- exact trace enumeration whose expected return matches direct evaluation;
- policy-free exact finite MDP compilation with all joint action outcomes preserved;
- separate compiled policy closure for finite-horizon and discounted Bellman policy evaluation;
- exact discounted value iteration with residual, value-error, and greedy-policy bounds;
- exact deterministic policy iteration with signed rational policy solves;
- a synthetic bounded two-echelon serial-inventory fixture, a source-crosschecked Clark--Scarf finite specialization, a source-crosschecked bounded Doğru physical-versus-balance-relaxed comparison, and a bounded two-stage fixed-batch execution with separate stationary newsvendor evidence;
- a separate Clark--Scarf (1960), Section III, exact finite-lattice specialization with source equation crosswalk, three exact oracle paths, explicit layouts and budgets, and nonbinding widened-cap diagnostics;
- shared validated tabular Q-values, V-values, rates, schedules, and observations;
- pure terminal-aware TD(0), SARSA, Expected SARSA, and Q-learning updates;
- seeded bounded resumable episodic runners with explicit generator ownership;
- canonical exact finite beliefs with post-transition prediction and conditioning;
- exact bounded belief-state policy evaluation with mixed-termination rejection;
- duplicate-free finite sets, including empty sets, plus nonempty finite-object refinements;
- lawful semiring, involution, exact positivity, and convex scalar contracts with an opaque nonnegative rational implementation;
- opaque source-by-target semiring matrices with checked indexing, composition, tensor, biproduct, transpose, conjugate transpose, compact structure, and trace;
- exact normalized stochastic matrices, proof-carrying deterministic matrices, and exact convex mixtures;
- checked signed rational finite payoffs, exact payoff pullback, and state-payoff pairing;
- exact priors, positive supports, pushforward, joints, conditioning, and support-restricted Bayesian inversion;
- checked Bayesian channels with explicit prior flow and prior-indexed almost-sure equality;
- raw purity-indexed stochastic-circuit syntax with explicit sharing, fanout, structural maps, and exact convex choice;
- first-order deterministic categorical compilation from finite quoted tables;
- finite typed hypergraphs, explicit quotient pushouts, structured cospans, and commuting open-system cells;
- directed circuit-decorated open topology with no invented reverse denotation;
- a separately validated boundary-functional finite DAG fragment with exact local-circuit semantics;
- finite reachable acyclic alternating arenas, opaque legal histories, receptive deterministic strategies, and bounded hidden-middle composition;
- bounded concrete finite optics and owner-refined finite open games with exact pure contextual equilibrium enumeration;
- a finite symmetric monoidal Markov IR with explicit object witnesses, full-tensor copy, fanout, symmetry, associators, and unitors;
- optional typed parametric reverse circuits plus a finite owned reverse-program syntax with checked layouts, bounded preparation, explicit diagonal accumulation, and opaque stored or recomputed tapes;
- checked exact distribution bind with support, work, and rational-size limits;
- checked exact-kernel composition without unrestricted `Category` or `Arrow` instances;
- optional dense rational CPU lowering from exact circuits with denotational differential tests;
- structured model, policy, sampling, compilation, solver, arithmetic, normalization, and conditioning errors.

Raw matrices can use empty objects. The vacuous empty-to-empty stochastic arrow is also valid, but a stochastic arrow from a nonempty source to an empty target is not. Normalized states, distributions, priors, and other probability-bearing finite objects remain nonempty. Both finite-witness modules export `sameFiniteLayout` as the canonical layout comparison. `sameFiniteSetLayout` and `sameFiniteObjectLayout` remain descriptive aliases. `matrixEquivalent` is labelled extensional equality; `sameMatrixLayout` compares the represented witnesses and row layout. Stochastic matrices deliberately have no transpose, dagger, compact, trace, or raw-addition API because those operations do not generally preserve normalization. Nominal roles protect stochastic, deterministic, and convex proofs from `coerce`. Copy-naturality reasoning requires the proof-carrying deterministic refinement.

`Markovian.Category.Payoff.Exact` represents a total exact rational payoff on an explicit finite set. `pullbackPayoff` computes conditional expected payoff against a normalized stochastic matrix, and `pairStatePayoff` evaluates a normalized singleton-source state against a payoff. The exact fixtures cover signed payoffs, identity, contravariant composition, reordered layouts, empty finite pullback, and the state-payoff pairing law.

Payoff pullback needs no prior and does not produce a posterior. Bayesian inversion is prior-indexed and maps positive output support to positive input support. It does not fill zero-evidence rows and is not matrix conjugate transpose. `BayesianChannel` composition checks its middle prior and has no plain `Category` or dagger instance. Exact POMDP filtering delegates to the same pushforward and conditioning algebra.

Circuit purity records provenance. Only deterministic syntax can use copy-naturality optimization. `shareCircuit` performs one stochastic execution and copies its result; `fanoutCircuit` performs conditionally independent branch executions. Exact circuit interpretation and dense CPU lowering share one nonnegative-rational matrix denotation. A raw-node-bounded fold supports static `Natural` cost reports with caller-owned primitive charges. Opaque identity, reassociation, and deterministic deduplication candidates receive checked witnesses only after exact endpoint-layout and row-major matrix checks under the supplied exact primitive interpreter. A witness certifies only that checked exact matrix interpretation. Cost improvement is not a proof obligation. Floating, CUDA, and neural backends require an explicit approximation relation and do not inherit exact-law claims.

The deterministic compiler supports identity, composition, products, pairing, projections, and finite quoted tables. It does not compile arbitrary Haskell functions or provide stochastic cartesian closure.

Open systems use structured cospans of finite typed directed hypergraphs. Sequential composition is an explicit finite pushout; tensor is disjoint union. Binary quotient members have canonical left-then-right order, and cocones compare interfaces by typed support rather than layout. Higher cells are commuting squares with type-, label-, order-, and incidence-preserving apex maps. Boundary reversal swaps cospan legs only and retains the original directed circuit state orientation. It is separate from matrix conjugate transpose and Bayesian inversion.

`OpenCircuit` attaches one directed global circuit decoration, and its existing denotation is unchanged. Separately, `AcyclicOpenSystem` accepts only topology in which every apex vertex has one input-boundary or edge-output producer and the edge dependency graph is acyclic. `AcyclicOpenCircuit` resolves label and ordered-signature entries to local purity-indexed circuits and gives the validated finite DAG an exact assignment-matrix denotation. Evaluation retains only values needed by later edges or output observations. It marginalizes dead values at the edge step. Multiple consumers copy one stored value, while distinct edge occurrences execute independently. Exact fixture laws cover identity, composition, tensor, sharing, discard, conditional independence, normalization, and schedule independence for successful denotations. Runtime cost can still grow exponentially with live-frontier width and boundary size.

Raw or cyclic `OpenSystem` values cannot use this interpreter. The separate `Markovian.Feedback.*.Exact` modules support only explicit one-tick delay, checked proper first-exit coproduct routing, and nilpotent reward-and-duration-preserving closure. They do not accept cyclic `OpenSystem` topology. Universal trace, arbitrary fixed points, arbitrary hypergraph black-boxing, continuous-time open Markov processes, and unrestricted MDP black-boxing remain deferred.

Checked feedback keeps normalized routing separate from raw matrix trace. Proper first exit solves and validates `H = C + D H`. Delayed execution retains output and successor state jointly. Timed execution retains reward, duration, and output jointly. A half-loop and half-exit has a valid marginal first exit. Timed feedback rejects it because duration support is unbounded. All operations have explicit limits. Deterministic reports include exact work and rational maxima for discarded and retained values.

`Markovian.Game.Arena` validates finite reachability, ownership, alternation, and acyclicity under explicit construction and comparison bounds. `Markovian.Game.Strategy` validates finite prefix closure, exhaustive Opponent receptivity, and one total Player response. Composition synchronizes literal middle move identities, hides them, and revalidates the visible strategy under one operation-wide work account. Composition is partial and can reject a hidden internal deadlock. Observational equality is bounded exact external-prefix equality, not contextual equivalence. This protocol layer has no general closure or category-law claim, justification pointers, views, innocence, payoff, best response, equilibrium, chance, recursion, or claim to Hyland--Ong or AJM game semantics.

`Markovian.Game.Open.*` is a separate finite formalism. It stores structural owner/profile trees, play, coplay, and context-indexed best-response membership; sequential and tensor composition reject repeated owners. Exact decisions enumerate represented finite functions and compare `Rational` utilities literally. Products, pure equilibria, and observational equality are preflight-bounded. Continuations are canonicalized, equality accepts only structural owner-preserving witnesses, and performed counts come from strict best-response, play, and coplay checks. Matching pennies has no pure open-game equilibrium, and the sequential counterexample retains a non-credible threat. There is no generic mixed lifting, repeated, continuous, subgame-perfect, or equilibrium-existence claim and no `Category` instance.

The separate `Markovian.Game.Profile.Finite`, `NormalForm.Exact`, `Correlated.Exact`, `Outcome.Exact`, `Stochastic.Exact`, and `Harsanyi.Exact` modules provide bounded exact candidate semantics. They distinguish independent mixtures, CE, CCE, correlated type priors, and joint reward/successor outcomes. Stochastic evaluation is finite-horizon, public-state, and terminal-before-horizon. Harsanyi checks report null types without inventing posteriors. `Open.Strategic.Exact` extracts only one checked closed context. These APIs do not find all equilibria: a rational-payoff three-player fixture needs `p=1/sqrt(2)`, and a zero-payoff fixture has a positive-dimensional equilibrium set.

The root library depends only on `base`. It exposes finite exact and exact-neutral structural semantics. Floating, sampled, learning, approximation, dense, and benchmark modules now have these optional-package boundaries:

- `packages/markovian-numerical` owns floating finite probability, rewards, objectives, kernels, models, policies, and approximate circuit interpretation;
- `packages/markovian-sampling` owns explicit generators and sampled finite interpreters;
- `packages/markovian-learning` owns tabular updates and bounded episodic runners;
- `packages/markovian-dense-exact` owns dense `Rational` storage lowered from exact circuits;
- `packages/markovian-exact-benchmarks` owns inventory fixtures, deterministic reports, and the six exact semantic benchmark components;

The continuous, autodiff, tensor, GPU, and neural layers remain separate optional packages:

- `packages/markovian-continuous` provides compact rational affine-uniform laws, exact polynomial moments, bounded bivariate symbolic accounting, checked affine kernels, and positive-evidence finite-observation conditioning;
- `packages/markovian-continuous-numerical` provides explicit rational-to-`Double` conversion, bounded deterministic GK15/7 quadrature, owned SplitMix64 sampling, and resumable Welford Monte Carlo;
- `packages/markovian-autodiff` provides a closed typed polynomial and `tanh` language, exact formal-polynomial and checked-Double compilers, bounded reverse lowering, and opaque reusable tapes;
- `packages/markovian-tensor` provides region-scoped host F64 buffers, typed shapes, checked immutable layouts, deterministic CPU primitives, staged allocation cleanup, separate semantic owners and storage IDs, and opaque primitive VJP tapes;
- `packages/markovian-tensor-reverse` connects only closed host F64 `tanh` and pointwise-multiplication symbols to the bounded effect reverse interpreter;
- `packages/markovian-safetensors` parses and emits only a bounded canonical metadata-free F64 profile pinned to SafeTensors revision `6eb4dc9a28ebce297606e0f4836bbf28839cacef`;
- `backends/markovian-gpu` provides prepared F64 matrix and matrix-VJP plans, CPU reference dispatch, and an optional admitted CUDA 13 executor with explicit ownership, synchronization, cleanup, and pre-launch fallback;
- `backends/markovian-neural` provides checked dense networks with manual VJPs, typed parametric reverse composition, stable categorical operations, sized structural action masks, approximate entropy/cross-entropy/KL/mutual-information calculations and gradients, linear REINFORCE and actor-critic updates, replay storage, target networks, and one standard or Double-DQN batch update;
- `backends/markovian-neural-bridge` checks exact global action layouts against policy or dense output widths and compiles each continuing state's exact availability order into a Boolean structural mask under explicit state, action-entry, and work limits. Terminal states remain explicit.

The autodiff package differentiates only its closed first-order syntax. Its test-only neural integration checks one `2 -> 2 tanh -> 2` two-layer fixture for every primal, input, weight, and bias coordinate under both tape policies. This does not add a general neural lowering API. The package has no arbitrary callback, recursion, branch, stochastic node, tensor runtime, or device path. D-067 is accepted only for the separate effect interpreter and bounded host adapter described above.

The tensor package is host-only and F64-only. Rank zero is one scalar. Numerical primitives require finite values and use fixed-order single-threaded loops. Payload and work budgets are preflighted, including atomic multi-output VJPs. It has no arbitrary strides, broadcasting, mutation, raw pointers, BLAS, device buffers, generic reverse-program lowering, or performance claim. The separate SafeTensors package supports only bounded metadata-free F64 files; it rejects unsupported dtypes and metadata and does not serialize ownership or execution resources. The optional GPU package consumes checked tensor inputs only for positive-size F64 matrix multiplication and its declared VJP. D-067 and D-073 are accepted only for these bounded scopes.

The continuous exact and continuous numerical libraries are each `base`-only. Their integration dependency is test-only. These packages do not provide arbitrary measurable callbacks, point conditioning, continuous-to-continuous disintegration, certified floating bounds, continuous MDP execution, or release-readiness evidence.

The bridge depends on the root and neural libraries. Neither existing library depends on it. Complete compilation is preflighted and returns no partial collection after exhaustion. Nominal roles reject representational action relabelling. The bridge gathers available logits or Q-values before softmax or argmax; it does not construct multiplicative numeric masks or additive negative-infinity masks.

The reverse interpreters keep primal and cotangent types distinct, combine independent parameters as nested pairs, and declare zero, addition, scalar action, validation, finite layout, ownership metadata, and equality for prepared-program cotangent spaces. Primitive pullbacks must be additive and homogeneous; input and parameter diagonals use the declared addition. The finite syntax accepts only caller-owned primitives, identity, composition, tensor, shared input, and shared parameters. Preparation is explicitly bounded. Opaque tapes either store a captured pullback or recompute that primitive from retained immutable values and check output agreement. This computes sensitivities from supplied VJPs only. The neural package remains an experimental numerical reference with no tensor framework, general autodiff, checkpoint scheduler, recursion, device runtime, environment runner, or complete trainer. Its exact scalar, finite-difference, and worked-example tests do not support convergence, scalability, or production claims.

The CUDA package flag is disabled by default so ordinary builds require no GPU toolkit. On a CUDA host, run:

```sh
cabal test markovian-gpu-test --project-file=cabal.project.ci -fcuda \
  --extra-include-dirs=/usr/local/cuda/include --test-show-details=direct
cabal bench markovian-gpu-bench --project-file=cabal.project.ci -fcuda \
  --extra-include-dirs=/usr/local/cuda/include
bash backends/markovian-gpu/scripts/check-device-boundary
```

The enabled test requires structured admission, module load, a known-answer self-test, and CPU/CUDA matrix and VJP differentials. The benchmark records one excluded warmup and 20 CPU samples plus 20 transfer-inclusive CUDA samples when enabled.

The 2026-08-26 GB10 measurements predate the current executor and matrix/VJP kernel. They are historical evidence for the removed list-only dense call, not performance or correctness evidence for this implementation.

[The current tensor-fragment record](docs/evidence/CUDA-TENSOR-2026-09-02.md) contains focused enabled differentials and raw 64-by-64 samples. [The 2026-08-26 record](docs/evidence/CUDA-2026-08-26.md) is retained only as historical evidence for the removed list API. Both are local records, not general performance or device-correctness claims.

CUDA 13.0 `nvcc` V13.0.88 was available at `/usr/local/cuda/bin/nvcc`. This command reproduced the committed PTX files exactly:

```sh
backends/markovian-gpu/scripts/build-ptx
```

## Inventory benchmark

Run the inventory reproducibility benchmarks with:

```sh
cabal bench inventory-control-bench --project-file=cabal.project.ci
cabal bench clark-scarf-1960-bench --project-file=cabal.project.ci
cabal bench dogru-inventory-bench --project-file=cabal.project.ci
cabal bench fixed-batch-rnq-bench --project-file=cabal.project.ci
```

The Clark--Scarf executable reports only its finite Section III specialization. The Doğru executable reports a two-retailer finite-horizon adaptation with separate physical and signed-relaxed actions. The fixed-batch executable keeps its finite-horizon oracle separate from stationary newsvendor calculations on an explicit finite Cartesian `R1`/`R2` domain. None is a published numeric reproduction, average-cost solution, unbounded result, or convergence claim. CI runs all four executables in the working tree and from the unpacked `markovian-exact-benchmarks` archive graph. Each run enforces one warm-up and twenty identical semantic reports.

Each executable uses one excluded warm-up and twenty complete measured build, solve, and report samples. It prints raw nanosecond samples and toolchain metadata. Every sample must reproduce the same exact semantic report. The synthetic benchmark also prints sample statistics.

This is a **synthetic bounded serial fixture**. Demand is a geometric law conditioned on its configured finite cap. The report separates retained mass, omitted mass, and horizon exceedance probability. Exact values apply only to the conditional bounded-demand model, and truncation probability is not a value-error bound. The primary-versus-widened check validates equal model provenance, a larger order cap, and period-wise target-set widening before it can report stability. The semantic report prints both complete model parameter sets and target grids. The timing output is reproducibility evidence, not a performance comparison.

## Example

```sh
scripts/bootstrap-tools
direnv allow .
cabal run Sample --project-file=cabal.project.ci
```

The sample evaluates one exact transition with reward `2`, discount `1/2`, and terminal payoff `7`. Its expected return is `11/2`.

## Verification

The package tests GHC 9.4.8 and 9.8.4. The required CI checks are:

```bash
bash scripts/check-package-manifest
bash scripts/check-release-metadata
bash scripts/check-release-policy
python3 scripts/test_release_tool.py
while IFS=$'\t' read -r package_name package_dir dependency_tier; do
  if [[ -z "$package_name" || "$package_name" == \#* ]]; then
    continue
  fi
  (cd "$package_dir" && cabal check)
done < ci/packages.tsv
cabal build all --project-file=cabal.project.ci
cabal build \
  inventory-control-bench \
  clark-scarf-1960-bench \
  dogru-inventory-bench \
  fixed-batch-rnq-bench \
  feedback-exact-bench \
  mixed-games-exact-bench \
  markovian-continuous-bench \
  markovian-continuous-numerical-bench \
  markovian-autodiff-bench \
  markovian-tensor-bench \
  markovian-gpu-bench \
  --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
cabal bench inventory-control-bench --project-file=cabal.project.ci
cabal bench clark-scarf-1960-bench --project-file=cabal.project.ci
cabal bench dogru-inventory-bench --project-file=cabal.project.ci
cabal bench fixed-batch-rnq-bench --project-file=cabal.project.ci
cabal bench feedback-exact-bench --project-file=cabal.project.ci
cabal bench mixed-games-exact-bench --project-file=cabal.project.ci
cabal bench markovian-continuous-bench --project-file=cabal.project.ci
cabal bench markovian-continuous-numerical-bench --project-file=cabal.project.ci
cabal bench markovian-autodiff-bench --project-file=cabal.project.ci
cabal bench markovian-tensor-bench --project-file=cabal.project.ci
cabal bench markovian-gpu-bench --project-file=cabal.project.ci
bash packages/markovian-tensor/scripts/check-tensor-boundary
bash backends/markovian-gpu/scripts/check-device-boundary
# Warning-free evidence: isolated installation, one interface per package.
rm -rf /tmp/markovian-doc-store /tmp/markovian-doc-build /tmp/markovian-doc-environment
mapfile -t documentation_packages < <(awk -F '\t' '$1 !~ /^#/ { print $1 }' ci/packages.tsv)
cabal --store-dir=/tmp/markovian-doc-store install \
  --lib "${documentation_packages[@]}" \
  --project-file=cabal.project.ci \
  --builddir=/tmp/markovian-doc-build \
  --package-env=/tmp/markovian-doc-environment \
  --enable-documentation \
  --overwrite-policy=always 2>&1 | tee haddock-install.log
! grep -nE '(^|[[:space:]])Warning:' haddock-install.log
python3 scripts/release_tool.py check-haddock-interfaces /tmp/markovian-doc-store

# Coverage evidence only; --no-warnings does not prove warning freedom.
cabal haddock all \
  --project-file=cabal.project.ci \
  --haddock-hyperlink-source \
  --haddock-for-hackage \
  --haddock-options=--no-warnings > haddock-coverage.log 2>&1
python3 scripts/check-haddock-coverage haddock-coverage.log
cabal build all --project-file=cabal.project.ci --prefer-oldest
cabal test all --project-file=cabal.project.ci --prefer-oldest
hlint app src test backends/*/src backends/*/test backends/*/bench packages/*/src packages/*/test packages/*/bench
find src app test backends packages -type f -name '*.hs' -print0 \
  | sort -z \
  | xargs -0 fourmolu --mode check
bash -n \
  scripts/bootstrap-tools \
  scripts/check-book \
  scripts/install-doc-tools \
  scripts/check-refinement-roles \
  scripts/check-circuit-purity \
  scripts/check-acyclic-proof-boundary \
  scripts/check-acyclic-purity \
  scripts/check-game-core-boundary \
  scripts/check-mixed-game-boundary \
  scripts/check-feedback-boundary \
  scripts/check-package-manifest \
  backends/markovian-gpu/scripts/build-ptx \
  backends/markovian-gpu/scripts/check-device-boundary \
  backends/markovian-neural/scripts/check-reverse-program-boundary \
  backends/markovian-neural-bridge/scripts/check-exact-support-boundary \
  packages/markovian-continuous/scripts/check-continuous-boundary \
  packages/markovian-continuous-numerical/scripts/check-continuous-numerical-boundary \
  packages/markovian-autodiff/scripts/check-autodiff-boundary \
  packages/markovian-reverse/scripts/check-reverse-boundary \
  packages/markovian-tensor/scripts/check-tensor-boundary \
  packages/markovian-tensor-reverse/scripts/check-tensor-reverse-boundary \
  packages/markovian-safetensors/scripts/check-safetensors-boundary
scripts/check-root-topology
scripts/check-refinement-roles
scripts/check-circuit-purity
bash packages/markovian-reverse/scripts/check-reverse-boundary
bash backends/markovian-neural/scripts/check-reverse-program-boundary
bash backends/markovian-neural-bridge/scripts/check-exact-support-boundary
scripts/check-acyclic-proof-boundary
scripts/check-acyclic-purity
scripts/check-game-core-boundary
scripts/check-mixed-game-boundary
scripts/check-feedback-boundary
bash packages/markovian-continuous/scripts/check-continuous-boundary
bash packages/markovian-continuous-numerical/scripts/check-continuous-numerical-boundary
bash packages/markovian-autodiff/scripts/check-autodiff-boundary
bash packages/markovian-tensor/scripts/check-tensor-boundary
bash packages/markovian-tensor-reverse/scripts/check-tensor-reverse-boundary
bash packages/markovian-safetensors/scripts/check-safetensors-boundary
bash backends/markovian-gpu/scripts/check-device-boundary
scripts/check-book
cabal-fmt --check Markovian.cabal \
  backends/markovian-gpu/markovian-gpu.cabal \
  backends/markovian-neural/markovian-neural.cabal \
  backends/markovian-neural-bridge/markovian-neural-bridge.cabal \
  packages/markovian-continuous/markovian-continuous.cabal \
  packages/markovian-continuous-numerical/markovian-continuous-numerical.cabal \
  packages/markovian-autodiff/markovian-autodiff.cabal \
  packages/markovian-reverse/markovian-reverse.cabal \
  packages/markovian-tensor/markovian-tensor.cabal \
  packages/markovian-tensor-reverse/markovian-tensor-reverse.cabal \
  packages/markovian-safetensors/markovian-safetensors.cabal \
  packages/markovian-numerical/markovian-numerical.cabal \
  packages/markovian-sampling/markovian-sampling.cabal \
  packages/markovian-learning/markovian-learning.cabal \
  packages/markovian-dense-exact/markovian-dense-exact.cabal \
  packages/markovian-exact-benchmarks/markovian-exact-benchmarks.cabal
```

Fourmolu 0.20 does not parse the repository's three LaTeX-style literate Haskell files. CI excludes only those `.lhs` files from Fourmolu; GHC and HLint still check them. CI creates all 16 source archives, enables the manifested neural integration flag, checks the archive plan against all 18 suites and 11 benchmarks, and runs the applicable unpacked boundaries. Enabled CUDA compilation runs in the digest-pinned no-GPU workflow. Device execution and all four Compute Sanitizer tools run only in the protected UUID-bound hardware workflow.

## Project documents

- [The Markovian Book](docs/book/src/introduction.md) is the user and contributor guide.
- [TODO.md](TODO.md) is the prioritized implementation plan.
- [docs/CONTEXT.md](docs/CONTEXT.md) is the current repository state.
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) defines semantic contracts and boundaries.
- [docs/DECISIONS.md](docs/DECISIONS.md) records technical decisions and their required evidence.
- [docs/WORKFLOWS.md](docs/WORKFLOWS.md) defines required evidence and change procedures.
- [CHANGELOG.md](CHANGELOG.md) records released user-visible changes.

Read `docs/CONTEXT.md` before changing semantic code.
