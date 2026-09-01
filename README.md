# Markovian

Markovian is an experimental Haskell package for finite stochastic kernels, Markov reward processes, Markov decision processes, policies, and bounded interpreters.

The package is greenfield and unreleased. It makes no compatibility promise. Incorrect interfaces are removed rather than retained behind shims.

Capability labels in this repository have these meanings:

- **Implemented:** source and deterministic fixtures exist in the current worktree.
- **Experimental:** the API is unreleased and can change without migration support.
- **Out of scope:** no support claim exists for convergence, production training, tensor frameworks, autodiff, or neural devices.

Integration acceptance is still open. D-053 is accepted. D-054 through D-060 remain proposed until the complete four-package compiler, lower-bound, Haddock, formatting, source-archive, compile-fail, benchmark, and deterministic-report gates pass. The implementation descriptions below report worktree capabilities, not release acceptance.

## Documentation

[The Markovian Book](docs/book/src/introduction.md) is the user and contributor guide. It covers model construction, exact evaluation, the [bounded serial-inventory benchmark](docs/book/src/inventory-control.md), the [fixed-batch execution and newsvendor evidence](docs/book/src/fixed-batch-inventory.md), learning, POMDPs, matrices, Bayesian inference, circuits, open systems, finite interaction protocols, [finite open games](docs/book/src/finite-open-games.md), and optional backends. Foundation chapters explain the [algebra](docs/book/src/algebra-primer.md), [category theory](docs/book/src/category-primer.md), [measure theory](docs/book/src/measure-theory-primer.md), [categorical probability](docs/book/src/categorical-probability.md), [information theory](docs/book/src/information-theory.md), [categorical structure of learning](docs/book/src/categorical-learning.md), and [polarity, push-pull duality, and game semantics](docs/book/src/polarity-and-games.md) behind those APIs. The book also gives an equation-level [law catalogue](docs/book/src/laws-and-boundaries.md), [derived mathematical insights](docs/book/src/categorical-insights.md), and an annotated [bibliography with guided reading routes](docs/book/src/references.md).

Build the searchable HTML book with the pinned documentation tool:

```sh
scripts/install-doc-tools
scripts/check-book
```

Open `docs/book/build/index.html` after the build succeeds. Haddock remains the API-signature reference.

The public book is <https://josephjohncox.github.io/Markovian/>. [Pages run 33126170927](https://github.com/josephjohncox/Markovian/actions/runs/33126170927) deployed merge commit `1268191a025c22fd9b995a1025d9ca810ff43451` from `main`.

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
- standard probability-monad, Kleisli `Category`, `Arrow`, and `ArrowChoice` instances;
- dense rational CPU lowering with denotational differential tests;
- structured model, policy, sampling, compilation, solver, arithmetic, normalization, and conditioning errors.

Raw matrices can use empty objects. The vacuous empty-to-empty stochastic arrow is also valid, but a stochastic arrow from a nonempty source to an empty target is not. Normalized states, distributions, priors, and other probability-bearing finite objects remain nonempty. Both finite-witness modules export `sameFiniteLayout` as the canonical layout comparison. `sameFiniteSetLayout` and `sameFiniteObjectLayout` remain descriptive aliases. `matrixEquivalent` is labelled extensional equality; `sameMatrixLayout` compares the represented witnesses and row layout. Stochastic matrices deliberately have no transpose, dagger, compact, trace, or raw-addition API because those operations do not generally preserve normalization. Nominal roles protect stochastic, deterministic, and convex proofs from `coerce`. Copy-naturality reasoning requires the proof-carrying deterministic refinement.

`Markovian.Category.Payoff.Exact` represents a total exact rational payoff on an explicit finite set. `pullbackPayoff` computes conditional expected payoff against a normalized stochastic matrix, and `pairStatePayoff` evaluates a normalized singleton-source state against a payoff. The exact fixtures cover signed payoffs, identity, contravariant composition, reordered layouts, empty finite pullback, and the state-payoff pairing law.

Payoff pullback needs no prior and does not produce a posterior. Bayesian inversion is prior-indexed and maps positive output support to positive input support. It does not fill zero-evidence rows and is not matrix conjugate transpose. `BayesianChannel` composition checks its middle prior and has no plain `Category` or dagger instance. Exact POMDP filtering delegates to the same pushforward and conditioning algebra.

Circuit purity records provenance. Only deterministic syntax can use copy-naturality optimization. `shareCircuit` performs one stochastic execution and copies its result; `fanoutCircuit` performs conditionally independent branch executions. Exact circuit interpretation and dense CPU lowering share one nonnegative-rational matrix denotation. A raw-node-bounded fold supports static `Natural` cost reports with caller-owned primitive charges. Opaque identity, reassociation, and deterministic deduplication candidates receive checked witnesses only after exact endpoint-layout and row-major matrix checks under the supplied exact primitive interpreter. A witness certifies only that checked exact matrix interpretation. Cost improvement is not a proof obligation. Floating, CUDA, and neural backends require an explicit approximation relation and do not inherit exact-law claims.

The deterministic compiler supports identity, composition, products, pairing, projections, and finite quoted tables. It does not compile arbitrary Haskell functions or provide stochastic cartesian closure.

Open systems use structured cospans of finite typed directed hypergraphs. Sequential composition is an explicit finite pushout; tensor is disjoint union. Binary quotient members have canonical left-then-right order, and cocones compare interfaces by typed support rather than layout. Higher cells are commuting squares with type-, label-, order-, and incidence-preserving apex maps. Boundary reversal swaps cospan legs only and retains the original directed circuit state orientation. It is separate from matrix conjugate transpose and Bayesian inversion.

`OpenCircuit` attaches one directed global circuit decoration, and its existing denotation is unchanged. Separately, `AcyclicOpenSystem` accepts only topology in which every apex vertex has one input-boundary or edge-output producer and the edge dependency graph is acyclic. `AcyclicOpenCircuit` resolves label and ordered-signature entries to local purity-indexed circuits and gives the validated finite DAG an exact assignment-matrix denotation. Evaluation retains only values needed by later edges or output observations. It marginalizes dead values at the edge step. Multiple consumers copy one stored value, while distinct edge occurrences execute independently. Exact fixture laws cover identity, composition, tensor, sharing, discard, conditional independence, normalization, and schedule independence for successful denotations. Runtime cost can still grow exponentially with live-frontier width and boundary size.

Raw or cyclic `OpenSystem` values cannot use this interpreter. Feedback, trace, fixed points, arbitrary hypergraph black-boxing, continuous-time open Markov processes, and unrestricted MDP black-boxing remain deferred.

`Markovian.Game.Arena` validates finite reachability, ownership, alternation, and acyclicity under explicit construction and comparison bounds. `Markovian.Game.Strategy` validates finite prefix closure, exhaustive Opponent receptivity, and one total Player response. Composition synchronizes literal middle move identities, hides them, and revalidates the visible strategy under one operation-wide work account. Composition is partial and can reject a hidden internal deadlock. Observational equality is bounded exact external-prefix equality, not contextual equivalence. This protocol layer has no general closure or category-law claim, justification pointers, views, innocence, payoff, best response, equilibrium, chance, recursion, or claim to Hyland--Ong or AJM game semantics.

`Markovian.Game.Open.*` is a separate finite formalism. It stores structural owner/profile trees, play, coplay, and context-indexed best-response membership; sequential and tensor composition reject repeated owners. Exact decisions enumerate represented finite functions and compare `Rational` utilities literally. Products, pure equilibria, and observational equality are preflight-bounded. Continuations are canonicalized, equality accepts only structural owner-preserving witnesses, and performed counts come from strict best-response, play, and coplay checks. Matching pennies has no pure equilibrium, and the sequential counterexample retains a non-credible threat. There is no mixed, correlated, repeated, stochastic, Bayesian, continuous, subgame-perfect, or equilibrium-existence claim and no `Category` instance.

The semantic core depends only on `base`. GPU runtimes and neural contracts remain outside it in separate packages:

- `backends/markovian-gpu` provides an optional CUDA 13 driver backend, CPU/GPU differential tests, and a transfer-inclusive benchmark;
- `backends/markovian-neural` provides checked dense networks with manual VJPs, typed parametric reverse composition, stable categorical operations, sized structural action masks, approximate entropy/cross-entropy/KL/mutual-information calculations and gradients, linear REINFORCE and actor-critic updates, replay storage, target networks, and one standard or Double-DQN batch update;
- `backends/markovian-neural-bridge` checks exact global action layouts against policy or dense output widths and compiles each continuing state's exact availability order into a Boolean structural mask under explicit state, action-entry, and work limits. Terminal states remain explicit.

The bridge depends on the root and neural libraries. Neither existing library depends on it. Complete compilation is preflighted and returns no partial collection after exhaustion. Nominal roles reject representational action relabelling. The bridge gathers available logits or Q-values before softmax or argmax; it does not construct multiplicative numeric masks or additive negative-infinity masks.

The reverse interpreters keep primal and cotangent types distinct, combine independent parameters as nested pairs, and declare zero, addition, scalar action, validation, finite layout, ownership metadata, and equality for prepared-program cotangent spaces. Primitive pullbacks must be additive and homogeneous; input and parameter diagonals use the declared addition. The finite syntax accepts only caller-owned primitives, identity, composition, tensor, shared input, and shared parameters. Preparation is explicitly bounded. Opaque tapes either store a captured pullback or recompute that primitive from retained immutable values and check output agreement. This computes sensitivities from supplied VJPs only. The neural package remains an experimental numerical reference with no tensor framework, general autodiff, checkpoint scheduler, recursion, device runtime, environment runner, or complete trainer. Its exact scalar, finite-difference, and worked-example tests do not support convergence, scalability, or production claims.

The CUDA package flag is disabled by default so ordinary builds require no GPU toolkit. On a CUDA host, run:

```sh
cabal test markovian-gpu-test --project-file=cabal.project -fcuda --test-show-details=direct
cabal bench markovian-gpu-bench --project-file=cabal.project -fcuda
```

The 2026-08-26 evidence run used an NVIDIA GB10 with driver 580.173.02 and compute capability 12.1. The differential test passed.

The benchmark uses one excluded warmup and 20 measured runs. It measured a transfer-inclusive mean of `267.843920400 ms`. The sample standard deviation was `3.025869898 ms`, and the maximum differential error was `0.000e0`.

[The complete CUDA evidence record](docs/evidence/CUDA-2026-08-26.md) contains raw samples, the range, tool versions, historical measurements, and revision context. The result shows local execution only. It is not a general performance claim.

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

The Clark--Scarf executable reports only its finite Section III specialization. The Doğru executable reports a two-retailer finite-horizon adaptation with separate physical and signed-relaxed actions. The fixed-batch executable keeps its finite-horizon oracle separate from stationary newsvendor calculations on an explicit finite Cartesian `R1`/`R2` domain. None is a published numeric reproduction, average-cost solution, unbounded result, or convergence claim. CI runs all four executables in the working tree and again from the unpacked root source archive; each run enforces one warm-up and twenty identical semantic reports.

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
for dir in . backends/markovian-gpu backends/markovian-neural backends/markovian-neural-bridge; do
  (cd "$dir" && cabal check)
done
cabal build all --project-file=cabal.project.ci
cabal build \
  inventory-control-bench \
  clark-scarf-1960-bench \
  dogru-inventory-bench \
  fixed-batch-rnq-bench \
  --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
cabal bench inventory-control-bench --project-file=cabal.project.ci
cabal bench clark-scarf-1960-bench --project-file=cabal.project.ci
cabal bench dogru-inventory-bench --project-file=cabal.project.ci
cabal bench fixed-batch-rnq-bench --project-file=cabal.project.ci
set -o pipefail
cabal haddock all \
  --project-file=cabal.project.ci \
  --haddock-all \
  --haddock-hyperlink-source 2>&1 | tee haddock.log
! grep -nE '(^|[[:space:]])Warning:' haddock.log
cabal build all --project-file=cabal.project.ci --prefer-oldest
cabal test all --project-file=cabal.project.ci --prefer-oldest
hlint src bench backends/*/src test backends/*/test
find src app bench test backends -type f -name '*.hs' -print0 \
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
  backends/markovian-gpu/scripts/build-ptx \
  backends/markovian-neural/scripts/check-reverse-program-boundary \
  backends/markovian-neural-bridge/scripts/check-exact-support-boundary
scripts/check-refinement-roles
scripts/check-circuit-purity
bash backends/markovian-neural/scripts/check-reverse-program-boundary
bash backends/markovian-neural-bridge/scripts/check-exact-support-boundary
scripts/check-acyclic-proof-boundary
scripts/check-acyclic-purity
scripts/check-game-core-boundary
scripts/check-book
cabal-fmt --check Markovian.cabal \
  backends/markovian-gpu/markovian-gpu.cabal \
  backends/markovian-neural/markovian-neural.cabal \
  backends/markovian-neural-bridge/markovian-neural-bridge.cabal
```

Fourmolu 0.20 does not parse the repository's three LaTeX-style literate Haskell files. CI excludes only those `.lhs` files from Fourmolu; GHC and HLint still check them. CI creates four source archives, builds and tests the unpacked packages, runs each applicable compile-fail boundary, and reruns all four inventory semantic-report benchmarks from the unpacked root archive.

## Project documents

- [The Markovian Book](docs/book/src/introduction.md) is the user and contributor guide.
- [TODO.md](TODO.md) is the prioritized implementation plan.
- [docs/CONTEXT.md](docs/CONTEXT.md) is the current repository state.
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) defines semantic contracts and boundaries.
- [docs/DECISIONS.md](docs/DECISIONS.md) records technical decisions and their required evidence.
- [docs/WORKFLOWS.md](docs/WORKFLOWS.md) defines required evidence and change procedures.
- [CHANGELOG.md](CHANGELOG.md) records unreleased user-visible changes.

Read `docs/CONTEXT.md` before changing semantic code.
