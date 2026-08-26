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
- structured model, policy, sampling, compilation, solver, arithmetic, normalization, and conditioning errors.

The semantic core depends only on `base`. Sampling frameworks, tensors, GPU runtimes, autodiff, and neural libraries remain outside the core.

## Example

```sh
scripts/bootstrap-tools
direnv allow .
cabal run Sample --project-file=cabal.project.ci
```

The sample evaluates one exact transition with reward `2`, discount `1/2`, and terminal payoff `7`. Its expected return is `11/2`.

## Verification

The package tests GHC 9.4.8 and 9.8.4. The required gates are:

```sh
cabal check
cabal build all --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
cabal haddock all --project-file=cabal.project.ci --haddock-all --haddock-hyperlink-source
cabal build all --project-file=cabal.project.ci --prefer-oldest
cabal test all --project-file=cabal.project.ci --prefer-oldest
hlint src
fourmolu --mode check $(find src app test -type f -name '*.hs')
cabal-fmt --check Markovian.cabal
```

CI also builds and tests the unpacked source distribution.

## Project documents

- [TODO.md](TODO.md) is the prioritized implementation plan.
- [docs/CONTEXT.md](docs/CONTEXT.md) is the current repository state.
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) defines semantic contracts and boundaries.
- [docs/DECISIONS.md](docs/DECISIONS.md) records accepted decisions and proof obligations.
- [docs/WORKFLOWS.md](docs/WORKFLOWS.md) defines required evidence and change procedures.
- [CHANGELOG.md](CHANGELOG.md) records unreleased user-visible changes.

Read `docs/CONTEXT.md` before changing semantic code.
