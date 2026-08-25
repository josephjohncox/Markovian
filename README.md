# Markovian

Markovian is an early Haskell prototype for stochastic processes and decision models.

The current API is experimental and has known semantic defects. Do not use it for production decisions or numerical claims.

The additive core defines validated floating values, separate exact rational reference values, objectives, finite kernels, policies, one-step MDP interfaces, and validated policy closure. The exact reference interpreter computes finite-horizon expected return by bounded state recursion. Exact tests cover functor, Kleisli, joint-outcome, trace, reward-timing, and horizon laws. GHC 9.4.8 and 9.8.4 build the core and run its contracts. Seeded sampling, migration adapters, and replacement learning interpreters are not implemented.

## Project documents

- [TODO.md](TODO.md) gives the prioritized work plan and acceptance criteria.
- [docs/CONTEXT.md](docs/CONTEXT.md) gives the short repository state for session resume.
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) defines the target architecture and semantic contracts.
- [docs/WORKFLOWS.md](docs/WORKFLOWS.md) defines the required workflow and evidence rules.
- [docs/DECISIONS.md](docs/DECISIONS.md) records architecture decisions and proof obligations.
- [CHANGELOG.md](CHANGELOG.md) will record released user-visible changes. It is still a generated placeholder.

## Development environment

GHCup supplies the pinned compiler, Cabal, HLS, and Fourmolu versions. The bootstrap script installs missing GHCup tools and project-local HLint and cabal-fmt binaries.

```sh
scripts/bootstrap-tools
direnv allow .
```

After approval, direnv loads GHC 9.8.4 when you enter this directory. `toolchain.env` is the version authority. `.envrc` does not install or download tools.

## Current code

- `src/Markovian/Probability*` and `src/Markovian/Reward*` contain separate floating and exact validated values.
- `src/Markovian/Objective*` contains floating and exact discounts plus transition horizons.
- `src/Markovian/Kernel*`, `src/Markovian/MRP.hs`, `src/Markovian/MDP*`, and `src/Markovian/Policy*` contain the floating one-layer, exact composable, and validated policy-closure interfaces.
- `src/Markovian/Interpreter/Exact.hs` contains exact finite-horizon expectation.
- `src/Markovian.hs` and `src/QLearning.hs` remain the legacy API.
- `app/Sample/Main.hs` and `app/QLearning/Main.hs` remain legacy demonstrations.
- `test/Main.hs` contains twenty-three deterministic core, law, interpreter, and legacy contracts.

The pinned environment and hosted CI pass package, build, test, Haddock, formatting, lower-bound, and source-distribution gates on the P0 baseline.

Read [docs/CONTEXT.md](docs/CONTEXT.md) before you change the source.
