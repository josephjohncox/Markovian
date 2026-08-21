# Markovian

Markovian is an early Haskell prototype for stochastic processes and decision models.

The current API is experimental and has known semantic defects. Do not use it for production decisions or numerical claims.

The first additive core slice defines validated floating probabilities, weights, finite distributions, and rewards. It also defines finite kernels, policies, and one-step MRP and MDP interfaces. The pinned GHC 9.8.4 environment builds this slice and runs its contract tests. Objectives, interpreters, policy closure, exact-reference values, and migration adapters are not implemented.

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

- `src/Markovian/Probability.hs` and `src/Markovian/Reward.hs` contain opaque validated values.
- `src/Markovian/Kernel.hs`, `src/Markovian/MRP.hs`, `src/Markovian/MDP.hs`, and `src/Markovian/Policy.hs` contain the additive one-layer core interfaces.
- `src/Markovian.hs` and `src/QLearning.hs` remain the unchanged legacy API.
- `app/Sample/Main.hs` and `app/QLearning/Main.hs` remain legacy demonstrations.
- `test/Main.hs` contains deterministic core contract tests.

The pinned environment passes Cabal package, build, test, Haddock, formatting, and source-distribution commands. CI and the legacy characterization suite remain incomplete.

Read [docs/CONTEXT.md](docs/CONTEXT.md) before you change the source.
