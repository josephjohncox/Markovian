# Install and build

## Requirements

Use a Unix-like system with GHCup, `cargo`, and `direnv`. The repository pins the Haskell and documentation tool versions in `toolchain.env`.

The default build does not require CUDA.

## Install the Haskell tools

Run these commands from the repository root:

```sh
scripts/bootstrap-tools
direnv allow .
```

The script installs the pinned GHC, Cabal, HLS, Fourmolu, HLint, and `cabal-fmt` versions under `.direnv`.

## Build and test the packages

```sh
cabal build all --project-file=cabal.project.ci
cabal test all \
  --project-file=cabal.project.ci \
  --test-show-details=direct
```

The project file includes the semantic core and both optional backend packages. The CUDA package flag stays disabled.

## Run the first example

```sh
cabal run Sample --project-file=cabal.project.ci
```

The example prints this exact result:

```text
Expected return: 11 % 2
```

The value contains one transition reward and one discounted terminal payoff.

## Install the documentation tool

```sh
scripts/install-doc-tools
```

This script installs the pinned `mdbook` version into `.direnv/bin`. It uses the version in `toolchain.env` and Cargo's locked dependency graph.

Build and check the book:

```sh
scripts/check-book
```

Open `docs/book/build/index.html` in a browser after the command succeeds.

For live editing, run:

```sh
mdbook serve docs/book --open
```

## Build Haddock API documentation

```sh
set -o pipefail
cabal haddock all \
  --project-file=cabal.project.ci \
  --haddock-all \
  --haddock-hyperlink-source 2>&1 | tee haddock.log
! grep -nE '(^|[[:space:]])Warning:' haddock.log
```

The book explains concepts and workflows. Haddock lists exact types, constructors, and error values.

## Optional CUDA build

Use a supported NVIDIA host with the CUDA driver and toolkit:

```sh
cabal test markovian-gpu-test \
  --project-file=cabal.project \
  -fcuda \
  --test-show-details=direct

cabal bench markovian-gpu-bench \
  --project-file=cabal.project \
  -fcuda
```

The GPU benchmark includes transfer and setup time. Do not compare its result with a kernel-only benchmark.
