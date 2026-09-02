# Install and build

## Requirements

Use a Unix-like system with GHCup, `cargo`, and `direnv`. The repository pins the Haskell and documentation tool versions in `toolchain.env`.

The default build does not require CUDA.

## Install the Haskell tools

Run these commands from the repository root:

```sh
bash scripts/bootstrap-tools
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

The project file includes the current 16-package integration graph. The CUDA package flag stays disabled.

Repository consumers must add optional packages explicitly. Floating models need `markovian-numerical`. Sampled interpreters also need `markovian-sampling`. Tabular runners need `markovian-learning`. Host tensors need `markovian-tensor`; closed primitive tapes and the bounded reverse adapter need `markovian-tensor-reverse`. The bounded metadata-free F64 serialization profile needs `markovian-safetensors` and `markovian-tensor`. Dense neural code uses `markovian-neural`; `denseReverseCircuit` also requires the package's declared `markovian-reverse` edge.

## Run the first example

```sh
cabal run Sample --project-file=cabal.project.ci
```

The example prints this exact result:

```text
Expected return: 11 % 2
```

The value contains one transition reward and one discounted terminal payoff.

## Build a checked source archive

Markovian is not published. Use only a checked preparation archive from an immutable revision.

Validate an archive before extraction:

```sh
bash scripts/check-release-archive \
  Markovian-0.1.0.0.tar.gz \
  --name Markovian \
  --version 0.1.0.0 \
  --extract unpacked
```

Then build from `unpacked/Markovian-0.1.0.0`. See [Release preparation](release-preparation.md) for checksums, SBOMs, and provenance.

## Install the documentation tool

```sh
bash scripts/install-doc-tools
```

This script installs the pinned `mdbook` version into `.direnv/bin`. It uses the version in `toolchain.env` and Cargo's locked dependency graph.

Build and check the book:

```sh
bash scripts/check-book
```

Open `docs/book/build/index.html` in a browser after the command succeeds.

For live editing, run:

```sh
mdbook serve docs/book --open
```

## Build Haddock API documentation

Use the same two-stage gate as CI and release preparation. First, install all 16 libraries with documentation into a fresh isolated Cabal store. Reject every warning and require one `.haddock` interface per manifest package. Then run a separate `cabal haddock all --haddock-options=--no-warnings` pass only to collect declaration coverage with `scripts/check-haddock-coverage`.

The second pass does not prove warning freedom. See [Release preparation](release-preparation.md) for the exact commands. The book explains concepts and workflows. Haddock lists exact types, constructors, and error values.

## Optional CUDA build

Enabled builds require the pinned CUDA 13.0 headers at compile time. Hardware
execution is bounded to the `sm_121` device profile. The executable links
`libdl`, not `libcuda`; missing-driver behavior is checked at runtime.

```sh
cabal test markovian-gpu-test \
  --project-file=cabal.project \
  -fcuda \
  --extra-include-dirs=/usr/local/cuda/include \
  --test-show-details=direct

cabal bench markovian-gpu-bench \
  --project-file=cabal.project \
  -fcuda \
  --extra-include-dirs=/usr/local/cuda/include
```

The GPU benchmark includes transfer and setup time. Do not compare its result with a kernel-only benchmark.
