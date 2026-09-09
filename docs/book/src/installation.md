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

The script selects project-local GHC 9.14.1/Cabal 3.18.1.0 and pinned standalone
tools without changing global defaults. GHC 9.8.4 is used only to construct
standalone HLint/cabal-fmt, never to build or test Markovian or run its HLS.

HLS 2.14.0.0 is built from the pinned official source with isolated configuration,
cache and store, not the generic GHCup bindist. Only this external tool uses the
approved upstream bounds recipe. The original source is unchanged; the generated
project strengthens the ABI flag. Because the raw upstream targets omit
`ghc-check`, both local HLS launcher names run the mandatory external full boot DB
and compiler/shared-library identity guard before starting LSP. The construction
receipt also binds every compiler/Cabal PATH selector's exact link, target and
content/mode identity. Seal and launch reject missing, redirected, replaced or
unexpected selectors and redirected tool directories before any guarded subprocess;
there is no system fallback or opportunistic repair. These are launch-time checks,
not protection against concurrent filesystem mutation after validation. Configure editors
to use the local launcher with no arguments or `--lsp`; `--check-only` tests the
guard, not project operation. Generic-wrapper options are not supported.

Keep the entire absolute
`.direnv/hls-official-2.14.0.0-ghc-9.14.1-<recipe-sha256>` build/store tree.
The digest covers canonical sorted compact JSON recipe bytes. Changed recipes
require explicit bootstrap/`--install` construction at a new absent root; check-only
and LSP never install. Preserve the unsuffixed historical installation unchanged.
The build is dynamically linked and not relocatable. Existing sealed builds are
explicitly reused; failed builds or identity drift fail closed. Preserve their
trees/logs unchanged; never repair/reseal an invalid same-identity build or use an ABI
fallback. Upstream GHC 9.14 excludes integrated HLint, Fourmolu, Ormolu,
stylish-haskell, Retrie, Stan and Splice; standalone tools do not restore them.
See the repository README for the source pins and operational evidence boundary.

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
  Markovian-2026.9.3.0.tar.gz \
  --name Markovian \
  --version 2026.9.3.0 \
  --extract unpacked
```

Then build from `unpacked/Markovian-2026.9.3.0`. See [Release preparation](release-preparation.md) for checksums, SBOMs, and provenance.

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
