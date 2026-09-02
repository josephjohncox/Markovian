# CUDA tensor-fragment evidence — 2026-09-02

## Scope

This record covers the uncommitted D-074 worktree based on revision
`1834799773c6f600624d879b1d823c5e6f2a09cf`. It covers only the prepared F64
64-by-64 matrix kernel and the focused 2-by-3/3-by-2 matrix and VJP fixtures on
the host below. It is not release evidence and does not establish generic
reverse-program lowering, arbitrary tensor execution, other-device support,
GPU advantage, or general device correctness.

## Host and toolchain

- Architecture: `aarch64`
- Device: NVIDIA GB10
- UUID: `GPU-ac353d74-ffaf-96d2-7849-b8d03d5cd1a7`
- Driver: `580.173.02`
- Compute capability: `12.1`
- CUDA compiler: `13.0`, build `13.0.88`
- GHC: `9.8.4`
- cabal-install: `3.16.1.0`
- PTX target: `sm_121`
- PTX SHA-256: `a01d2c898a78dc4f603a8919c9f84019b79066201bc12430fedc725ac97f6239`
- Build profile: Cabal `-O0` through `cabal.project.ci`

## Commands and focused results

The CUDA-disabled suite and device compile-fail boundary passed:

```sh
cabal test markovian-gpu-test --project-file=cabal.project.ci \
  --test-show-details=direct
bash backends/markovian-gpu/scripts/check-device-boundary
```

The CUDA-enabled suite passed structured probe/admission, known-answer module
self-test, CPU/CUDA matrix differential, and both matrix-VJP output
differentials:

```sh
cabal test markovian-gpu-test --project-file=cabal.project.ci -fcuda \
  --extra-include-dirs=/usr/local/cuda/include \
  --test-show-details=direct
```

CUDA C compilation with `-std=c11 -Wall -Wextra -Werror -pedantic`
(`-Wno-overlength-strings` only for the generated PTX string literal) passed,
and CUDA 13 regenerated the committed PTX and header.

A later sequential repair replaced direct `libcuda` linkage with an
executor-owned `RTLD_NOW | RTLD_LOCAL` driver table and CUDA 13 versioned
symbols. On this host, the enabled suite passed deterministic nonexistent
library and incomplete-symbol-table fixtures before it ran the hardware
matrix/VJP differentials and transaction faults. The bridge now has compile-
time checks for CUDA 13.0, host/device pointer widths, UUID size, and all fixed
Haskell-facing array boundaries. It rejects hardware outside the bounded
compute-capability 12.1 profile before context creation. The Cabal enabled
library links `libdl`, not `libcuda`; `ldd` and a no-GPU hosted compile-only
receipt remain acceptance gates rather than claims in this local record.

The repository now defines a digest-pinned
`nvidia/cuda:13.0.0-devel-ubuntu22.04` compile-only workflow. It verifies the
CUDA driver-header digest, regenerates both committed PTX artifacts, compiles
the complete enabled C/Haskell path, and runs missing-driver/symbol contracts
without GPU access. The workflow definition is not evidence that hosted CI has
run it.

This original run did not include Compute Sanitizer or fault injection. A later
repair run on the same local GB10 used `-fcuda -fcuda-fault-injection` and
passed deterministic allocation, both input transfers, launch, second VJP
launch, synchronization, copy-back, all three frees, teardown, combined
primary/cleanup, action-exception/cleanup, and scoped-fork ownership fixtures.
The repaired C bridge also compiled with `-Wall -Wextra -Werror`. Compute
Sanitizer was attempted after the repair, but the executable is not installed
on this host (`compute-sanitizer: command not found`). No sanitizer or protected
hosted-runner pass is claimed.

## Repair validation benchmark

The repaired benchmark was rerun on the local GB10 with explicit
`DeviceByUUID "ac353d74ffaf96d27849b8d03d5cd1a7"` selection:

```sh
MARKOVIAN_CUDA_DEVICE_UUID=ac353d74ffaf96d27849b8d03d5cd1a7 \
  cabal bench markovian-gpu-bench --project-file=cabal.project.ci -fcuda \
  --extra-include-dirs=/usr/local/cuda/include
```

The report identified `linux`, `aarch64`, and GHC `9.8`, checked every CPU and
CUDA output coordinate against the independent row-major oracle, and matched
the committed coordinate-weighted semantic checksum `386.2421875`. One warmup
was excluded for each path. The 20 measured CUDA transfer-inclusive samples
were:

```text
0.274640 0.277168 0.274384 0.254112 0.253936
0.282768 0.254385 0.251104 0.288400 0.256961
0.252688 0.305936 0.258369 0.253120 0.249489
0.243808 0.306161 0.272448 0.256512 0.249521
```

- Mean: `0.265795500 ms`
- Sample standard deviation: `0.018573470 ms`
- Minimum: `0.243808000 ms`
- Maximum: `0.306161000 ms`

The 20 CPU total samples were:

```text
9.001145 9.102425 8.989720 9.351657 9.132361
9.124728 9.083273 9.324905 9.308985 9.288488
9.107449 9.243129 9.215913 9.248584 9.399065
9.277433 9.128489 9.471913 9.286632 9.339305
```

- Mean: `9.221279950 ms`
- Sample standard deviation: `0.131993594 ms`
- Minimum: `8.989720000 ms`
- Maximum: `9.471913000 ms`

These local measurements do not establish a speedup claim. The paths have
different implementations, and the report is not protected hosted evidence.

## Earlier transfer-inclusive benchmark

Command:

```sh
cabal bench markovian-gpu-bench --project-file=cabal.project.ci -fcuda \
  --extra-include-dirs=/usr/local/cuda/include
```

The benchmark performed one excluded warmup and 20 measured runs. CUDA samples
include per-call device allocation, host-to-device transfer, one kernel launch,
stream synchronization, device-to-host transfer, and per-call buffer cleanup.
They exclude executor admission and teardown. The deterministic unweighted output checksum was `-0.37890625`. The repaired
benchmark uses an independently computed coordinate-weighted semantic checksum
of `386.2421875`, checks every measured output coordinate, and emits host,
architecture, compiler, selected-device, admission, and raw-sample metadata.
The samples below predate that report format and remain historical local data.

### CUDA transfer-inclusive samples (ms)

```text
0.276753 0.284513 0.270529 0.331969 3.504524
0.284929 0.260177 0.244592 0.323313 0.264017
3.783101 0.296353 0.260369 0.256193 0.240977
0.361553 0.308657 0.304673 0.255729 0.248561
```

- Mean: `0.618074100 ms`
- Sample standard deviation: `1.036248656 ms`
- Minimum: `0.240977000 ms`
- Maximum: `3.783101000 ms`

### CPU total samples (ms)

The CPU samples use the checked single-threaded list-indexing tensor reference
under the same `-O0` project profile. They are retained for reproducibility,
not as a fair optimized CPU baseline and not as speedup evidence.

```text
2756.295485 2748.461009 2716.380382 2769.649803 2647.592397
2608.351971 2519.506798 2718.530522 2517.500139 2781.558616
2714.164045 2655.803762 2729.594085 2756.019041 2786.807308
2728.137234 2742.646820 2665.090027 2787.257153 2783.748437
```

- Mean: `2706.654751700 ms`
- Sample standard deviation: `81.414017290 ms`
- Minimum: `2517.500139000 ms`
- Maximum: `2787.257153000 ms`

The CPU and CUDA timings measure different implementation maturity and must not
be used for a performance-comparison claim.
