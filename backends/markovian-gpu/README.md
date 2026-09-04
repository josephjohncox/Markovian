# markovian-gpu

Optional CUDA 13 execution for the checked F64 matrix-product and matrix-VJP
fragment in `markovian-tensor`. The package also depends on
`markovian-tensor-reverse` at the optional integration tier, but it does not
provide a CUDA resolver for that package's closed host reverse programs.

The default build is CUDA-disabled. It still tests CPU execution, explicit
`CUDANotCompiled` failure, and configured pre-launch fallback. An enabled
executor admits one selected device by creating a private context, loading the
committed `sm_121` PTX, creating a private stream, and running a known-answer
self-test. Rank-2 scoped executors cannot escape their owner callback. Calls
and teardown take the same lock, so close waits for in-flight work before the
native owner is destroyed. Calls return only after synchronization and host
copy-back. Primary, action-exception, and bounded cleanup diagnostics are
retained together. Cleanup failure always prohibits fallback without changing
the separate user-launch history.

```sh
cabal test markovian-gpu-test --project-file=cabal.project.ci -fcuda \
  --extra-include-dirs=/usr/local/cuda/include --test-show-details=direct
cabal bench markovian-gpu-bench --project-file=cabal.project.ci -fcuda \
  --extra-include-dirs=/usr/local/cuda/include

# Protected validation only: enables deterministic Driver API fault hooks.
cabal test markovian-gpu-test --project-file=cabal.project.ci \
  -fcuda -fcuda-fault-injection \
  --extra-include-dirs=/usr/local/cuda/include --test-show-details=direct
```

`profile.json` is the only profile authority. It fixes the CUDA ABI, PTX,
kernel ABI, device constraints, numeric policies, admission tests, and receipt
schema. `scripts/check-profile` checks generated C, Haskell, and toolchain
artifacts against its digest. The committed PTX targets only this bounded
`sm_121` profile. Enabled builds compile against pinned CUDA 13.0 headers but
link only `libdl`. At runtime the
bridge opens `libcuda.so.1` with `RTLD_NOW | RTLD_LOCAL` and resolves the
complete required table, including the CUDA 13 versioned ABI names, before it
calls `cuInit`. A missing library or symbol is an explicit pre-launch error:
`RequireCUDA` returns it, while `PreferCUDA FallbackBeforeUserLaunch` can use
the CPU reference. A device outside compute capability 12.1 is also rejected before context creation. Native creation verifies the probed UUID before context creation. Admission evidence returns this native-verified UUID. There is no fallback after launch commitment.

The executor owns the driver handle and function table. A failed device free poisons the executor and retains that allocation for teardown. The executor rejects later launches after cleanup uncertainty. Teardown attempts stream, module, and context destruction before `dlclose`. It does not call `dlclose` while a context can remain live. The generated PTX header and Haskell artifact module contain one target, ABI, byte length, and digest record. The CUDA-disabled build compiles without CUDA headers or
a driver library. The digest-pinned compile-only workflow regenerates the PTX
and generated C header, checks the pinned CUDA driver-header digest, compiles
the enabled C/Haskell path with strict C warnings, and runs missing-library and missing-symbol fixtures without a GPU. Owned early and late incomplete-driver fixtures verify that symbol admission does not call `cuInit`.

The exact specification interprets each finite binary64 input word as an exact
dyadic rational. The CPU refinement uses separate multiplication and addition
in ascending `k` order. The CUDA refinement uses the committed
`fma.rn.f64` PTX sequence. Tests compare each path separately with the exact
fixture. Neither floating path is the oracle for the other.

The protected hardware workflow normalizes its configured NVIDIA UUID and
binds tests and benchmarks to `DeviceByUUID`; a missing selected device fails.
It runs allocation, transfer, launch, second-launch, synchronization,
copy-back, free, and teardown fault paths before Compute Sanitizer. Direct
sanitizer invocations use Cabal's `markovian_gpu_datadir` override and obtain
the executable with `cabal list-bin`, so package data is not cwd-relative. The
`cuda-fault-injection` flag is for validation builds only.

The workflow creates one session identity. It retains the test and benchmark
executables, exact commands and exit values, logs, native device observations,
tool version output, semantic checksum, and ordered raw samples. The receipt
binds each file by SHA-256. `scripts/cuda_profile.py validate-receipt` rejects
cross-session, command, outcome, artifact, and observation substitutions. The
workflow also requests GitHub attestations for the validated files. A temporary
workflow artifact is not durable evidence. No D-077 hardware receipt exists
until the protected workflow runs and an immutable evidence store retains and
verifies its complete output.

The package does not claim support for older devices, arbitrary tensor graphs,
arbitrary reverse programs, general device correctness, cross-device bit
equality, GPU speedup, or release readiness.
