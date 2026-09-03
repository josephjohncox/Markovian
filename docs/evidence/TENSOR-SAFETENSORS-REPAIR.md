# Tensor allocation and SafeTensors repair evidence

## Scope

This record covers the dirty integration overlay based on revision `1834799773c6f600624d879b1d823c5e6f2a09cf`. It is focused local implementation evidence only. It does not accept D-072 or D-073, establish general tensor or SafeTensors semantics, or satisfy an immutable release gate.

## Deterministic tensor allocation evidence

`markovian-tensor-fault-test` passed deterministic fixtures for:

- complete multi-output preflight before the first allocator call;
- first and second allocation failure;
- a synchronously thrown allocator failure;
- explicit partial-set finalization;
- rollback without storage-ID, payload, buffer, or work commit;
- retry after failure and atomic two-output commit;
- rollback and session-close cleanup diagnostics;
- success, `Left`, action exception, and action-exception plus cleanup exits;
- zero tracked live allocations after every close.

## Bounded SafeTensors evidence

`markovian-safetensors-test` passed the metadata-free F64 profile fixtures for scalar, empty, infinity, NaN payload, signed zero, subnormal, and transposed logical row-major materialization. It also passed malformed JSON and UTF-8, duplicate semantic names and descriptor keys, missing and unknown fields, unsupported metadata and dtype, numeric overflow, offset order, shape-size mismatch, holes, overlap, truncation, trailing bytes, deterministic name ordering, decode/encode/decode identity, and exact and one-below file/header/tensor/name/rank/dimension/element/payload limits.

The canonical 504-byte wire fixture has SHA-256 `766d0e87d116d8a8d8ca88b7c6fc05edcde75b91aa3e4d318ffe35275e17da21`. Its checked-in lowercase hex representation has SHA-256:

```text
19e1744ced14e53bfcde3042b4abf1f6e2d09648ed6da0c0117050e10e5f2832
```

The opacity and region-escape scripts passed for both tensor storage and SafeTensors files. The SafeTensors exposed module has 100% Haddock declaration coverage with no focused Haddock warning. The complete mdBook link, local MathJax, and 414 display-math checks passed. Cabal built and ran the tensor, allocator-fault, and SafeTensors suites from freshly unpacked local source archives containing only the two package sources and their declared data files.

## Focused commands

```sh
cabal test markovian-tensor-fault-test markovian-safetensors-test \
  --project-file=cabal.project.ci --test-show-details=direct
bash packages/markovian-tensor/scripts/check-tensor-boundary
bash packages/markovian-safetensors/scripts/check-safetensors-boundary
cabal sdist markovian-tensor markovian-safetensors
```

The focused tensor-fault and SafeTensors suites passed with GHC 9.4.8 and 9.8.4; the recorded cabal-install was 3.16.1.0. The SafeTensors suite also passed its focused `--prefer-oldest` solve with `bytestring-0.11.1.0`. At the time of this focused repair record, complete-graph preferred-oldest, full-graph isolated Haddock, full archive, hosted, attestation, protected hardware, and publication authorization remained open. Those gates were completed separately before release.
