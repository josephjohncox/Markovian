# D-077 CUDA evidence receipts

**Decision status:** Proposed

This document defines the repository-side receipt boundary. It records no hardware run and makes no device-correctness or performance claim.

## Profile authority

`backends/markovian-gpu/profile.json` is the only GPU profile authority. Its SHA-256 addresses the complete reviewed profile.

The authority fixes these items:

- the CUDA header ABI and required driver symbols;
- the compile image and PTX ISA, target, and digests;
- the kernel name, ABI, launch width, and index type;
- the admitted device capability and UUID width;
- the exact dyadic, CPU operation-order, and CUDA FMA policies;
- the admission test names and order;
- benchmark inputs, counts, warmups, and exact checksum;
- the complete receipt fields, record commands, success markers, and size bounds;
- the total profile and receipt validation failure order.

`backends/markovian-gpu/scripts/check-profile` checks the authority and generated artifacts. It also tests malformed and mismatched receipts.

The profile digest for this worktree is generated into `toolchain.env`, the C profile header, and the internal Haskell artifact. The device plan report includes that digest.

## Three numeric layers

The exact specification interprets each finite binary64 input word as one exact dyadic rational. Matrix multiplication sums exact rational products. The VJP uses the two stated exact rational matrix products.

The CPU refinement multiplies and adds separate binary64 values in ascending `k` order. Its fixture comparison uses the CPU tolerance in the authority.

The CUDA refinement uses the committed PTX `fma.rn.f64` sequence in ascending `k` order. Its fixture comparison uses the separate CUDA tolerance in the authority.

Neither floating implementation is the oracle for the other. The cancellation fixture also shows that a floating refinement can differ from the exact result.

## Same-session receipt set

The protected workflow now creates one session identity before tests run. Every correctness, sanitizer, and benchmark log prints these bindings:

- session identity;
- source revision;
- profile SHA-256.

The receipt binds the following immutable values:

- the source revision, workflow run, attempt, job, and session identity;
- the retained canonical profile and PTX bytes and their SHA-256 values;
- the configured, `nvidia-smi`, and native runtime UUID values;
- the observed driver API, driver, toolkit, and sanitizer versions;
- the retained test and benchmark executable bytes and SHA-256 values;
- each exact command, command SHA-256, exit code, log, and log SHA-256;
- the required correctness, boundary, sanitizer, and benchmark success markers;
- the exact semantic checksum and ordered raw CUDA samples.

The validator rejects unknown fields, missing records, changed artifacts, cross-session logs, command substitution, nonzero outcomes, and observation mismatches. Profile and receipt JSON inputs are each limited to 1,048,576 bytes and 128 container levels before parsing. Excessive nesting is reported as the corresponding JSON failure instead of escaping the validator. Correctness and all sanitizer records use the same retained test executable. Sample indices must be exactly `01` through `20`. Each sample must be finite and nonnegative.

The workflow asks GitHub to attest every validated evidence file. A workflow artifact remains temporary storage. A future claim must retain the evidence and verified attestation in an immutable release or provenance store.

## Validation failure order

The validator returns the first applicable class in this total order. A code prefix is part of the stable diagnostic.

| Order | Profile failure code | Meaning |
| --- | --- | --- |
| 1 | `P001_PROFILE_READ` | The profile file cannot be read. |
| 2 | `P002_PROFILE_JSON` | The profile is not JSON. |
| 3 | `P003_PROFILE_ROOT_TYPE` | The profile root is not an object. |
| 4 | `P004_PROFILE_CANONICAL` | The profile bytes are not canonical JSON. |
| 5 | `P005_PROFILE_SCHEMA` | A field, nested type, or field set is invalid. |
| 6 | `P006_PROFILE_VALUE` | A reviewed policy or schema value differs. |
| 7 | `P007_PROFILE_CONSUMER` | A source, PTX, fixture, or workflow consumer differs. |
| 8 | `P008_PROFILE_GENERATED` | A generated artifact is stale. |

| Order | Receipt failure code | Meaning |
| --- | --- | --- |
| 1 | `R001_RECEIPT_READ` | The receipt file cannot be read. |
| 2 | `R002_RECEIPT_JSON` | The receipt is not JSON. |
| 3 | `R003_RECEIPT_ROOT_TYPE` | The receipt root is not an object. |
| 4 | `R004_RECEIPT_CANONICAL` | The receipt bytes are not canonical JSON. |
| 5 | `R005_RECEIPT_SCHEMA` | A field, nested type, or field set is invalid. |
| 6 | `R006_RECEIPT_BINDING` | The source, session, workflow, profile, or PTX binding differs. |
| 7 | `R007_RECEIPT_OBSERVATION` | A device or tool observation differs. |
| 8 | `R008_RECEIPT_RECORD` | A command, record order, digest, or exit value differs. |
| 9 | `R009_RECEIPT_ARTIFACT` | An executable is missing, changed, oversized, or not executable. |
| 10 | `R010_RECEIPT_LOG` | A log is missing, changed, oversized, or misnamed. |
| 11 | `R011_RECEIPT_OUTCOME` | A log lacks a required command, binding, exit, or success marker. |
| 12 | `R012_RECEIPT_BENCHMARK` | The checksum, warmup, count, sample order, or sample value differs. |

## Historical D-074 boundary

The repository does not retain a complete D-077 receipt set for the D-074 release run. The D-074 pass statements remain immutable historical release records. They cannot satisfy D-077.

`docs/evidence/CUDA-TENSOR-2026-09-02.md` retains the available commands and raw benchmark samples. It combines runs and lacks the required executable, log, profile, and session bindings.

The published `v2026.9.3.0` source remains unchanged. `release/published-releases.json` prevents release preparation from rebuilding that version from another revision.
