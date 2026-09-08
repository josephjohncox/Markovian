# D-077 CUDA evidence receipts

**Decision status:** Proposed

This document defines the repository-side receipt boundary. No successful D-077 hardware receipt is recorded. The failed attempt below stopped before hardware tests and supplies no device-correctness or performance evidence.

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

The workflow and receipt validator share one sanitizer-version parser. It reads exactly one anchored `Version` line and compares the complete version token. Copyright/build numbers, unanchored strings, missing or malformed versions, duplicate Version lines, and partial version matches cannot supply the observation.

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

## Failed protected attempt and parser repair — 2026-09-08

[Run 34172607126](https://github.com/josephjohncox/Markovian/actions/runs/34172607126), attempt 1, selected source `4ea3d3db9570255320945311b29ff53359680ab7`. Step 6 failed while parsing this actual tool output:

```text
NVIDIA (R) Compute Sanitizer
Copyright (c) 2020-2025 NVIDIA Corporation
Version 2025.3.1.0 (build 36400806) (public-release)
```

The old banner-spanning regex stopped at the copyright digits and reported `Compute Sanitizer output has no version`. All correctness, sanitizer, benchmark, receipt, attestation and upload steps were skipped. No workflow artifact exists. Runner 25 processed this one failed job, removed its registration, and exited; the runners API then returned zero runners.

The repair uses the shared anchored parser in both collection and receipt checking. The receipt validator also rejects partial or ambiguous version observations under the existing `R007_RECEIPT_OBSERVATION` code. Profile/schema/digests, public Haskell interfaces, numeric policies and failure-class order are unchanged. Three existing record loops now state `strict=True`; the earlier record-count guard still rejects length mismatches first.

Local validation passed all 16 profile/receipt tests, including the real multiline banner, CRLF, malformed/duplicate/missing lines and complete-token checks. Direct execution of the literal workflow Python body against the installed sanitizer reproduced the old failure, returned `2025.3.1.0` with the repair, and rejected an ambiguous banner. Ruff, actionlint and diff checks passed. These are parser/regression results, not a successful hardware rerun.

Independent precommit review `49e6b779-caba-4807-9515-07e94d9f1664` passed the six-file repair with no findings. It inspected source and parent logs, not independent command reruns. Parent also passed 33 capability tests, release-policy checks, book/MathJax checks, all 16 source archives, archive-only teaching/capability gates, and all 16 archived profile tests. All six changed files matched their archive bytes. Logs are `/tmp/d077-version-{profile,capabilities,capability-tests,policy,book,sdist,archive,archive-profile}.log`. The unrelated learning-checker formatting edit remains outside the repair.

Failure evidence is retained privately at `/home/josephcox/.local/share/markovian/evidence/d077-34172607126/failure-audit.DYyL4s3F`, with API records, failed-job and runner-end logs, and `SHA256SUMS`. The failed-job log digest is `4dc279538d9b593e3b8b38d021e5ab8cdbc4d23e3ab41ff4096fb1e0374a3260`. These local copies are not an immutable publication. The named GitHub environment had no required-reviewer or deployment-branch rules; this restoration did not add or imply those protections.

## Historical D-074 boundary

The repository does not retain a complete D-077 receipt set for the D-074 release run. The D-074 pass statements remain immutable historical release records. They cannot satisfy D-077.

`docs/evidence/CUDA-TENSOR-2026-09-02.md` retains the available commands and raw benchmark samples. It combines runs and lacks the required executable, log, profile, and session bindings.

The published `v2026.9.3.0` source remains unchanged. `release/published-releases.json` prevents release preparation from rebuilding that version from another revision.
