# D-077 CUDA evidence receipts

**Decision status:** Accepted

This document defines the repository-side receipt boundary. Run `34181389307` produced a validated same-session receipt and 14 cryptographically verified subjects, recorded below. The [2026-09-08 user amendment](../WORKFLOWS.md#gpu-deployment-evidence) replaces permanent retention with deployment-scoped availability and verification. D-077 is Accepted for the bounded repair and policy, not deployment or release; see the dated acceptance note below. The two earlier failed attempts remain unvalidated and are not combined with the successful run.

## Profile authority

`backends/markovian-gpu/profile.json` is the only GPU profile authority. Its SHA-256 addresses the complete profile. A new digest does not itself establish review or acceptance.

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

The workflow asks GitHub to attest every validated evidence file. A workflow artifact remains temporary storage. The current 90-day retention is allowed under the user amendment.

At each GPU deployment or promotion, verify the complete raw directory and every attestation subject against the actual deployed revision. Preserve signature, issuer, source, workflow/run, and transparency checks. Later expiry does not invalidate past verification, but missing raw data cannot support new verification or promotion. Keep compact revision, profile, run/session, subject-digest, verification-result, and expiry records after raw-data expiry.

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

## Verified hardware execution — 2026-09-08

[Run 34181389307](https://github.com/josephjohncox/Markovian/actions/runs/34181389307), attempt 1, completed every job step successfully at source `3e850085fa96c4e48a80270b9e49e9f55fe0f757`. The merged source tree equals reviewed PR8 head `332c879ed21e6f9e520533a4fb2c53c475141b2f`. Independent combined review `129c88fb-e070-4fda-86fd-a6e01c90a5c9` passed with no findings; parent separately verified all ten exact-head hosted checks before merge. The reviewer inspected source and parent logs, not independent command reruns.

The exact-commit receipt validator passed all six records: correctness, memcheck, initcheck, racecheck, synccheck and benchmark. Validation bound the revision, session, profile/PTX, configured and observed UUIDs, executable/command/log digests, successful outcomes, exact checksum `49439/128`, and 20 ordered finite nonnegative raw samples. The session is `github-34181389307-1-3e850085fa96c4e48a80270b9e49e9f55fe0f757`; the device is `GPU-ac353d74-ffaf-96d2-7849-b8d03d5cd1a7`.

| Item | SHA-256 |
| --- | --- |
| Profile | `5fbed61193cf483a2ff5642c7487ad052add4ed52da1a83a110da4711c7480dd` |
| PTX | `a01d2c898a78dc4f603a8919c9f84019b79066201bc12430fedc725ac97f6239` |
| Receipt | `d3359765706ef46de3a410d2d2fe57918abf42738ce8667e32dd48b54fd27ce2` |
| Test executable | `3fcbcefdb993e9a90a1e7b5ddbe72c33fd3fa47758d07b40fbf3148c95145de5` |
| Benchmark executable | `ff67a719b38b548d3642a42f394a71326cc594a7168431afdca18a78d0a5388a` |
| Downloaded artifact ZIP | `f40802a47f9074b7ce13559a6c3d1a079b242bec3369f002f05d13fa331996b0` |
| Retained selected Sigstore bundle JSON | `2a0cb134bb581786329ea39055e29d1f4645b60d3e2b6364ce0b045b876e0868` |

The artifact is `cuda-hardware-3e850085fa96c4e48a80270b9e49e9f55fe0f757-1`, ID `10039045151`. Its downloaded ZIP digest matches GitHub's artifact metadata. The selected attestation bundle covers exactly the 14 downloaded subject names and SHA-256 values, with no missing or additional subjects.

**Cryptographic verification:** Parent ran Cosign 2.4.3 `verify-blob-attestation` separately for all 14 actual downloaded files, using `--new-bundle-format`, `--type slsaprovenance1`, default claims/certificate/transparency checks, and a clean environment. Every call returned `Verified OK`. No insecure flags, custom trust roots or downloaded-binary execution were used. The exact certificate identity was `https://github.com/josephjohncox/Markovian/.github/workflows/cuda-hardware.yml@refs/heads/main`; the issuer was `https://token.actions.githubusercontent.com`.

The signed statement was also checked for the exact repository, workflow path/ref, source Git commit, builder identity, self-hosted runner class, dispatch event and invocation `https://github.com/josephjohncox/Markovian/actions/runs/34181389307/attempts/1`. API retrieval or unsigned metadata selection alone was not treated as verification. The verification tool's binary SHA-256 is `fd266577e69532f9a5b0cba85147958ce0ae97e285ceb33b81f489c3c7ee2a9b`.

**Materialization boundary:** The raw extraction gave both executable files mode `0600`, so the unchanged validator rejected it with `R009_RECEIPT_ARTIFACT`. After all 14 subjects passed cryptographic verification, parent made a separate byte-identical copy and restored only owner execute permission on those two files (`0600` to `0700`). The exact committed validator then passed with explicit revision/session/UUID arguments. The ZIP and raw extraction were not modified, no validator check was bypassed, and no downloaded executable was run.

**Retained audit:** `/home/josephcox/.local/share/markovian/evidence/d077-34181389307/audit-txi4c5bv` contains the raw ZIP/payload, separate verification copy, exact-commit validator, API metadata, selected bundle/statement, all 14 verification commands/results/logs, materialization record, and checksum manifests. `AUDIT-SHA256SUMS` binds the local audit files; this is not a claim that local storage is immutable. This audit records the original verification. This amendment does not assert a fresh check of local availability.

Runner 27 processed this one job, removed its registration and exited with listener code 0. A fresh runners API query returned zero runners. The pinned, update-disabled runner archive was version 2.337.0 with SHA-256 `9b1dc70626422526e3c94767cf024896beb15da5342a3f4819bf2feac13e0393`.

**Original retention limit:** The GitHub artifact expires on `2026-12-07T02:49:34Z`. The policy at verification time required immutable retention. Expiring artifacts and local copies did not satisfy that requirement.

**Current limits after the 2026-09-08 user amendment:** Permanent retention is no longer a future deployment or promotion blocker. Full evidence and signature verification remain mandatory at deployment or promotion. The compact record above preserves the exact tested revision, profile, run/session, subject digests, verification method/result, and artifact expiry. It does not replace missing raw data or attest a descendant revision. Governance acceptance may cite exact tested `3e850085fa96c4e48a80270b9e49e9f55fe0f757` plus an explicit reviewed docs-only delta. Actual GPU deployment requires evidence binding its deployed revision.

The recorded named environment has no required-reviewer rules or deployment-branch policy, as independently rechecked during the original audit without modification. The user amendment changes no permissions and adds no infrastructure gate. This run does not accept D-077, publish a release, establish general GPU correctness, or justify a speedup claim.

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

## Racecheck summary repair contract — 2026-09-08

[Run 34175882779](https://github.com/josephjohncox/Markovian/actions/runs/34175882779), attempt 1, selected source `6a6ebfc5744aa10704c9bdbc8967ee05b289422d`. Steps 6–10 completed successfully, but step 11 rejected the candidate receipt with `R011_RECEIPT_OUTCOME`. The profile required `ERROR SUMMARY: 0 errors` for racecheck. The retained tool log instead ended with:

```text
========= RACECHECK SUMMARY: 0 hazards displayed (0 errors, 0 warnings)
evidence-record-exit: 0
```

Attestation and upload were skipped. Runner 26 processed this one job, deregistered and exited. The raw 14-file candidate payload is preserved unchanged at `/home/josephcox/.local/share/markovian/evidence/d077-34175882779/failed-payload-preserved`, with a sibling digest manifest and explicit failed-attempt label. It is not a validated receipt or immutable publication.

The following repair boundary is frozen before implementation:

- Change only racecheck's required success marker to `RACECHECK SUMMARY: 0 hazards displayed (0 errors, 0 warnings)` in the profile authority and its validator specification. Do not accept the generic error summary as an alternative for racecheck.
- Keep memcheck, initcheck and synccheck on `ERROR SUMMARY: 0 errors`. Keep all commands, exit-code requirements, record/binding checks, failure-class order, schema versions, numeric policies, PTX, public Haskell interfaces, package versions and dependency edges unchanged.
- Regenerate all three profile-bound outputs with the new profile digest. This is a new profile identity, not a reinterpretation of the failed run's profile.
- Build sanitizer test output from independently recorded tool-specific summaries, not from the profile's expected marker list. Require regressions for the observed racecheck summary, generic/wrong-tool summaries, missing summaries, and nonzero hazard/error/warning counts. Recompute mutated log digests so these tests reach the outcome check.
- Obtain independent repair review and pass local/archive/hosted gates before another hardware run. The new run must bind the newly merged source and profile; do not rewrite or relabel either failed payload.

The implemented correction has profile digest `5fbed61193cf483a2ff5642c7487ad052add4ed52da1a83a110da4711c7480dd`. The three generated artifacts and the deterministic plan golden now bind that digest. A structural comparison confirmed that the racecheck marker is the only profile-value change. The teaching output receipt was refreshed by executing the teaching gate; only its source fingerprint changed.

The independent observed-summary fixture first reproduced the old `R011_RECEIPT_OUTCOME` failure. After repair, all 19 profile/receipt tests and the warning-error-enabled CUDA-disabled GPU suite passed. The plan test initially rejected the old profile digest, then passed after that digest alone was updated. Capability/release-policy checks, 77 compiled Haskell fences and teaching executions, book/MathJax checks, and all 16 source archives with archive-only teaching/capability checks passed. Logs are `/tmp/d077-racecheck-{red,profile,gpu,capabilities,policy,learning,book,sdist,archive}.log`.

Independent review `9a648cbf-dc8e-4430-bd29-c815f625ab29` passed the racecheck-only repair at `15bc22d327ca2865a029906291d7de7e8f079830` with no findings after reviewer quota became available. It inspected source and parent logs, not independent test executions. Earlier quota/authentication failures produced no verdict. The follow-on reporting correction below is a separate review scope; the combined repair must pass its review and hosted gates before a new run. No new hardware run is authorized by these local checks alone. D-077 remains Proposed. At that review, successful command steps did not replace validated, attested and immutably retained same-session evidence. The 2026-09-08 user amendment supersedes only the forward-looking permanent-retention requirement. Environment protections and release governance are unchanged.

## Follow-on benchmark label contract — 2026-09-08

A read-only check of the same retained failed payload found a second issue hidden behind R011. The benchmark contains 20 correctly numbered raw sample lines, followed by `cuda-transfer-inclusive sample standard deviation: 0.019122359 ms`. The R012 collector treats every line beginning `cuda-transfer-inclusive sample ` as a raw sample, so that statistic becomes an invalid 21st entry. This diagnostic does not validate or modify the failed receipt.

Before implementation, freeze this additional producer-only correction:

- Rename the printed statistic label from `sample standard deviation` to `standard deviation (sample)`. Retain its sample-statistic meaning, formula, value and units; raw sample labels and ordering remain unchanged.
- Do not relax the validator, alter its sample-line pattern, discard arbitrary malformed sample lines, or change the required count of 20. The profile digest remains `5fbed61193cf483a2ff5642c7487ad052add4ed52da1a83a110da4711c7480dd`.
- Add the noncolliding producer label to consumer checks, include the summary statistic in positive receipt fixtures, and retain a rehashed negative fixture with the old colliding label. Run the actual CUDA-disabled benchmark to exercise its shared formatter without claiming GPU evidence.
- Review the combined repair and pass the gates before a newly bound hardware run. Historical payloads remain untouched.

The observed colliding-label fixture reproduced `R012_RECEIPT_BENCHMARK` before the correction. Afterward, all 21 profile tests passed, including producer-label drift and rehashed colliding-summary rejection. The actual shared formatter was exercised through the warning-error-enabled CUDA-disabled benchmark: it emitted exactly 20 parseable `cpu-total sample NN` lines and a separate `standard deviation (sample)` line. This is reporting evidence, not GPU performance evidence. Logs are `/tmp/d077-summary-{red,profile,ruff,benchmark}.log`.

## Historical D-074 boundary

The repository does not retain a complete D-077 receipt set for the D-074 release run. The D-074 pass statements remain immutable historical release records. They cannot satisfy D-077.

`docs/evidence/CUDA-TENSOR-2026-09-02.md` retains the available commands and raw benchmark samples. It combines runs and lacks the required executable, log, profile, and session bindings.

The published `v2026.9.3.0` source remains unchanged. `release/published-releases.json` prevents release preparation from rebuilding that version from another revision.

## Bounded acceptance — 2026-09-08

Readiness PASS `a6b05ca2-3b1f-4202-b01c-378a1353957b` (`d077-acceptance-review.md`) supports the separate parent-authorized [D-077 acceptance](../DECISIONS.md#d-077-govern-gpu-profiles-and-evidence-truth): existing profile authority, schemas, failure order, exact-dyadic versus CPU/CUDA comparisons, receipt repair, and deployment-scoped verification policy only. Earlier Proposed statements above are point-in-time records, not current status.

The hardware binding remains source `3e850085fa96c4e48a80270b9e49e9f55fe0f757`, run `34181389307` / attempt `1`, profile `5fbed61193cf483a2ff5642c7487ad052add4ed52da1a83a110da4711c7480dd`. The separately reviewed docs-only delta from that source to `c86b4e0241debe0a9ea51b6e9f962d89ea8293df` contains exactly `RELEASE-CHECKLIST.md`, `TODO.md`, `backends/markovian-gpu/CHANGELOG.md`, `docs/CONTEXT.md`, `docs/DECISIONS.md`, `docs/WORKFLOWS.md`, and this file. Its supplied manifest and patch are `/tmp/proposals-d077-governance-delta.json` and `/tmp/proposals-d077-governance-delta.patch`. Neither the governance endpoint nor status-edit base `049372690908f179a095bb170ec7e80034b04d2e` is hardware-tested by this receipt.

No new hardware or cryptographic execution is claimed by the status edit. Acceptance does not deploy, release, change permissions, invent environment protections, or establish general correctness, portability, or speedup. Finite raw retention is allowed; every actual deployment/promotion still requires complete evidence and signature verification bound to its deployed revision.
