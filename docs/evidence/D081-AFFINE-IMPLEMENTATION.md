# D-081 bounded affine-view implementation acceptance

**Decision status:** Accepted

**Availability:** UNRELEASED

Parent accepts D-081 only within the implemented bounded host-F64 scope. The separate authority is `/tmp/d081-acceptance-status-authority.md`. The implementation source commit is `cc900878dbf6f7bdc33f95affa9c15d2ea6f97ad`. This record does not claim an acceptance commit or main integration.

## Scope and contract

The accepted API is `Markovian.Tensor.Affine` in `markovian-tensor`. It provides checked immutable signed affine maps, permutation, reversal, bounded slicing, binding to the actual original base, materialization, and base-coordinate pullback. It rejects out-of-bounds and overlapping maps, including non-singleton zero strides. It preserves distinct nominal region, owner, storage, shape, and map evidence.

Read the [canonical affine contract](../plans/D081-AFFINE-VIEWS.md) with the [materialization addendum](../plans/D081-MATERIALIZATION-ADDENDUM.md). The addendum changes only its named resource, producer, failure, and fixture requirements. The [source proof](D081-MATERIALIZATION/SOURCE-PROOF.md) and [operative fixtures](D081-MATERIALIZATION/FIXTURES-OPERATIVE.md) retain their frozen meaning. Historical phase labels in these files and the [transpose prerequisite record](D081-TRANSPOSE-REVERSE-EQUIVALENCE.md) are not current implementation status.

Acceptance includes the bounded admission ledgers, prefix credit before demand, complete producer accounting, and staged registry publication. Logical source-resource bounds are not a physical-allocation theorem. Lifetime A still requires payload-dependent work to complete inside the callback. Callers must join or cancel-and-join dependent children on every exit. The runner does not supply automatic joins, universal post-close rejection, close/read synchronization, or prompt reclamation. Post-finalization use remains unsupported.

This decision excludes arbitrary map composition, broadcasting, mutation, general dtypes, borrowed pointers, persistent devices, and generic tensor or device lowering. It changes no release membership, asset, package version, or topology. All 16 package versions remain `2026.9.3.0`.

## Selected implementation evidence

The retained final-source campaign is `/tmp/d081-final-source-c0wKSVCX`. Its `REPORT.md` separates current-source executions from historical evidence and failed attempts. Select `closeout-qualified.json`, not an unconditional gate census.

| Evidence | Selected binding |
| --- | --- |
| Final source inventory | `current-inventory.json`, SHA256 `5c0dd6dffc81a17250b7d3b2721b39e34e7c5c284e2d23097a4252864226a2fd` |
| Successor Internal | SHA256 `47aa263ff42f5691e38016e379150d271343fa685bbcd726b50fb701114901a5` |
| Successor proof applicability | `LINT-PROOF-BRIDGE.md`, `proof-bridge-binding.json`, and `successor-desugared-r3.log` |
| Runtime and fault controls | `default-optimization-results.json`, `focused-results.json`, `publication-r8-results.json`, and `cleanup-results.json` |
| Broad native gates | `broad-results.json` and `broad-plans-r2.json`: 18 suites, 11 benchmarks, and modern lower-bound tests |
| Archives and installed evidence | `archive-bindings.json`, `consumer-results.json`, `haddock-result.json`, and `installed-module-bindings.json` |
| Actual teaching runs | `teaching-execution-binding.json`: seven runs before the receipt refresh |

The bridge covers the successor's ordered pair guards and monomorphic address partial application. Copied predecessor `internal`, `proofImpact`, and anchor records in `proof-bridge-binding.json` remain historical context. They do not assert current whole-module identity. Installed-library evidence and client optimization levels remain distinct. Fresh documentation archives do not become the older executed runtime archives.

The parent identity audit is `/tmp/d081-parent-final-runtime-audit-sxff8oi5/result.json`. It checked retained source, patch, archives, installed bindings, commands, and protections. It did not rerun project tests. The implementation commit record is `/tmp/d081-parent-implementation-commit-sq4kvfqh/result.json`.

Independent runtime review `cc1c55ff-16c9-4030-aaed-428a8aedd8fb` found no additional runtime defect. Its `d081-complete-runtime-review.md` retained a BLOCK on unconditional readiness. Exception follow-up `2f233fce-fe78-493c-96f4-7090dfacff6e`, `d081-index-exception-follow-up-review.md`, returned OK with notes for qualified readiness. Both records reside under the agent session's `subagent-artifacts/outputs/` review directories.

## Accepted historical index exception

Parent separately accepted the single exception in `/tmp/d081-parent-index-exception-disposition.md`. Historical strict writer-index byte preservation failed. The recorded SHA256 transition was `31e000912272e7c00a500851619cbf1d872815ee7e352673f9a277f2d6ee7f4b` to `c6338f7413472edc04aba5cba2bb38132717b2bf480dbc2918f6a35b5ebf966b`.

Forensics established checked index-entry equality and unchanged source identities. The old raw index bytes and cause are unavailable. No stat-cache-only conclusion or culprit attribution follows. The original failure, historical BLOCK, and baseline remain intact. Prospective continuity does not turn historical preservation into PASS. This exception authorizes no new drift.

## Separate acceptance-status index exception

Parent separately accepts the proposals-index transition from `ffebdcdeb94b87200470752002566b72f60d2f6452e42a7c23dcfd9023cfc516` to `0eb8924244c995425b55c851716cadef1b490a3f8a93d1aea7463cc211fbc85a`. Read the [separate disposition](file:///tmp/d081-proposals-index-exception-disposition.md) and [forensic result](file:///tmp/d081-parent-proposals-index-forensics-saq48zf_/result.json) before reviewing this status delta.

Both raw indexes are available for this event. The 118 changed bytes affect only cached ctime/mtime fields for seven byte-identical teaching outputs and the trailing checksum. All 604 entry paths, object IDs, modes, flags, stages, other stat fields, padding, and TREE bytes remain unchanged. The writing process is undetermined. Timestamp correlation does not identify a cause.

This finding does not resolve the earlier event's stronger uncertainty limits. Both strict byte-preservation checks remain failed. The original campaigns, failures, raw indexes, and baselines remain intact. Parent accepts this separate exception only for qualified completion and review. Prospective continuity uses a separate snapshot, not a repaired historical baseline. Any further unexplained drift stops work.

## Other decisions

D-082 remains Proposed and unimplemented. Its D-077 and D-081 acceptance prerequisites are satisfied. Graph contract freeze, implementation, independent review, and explicit hardware authority remain pending. This acceptance supplies no graph or deployed-revision hardware evidence.

D-083 is the next selected design and placement task. Its placement is not approved. D-084, D-085, and the separate EL-03, EL-04, and EL-05 frontiers remain unchanged and Proposed. No acceptance or release follows for any other frontier.
