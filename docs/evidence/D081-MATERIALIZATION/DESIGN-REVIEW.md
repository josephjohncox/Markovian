# D081 r4 — independent prospective design review

## Review

- **Correct:** The controlling r4 amendment supplies source-derived work, construction, live-storage and retention bounds for startup, prefix scanning, all four operation rows and finite failure phases.
- **Correct:** The source, final equations and **`FIXTURES-OPERATIVE.md`** agree. The parent resolved the brief’s mistaken fixture-file selection during this review; details below.
- **Fixed:** None. Review-only; no commands, mutations, tests or hash recomputation.
- **No issues found.**
- **Merge verdict: OK for prospective design review only.**
- **Prospective design verdict: DESIGN PASS.**

The controlling candidate is adequate for **separate parent arbitration/refreeze consideration**. This verdict does not adopt the contract, authorize implementation, validate the held implementation delta, or grant acceptance/release authority.

## Basis and scope

Paths below use:

- **E:** `/tmp/d081-proof-assembly-QQfz0BTQ`
- **I4:** `E/candidate-r4/Markovian/Tensor/Internal.hs`
- **V2:** `/tmp/d081-retention-v2-B0ZMyODc`

I read the controlling directions, original design authority, prior v2 review39a40477, complete frozen31bf plan, complete r4 amendment/proof, three advisory schedules, complete candidate source, supporting Shape/library sources, clarification authorities, equations, all nineteen final fixture paths and their 133 negative policies, and retained control sources/results.

The reviewed scratch source is attributed to SHA-256:

`8b0b50628bf10e260f6048e2fe96dcc10359de31c7958591fecfbc6aef98dee5`

It is distinct from held417337 and historically executed a650c2e0. Those identities come from retained provenance and the parent identity audit—not a reviewer recomputation. Embedded copies of the amendment and advisories were not counted as independent evidence.

## Source-bound proof assessment

### Elementary schedules and composition

`SOURCE-PROOF.md` §§1–4 reconciles the advisory schedules rather than summing incompatible allowances:

- Aggregate constructors, calls, saved fields and force operations are expanded into primitive assignments/inspections. Numeric referents are counted separately from list element/reference slots.
- The admitted-cons sum is **323≤384**; per-inspection transient and survivor inventories are **37≤40** and **11≤12**.
- Complete common fixed inventories are **1872≤2048 work**, **420≤448 transient slots**, and **113≤128 body-common survivor slots**. The alternative pre-body diagnostic inventory is **116≤128**.
- Transaction/private-prepare controls are included before header demand. Optional owner-record/outer-witness completion has its own fixed allowance, without forcing the witness tail or borrowing tensor-producer storage.
- Shared geometry helpers and charge/usage objects are charged once. Prefix storage remains reserved during the body, so full Q covers both the preceding prefix peak and subsequent simultaneous storage.
- The eight-slot construction argument is independent of the H/R lifetime inventory; `L=8W+Q` is not used as a substitute for either proof.

The four expanded rows agree with the A-plus-body composition and I4’s formula branches (`I4:545–584`; `OPERATIVE-AMENDMENT.md` §§3–4; `equations-FINAL.py:8–37`). In particular, the resulting constant W/H/R triples are:

| Operation | W | H | R |
|---|---:|---:|---:|
| New map |3870|841|195|
| Transform |4857|921|219|
| Bind |3920|1126|246|
| Pullback |5168|1556|277|

The proof retains the actual `V+2P` coordinate calls, separate pair-suffix terminals, quadratic permutation work, and sequential coordinate workspace. It introduces neither an address cache nor retained operation history.

### Prefix and failure boundaries

The source implements local Q independently of historical successful peak, with cells→work→live checks before the next cons **or nil** (`I4:475–545`). Startup minima, initial usage and runtime minimum checks use the revised constants consistently.

The corrected oracle returns local Q unchanged by the historical maximum and exercises above/equal/below cases and successive debits (`parametric_equations.py:3–37`; `ProofMain.hs:29–43`).

The failure partition covers fixed-entry rejection, failed debit, semantic header exits, old-payload/full-reservation rejection, post-admission descriptor/geometry failure, finite staging/initialization/pre-put failure, and post-put delivery failure (`OPERATIVE-AMENDMENT.md` §5; `SOURCE-PROOF.md` §§2,4–5). The pullback allowance separately prepays finite cleanup and the kernel diagnostic: **88+39=127≤128**.

Immutable failed-attempt ledgers do not erase materialized caller-retained output. External allocator/finalizer/String execution and repeated asynchronous cleanup remain explicitly outside the total finite theorem.

### Producers, library lowering and ownership

The producer inventory accounts for newly materialized base metadata in bind and independently caller-retained seed metadata in pullback. Completed results are distinguished from entirely pending caller-selected producer applications (`OPERATIVE-AMENDMENT.md` §§1,6; `SOURCE-PROOF.md` §4).

The installed-version List source contains the unfused `scanr`, lazy `scanrFB` pair, `strictUncurryScanr`, rewrite rules, and `drop`/`unsafeDrop` definitions. The proof supplies a dominating elementary expansion, including discarded-prefix machinery and converted operands; it does not rely merely on documentation’s complexity statements (`library-source/GHC.Internal.List.txt`, those definitions; `library-source/identity.json`; `SOURCE-PROOF.md` §4).

Empty stride values remain unforced. Nonempty value demand requires the retained suffix products, not the discarded full product. Completed finite refinement leaves no B-sized validation traversal pending: the existing reads precede selection of the validation result (`I4:1696–1706`).

Registry work appears once as:

`16n+8+8(n+2)=24n+24`

The fully forced committed-spine prerequisite remains universal, including legacy allocation paths. Actual publication—not preparation or result delivery—is the ownership boundary (`I4:1473–1478`; `SOURCE-PROOF.md` §4). The five clarification scopes remain coherent with these amendments.

## Equations, fixtures and retained evidence

The final equation source and complete JSON traces correctly implement cumulative sums and peak maxima. They distinguish actual transform scan counts from the common transform reservation (`equations-FINAL.py:64–121`).

The earlier-peak discriminator is particularly important:

- rank512-base bind Q: **67932**
- subsequent pullback Q: **42297**

Therefore every whole-path policy with live67931 fails the **earlier bind**, including competitions with later cumulative cells/work shortages. The final traces preserve that first-event precedence.

Retained evidence is attributed narrowly:

| Evidence | Supported conclusion |
|---|---|
| r4 O0/O2 formula/prefix logs | Each records432 numeric-vector checks and prefix/priority/poison controls. |
| Candidate-r2 `FailuresMain` | Bounded scalar admission/metadata controls; not a full r4 runtime execution. |
| Demand-r2 `PrefixDemandMain` |512 work-first,511 live-first and512 cells-first requested inspections; not allocation counts. |
| V2 refinement/metadata/stride controls | IO-return completion, deferred/preforced/repeated metadata distinctions, actual empty-stride production, and actual transpose-seed values. |
| V2 batch controls | Lazy items and bounded synchronous post-publication ownership behavior—not universal asynchronous cleanup correctness. |

The failed v2 pending-reshape expectation and failed assembly r3 global-priority assertion remain visible alongside their corrected revisions. The historical eager-item/post-commit “unexecuted” wording is explicitly corrected by `STATUS-ANNOTATION.md`.

## Parent-resolved fixture-selection provenance note

The original review brief incorrectly designated **`FIXTURES-FINAL.md`** as canonical. I raised its disagreement with the operative source/equations.

The parent explicitly clarified through the blocking supervisor reply that:

- **`FIXTURES-OPERATIVE.md` controls**, alongside candidate-r4, `equations-FINAL.json` and amendment §7.
- `FIXTURES-FINAL.md` remains immutable, **nonoperative historical evidence**, despite its filename.
- This is a selection correction, not a source, coefficient or numerical-proof change.

The seven historical pull-containing rows are each **320 cells and40 work below** the controlling r4 totals; peaks are unchanged:

| Fixture | Controlling cells / work / peak |
|---|---:|
| signed-pull |123442 /14894 /2129|
| empty-pull |112594 /13538 /2129|
| mixed1024 |316994 /38066 /10305|
| batches512-0-512 |316994 /38066 /10305|
| rank512-base-scalar-pull-peak-before |6895157 /848044 /67932|
| scalar-base-rank512-seed |10243125 /1261740 /104761|
| pull-private-failure |123442 /14894 /2129|

Evidence: `FIXTURES-FINAL.md:14–25`, `FIXTURES-OPERATIVE.md:14–25`, final JSON fixture entries, and `I4:568–574`. No artifact was edited or rerun.

## Final disposition

**DESIGN PASS on the parent-clarified controlling r4 set.**

The repository/implementation hold remains in force. D081 remains Proposed and unaccepted; D082 remains blocked. Retained executions and identity attestations do not establish physical allocation bounds, whole-current-source validation, universal asynchronous safety, or implementation acceptance.