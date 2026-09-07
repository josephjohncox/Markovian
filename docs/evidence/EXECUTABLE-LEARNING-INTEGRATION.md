# Executable learning: EL-07 integration evidence

Status: **local implementation integration; final independent review pending**.
Branch: `frontier/executable-learning`; implementation input: `42d7295`.
This is development evidence, not acceptance, publication or release preparation.
The [checklist](../plans/EXECUTABLE-LEARNING-TODO.md) and
[baseline review](LEARNING-REVIEW-2026-09-05.md) remain the requirement record.

## Review and requirement audit

EL-01 passed independent contract review `fc1ffb2b-42cd-4d41-aa60-71d1fbbaef12`.
EL-02 passed re-review after the real displayed-layout defect was repaired.
EL-03, EL-04 and EL-05 passed the supplied independent implementation/teaching
reviews recorded in the checklist; EL-03's constructor probe was subsequently
fully applied and mutation-tested. The supplied EL-06 review also **passed**, with
no findings and high confidence: it checked cumulative resource accounting,
rational boundaries, saturation, atomic failure, error precedence, independent
Integer/literal oracles, lesson and archive wiring. Those reviews inspected
source/logs but did not independently rerun commands. None is a final EL-07 review.

Requirement-to-artifact audit:

| Requirement | Implemented evidence |
| --- | --- |
| F01–F08/current versus immutable release truth | Current guidance, nine capability records, separate released-module digest record; 33 capability negatives |
| Runnable teaching, fragment context/layout, freshness | `docs/learning/fences.json`, compiled context modules, seven executed outputs and receipt; 34 checker tests |
| Routes, notation, hypotheses, checked exercises | Three routes, orientation, laboratories A–G, paired difference, reward JVP and aggregation lessons |
| Paired dependence, not universal variance reduction | EL-03 contract, continuous implementation, independent affine moment oracle, private saturation probes and two opacity probes |
| Fixed-topology reward sensitivity, not probability/general AD | EL-04 contract, root implementation, determinant/dual-unrolling oracles, cumulative prefix tests, four nominal roles |
| Supplied fixed-policy partition, not policy/POMDP search | EL-05 contract, opaque owned quotient/witness, joint-law/observation/payoff tests, independent finite values/traces and nine nominal roles |
| Semantic admission distinct from executor assumptions | EL-06 contract and real checked-bind contrast; hypothetical hit is not a cache/performance implementation |
| Final integration/review | Local gates below; final independent review and parent completion audit remain open |

No semantic rewrite was needed during integration. Narrow integration repairs:
CI explicitly builds tests and benchmarks together; CI's Python syntax inventory
now includes both capability scripts; archive-only teaching also runs the archived
capability checker and its negative tests; one existing nested link-check condition
was simplified to satisfy Ruff. The pre-existing whitespace-only change in
`scripts/test_learning.py` is preserved outside this intended commit.

## Executed source gates

Host: GHC 9.8.4, Cabal 3.16.1.0, aarch64 Linux. Commands run from the repository
root unless noted. `/tmp/el07-*` logs are local inspection artifacts, not immutable
published receipts. The durable results below are shipped in the root archive.

| Actual command/gate | Result and local log |
| --- | --- |
| `cabal build all --project-file=cabal.project.ci --enable-tests --enable-benchmarks` | Passed project `-Werror` build, including all 11 benchmarks; `/tmp/el07-build.log` |
| `cabal test all --project-file=cabal.project.ci --test-show-details=direct` | **18 suites, 293 line-start PASS labels**; `/tmp/el07-tests.log` (labels are not an exhaustive assertion count) |
| `cabal build all --project-file=cabal.project.ci --ghc-options=-Wunused-packages` | Prescribed CI unused-public-dependency gate passed; `/tmp/el07-unused-ci.log` |
| `cabal bench all --project-file=cabal.project.ci` | All **11** benchmark components finished; `/tmp/el07-benchmarks.log`. Host-only semantic runs/timings, not new comparative performance evidence |
| All 19 CI boundary/profile scripts, plus root-topology | Passed named/category checks and private probes; `/tmp/el07-boundaries.log`, `/tmp/el07-metadata-negatives.log`; CUDA profile includes 12 Python tests, not CUDA execution |
| Additional direct compilation of all tracked compile-fail Haskell fixtures | **54** rejected using `-package-env - -hide-all-packages`, copied current Cabal package databases and exact `name-2026.9.3.0-inplace` units; `/tmp/el07-exact-boundaries-current.log`, commands/diagnostics in `/tmp/el07-exact-fixtures/`. Root fixtures receive no sibling capability |
| Package manifest, release metadata, policy, root topology, all package-local `cabal check` | Passed: 16 packages, 18 suites, 11 benchmarks, two policy tests; `/tmp/el07-metadata-negatives.log` |
| `python3 scripts/test_release_tool.py`, `test_capabilities.py`, `test_learning.py`; `check-capabilities --verify-release` | **32/33/34** tests passed; immutable membership verified; `/tmp/el07-metadata-negatives.log`. Expected malformed compiler/runtime fixtures are asserted negatives |
| Full CI HLint roots including `docs/learning`; Fourmolu on `.hs`; cabal-fmt on Cabal files | Passed, HLint no hints; `/tmp/el07-format.log`. Three existing literate `.lhs` files use GHC/HLint, not unsupported Fourmolu parsing |
| Ruff and `py_compile` on 11 Python sources; ShellCheck and individual `bash -n` on 30 shell sources | Passed; `/tmp/el07-python-shell-final.log` |
| `actionlint`; `git diff --check` | Passed; `/tmp/el07-actionlint.log` |
| `python3 scripts/check-learning --run` | All registered contexts compiled and all seven outputs matched; `/tmp/el07-learning-run.log` |
| `bash scripts/check-book` | 77 classified Haskell fences, 52 Markdown files, local links/includes, capability/output freshness, pinned MathJax digest and **422** source-to-generated math blocks; `/tmp/el07-book.log` |

## Authoritative documentation and actual browser evidence

Warning-enabled isolated installation (no warning suppression):

```sh
mapfile -t packages < <(awk -F '\t' '$1 !~ /^#/ && NF == 3 {print $1}' ci/packages.tsv)
cabal --store-dir=/tmp/el07-doc-store install --lib "${packages[@]}" \
  --project-file=cabal.project.ci --builddir=/tmp/el07-doc-build \
  --package-env=/tmp/el07-doc-environment --enable-documentation \
  --overwrite-policy=always > /tmp/el07-haddock-install.log 2>&1
python3 scripts/release_tool.py check-haddock-log /tmp/el07-haddock-install.log
python3 scripts/release_tool.py check-haddock-interfaces /tmp/el07-doc-store
```

Passed warning-log validation and installed interfaces for **all 16 packages**.
Inspected the installed continuous measure, feedback value and aggregation pages:
all **26** new public operations/accessors have visible signatures using public
types, no private meter/atom/row/unsafe implementation types
(`/tmp/el07-public-signatures.log`). Separately, the repository's coverage-only
`cabal haddock all -j1 --project-file=cabal.project.ci --haddock-hyperlink-source
--haddock-for-hackage --haddock-options=--no-warnings` pass and
`python3 scripts/check-haddock-coverage /tmp/el07-haddock-coverage.log` confirmed
**133** exact public-module rows at 100%. That suppressed pass supplies **no**
warning evidence; the isolated installation above is authoritative.

Actual browser startup/typesetting also passed. Used the already installed
`~/.cache/ms-playwright/chromium-1228/chrome-linux/chrome` (**Chromium
149.0.7827.0**), without installing a browser, Node package or runtime dependency.
For each of the 51 generated chapter pages (SUMMARY is navigation, not a page):

```sh
"$browser" --headless --no-sandbox --disable-gpu --disable-dev-shm-usage \
  --no-first-run --timeout=30000 --virtual-time-budget=10000 --dump-dom \
  "file://$PWD/docs/book/build/reward-jvp.html"
```

A Python HTMLParser checked every page's output: its `mjx-container[display=true]`
count equals its original display-block count, every container uses SVG, an SVG
exists for each container, no `data-mjx-error`/`merror` node exists, and no opening
paragraph display delimiter remains. **51 pages, 422 display blocks, 422 SVG math
containers**, zero MathJax error nodes. Logs: `/tmp/el07-browser.log`; complete DOM
and Chromium stderr per page: `/tmp/el07-browser-dom/`; host runner:
`/tmp/el07-browser.py`. MathJax itself is the repository's digest-checked 3.2.2
bundle. This is actual startup/DOM evidence, not a visual, accessibility,
interactive-navigation, cross-browser or hosted-Pages audit. Chromium stderr is
retained rather than claimed empty. No new pinned browser/CI-install policy was
introduced.

## Source archives and final freshness

`cabal sdist all --project-file=cabal.project.ci
--output-directory=dist-newstyle/el07-sdist` produced **all 16** development
archives (`/tmp/el07-sdist.log`).
`python3 scripts/check-learning-archive dist-newstyle/el07-sdist` then passed
bounded validation of every archive, reconstruction of the original package
layout without Git/checkout links, archived capability checking plus all **33**
negatives, teaching-context compilation, all **seven** executed output comparisons
and all **34** learning negatives (`/tmp/el07-archive-learning.log`). Unlike earlier
versions of this gate, capability tools/records/contracts now come from archives
on this routine path too.

Separately extracted all 16 validated archives to
`/tmp/el07-archive-full/source`. Using **only shipped** `cabal.project.ci`, sources,
contexts, scripts, manifests and receipts, ran:

```sh
cabal build all --project-file=cabal.project.ci --enable-tests --enable-benchmarks
cabal test all --project-file=cabal.project.ci --test-show-details=direct
bash scripts/check-feedback-boundary
bash scripts/check-aggregation-boundary
bash packages/markovian-continuous/scripts/check-continuous-boundary
bash scripts/check-root-topology
python3 scripts/check-capabilities
python3 scripts/check-learning --run
```

All passed: **18 archive test suites**, all benchmark components compiled,
EL-03/04/05 archived exact-unit opacity/role/private probes, root topology, all
seven archive-only outputs. `/tmp/el07-archive-full.log` records the commands'
output. The archive reconstruction contains no Git directory or symlinks; only
compiler/system tools and ordinary external Cabal dependencies are host inputs.
Archive benchmarks were compiled, not separately executed; the 11 benchmark
executions above were in the checkout.

Final receipt regeneration (`check-learning --write`) and subsequent `--run`
passed (`/tmp/el07-learning-write-final.log`, `/tmp/el07-learning-run-final.log`);
stdout files did not change. Only the receipt source digest changed for the
extra-doc membership edit. The final book gate also passed
(`/tmp/el07-book-final.log`). A byte-for-byte archive audit covers **106** changed
or new distributed files since the review baseline: all implementation/test
sources, negative/private fixtures, context modules, seven outputs, manifest,
receipt, scripts, contracts, capability records, durable review/checklist and this
integration record. No changed path was missing. Archives were refreshed after
final evidence-only edits; `/tmp/el07-archive-membership-final.log` records that
final-byte audit. Root artifact:
`dist-newstyle/el07-sdist/Markovian-2026.9.3.0.tar.gz`.

`/tmp/el07-invariants-final.log` confirms all tracked Cabal versions and dependency
declarations unchanged from the baseline, D-077–D-085 still **Proposed**, immutable
tag object/target unchanged, and historical published-release/membership records
untouched. `/tmp/el07-versions.log` records installed tools matching the repository's
pinned GHC/Cabal/Fourmolu/HLint/ShellCheck/cabal-fmt/mdBook versions. The intended
commit excludes the pre-existing formatting-only Python diff; archive membership
matches the preserved working bytes, including that disclosed diff.

## Failures, exclusions and remaining audit

- An optional **expanded** `-Wunused-packages --enable-tests --enable-benchmarks`
  experiment failed on the pre-existing direct root dependencies of
  `dogru-inventory-bench` and `fixed-batch-rnq-bench`
  (`/tmp/el07-unused-packages.log`). The prescribed CI unused-package gate and
  the required full warning-enabled build each passed separately. No dependency
  was removed, bogus import added or warning suppressed to conceal this result.
  Extending that unused-dependency policy to every benchmark remains a parent
  topology/policy audit item, not a passing gate here.
- Initial broad Ruff found one existing nested condition in `check-book-links`;
  the narrow equivalent condition above passes the complete rerun.
- Initial direct-boundary inspection tried to read Cabal's temporary environment
  after Cabal removed it; copied it within `cabal exec` instead. A second
  overbroad fixture inventory included old ignored extracted archives; replaced
  it with `git ls-files '*compile-fail/*.hs'`. Only the final 54-fixture run is
  claimed. Neither failure changed a library or a negative fixture. An initial
  invariant regex omitted Markdown's bold status markers; the corrected exact
  `**Status:** Proposed` assertion passed for all nine decisions.
- No GHC 9.4.8, preferred-oldest, CUDA-enabled compilation/hardware/sanitizer,
  hosted CI, publication, tag, version or proposal-acceptance result is claimed.
  Index/LSP extension tools were unavailable to this worker; actual compiler
  evidence is authoritative. No cache was rebuilt solely for editor diagnostics.
- Final EL-07 independent correctness/teaching review and parent checkbox/commit
  audit are still required. Earlier reviews do not self-award that final gate.
