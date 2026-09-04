# Release preparation data

`packages.tsv` lists the current 16-package integration graph. It is not publication approval.

`exposed-modules` contains reviewed public-module snapshots for each manifest package. `API-REVIEW.md` records the completed API review.

`components.tsv` lists all 18 mandatory test suites, 11 benchmarks, and component flags. The release tool checks it against all Cabal files. `published-releases.json` is mandatory for ordinary metadata checks and source-checkout checks. It prevents a later revision from rebuilding an already published version. The current bounded proposal work retains package versions and bounds under the task's no-version-change invariant, so it is not release source and cannot pass that source boundary from another revision. A future candidate still requires an intended new version, updated notes, API review, and all release gates. Direct `cabal sdist` output is not an admitted release candidate.

The combined archive project enables every flag in `components.tsv`. Preparation runs each component by its full Cabal target and checks the Cabal plan. It also generates one isolated archive dependency-closure project per package and reruns that package's suites, benchmarks, and boundaries.

A successful run writes `component-results.json`. This deterministic report binds each result to the component manifest and to the SHA-256 and byte count of its invocation log in `component-logs/`. Before hashing, preparation normalizes line endings and replaces the temporary stage, source, and home paths with fixed markers. It does not remove command output. The bundle checksum covers those logs and the result report. `manifest.json` also records SHA-256 and byte counts for every packaged `test/golden` semantic report.

The Haddock gate has two separate passes. An isolated installation supplies warning-free evidence and one interface per package.

A second pass uses `--no-warnings` only to measure declaration coverage. It does not supply warning-free evidence.

The release tool also checks the exact public sibling dependency graph, unique public module ownership, and one exposed-module golden per package. The release record states that the bounded `markovian-tensor-reverse` package and SafeTensors profile passed their immutable-revision gates. Hardware pass statements for D-074 remain historical because the repository lacks a complete D-077 same-session receipt set.

Release preparation accepts only a clean checkout at the exact requested commit. It publishes the output with one no-replace rename.

Publication needs separate human authorization.
