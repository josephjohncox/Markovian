# Release preparation

Release `2026.9.3.0` passed D-061 through D-076 and the complete preparation, attestation, and protected CUDA evidence gates. These decisions are accepted only for their stated bounded scopes.

Release preparation creates checked local artifacts. It does not upload packages, create tags, or create releases.

## Calendar versioning

Coordinated releases use the UTC calendar version `YYYY.M.D.N`. The year has four digits. The month and day use canonical decimal notation without leading zeroes. The final component is a zero-based same-day release sequence. For example, the first release on 2026-09-03 is `2026.9.3.0`; another release on that UTC date is `2026.9.3.1`.

All 16 packages use the same calendar version. The Git tag is `vYYYY.M.D.N`, and every `source-repository this` section identifies that exact tag and package subdirectory. The release metadata checker rejects non-calendar versions, impossible dates, leading zeroes, mixed package versions, or mismatched tags.

Public sibling bounds still use Cabal's `^>=` operator. The `YYYY.M` pair therefore defines the PVP compatibility line for a coordinated release.

## Preparation contract

`release/packages.tsv` owns the bounded 16-package list, versions, and dependency tiers. `release/components.tsv` owns the 18 required test suites, 11 benchmarks, and the neural integration flag. These files describe the released integration graph. They do not by themselves authorize publication.

The preparation script requires a full lowercase 40-character commit object ID and a clean worktree at that exact commit. It refuses Hackage credentials and an existing output directory.

The script runs package checks, tests, lower-bound resolution, boundaries, benchmarks, Haddock, and the book check. Its warning-enabled Haddock installation uses a fresh store, rejects every build and Haddock warning, and requires one interface per package. Cabal 3.16 emits a two-line missing-package-list advisory in the scrubbed home even when `active-repositories: :none`; the warning checker permits only that exact non-build advisory. A separate serial `cabal haddock all` pass uses `--haddock-options=--no-warnings` only to produce declaration-coverage rows. The coverage checker excludes declared private modules and requires exact public coverage; the suppressed pass is never warning evidence. The script then creates each source archive twice.

The script compares archive bytes before extraction. It rejects traversal, links, duplicate entries, unsafe modes, credential-like names, and size-budget failures. Checked extraction writes regular files and directories only and verifies that the archive bytes did not change after validation.

The script builds and tests one archive-only graph. It enables tests, benchmarks, and `+markovian-integration` for `markovian-neural`. It checks the Cabal plan against `release/components.tsv`. Each component receipt includes the compiler, result, normalized invocation log, byte count, and SHA-256. The script then tests each package from an isolated archive dependency closure. It also runs that package's benchmarks and boundaries and builds a fresh exact consumer. No installed in-tree package can satisfy these consumers.

After all checks pass, the script writes:

- source archives;
- one SPDX 2.3 source SBOM for each archive, including file license fields and the package verification code;
- a deterministic artifact manifest with byte counts and SHA-256 digests for every packaged `test/golden` semantic report;
- the exact source revision; and
- `SHA256SUMS` covering every archive, SBOM, the manifest, and `SOURCE-REVISION`.

The pinned independent `spdx-tools` validator checks every SBOM. The script writes the bundle in a temporary sibling directory. Linux `renameat2(RENAME_NOREPLACE)` finalizes it atomically and refuses a destination created by a concurrent process. Cleanup remains active until that rename succeeds.

## Run local preparation

Use a clean immutable revision:

```sh
bash scripts/prepare-release \
  --revision "$(git rev-parse HEAD)" \
  --output ../markovian-release-artifacts
```

The repository scripts can have mode `0644` in a Cabal source archive. Run them with `bash scripts/...` after extraction.

## Verify a downloaded bundle before extraction

First verify provenance for every bundle subject. Then verify all recorded digests. Do not inspect or extract an archive before both checks pass.

```sh
gh attestation verify markovian-release-artifacts/ \
  --repo josephjohncox/Markovian \
  --signer-workflow \
    josephjohncox/Markovian/.github/workflows/release-prepare.yml \
  --deny-self-hosted-runners

(
  cd markovian-release-artifacts
  sha256sum -c SHA256SUMS
)
```

Also verify that `SOURCE-REVISION` is the authorized commit object ID. Test wrong repository, workflow, revision, and digest cases. An attestation binds artifact digests to workflow identity. It does not prove source safety or semantic correctness.

After provenance and digest checks pass, use the bounded extractor:

```sh
bash scripts/check-release-archive \
  markovian-release-artifacts/archives/Markovian-2026.9.3.0.tar.gz \
  --name Markovian \
  --version 2026.9.3.0 \
  --extract unpacked
```

Build the package from the checked directory:

```sh
cd unpacked/Markovian-2026.9.3.0
cabal build all
cabal test all --test-show-details=direct
```

The file `examples/release/exact` is a fresh consumer package. The preparation script builds it against the unpacked root archive.

## Workflow permissions

The manual `release-prepare.yml` workflow has three jobs. An unprivileged job validates the hostile-input boundary before any privileged job can start. Every job independently validates the revision syntax. The preparation job resolves the quoted revision as a commit and compares it with `HEAD`.

Only the attestation job gets OIDC and attestation permissions. It starts only after validation and preparation succeed. It does not check out or execute repository code. No job gets a Hackage credential.

Before each use, review each action commit against its current official release. The workflow attests the archives, SBOMs, manifest, source revision, and `SHA256SUMS`.

Hackage Security signs repository indexes. It does not provide a Markovian author signature for these source archives.

## Publication boundary

Stop after preparation and attestation. Ask the user for explicit approval before an external candidate or publication operation.

Hackage publication is not atomic across packages. A failed later package can leave an earlier package published. Published versions cannot be replaced. Use a new PVP version when code must change. A Hackage metadata revision cannot change package source. Record a partial publication as partial.

## Official references

- Cabal package metadata: <https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html>
- Cabal source distributions: <https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-sdist>
- Hackage upload policy: <https://hackage.haskell.org/upload>
- Package Versioning Policy: <https://pvp.haskell.org/>
- SPDX 2.3 package information: <https://spdx.github.io/spdx-spec/v2.3/package-information/>
- SPDX 2.3 file information: <https://spdx.github.io/spdx-spec/v2.3/file-information/>
- SPDX Python tools: <https://github.com/spdx/tools-python>
- GitHub artifact attestations: <https://docs.github.com/en/actions/concepts/security/artifact-attestations>
- GitHub Actions security: <https://docs.github.com/en/actions/security-for-github-actions/security-guides/security-hardening-for-github-actions>
