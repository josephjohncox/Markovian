# First-release checklist

This checklist separates preparation from external publication. Preparation has no publication credential.

## 1. Freeze the candidate API

- [ ] Complete D-061 and D-067.
- [ ] Review every file in `release/exposed-modules`.
- [ ] Check complete exposed declaration Haddock.
- [ ] Confirm the coordinated UTC CalVer `YYYY.M.D.N` and full PVP sibling bounds.
- [ ] Confirm each supported compiler with a complete job.
- [ ] Confirm every `source-repository this` field identifies `vYYYY.M.D.N`.
- [ ] Review `MIGRATION.md` and `RELEASE-NOTES.md`.

## 2. Check metadata and licenses

- [ ] Run `bash scripts/check-release-metadata`.
- [ ] Run `bash scripts/check-release-policy`.
- [ ] Run `cabal check` for each manifest package.
- [ ] Review each package README and changelog.
- [ ] Review `LICENSE` and `THIRD_PARTY_NOTICES.md`.
- [ ] Check the generated SPDX 2.3 source SBOMs with the pinned independent `spdx-tools` validator.
- [ ] Confirm package verification codes, file license information, and the `2024-2026` copyright range.

## 3. Prepare artifacts

Use a clean immutable revision:

```sh
bash scripts/prepare-release \
  --revision "$(git rev-parse HEAD)" \
  --output ../markovian-release-artifacts
```

- [ ] Confirm that both source archive runs have equal bytes.
- [ ] Verify that `SHA256SUMS` covers every archive, SBOM, `manifest.json`, and `SOURCE-REVISION`.
- [ ] Review `manifest.json`, every packaged semantic-golden checksum, and all SBOM subjects.
- [ ] Confirm race-safe no-replace finalization by testing an existing destination.
- [ ] Build the combined archive-only package graph and every package's isolated archive dependency closure.
- [ ] Check that the Cabal plan contains every entry in `release/components.tsv`.
- [ ] Check that `component-results.json` contains every required suite and benchmark.
- [ ] Check each component result against its bundled normalized invocation log and SHA-256.
- [ ] Confirm that the isolated Haddock logs contain no warnings.
- [ ] Confirm that each package has one isolated Haddock interface.
- [ ] Use the separate `--no-warnings` pass only for declaration coverage.
- [ ] Build and run the fresh exact consumer.
- [ ] Keep the preparation workflow free of Hackage credentials.
- [ ] Run the digest-pinned CUDA 13 compile-only workflow. Confirm strict
  enabled C/Haskell compilation, driver-header/PTX/generated-header
  reproduction, no startup `libcuda` dependency, and missing-library,
  missing-symbol, and unsupported-device receipts without a GPU.
- [ ] Run the separate UUID-bound protected CUDA workflow. Confirm that all
  four Compute Sanitizer tools use the `cabal list-bin` executable with the
  Cabal `markovian_gpu_datadir` override.

## 4. Verify hosted provenance

- [ ] Run `release-prepare.yml` for the exact revision.
- [ ] Confirm that the build job used a GitHub-hosted runner.
- [ ] Confirm the workflow action SHAs against current official releases.
- [ ] Download the complete artifact from the attested workflow.
- [ ] Before extraction, verify provenance for archives, SBOMs, component evidence, the manifest, `SOURCE-REVISION`, and `SHA256SUMS`.
- [ ] Verify every digest from the bundle root with `sha256sum -c SHA256SUMS`.
- [ ] Compare `SOURCE-REVISION` with the separately authorized commit object ID.
- [ ] Test wrong repository, workflow, revision, digest, hostile revision input, and concurrent destination rejection.

GitHub attestations bind digests to workflow identity. They do not prove source safety or semantic correctness.

## 5. Ask for publication approval

Stop after preparation. Get explicit user approval before any candidate upload, tag, release, or publication.

The Git tag and GitHub release name must be `vYYYY.M.D.N` and must match all package versions. Hackage publication across packages is not atomic. Record each successful package before the next package.

## 6. Roll back a failed operation

For preparation failure, delete the temporary output. The script does this before it returns an error.

For a candidate failure, delete or replace only the candidate. Do not describe it as a release.

For partial publication, stop immediately. Do not reuse or replace a published version.

Record the published subset and the exact failure. Fix the problem with a new PVP version when code must change.

Hackage does not permit replacement of published package versions. A metadata revision cannot change package code.
