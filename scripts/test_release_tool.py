#!/usr/bin/env python3
from __future__ import annotations

import importlib.util
import io
import json
import subprocess
import sys
import tarfile
import tempfile
import unittest
from pathlib import Path

SPEC = importlib.util.spec_from_file_location("release_tool", Path(__file__).with_name("release_tool.py"))
assert SPEC is not None and SPEC.loader is not None
release_tool = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = release_tool
SPEC.loader.exec_module(release_tool)


class ReleaseToolTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.package = release_tool.Package("demo", Path("."), "2026.9.3.0", 0)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def archive(self, extra=None) -> Path:
        path = self.root / "demo-2026.9.3.0.tar.gz"
        members = [
            ("demo-2026.9.3.0/", None, tarfile.DIRTYPE, 0o755),
            ("demo-2026.9.3.0/demo.cabal", b"name: demo\nversion: 2026.9.3.0\n", tarfile.REGTYPE, 0o644),
            ("demo-2026.9.3.0/LICENSE", b"BSD-3-Clause\n", tarfile.REGTYPE, 0o644),
            ("demo-2026.9.3.0/README.md", b"# demo\n", tarfile.REGTYPE, 0o644),
            ("demo-2026.9.3.0/CHANGELOG.md", b"# changes\n", tarfile.REGTYPE, 0o644),
        ]
        if extra:
            members.extend(extra)
        with tarfile.open(path, "w:gz") as archive:
            for name, payload, kind, mode in members:
                info = tarfile.TarInfo(name)
                info.type = kind
                info.mode = mode
                info.mtime = 0
                info.size = 0 if payload is None else len(payload)
                archive.addfile(info, None if payload is None else io.BytesIO(payload))
        return path

    def test_valid_archive_extracts_without_links(self) -> None:
        path = self.archive()
        info = release_tool.validate_archive(path, self.package)
        extracted = release_tool.extract_archive(info, self.root / "out")
        self.assertEqual((extracted / "README.md").read_text(), "# demo\n")
        self.assertEqual(info.entries, 5)

    def test_traversal_is_rejected(self) -> None:
        path = self.archive([("demo-2026.9.3.0/../escape", b"bad", tarfile.REGTYPE, 0o644)])
        with self.assertRaisesRegex(release_tool.ReleaseError, "unsafe archive member"):
            release_tool.validate_archive(path, self.package)

    def test_link_is_rejected(self) -> None:
        path = self.archive([("demo-2026.9.3.0/link", None, tarfile.SYMTYPE, 0o777)])
        with self.assertRaisesRegex(release_tool.ReleaseError, "not a regular file"):
            release_tool.validate_archive(path, self.package)

    def test_duplicate_member_is_rejected(self) -> None:
        path = self.archive([("demo-2026.9.3.0/README.md", b"again", tarfile.REGTYPE, 0o644)])
        with self.assertRaisesRegex(release_tool.ReleaseError, "duplicate archive member"):
            release_tool.validate_archive(path, self.package)

    def test_world_writable_member_is_rejected(self) -> None:
        path = self.archive([("demo-2026.9.3.0/open", b"bad", tarfile.REGTYPE, 0o666)])
        with self.assertRaisesRegex(release_tool.ReleaseError, "unsafe mode"):
            release_tool.validate_archive(path, self.package)

    def test_byte_budget_has_exact_and_one_below_boundaries(self) -> None:
        path = self.archive([("demo-2026.9.3.0/large", b"12345", tarfile.REGTYPE, 0o644)])
        baseline = release_tool.validate_archive(path, self.package)
        exact = release_tool.validate_archive(
            path, self.package, max_unpacked_bytes=baseline.unpacked_bytes
        )
        self.assertEqual(exact.unpacked_bytes, baseline.unpacked_bytes)
        destination = self.root / "not-created"
        with self.assertRaisesRegex(release_tool.ReleaseError, "unpacked-byte budget"):
            release_tool.validate_archive(
                path, self.package, max_unpacked_bytes=baseline.unpacked_bytes - 1
            )
        self.assertFalse(destination.exists())

    def test_entry_budget_has_exact_and_one_below_boundaries(self) -> None:
        path = self.archive()
        release_tool.validate_archive(path, self.package, max_entries=5)
        with self.assertRaisesRegex(release_tool.ReleaseError, "entry-count budget"):
            release_tool.validate_archive(path, self.package, max_entries=4)

    def test_sbom_is_deterministic_and_binds_archive(self) -> None:
        path = self.archive()
        info = release_tool.validate_archive(path, self.package)
        first = release_tool.generate_sbom(info, "a" * 40, 1)
        second = release_tool.generate_sbom(info, "a" * 40, 1)
        self.assertEqual(first, second)
        self.assertIn(info.sha256, first["documentNamespace"])
        package = first["packages"][0]
        self.assertRegex(
            package["packageVerificationCode"]["packageVerificationCodeValue"],
            r"^[0-9a-f]{40}$",
        )
        self.assertTrue(all("licenseInfoInFiles" in item for item in first["files"]))
        encoded = json.dumps(first, sort_keys=True)
        self.assertIn("SPDX-2.3", encoded)
        self.assertIn("BSD-3-Clause", encoded)

    def test_semantic_evidence_checksums_are_deterministic(self) -> None:
        path = self.archive(
            [
                (
                    "demo-2026.9.3.0/test/golden/report.txt",
                    b"deterministic report\n",
                    tarfile.REGTYPE,
                    0o644,
                )
            ]
        )
        info = release_tool.validate_archive(path, self.package)
        expected = release_tool.sha256_bytes(b"deterministic report\n")
        evidence = release_tool.archive_semantic_evidence(info)
        self.assertEqual(
            evidence,
            [
                {
                    "path": "test/golden/report.txt",
                    "bytes": 21,
                    "sha256": expected,
                }
            ],
        )
        self.assertEqual(evidence, release_tool.archive_semantic_evidence(info))

    def test_revision_validation_rejects_hostile_values(self) -> None:
        hostile = (
            "'$(touch injected)'",
            '"; echo injected',
            "a b",
            "--help",
            "a" * 39,
            "A" * 40,
            "a" * 40 + "\ncommand",
            "a" * 40 + "\t--option",
        )
        for value in hostile:
            with self.subTest(value=value), self.assertRaises(release_tool.ReleaseError):
                release_tool.validate_revision(value)
        self.assertEqual(release_tool.validate_revision("a" * 40), "a" * 40)

    def test_ci_manifest_must_exactly_match_release_topology(self) -> None:
        packages = [
            release_tool.Package("demo", Path("."), "2026.9.3.0", 0),
            release_tool.Package("demo-extra", Path("extra"), "2026.9.3.0", 1),
        ]
        manifest = self.root / "ci.tsv"
        manifest.write_text("demo\t.\t0\ndemo-extra\textra\t1\n")
        release_tool.check_ci_manifest(packages, manifest)
        manifest.write_text("demo\t.\t0\ndemo-extra\twrong\t1\n")
        with self.assertRaisesRegex(release_tool.ReleaseError, "manifests differ"):
            release_tool.check_ci_manifest(packages, manifest)

    def test_release_decisions_must_all_be_accepted(self) -> None:
        accepted = "\n".join(
            f"### D-{number:03d}: Decision {number}\n\n**Status:** Accepted\n"
            for number in range(61, 77)
        )
        release_tool.check_release_decision_statuses(accepted)
        proposed = accepted.replace("**Status:** Accepted", "**Status:** Proposed", 1)
        with self.assertRaisesRegex(release_tool.ReleaseError, "must be Accepted"):
            release_tool.check_release_decision_statuses(proposed)

    def test_public_dependency_graph_rejects_missing_and_extra_edges(self) -> None:
        root = release_tool.Package("Markovian", Path("."), "2026.9.3.0", 0)
        gpu = release_tool.Package("markovian-gpu", Path("gpu"), "2026.9.3.0", 2)
        release_tool.check_public_sibling_dependencies(root, set())
        release_tool.check_public_sibling_dependencies(gpu, {"markovian-tensor"})
        with self.assertRaisesRegex(release_tool.ReleaseError, "reviewed graph requires"):
            release_tool.check_public_sibling_dependencies(root, {"markovian-numerical"})
        with self.assertRaisesRegex(release_tool.ReleaseError, "reviewed graph requires"):
            release_tool.check_public_sibling_dependencies(gpu, set())
        with self.assertRaisesRegex(release_tool.ReleaseError, "reviewed graph requires"):
            release_tool.check_public_sibling_dependencies(
                gpu, {"markovian-tensor", "markovian-tensor-reverse"}
            )

    def test_archive_project_enables_every_manifested_flag(self) -> None:
        components = [
            release_tool.Component("test", "demo", "unit", ()),
            release_tool.Component("test", "markovian-neural", "integration", ("+markovian-integration",)),
            release_tool.Component("benchmark", "demo", "speed", ("+slow-evidence",)),
        ]
        text = release_tool.archive_project_text(components)
        self.assertIn("active-repositories: :none", text)
        self.assertIn("package *\n  ghc-options: -Werror\n  tests: True\n  benchmarks: True", text)
        self.assertIn("package markovian-neural\n  flags: +markovian-integration", text)
        self.assertIn("package demo\n  flags: +slow-evidence", text)
        self.assertEqual(text, release_tool.archive_project_text(list(reversed(components))))

    def test_haddock_log_allows_only_exact_offline_cabal_advisory(self) -> None:
        log = self.root / "haddock.log"
        log.write_text("documentation complete\n", encoding="utf-8")
        self.assertEqual("documentation complete\n", release_tool.check_haddock_log(log))
        log.write_text(
            "\n".join(release_tool.ALLOWED_CABAL_NO_INDEX_ADVISORY) + "\n",
            encoding="utf-8",
        )
        self.assertEqual("", release_tool.check_haddock_log(log))

        log.write_text("Warning: missing link destination\n", encoding="utf-8")
        with self.assertRaisesRegex(release_tool.ReleaseError, "unexpected build or Haddock"):
            release_tool.check_haddock_log(log)

        log.write_text(
            release_tool.ALLOWED_CABAL_NO_INDEX_ADVISORY[0] + "\ntruncated\n",
            encoding="utf-8",
        )
        with self.assertRaisesRegex(release_tool.ReleaseError, "unexpected build or Haddock"):
            release_tool.check_haddock_log(log)

    def test_archive_consumer_project_uses_complete_dependency_closure(self) -> None:
        packages = [
            release_tool.Package("alpha", Path("alpha"), "2026.9.3.0", 0),
            release_tool.Package("beta", Path("beta"), "2026.9.3.0", 1),
            release_tool.Package("gamma", Path("gamma"), "2026.9.3.0", 2),
        ]
        for package in packages:
            (self.root / package.directory).mkdir()
            (self.root / "archives" / package.archive_stem).mkdir(parents=True)
        (self.root / "alpha" / "alpha.cabal").write_text(
            "name: alpha\nlibrary\n  build-depends: base >=4.17 && <5\n"
        )
        (self.root / "beta" / "beta.cabal").write_text(
            "name: beta\nlibrary\n  build-depends:\n    , base >=4.17 && <5\n"
            "    , alpha ^>=2026.9.3.0\n"
        )
        (self.root / "gamma" / "gamma.cabal").write_text(
            "name: gamma\nlibrary\n  build-depends:\n    , base >=4.17 && <5\n"
            "test-suite integration\n  build-depends:\n    , gamma ==2026.9.3.0\n"
            "    , beta ^>=2026.9.3.0\n"
        )
        components = [release_tool.Component("test", "gamma", "integration", ("+full",))]
        text = release_tool.archive_consumer_project_text(
            self.root, self.root / "archives", packages, components, "gamma"
        )
        self.assertIn("active-repositories: :none", text)
        self.assertIn("alpha-2026.9.3.0", text)
        self.assertIn("beta-2026.9.3.0", text)
        self.assertIn("gamma-2026.9.3.0", text)
        self.assertIn("package *\n  ghc-options: -Werror\n  tests: False", text)
        self.assertIn("package gamma\n  tests: True\n  benchmarks: True\n  flags: +full", text)

    def test_component_manifest_covers_declarations_and_flag_guard(self) -> None:
        cabal = self.root / "demo.cabal"
        cabal.write_text(
            "name: demo\n"
            "flag integration\n"
            "  default: False\n"
            "library\n"
            "  build-depends: base >=4.17 && <5\n"
            "test-suite unit\n"
            "  if !flag(integration)\n"
            "    buildable: False\n"
            "benchmark speed\n"
            "  type: exitcode-stdio-1.0\n"
        )
        manifest = self.root / "components.tsv"
        manifest.write_text(
            "test\tdemo\tunit\t+integration\n"
            "benchmark\tdemo\tspeed\t-\n"
        )
        components = release_tool.parse_components(manifest, [self.package])
        release_tool.check_components(self.root, [self.package], components)

        manifest.write_text("test\tdemo\tunit\t+integration\n")
        incomplete = release_tool.parse_components(manifest, [self.package])
        with self.assertRaisesRegex(release_tool.ReleaseError, "unmanifested components"):
            release_tool.check_components(self.root, [self.package], incomplete)

    def test_cabal_plan_requires_every_manifested_component(self) -> None:
        components = [
            release_tool.Component("test", "demo", "unit", ()),
            release_tool.Component("benchmark", "demo", "speed", ()),
        ]
        plan = self.root / "plan.json"
        plan.write_text(
            json.dumps(
                {
                    "install-plan": [
                        {"pkg-name": "demo", "component-name": "test:unit"},
                        {"pkg-name": "demo", "components": {"bench:speed": {}}},
                    ]
                }
            )
        )
        release_tool.check_component_plan(plan, components)
        plan.write_text(
            json.dumps(
                {"install-plan": [{"pkg-name": "demo", "component-name": "test:unit"}]}
            )
        )
        with self.assertRaisesRegex(release_tool.ReleaseError, "omits mandatory"):
            release_tool.check_component_plan(plan, components)

    def test_component_receipts_are_deterministic_and_execution_log_bound(self) -> None:
        manifest = self.root / "components.tsv"
        manifest.write_text("test\tdemo\tunit\t-\nbenchmark\tdemo\tspeed\t-\n")
        components = release_tool.parse_components(manifest, [self.package])
        receipts = self.root / "receipts.tsv"
        logs = self.root / "component-logs"
        logs.mkdir()
        compiler = "ghc-9.8.4"
        receipt_rows = []
        for component in components:
            log_name = f"component-logs/{component.kind}-{component.name}.log"
            log = self.root / log_name
            log.write_text(f"executed {component.target}\n")
            receipt_rows.append(
                f"{component.kind}\t{component.target}\t{compiler}\tpassed\t"
                f"{log_name}\t{release_tool.sha256_file(log)}\n"
            )
        receipts.write_text("".join(receipt_rows))
        first = release_tool.component_results(components, manifest, receipts)
        second = release_tool.component_results(components, manifest, receipts)
        self.assertEqual(first, second)
        release_tool.validate_component_results(first, components, manifest, self.root)
        first["testSuites"][0]["result"] = "skipped"
        with self.assertRaisesRegex(release_tool.ReleaseError, "does not match"):
            release_tool.validate_component_results(first, components, manifest, self.root)

    def test_component_receipts_reject_changed_or_unbound_logs(self) -> None:
        manifest = self.root / "components.tsv"
        manifest.write_text("test\tdemo\tunit\t-\n")
        components = release_tool.parse_components(manifest, [self.package])
        receipts = self.root / "receipts.tsv"
        logs = self.root / "component-logs"
        logs.mkdir()
        log = logs / "test-unit.log"
        log.write_text("real command output\n")
        receipts.write_text(
            "test\tdemo:test:unit\tghc-9.8.4\tpassed\t"
            f"component-logs/test-unit.log\t{release_tool.sha256_file(log)}\n"
        )
        result = release_tool.component_results(components, manifest, receipts)
        log.write_text("changed command output\n")
        with self.assertRaisesRegex(release_tool.ReleaseError, "execution log changed"):
            release_tool.validate_component_results(result, components, manifest, self.root)
        receipts.write_text(
            "test\tdemo:test:unit\tghc-9.8.4\tpassed\t../outside.log\t"
            f"{'0' * 64}\n"
        )
        with self.assertRaisesRegex(release_tool.ReleaseError, "invalid component execution log path"):
            release_tool.component_results(components, manifest, receipts)

    def test_checkout_state_rejects_mismatch_and_dirty_tree(self) -> None:
        revision = "a" * 40
        release_tool.validate_checkout_state(revision, revision, revision, "")
        with self.assertRaisesRegex(release_tool.ReleaseError, "does not resolve"):
            release_tool.validate_checkout_state(revision, "b" * 40, revision, "")
        with self.assertRaisesRegex(release_tool.ReleaseError, "clean worktree"):
            release_tool.validate_checkout_state(revision, revision, revision, " M file")

    def test_source_checkout_uses_the_exact_clean_commit(self) -> None:
        repository = self.root / "repository"
        repository.mkdir()
        subprocess.run(["git", "init", "-q", repository], check=True)
        (repository / "tracked").write_text("value\n")
        subprocess.run(["git", "-C", repository, "add", "tracked"], check=True)
        subprocess.run(
            [
                "git",
                "-C",
                repository,
                "-c",
                "user.name=Release Test",
                "-c",
                "user.email=release-test@example.invalid",
                "-c",
                "commit.gpgsign=false",
                "commit",
                "-qm",
                "fixture",
            ],
            check=True,
        )
        revision = subprocess.run(
            ["git", "-C", repository, "rev-parse", "HEAD"],
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()
        release_tool.check_source_checkout(repository, revision)
        (repository / "untracked").write_text("dirty\n")
        with self.assertRaisesRegex(release_tool.ReleaseError, "clean worktree"):
            release_tool.check_source_checkout(repository, revision)

    def test_haddock_interfaces_require_exactly_one_per_package(self) -> None:
        store = self.root / "store"
        (store / "first").mkdir(parents=True)
        (store / "first" / "demo.haddock").write_bytes(b"interface")
        release_tool.check_haddock_interfaces(store, [self.package])
        (store / "second").mkdir()
        (store / "second" / "Demo.haddock").write_bytes(b"duplicate")
        with self.assertRaisesRegex(release_tool.ReleaseError, "found 2"):
            release_tool.check_haddock_interfaces(store, [self.package])

    def test_manifest_rejects_unsafe_duplicate_and_nonmonotone_rows(self) -> None:
        cases = (
            "demo\t../demo\t2026.9.3.0\t0\n",
            "demo\t.\t2026.9.3.0\t0\nDemo\tother\t2026.9.3.0\t1\n",
            "demo\t.\t2026.9.3.0\t1\nother\tother\t2026.9.3.0\t0\n",
        )
        for index, content in enumerate(cases):
            manifest = self.root / f"bad-{index}.tsv"
            manifest.write_text(content)
            with self.subTest(index=index), self.assertRaises(release_tool.ReleaseError):
                release_tool.parse_manifest(manifest)

    def test_manifest_requires_valid_coordinated_calver(self) -> None:
        invalid = (
            "demo\t.\t1.2.3.4\t0\n",
            "demo\t.\t2026.09.3.0\t0\n",
            "demo\t.\t2026.9.03.0\t0\n",
            "demo\t.\t2026.9.3.00\t0\n",
            "demo\t.\t2026.2.30.0\t0\n",
            "demo\t.\t2026.9.3.0\t0\nother\tother\t2026.9.3.1\t1\n",
        )
        for index, content in enumerate(invalid):
            manifest = self.root / f"bad-calver-{index}.tsv"
            manifest.write_text(content)
            with self.subTest(index=index), self.assertRaises(release_tool.ReleaseError):
                release_tool.parse_manifest(manifest)

        manifest = self.root / "valid-calver.tsv"
        manifest.write_text("demo\t.\t2024.2.29.0\t0\n")
        self.assertEqual(release_tool.parse_manifest(manifest)[0].version, "2024.2.29.0")

    def test_sbom_subject_mismatch_is_rejected(self) -> None:
        archive_dir = self.root / "archives"
        sbom_dir = self.root / "sbom"
        archive_dir.mkdir()
        sbom_dir.mkdir()
        path = self.archive()
        moved = archive_dir / path.name
        path.replace(moved)
        manifest = self.root / "packages.tsv"
        manifest.write_text("demo\t.\t2026.9.3.0\t0\n")
        (sbom_dir / "demo-2026.9.3.0.spdx.json").write_text(
            json.dumps({"documentNamespace": "wrong"})
        )
        packages = release_tool.parse_manifest(manifest)
        with self.assertRaisesRegex(release_tool.ReleaseError, "SBOM semantic mismatch"):
            release_tool.generate_artifact_manifest(
                packages, archive_dir, sbom_dir, "a" * 40, 0
            )

    def test_archive_mutation_after_validation_is_rejected(self) -> None:
        path = self.archive()
        info = release_tool.validate_archive(path, self.package)
        path.write_bytes(path.read_bytes() + b"changed")
        with self.assertRaisesRegex(release_tool.ReleaseError, "changed after validation"):
            release_tool.extract_archive(info, self.root / "mutated")

    def test_existing_package_root_is_not_overwritten(self) -> None:
        path = self.archive()
        info = release_tool.validate_archive(path, self.package)
        destination = self.root / "existing"
        package_root = destination / self.package.archive_stem
        package_root.mkdir(parents=True)
        marker = package_root / "marker"
        marker.write_text("keep")
        with self.assertRaisesRegex(release_tool.ReleaseError, "already exists"):
            release_tool.extract_archive(info, destination)
        self.assertEqual(marker.read_text(), "keep")

    def test_finalization_is_atomic_and_does_not_replace(self) -> None:
        stage = self.root / ".stage"
        output = self.root / "output"
        stage.mkdir()
        (stage / "value").write_text("new")
        release_tool.finalize_directory(stage, output)
        self.assertFalse(stage.exists())
        self.assertEqual((output / "value").read_text(), "new")

        second_stage = self.root / ".second-stage"
        second_stage.mkdir()
        (second_stage / "value").write_text("replacement")
        with self.assertRaisesRegex(release_tool.ReleaseError, "already exists"):
            release_tool.finalize_directory(second_stage, output)
        self.assertEqual((output / "value").read_text(), "new")
        self.assertTrue(second_stage.exists())

        other_parent = self.root / "other"
        other_parent.mkdir()
        foreign_stage = other_parent / ".stage"
        foreign_stage.mkdir()
        with self.assertRaisesRegex(release_tool.ReleaseError, "same parent"):
            release_tool.finalize_directory(foreign_stage, self.root / "another-output")
        self.assertTrue(foreign_stage.exists())


if __name__ == "__main__":
    unittest.main()
