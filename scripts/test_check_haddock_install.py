#!/usr/bin/env python3
"""Completeness and warning regressions for the installed-component CI gate."""

import io
import json
import os
from pathlib import Path
import shutil
import tarfile
import tempfile
import unittest
from unittest.mock import patch

import check_haddock_install as gate
from release_tool import ReleaseError, sha256_file


class InstalledHaddockTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.root = self.base / "source"
        self.run = self.base / "run"
        self.run.mkdir()
        (self.root / "release").mkdir(parents=True)
        (self.root / "cabal.project.ci").write_text("packages: .\n")
        actual = Path(__file__).resolve().parents[1]
        shutil.copy2(actual / "release/packages.tsv", self.root / "release/packages.tsv")
        self.packages = gate.parse_manifest(self.root / "release/packages.tsv")
        self.compiler = "ghc-9.14.1"
        (self.run / "cabal.config").write_text("logs-dir: fresh\n")
        self.binding = {"compiler": self.compiler, "cabal": "3.18.1.0",
                        "config_sha256": sha256_file(self.run / "cabal.config"),
                        "sources": {}, "started_ns": 1,
                        "manifest_sha256": sha256_file(self.root / "release/packages.tsv")}
        self.plan = {"compiler-id": self.compiler, "compiler-abi": "dbd5",
                     "cabal-version": "3.18.1.0",
                     "install-plan": []}
        for package in self.packages:
            directory = self.root / package.directory
            directory.mkdir(parents=True, exist_ok=True)
            cabal = directory / f"{package.name}.cabal"
            components = ["lib"]
            text = "library\n"
            if package.name == "markovian-dense-exact":
                text += "library legacy-finite-ir\n"
                components.append("lib:legacy-finite-ir")
            cabal.write_text(text)
            archive = self.run / "build/sdist" / f"{package.archive_stem}.tar.gz"
            archive.parent.mkdir(parents=True, exist_ok=True)
            with tarfile.open(archive, "w:gz") as tar:
                member = tarfile.TarInfo(f"{package.archive_stem}/{cabal.name}")
                member.size = len(text.encode())
                tar.addfile(member, io.BytesIO(text.encode()))
            for component in components:
                unit = package.archive_stem + ("-l-legacy-finite-ir" if ":" in component else "") + "-abc123"
                item = {"type": "configured", "style": "global", "id": unit,
                        "pkg-name": package.name, "pkg-version": package.version,
                        "component-name": component,
                        "pkg-src": {"type": "local-tar", "path": str(archive)},
                        "pkg-src-sha256": sha256_file(archive)}
                self.plan["install-plan"].append(item)
                log = self.run / "logs" / self.compiler / f"{unit}.log"
                log.parent.mkdir(parents=True, exist_ok=True)
                label = "library" if component == "lib" else "library 'legacy-finite-ir'"
                text = f"Building {label} for {package.archive_stem}...\n"
                if component == "lib":
                    text += f"Running Haddock on library for {package.archive_stem}...\nDocumentation created:\n"
                    interface = self.run / "store" / (self.compiler + "-dbd5") / unit / "share/doc" / f"{package.name}.haddock"
                    interface.parent.mkdir(parents=True)
                    interface.write_text("interface")
                label = "library" if component == "lib" else "internal library legacy-finite-ir"
                text += f"Installing {label} in {self.run}/store/{self.compiler}-dbd5/{unit}/lib\n"
                log.write_text(text)
        self.plan["install-plan"].append({"type": "configured", "pkg-name": "Markovian",
                                         "id": "unbuilt-executable", "component-name": "exe:demo"})
        (self.run / "build/cache").mkdir()
        (self.run / "parent.log").write_text("All installed.\n")
        self.log = next((self.run / "logs").rglob("Markovian-*.log"))
        self.inventory = patch.object(gate, "source_inventory", return_value={})
        self.inventory.start()
        self.addCleanup(self.inventory.stop)

    def verify(self):
        (self.run / "build/cache/plan.json").write_text(json.dumps(self.plan))
        return gate.verify_install(self.root, self.run, self.binding)

    def test_complete_set_ignores_unbuilt_executable(self):
        result = self.verify()
        self.assertEqual(len(result["logs"]), 17)
        self.assertEqual(len(result["interfaces"]), 16)

    def test_clean_parent_dirty_component_fails(self):
        self.log.write_text(self.log.read_text() + "Warning: hidden constructor\n")
        with self.assertRaisesRegex(ReleaseError, "unexpected build or Haddock warnings"):
            self.verify()

    def test_private_component_warning_fails(self):
        log = next((self.run / "logs").rglob("*-l-legacy-finite-ir-*.log"))
        log.write_text(log.read_text() + "warning: private compile warning\n")
        with self.assertRaisesRegex(ReleaseError, "unexpected build or Haddock warnings"):
            self.verify()

    def test_missing_log(self):
        self.log.unlink()
        with self.assertRaisesRegex(ReleaseError, "component logs"):
            self.verify()

    def test_duplicate_log(self):
        shutil.copy2(self.log, self.log.with_name("duplicate.log"))
        with self.assertRaisesRegex(ReleaseError, "component logs"):
            self.verify()

    def test_stale_log(self):
        os.utime(self.log, ns=(0, 0))
        with self.assertRaisesRegex(ReleaseError, "stale component log"):
            self.verify()

    def test_symlink_log(self):
        original = self.run / "original"
        self.log.rename(original)
        self.log.symlink_to(original)
        with self.assertRaisesRegex(ReleaseError, "stale component log"):
            self.verify()

    def test_compiler_mismatch(self):
        self.plan["compiler-id"] = "ghc-9.8.4"
        with self.assertRaisesRegex(ReleaseError, "plan compiler mismatch"):
            self.verify()

    def test_config_mismatch(self):
        (self.run / "cabal.config").write_text("changed")
        with self.assertRaisesRegex(ReleaseError, "capture config changed"):
            self.verify()

    def test_log_compiler_abi_mismatch(self):
        self.log.write_text(self.log.read_text().replace("ghc-9.14.1-dbd5", "ghc-9.14.1-ffff"))
        with self.assertRaisesRegex(ReleaseError, "installation mismatch"):
            self.verify()

    def test_log_compiler_mismatch(self):
        self.log.write_text(self.log.read_text().replace("ghc-9.14.1", "ghc-9.8.4"))
        with self.assertRaisesRegex(ReleaseError, "installation mismatch"):
            self.verify()

    def test_unit_mismatch(self):
        self.log.write_text(self.log.read_text().replace("abc123/lib", "wrong-unit/lib"))
        with self.assertRaisesRegex(ReleaseError, "installation mismatch"):
            self.verify()

    def test_wrong_component_identity(self):
        self.log.write_text(self.log.read_text().replace("Building library for Markovian-", "Building library for wrong-"))
        with self.assertRaisesRegex(ReleaseError, "component identity mismatch"):
            self.verify()

    def test_missing_haddock(self):
        self.log.write_text(self.log.read_text().replace("Documentation created:", "Not documented:"))
        with self.assertRaisesRegex(ReleaseError, "missing Haddock run"):
            self.verify()

    def test_duplicate_planned_library(self):
        self.plan["install-plan"].append(self.plan["install-plan"][0])
        with self.assertRaisesRegex(ReleaseError, "duplicate planned library"):
            self.verify()

    def test_missing_planned_private_library(self):
        self.plan["install-plan"] = [x for x in self.plan["install-plan"]
                                     if x["component-name"] != "lib:legacy-finite-ir"]
        with self.assertRaisesRegex(ReleaseError, "missing planned libraries"):
            self.verify()

    def test_source_mismatch(self):
        self.binding["sources"] = {"changed": {}}
        with self.assertRaisesRegex(ReleaseError, "source snapshot changed"):
            self.verify()

    def test_manifest_mismatch(self):
        self.binding["manifest_sha256"] = "wrong"
        with self.assertRaisesRegex(ReleaseError, "manifest changed"):
            self.verify()

    def test_archive_source_mismatch(self):
        (self.root / "Markovian.cabal").write_text("library\n-- changed\n")
        with self.assertRaisesRegex(ReleaseError, "archive/current source mismatch"):
            self.verify()

    def test_unpacked_archive_layout_uses_package_sources_only(self):
        archive_root = self.base / "unpacked"
        archive_root.mkdir()
        (archive_root / "cabal.project").write_text("packages: */*.cabal\n")
        for package in self.packages:
            shutil.copytree(self.root / package.directory,
                            archive_root / package.archive_stem)
        # Combined archive builds write here before the documentation gate. It
        # is not release source and must not enter the source binding.
        generated = archive_root / "dist-newstyle/cache/plan.json"
        generated.parent.mkdir(parents=True)
        generated.write_text("generated")
        directories = gate.package_roots(self.root, self.packages, archive_root)
        sources = gate.archive_source_inventory(
            directories, archive_root / "cabal.project")
        self.assertIn("Markovian/Markovian.cabal", sources)
        self.assertIn("cabal.project", sources)
        self.assertFalse(any("dist-newstyle" in name for name in sources))

    def test_unpacked_archive_source_mutation_fails_verification(self):
        archive_root = self.base / "unpacked"
        archive_root.mkdir()
        project = archive_root / "cabal.project"
        project.write_text("packages: */*.cabal\n")
        for package in self.packages:
            shutil.copytree(self.root / package.directory,
                            archive_root / package.archive_stem)
        directories = gate.package_roots(self.root, self.packages, archive_root)
        self.binding.update(
            source_kind="archive",
            archive_root=str(archive_root),
            sources=gate.archive_source_inventory(directories, project),
        )
        self.verify()
        (archive_root / "Markovian-2026.9.3.0/Markovian.cabal").write_text(
            "library\n-- changed\n"
        )
        with self.assertRaisesRegex(ReleaseError, "source snapshot changed"):
            self.verify()

    def test_archive_plan_mismatch(self):
        self.plan["install-plan"][0]["pkg-src-sha256"] = "wrong"
        with self.assertRaisesRegex(ReleaseError, "archive/plan hash mismatch"):
            self.verify()

    def test_interface_unit_mismatch(self):
        interface = next((self.run / "store").rglob("Markovian.haddock"))
        interface.rename(self.run / "store/Markovian.haddock")
        with self.assertRaisesRegex(ReleaseError, "interface unit mismatch"):
            self.verify()

    def test_dirty_parent(self):
        (self.run / "parent.log").write_text("Warning: parent failure\n")
        with self.assertRaisesRegex(ReleaseError, "unexpected build or Haddock warnings"):
            self.verify()

    def test_capture_fresh_config_and_directories_even_after_failure(self):
        (self.root / "toolchain.env").write_text(
            "export MARKOVIAN_GHC_VERSION=9.14.1\nexport MARKOVIAN_CABAL_VERSION=3.18.1.0\n")
        calls = []

        def execute(args, **kwargs):
            calls.append(args)
            if "--numeric-version" in args:
                kwargs["stdout"].write("9.14.1\n" if args[0] == "ghc" else "3.18.1.0\n")
            elif "user-config" in args:
                config = Path(next(x.split("=", 1)[1] for x in args if x.startswith("--config-file=")))
                config.write_text("-- generated default\n")
            else:
                return gate.subprocess.CompletedProcess(args, 1)
            return gate.subprocess.CompletedProcess(args, 0)

        with patch.object(gate.subprocess, "run", side_effect=execute):
            for _ in range(2):
                with self.assertRaisesRegex(ReleaseError, "command failed"):
                    gate.capture(self.root, self.base / "captures", "cabal", "ghc")
        runs = list((self.base / "captures").iterdir())
        self.assertEqual(len(runs), 2)
        for run in runs:
            text = (run / "cabal.config").read_text()
            self.assertIn(f"logs-dir: {run / 'logs'}", text)
            self.assertIn("active-repositories: :none", text)
            self.assertIn("remote-build-reporting: detailed", text)
            self.assertNotRegex(text, r"(?m)^repository[ \t]+")
            self.assertFalse((run / "verified.json").exists())
        for args in calls:
            self.assertFalse(any("--build-log" in arg or "--no-warnings" in arg for arg in args))

    def test_release_preparation_uses_complete_haddock_and_solver_gates(self):
        prepare = (Path(__file__).resolve().parent / "prepare-release").read_text()
        self.assertEqual(prepare.count("python3 scripts/check_haddock_install.py"), 2)
        self.assertIn('--archive-root="${stage}/unpacked"', prepare)
        self.assertNotIn("worktree-haddock-install.log", prepare)
        self.assertNotIn("archive-haddock.log", prepare)
        self.assertIn("scripts/check-correlated-solver", prepare)
        self.assertIn("check-correlated-solver)", prepare)


if __name__ == "__main__":
    unittest.main()
