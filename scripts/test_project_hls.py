#!/usr/bin/env python3
"""Offline recipe/installer/launcher regressions; no downloads or global tools."""
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import unittest
from unittest.mock import patch

HERE = Path(__file__).resolve().parent


def load(name):
    spec = importlib.util.spec_from_file_location(name, HERE / (name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


installer = load("install-hls-official")
guard = load("guard-hls")
launcher = load("project-hls")
ORIGINAL = (HERE / "fixtures/hls-2.14.0.0-project.txt").read_bytes()


class OfficialHlsTests(unittest.TestCase):
    def test_exact_official_recipe_and_one_byte_strengthening(self):
        pins = launcher.recipe()
        self.assertEqual(hashlib.sha256(ORIGINAL).hexdigest(), pins["original_project_sha256"])
        generated = installer.strengthen(ORIGINAL)
        self.assertEqual(len(ORIGINAL), len(generated))
        self.assertEqual([(i, a, b) for i, (a, b) in enumerate(zip(ORIGINAL, generated)) if a != b],
                         [(734, 45, 43)])
        self.assertEqual(hashlib.sha256(generated).hexdigest(), pins["generated_project_sha256"])
        self.assertEqual(installer.URL, pins["source_url"])
        self.assertEqual(installer.SHA, pins["source_sha256"])
        self.assertEqual(installer.INDEX, pins["index_state"])

    def test_no_additional_relaxations_or_guard_disable(self):
        for addition in (b"\nallow-newer: *\n", b"\nallow-older: *\n",
                         b"\nconstraints: ghc-check -ghc-check-use-package-abis\n"):
            with self.assertRaisesRegex(ValueError, "identity"):
                installer.strengthen(ORIGINAL + addition)

    def test_corrupt_archive_rejected_before_extraction(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            archive = root / "bad.tar.gz"
            archive.write_bytes(b"not official")
            with self.assertRaisesRegex(ValueError, "SHA256"):
                installer.extract(archive, root / "out")
            self.assertFalse((root / "out").exists())

    def test_unsafe_and_duplicate_archive_members(self):
        cases = [[("/escape", tarfile.REGTYPE, "")],
                 [("root/../../escape", tarfile.REGTYPE, "")],
                 [(installer.TOP + "/bad", tarfile.SYMTYPE, "/escape")],
                 [(installer.TOP + "/device", tarfile.CHRTYPE, "")],
                 [(installer.TOP + "/hard", tarfile.LNKTYPE, installer.TOP + "/foo")],
                 [(installer.TOP + "/duplicate", tarfile.REGTYPE, "")] * 2]
        for members in cases:
            with self.subTest(members=members), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                archive = root / "synthetic.tar.gz"
                with tarfile.open(archive, "w:gz") as target:
                    for name, kind, link in members:
                        member = tarfile.TarInfo(name)
                        member.type = kind
                        member.linkname = link
                        target.addfile(member, io.BytesIO(b""))
                with patch.object(installer, "SHA", installer.sha(archive)):
                    with self.assertRaises(ValueError):
                        installer.extract(archive, root / "out")
                self.assertFalse((root / "out").exists())

    def test_environment_allowlist(self):
        poison = {key: "/poison" for key in ("CABAL_CONFIG", "CABAL_DIR", "GHC_PACKAGE_PATH",
                                            "GHC_OPTIONS", "HOME", "PATH")}
        with patch.dict(os.environ, poison):
            env = installer.environment(Path("/isolated"), Path("/compiler/ghc"))
        self.assertEqual(env["HOME"], "/isolated/home")
        self.assertEqual(env["CABAL_CONFIG"], "/isolated/cabal/config")
        self.assertEqual(env["GHC_ENVIRONMENT"], "-")
        self.assertNotIn("GHC_PACKAGE_PATH", env)
        self.assertNotIn("GHC_OPTIONS", env)

    def test_existing_root_refused_without_writes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "sentinel").write_text("unchanged")
            result = subprocess.run([sys.executable, str(HERE / "install-hls-official.py"),
                                     "--root", directory, "--ghc", "/absent", "--cabal", "/absent"],
                                    capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual([p.name for p in root.iterdir()], ["sentinel"])

    def test_guard_requires_completed_build(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "build-receipt.json").write_text(json.dumps({"fresh_build_completed": False}))
            with self.assertRaisesRegex(ValueError, "no completed fresh build"):
                guard.check(root, Path(sys.executable))

    def test_guard_requires_original_installer_identity(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "inputs").mkdir()
            (root / "inputs/install-hls-official.py").write_text("changed")
            (root / "build-receipt.json").write_text(json.dumps({
                "fresh_build_completed": True, "installer_sha256": "0" * 64}))
            with self.assertRaisesRegex(ValueError, "installer hash mismatch"):
                guard.check(root, Path(sys.executable))

    def test_script_tampering_fails_closed(self):
        with patch.object(launcher, "sha", return_value="0" * 64):
            with self.assertRaisesRegex(ValueError, "script identity"):
                launcher.recipe()

    def test_guard_binds_seal_producer_before_tool_execution(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)

            def write(name, value):
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(value if isinstance(value, bytes) else value.encode())
                return path

            def record(name, value):
                return write(name, json.dumps(value))

            ghc = write("compiler/ghc-9.14.1", "offline compiler fixture")
            pkg = write("compiler/ghc-pkg-9.14.1", "offline package tool fixture")
            tools = {str(path): guard.sha(path) for path in (ghc, pkg)}
            record("inputs/toolchain.json", tools)
            installer_path = write("inputs/install-hls-official.py", b"installer fixture")
            selector_path = write("inputs/hls_selectors.py", (HERE / "hls_selectors.py").read_bytes())
            selectors = record("inputs/tool-selectors.json", {})
            archive = write("inputs/hls-src.tar.gz", b"archive fixture")
            plan = record("inputs/plan.json", {"install-plan": []})
            source = "source/haskell-language-server-2.14.0.0/"
            write(source + "cabal.project", ORIGINAL)
            write(source + "cabal.project.abi", installer.strengthen(ORIGINAL))
            boot = b"offline boot package database\nabi: fixture\n"
            write("inputs/boot-package-db.txt", boot)
            write("evidence/04-boot-package-db.log", boot)
            record("evidence/04-boot-package-db.result.json",
                   {"log_sha256": hashlib.sha256(boot).hexdigest()})
            write("cabal/config", "repository hackage.haskell.org\n"
                  "  url: https://hackage.haskell.org/\n  secure: True\n"
                  f"remote-repo-cache: {root}/cabal/packages\n"
                  f"store-dir: {root}/store\nlogs-dir: {root}/logs\n")
            server = write("install/haskell-language-server", "offline server fixture")
            library = write("runtime-library", "offline shared library fixture")
            receipt = record("build-receipt.json", {
                "fresh_build_completed": True,
                "installer_sha256": guard.sha(installer_path),
                "selector_helper_sha256": guard.sha(selector_path),
                "selectors_sha256": guard.sha(selectors),
                "plan_sha256": guard.sha(plan),
                "outputs": {server.name: guard.sha(server)},
            })
            seal = {"status": "SEALED", "original_source_files_verified": 1722,
                    "build_receipt_sha256": guard.sha(receipt),
                    "selectors_sha256": guard.sha(selectors), "selector_count": 1,
                    "inputs": {str(path.relative_to(root)): guard.sha(path)
                               for path in (root / "inputs").iterdir()},
                    "runtime_files": {str(library): guard.sha(library)}}
            # The selector suite checks real selector manifests. This fixture
            # isolates producer binding while exercising the complete guard.
            for producer in (guard.sha(HERE / "seal-hls.py"), "0" * 64, None):
                with self.subTest(producer=producer):
                    candidate = dict(seal)
                    if producer is not None:
                        candidate["seal_script_sha256"] = producer
                    record("runtime-seal.json", candidate)
                    with patch.object(guard, "SOURCE_SHA", guard.sha(archive)), \
                            patch.object(guard.hls_selectors, "validate", return_value=1), \
                            patch.object(guard.subprocess, "check_output", return_value=boot) as tool:
                        if producer == guard.sha(HERE / "seal-hls.py"):
                            actual_server, _, proof = guard.check(root, ghc)
                            self.assertEqual(actual_server, server)
                            self.assertEqual(proof["abi_guard"], "PASS")
                            tool.assert_called_once()
                        else:
                            with self.assertRaisesRegex(ValueError, "seal producer identity mismatch"):
                                guard.check(root, ghc)
                            tool.assert_not_called()

    def test_toolchain_pin_drift_fails_closed(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "toolchain.env").write_text("export MARKOVIAN_GHC_VERSION=9.8.4\n")
            with patch.object(launcher, "PROJECT", root):
                with self.assertRaisesRegex(ValueError, "pin mismatch"):
                    launcher.recipe()

    def test_persistent_project_local_root(self):
        root = launcher.build_root(launcher.recipe())
        pins = launcher.recipe()
        digest = hashlib.sha256(json.dumps(pins, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
        self.assertEqual(root, HERE.parent / (".direnv/hls-official-2.14.0.0-ghc-9.14.1-" + digest))
        self.assertEqual(root, launcher.build_root(dict(reversed(list(pins.items())))))
        self.assertNotEqual(root, launcher.build_root(dict(pins, index_state="different")))

    def test_missing_cabal_fails_before_construction(self):
        pins = launcher.recipe()
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "absent"
            ghc = Path(sys.executable)
            archive = Path(directory) / "archive.tar.gz"
            with patch.object(launcher.subprocess, "run") as run, \
                    patch.object(launcher, "check_inputs", return_value=ghc):
                with self.assertRaisesRegex(ValueError, "requires absolute"):
                    launcher.install(root, pins, ghc, None, archive, 8)
                run.assert_not_called()

    def test_fresh_construction_commands(self):
        pins = launcher.recipe()
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "absent"
            ghc = Path(sys.executable)
            cabal = Path(sys.executable)
            archive = Path(directory) / "archive.tar.gz"
            def fake_run(command, check):
                self.assertTrue(check)
                if command[1].endswith("install-hls-official.py"):
                    (root / "inputs").mkdir(parents=True)
                    (root / "inputs/toolchain.json").write_text(json.dumps({str(cabal.resolve()): "test"}))
            with patch.object(launcher.subprocess, "run", side_effect=fake_run) as run, \
                    patch.object(launcher, "check_inputs", return_value=ghc):
                launcher.install(root, pins, ghc, cabal, archive, 8)
            self.assertEqual(run.call_args_list[0].args[0], [sys.executable,
                str(HERE / "install-hls-official.py"), "--root", str(root), "--ghc", str(ghc),
                "--cabal", str(cabal), "--jobs", "8", "--archive", str(archive)])
            self.assertEqual([Path(call.args[0][1]).name for call in run.call_args_list],
                             ["install-hls-official.py", "seal-hls.py", "guard-hls.py"])
            self.assertEqual(run.call_args_list[-1].args[0][-1], "--check-only")

    def test_reuse_does_not_rebuild_or_reseal(self):
        pins = launcher.recipe()
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(launcher, "check_inputs", return_value=Path(sys.executable)), \
                    patch.object(launcher.subprocess, "run") as run:
                launcher.install(Path(directory), pins, None, None, None, 8)
            self.assertEqual(run.call_count, 1)
            self.assertEqual(Path(run.call_args.args[0][1]).name, "guard-hls.py")

    def test_failed_existing_build_is_not_resealed(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(launcher.subprocess, "run") as run:
                with self.assertRaises(FileNotFoundError):
                    launcher.install(Path(directory), launcher.recipe(), None, None, None, 8)
                run.assert_not_called()

    def test_dangling_construction_root_is_not_repaired(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "root"
            root.symlink_to(Path(directory) / "absent")
            with patch.object(launcher.subprocess, "run") as run:
                with self.assertRaisesRegex(ValueError, "symlink"):
                    launcher.install(root, launcher.recipe(), None, None, None, 8)
                run.assert_not_called()
            self.assertTrue(root.is_symlink())

    def test_editor_and_bootstrap_have_no_generic_hls_selection(self):
        for name in (".envrc", "scripts/bootstrap-tools"):
            text = (HERE.parent / name).read_text()
            self.assertNotIn('--hls "$MARKOVIAN_HLS_VERSION"', text)
            self.assertIn("scripts/project-hls.py", text)
            self.assertIn("--check-only", text)
        self.assertNotIn("--version", (HERE / "project-hls.py").read_text())


if __name__ == "__main__":
    unittest.main(verbosity=2)
