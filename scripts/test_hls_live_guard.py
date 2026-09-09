#!/usr/bin/env python3
"""Explicit post-build controls, not an offline CI test or a new HLS build.
Set HLS_TEST_BUILD and HLS_TEST_GHC to the sealed build and its installed compiler.
All negative controls mutate disposable copies; ghc-pkg itself is real.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
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


guard = load("guard-hls")
installer = load("install-hls-official")
sealer = load("seal-hls")


class LiveGuardTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = Path(os.environ["HLS_TEST_BUILD"]).resolve(strict=True)
        cls.ghc = Path(os.environ["HLS_TEST_GHC"]).resolve(strict=True)

    def fixture(self):
        evidence = os.environ.get("HLS_TEST_EVIDENCE")
        if evidence:
            Path(evidence).mkdir(parents=True, exist_ok=True)
            root = Path(tempfile.mkdtemp(prefix=self._testMethodName + "-", dir=evidence))
        else:
            temp = tempfile.TemporaryDirectory(prefix="hls-live-guard-")
            self.addCleanup(temp.cleanup)
            root = Path(temp.name)
        shutil.copytree(self.build / "tools", root / "tools", symlinks=True)
        shutil.copyfile(self.build / "runtime-seal.json", root / "runtime-seal.json")
        (root / "install").symlink_to(self.build / "install", target_is_directory=True)
        shutil.copytree(self.build / "inputs", root / "inputs")
        shutil.copytree(self.build / "evidence", root / "evidence")
        shutil.copyfile(self.build / "build-receipt.json", root / "build-receipt.json")
        source = root / "source/haskell-language-server-2.14.0.0"
        source.mkdir(parents=True)
        for name in ("cabal.project", "cabal.project.abi"):
            shutil.copyfile(self.build / "source/haskell-language-server-2.14.0.0" / name,
                            source / name)
        (root / "cabal").mkdir()
        (root / "cabal/config").write_text((self.build / "cabal/config").read_text().replace(
            str(self.build), str(root)))
        return root

    def test_all_1722_original_archive_files(self):
        archive = self.build / "inputs/hls-src.tar.gz"
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = installer.extract(archive, root)
            with tarfile.open(archive) as source:
                for member in source:
                    if member.isfile():
                        self.assertEqual((root / member.name).read_bytes(),
                                         source.extractfile(member).read())
            self.assertEqual(len(manifest), 1722)
            self.assertEqual(os.readlink(root / installer.TOP / "docs/contributing/plugin-tutorial.lhs"),
                             "plugin-tutorial.md")

    def test_real_guard_positive(self):
        server, env, proof = guard.check(self.build, self.ghc)
        self.assertEqual(proof["abi_guard"], "PASS")
        self.assertTrue(server.is_file())
        self.assertGreater(proof["boot_abi_count"], 0)
        self.assertGreater(proof["runtime_files_verified"], 0)
        self.assertEqual(env["GHC_ENVIRONMENT"], "-")

    def test_real_ghc_pkg_rejects_different_build_boot_abi(self):
        root = self.fixture()
        path = root / "inputs/boot-package-db.txt"
        boot = path.read_bytes()
        lines = boot.splitlines(keepends=True)
        index = next(i for i, line in enumerate(lines) if line.startswith(b"abi: "))
        lines[index] = b"abi:                  " + b"0" * 32 + b"\n"
        bad = b"".join(lines)
        self.assertNotEqual(boot, bad)
        for target in (path, root / "evidence/04-boot-package-db.log"):
            target.chmod(0o644)
            target.write_bytes(bad)
        result = root / "evidence/04-boot-package-db.result.json"
        data = json.loads(result.read_text())
        data["log_sha256"] = hashlib.sha256(bad).hexdigest()
        result.chmod(0o644)
        result.write_text(json.dumps(data))
        seal_path = root / "runtime-seal.json"
        seal = json.loads(seal_path.read_text())
        seal['inputs']['inputs/boot-package-db.txt'] = hashlib.sha256(bad).hexdigest()
        seal_path.write_text(json.dumps(seal))
        with self.assertRaisesRegex(ValueError, "BOOT PACKAGE ABI/identity mismatch"):
            guard.check(root, self.ghc)

    def test_disabled_abi_recipe_rejected(self):
        root = self.fixture()
        path = root / "source/haskell-language-server-2.14.0.0/cabal.project.abi"
        path.write_bytes(path.read_bytes().replace(b"+ghc-check-use-package-abis",
                                                  b"-ghc-check-use-package-abis"))
        with self.assertRaisesRegex(ValueError, "generated project policy mismatch"):
            guard.check(root, self.ghc)

    def test_plan_tamper_rejected(self):
        root = self.fixture()
        path = root / "inputs/plan.json"
        path.chmod(0o644)
        path.write_text(path.read_text() + "\n")
        with self.assertRaisesRegex(ValueError, "dependency plan mismatch"):
            guard.check(root, self.ghc)

    def test_global_bounds_override_rejected(self):
        root = self.fixture()
        path = root / "cabal/config"
        path.write_text(path.read_text() + "allow-newer: *\n")
        with self.assertRaisesRegex(ValueError, "isolated config policy mismatch"):
            guard.check(root, self.ghc)

    def test_local_override_rejected(self):
        root = self.fixture()
        (root / "source/haskell-language-server-2.14.0.0/cabal.project.abi.local").write_text(
            "allow-newer: *\n")
        with self.assertRaisesRegex(ValueError, "unexpected local override"):
            guard.check(root, self.ghc)

    def test_runtime_library_identity_mismatch_rejected(self):
        root = self.fixture()
        seal = json.loads((self.build / "runtime-seal.json").read_text())
        seal["runtime_files"][next(iter(seal["runtime_files"]))] = "0" * 64
        (root / "runtime-seal.json").write_text(json.dumps(seal))
        with self.assertRaisesRegex(ValueError, "linked library/compiler backend identity mismatch"):
            guard.check(root, self.ghc)

    def test_missing_runtime_seal_rejected(self):
        root = self.fixture()
        (root / "runtime-seal.json").unlink()
        with self.assertRaises(FileNotFoundError):
            guard.check(root, self.ghc)

    def test_selector_drift_blocks_seal_and_launch_before_any_exec(self):
        names = ('cabal', 'ghc', 'ghc-9.14.1', 'ghc-pkg', 'ghc-pkg-9.14.1')
        cases = [(name, change) for name in names for change in
                 ('missing', 'redirected', 'same-byte-target', 'regular-file')]
        cases += [('tools', change) for change in ('directory', 'directory-missing', 'directory-file')]
        cases += [('extra', 'unexpected'), ('manifest', 'malformed'), ('manifest', 'missing-required')]
        for name, change in cases:
            with self.subTest(name=name, change=change):
                root = self.fixture()
                target = root / 'tools' / name
                marker = root / 'EXECUTED'
                sentinel = root / 'sentinel'
                sentinel.write_text('#!/bin/sh\ntouch ' + str(marker) + '\nexit 73\n')
                sentinel.chmod(0o755)
                before = {p: guard.sha(p) for p in json.loads((root / 'inputs/toolchain.json').read_text())}
                if change in ('directory', 'directory-missing', 'directory-file'):
                    (root / 'tools').rename(root / 'original-tools')
                    if change == 'directory':
                        (root / 'tools').symlink_to(root / 'original-tools')
                    elif change == 'directory-file':
                        (root / 'tools').write_text('not a directory')
                elif change in ('malformed', 'missing-required'):
                    # Deliberately self-consistent disposable receipt: schema/required
                    # aliases must still reject, not merely an obsolete digest.
                    manifest_path = root / 'inputs/tool-selectors.json'
                    manifest = json.loads(manifest_path.read_text())
                    if change == 'malformed':
                        manifest['selectors']['cabal']['extra'] = 'invalid'
                    else:
                        del manifest['selectors']['ghc']
                        (root / 'tools/ghc').unlink()
                    manifest_path.chmod(0o644)
                    manifest_path.write_text(json.dumps(manifest))
                    receipt_path = root / 'build-receipt.json'
                    receipt = json.loads(receipt_path.read_text())
                    receipt['selectors_sha256'] = guard.sha(manifest_path)
                    receipt_path.write_text(json.dumps(receipt))
                    seal_path = root / 'runtime-seal.json'
                    seal = json.loads(seal_path.read_text())
                    seal['selectors_sha256'] = receipt['selectors_sha256']
                    seal['inputs']['inputs/tool-selectors.json'] = receipt['selectors_sha256']
                    seal['build_receipt_sha256'] = guard.sha(receipt_path)
                    seal_path.write_text(json.dumps(seal))
                elif change == 'unexpected':
                    target.symlink_to(sentinel)
                else:
                    if change in ('same-byte-target', 'regular-file'):
                        shutil.copy2(target.resolve(), root / 'same-byte-copy')
                    target.unlink()
                    if change == 'redirected':
                        target.symlink_to(sentinel)
                    elif change == 'same-byte-target':
                        target.symlink_to(root / 'same-byte-copy')
                    elif change == 'regular-file':
                        shutil.copy2(root / 'same-byte-copy', target)
                with patch.object(guard.subprocess, 'check_output') as tool, \
                        patch.object(guard.os, 'execve') as server, \
                        patch.object(sys, 'argv', ['guard-hls.py', '--root', str(root), '--ghc', str(self.ghc)]):
                    self.assertEqual(guard.main(), 1)
                    tool.assert_not_called()
                    server.assert_not_called()
                (root / 'runtime-seal.json').unlink()
                shutil.rmtree(root / 'source')
                (root / 'source').symlink_to(self.build / 'source')
                with patch.object(sealer.subprocess, 'check_output') as tool, \
                        patch.object(sys, 'argv', ['seal-hls.py', '--root', str(root)]):
                    with self.assertRaises((ValueError, FileNotFoundError)):
                        sealer.main()
                    tool.assert_not_called()
                self.assertFalse(marker.exists())
                self.assertFalse((root / 'runtime-seal.json').exists())
                self.assertEqual(before, {p: guard.sha(p) for p in before})
                (root / 'control-proof.json').write_text(json.dumps({
                    'selector': name, 'mutation': change, 'guard_blocked_before_tool_and_server': True,
                    'seal_blocked_before_tool': True, 'sentinel_executed': False,
                    'absolute_tools_unchanged': before}, indent=2))

    def test_wrong_compiler_rejected(self):
        with self.assertRaisesRegex(ValueError, "wrong compiler input"):
            guard.check(self.build, Path(sys.executable))


if __name__ == "__main__":
    unittest.main(verbosity=2)
