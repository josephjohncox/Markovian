#!/usr/bin/env python3
"""Offline construction/validation controls for complete HLS PATH selectors."""
import importlib.util
import json
from pathlib import Path
import shutil
import tempfile
import sys
import unittest
from unittest.mock import patch

import hls_selectors as selectors


class SelectorTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        for name in ('tools', 'inputs', 'compiler'):
            (self.root / name).mkdir()
        compiler = self.root / 'compiler'
        for name in ('ghc-9.14.1', 'ghc-pkg-9.14.1', 'haddock-9.14.1', 'runghc-9.14.1'):
            (compiler / name).write_text('#!/bin/sh\nexit 0\n')
            (compiler / name).chmod(0o755)
        for name in ('ghc', 'ghc-pkg', 'haddock', 'runghc'):
            (compiler / name).symlink_to(name + '-9.14.1')
        self.cabal = self.root / 'cabal'
        self.cabal.write_text('#!/bin/sh\nexit 0\n')
        self.cabal.chmod(0o755)
        self.ghc = compiler / 'ghc-9.14.1'
        self.digest = selectors.capture(self.root, self.ghc, self.cabal)
        self.tools = {str(p): selectors.sha(p) for p in
                      (self.ghc, compiler / 'ghc-pkg-9.14.1', self.cabal)}

    def validate(self):
        return selectors.validate(self.root, self.digest, self.tools)

    def test_complete_construction_includes_noncompiler_tools(self):
        self.assertEqual(self.validate(), 9)
        self.assertEqual(set(json.loads((self.root / 'inputs/tool-selectors.json').read_text())['selectors']),
                         {p.name for p in (self.root / 'compiler').iterdir()} | {'cabal'})

    def test_never_recaptures_existing_selectors(self):
        with self.assertRaisesRegex(ValueError, 'empty directory'):
            selectors.capture(self.root, self.ghc, self.cabal)

    def test_installer_rechecks_capture_before_first_tool_execution(self):
        spec = importlib.util.spec_from_file_location('installer', Path(__file__).with_name('install-hls-official.py'))
        installer = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(installer)
        fresh = self.root / 'fresh'
        capture = selectors.capture
        def changed_capture(root, ghc, cabal):
            digest = capture(root, ghc, cabal)
            (root / 'tools/cabal').unlink()
            return digest
        argv = ['install-hls-official.py', '--root', str(fresh), '--ghc', str(self.ghc), '--cabal', str(self.cabal)]
        with patch.object(selectors, 'capture', side_effect=changed_capture), \
                patch.object(installer.subprocess, 'run') as tool, patch.object(sys, 'argv', argv):
            with self.assertRaisesRegex(ValueError, 'missing or unexpected'):
                installer.main()
            tool.assert_not_called()
        receipt = json.loads((fresh / 'build-receipt.json').read_text())
        self.assertFalse(receipt['fresh_build_completed'])
        self.assertEqual(receipt['selectors_sha256'], selectors.sha(fresh / 'inputs/tool-selectors.json'))

    def test_missing_selector(self):
        (self.root / 'tools/cabal').unlink()
        with self.assertRaisesRegex(ValueError, 'missing or unexpected'):
            self.validate()

    def test_same_byte_wrong_target(self):
        copy = self.root / 'copy'
        shutil.copy2(self.cabal, copy)
        (self.root / 'tools/cabal').unlink()
        (self.root / 'tools/cabal').symlink_to(copy)
        with self.assertRaisesRegex(ValueError, 'link identity'):
            self.validate()

    def test_same_byte_regular_file(self):
        (self.root / 'tools/cabal').unlink()
        shutil.copy2(self.cabal, self.root / 'tools/cabal')
        with self.assertRaisesRegex(ValueError, 'link identity'):
            self.validate()

    def test_alias_resolution_changed_without_selector_change(self):
        (self.root / 'compiler/ghc').unlink()
        (self.root / 'compiler/ghc').symlink_to('ghc-pkg-9.14.1')
        with self.assertRaisesRegex(ValueError, 'resolution mismatch'):
            self.validate()

    def test_unexpected_selector(self):
        (self.root / 'tools/extra').symlink_to(self.cabal)
        with self.assertRaisesRegex(ValueError, 'missing or unexpected'):
            self.validate()

    def test_directory_redirection_even_to_exact_copy(self):
        (self.root / 'tools').rename(self.root / 'other-tools')
        (self.root / 'tools').symlink_to('other-tools')
        with self.assertRaisesRegex(ValueError, 'directory redirected'):
            self.validate()

    def test_manifest_drift(self):
        path = self.root / 'inputs/tool-selectors.json'
        path.chmod(0o644)
        path.write_text(path.read_text() + '\n')
        with self.assertRaisesRegex(ValueError, 'manifest identity'):
            self.validate()

    def test_malformed_records_even_with_matching_digest(self):
        path = self.root / 'inputs/tool-selectors.json'
        path.chmod(0o644)
        original = json.loads(path.read_text())
        for change in ('schema', 'path', 'record', 'required'):
            data = json.loads(json.dumps(original))
            if change == 'schema':
                data['schema'] = True
            elif change == 'path':
                data['selectors']['../escape'] = data['selectors'].pop('haddock')
            elif change == 'record':
                data['selectors']['cabal']['extra'] = 'invalid'
            else:
                del data['selectors']['ghc']
            path.write_text(json.dumps(data))
            self.digest = selectors.sha(path)
            with self.subTest(change=change), self.assertRaises(ValueError):
                self.validate()


if __name__ == '__main__':
    unittest.main(verbosity=2)
