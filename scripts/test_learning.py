#!/usr/bin/env python3
"""Deliberate failures for the executable-learning gate, including real GHC runs."""
from __future__ import annotations

import copy
import hashlib
import json
import runpy
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

CHECK = runpy.run_path(str(Path(__file__).with_name("check-learning")))
LearningError = CHECK["LearningError"]


class LearningTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.put("Fixture.cabal", """cabal-version: 3.0
name: Fixture
version: 0.1.0.0
build-type: Simple
executable fixture
  main-is: Main.hs
  hs-source-dirs: app
  build-depends: base
  default-language: Haskell2010
""")
        self.put("cabal.project.ci", "packages: .\n")
        self.put("app/Main.hs", 'module Main where\nmain :: IO ()\nmain = putStrLn "checked"\n')
        self.put("docs/book/src/lesson.md", '```haskell\nmain = putStrLn "checked"\n```\n')
        self.put("docs/learning/output.txt", "checked\n")
        self.put("scripts/check-learning", "fixture checker\n")
        self.put("release/packages.tsv", "Fixture\t.\t0.1.0.0\t0\n")
        self.manifest = {
            "schemaVersion": 1,
            "contexts": {"fixture": {"source": "app/Main.hs", "cabal": "Fixture.cabal", "target": "Fixture:exe:fixture"}},
            "fences": {"docs/book/src/lesson.md#1": {"class": "fragment", "contexts": ["fixture"]}},
            "runs": [{"context": "fixture", "args": [], "output": "docs/learning/output.txt"}],
        }
        self.put(CHECK["MANIFEST"], json.dumps(self.manifest))

    def put(self, name, text):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def validate(self):
        return CHECK["validate"](self.root, self.manifest)

    def rejects(self, text):
        with self.assertRaisesRegex(LearningError, text):
            self.validate()

    def test_valid_fragment(self):
        self.validate()

    def test_unclassified_fence(self):
        self.put("docs/book/src/new.md", "~~~hs\nx = 1\n~~~\n")
        self.rejects("unclassified")

    def test_deleted_fence(self):
        self.put("docs/book/src/lesson.md", "No code.\n")
        self.rejects("stale fences")

    def test_invalid_class(self):
        self.manifest["fences"]["docs/book/src/lesson.md#1"]["class"] = "looks-runnable"
        self.rejects("invalid classification")

    def test_unknown_context(self):
        self.manifest["fences"]["docs/book/src/lesson.md#1"]["contexts"] = ["absent"]
        self.rejects("invalid context")

    def test_missing_source(self):
        (self.root / "app/Main.hs").unlink()
        self.rejects("missing reference")

    def test_unsafe_source(self):
        self.manifest["contexts"]["fixture"]["source"] = "../escape.hs"
        self.rejects("unsafe reference")

    def test_symlink_escape(self):
        (self.root / "app/Main.hs").unlink()
        (self.root / "app/Main.hs").symlink_to(Path(__file__).resolve())
        self.rejects("missing reference")

    def test_wrong_component(self):
        self.manifest["contexts"]["fixture"]["target"] = "Fixture:exe:absent"
        self.rejects("missing compiled component")

    def test_wrong_package(self):
        self.manifest["contexts"]["fixture"]["target"] = "Other:exe:fixture"
        self.rejects("package mismatch")

    def test_not_cabal_member(self):
        self.put("other/Main.hs", (self.root / "app/Main.hs").read_text())
        self.manifest["contexts"]["fixture"]["source"] = "other/Main.hs"
        self.rejects("not in compiled component")

    def test_stale_fragment(self):
        self.put("docs/book/src/lesson.md", '```haskell\nmain = putStrLn "stale"\n```\n')
        self.rejects("stale source fragment")

    def layout_fixture(self, arguments_indent=8):
        self.put("app/Main.hs", '''module Main where
main :: IO ()
main = do
    let config =
            qLearningConfig
                discount
                (ConstantLearningRate alpha)
                (ConstantExploration epsilon)
                episodeLimit
                episodeStepLimit
    print config
data LearningRate = ConstantLearningRate Int
data Exploration = ConstantExploration Int
qLearningConfig d (ConstantLearningRate a) (ConstantExploration e) n s = d + a + e + n + s
discount, alpha, epsilon, episodeLimit, episodeStepLimit :: Int
discount = 1
alpha = 2
epsilon = 3
episodeLimit = 4
episodeStepLimit = 5
''')
        arguments = ["discount", "(ConstantLearningRate alpha)",
                     "(ConstantExploration epsilon)", "episodeLimit", "episodeStepLimit"]
        snippet = "let config =\n      qLearningConfig\n" + "".join(
            " " * arguments_indent + argument + "\n" for argument in arguments)
        self.put("docs/book/src/lesson.md", "```haskell\n" + snippet + "```\n")
        return snippet

    def test_fragment_accepts_different_indentation_width(self):
        self.layout_fixture()
        self.validate()

    def test_fragment_rejects_qlearning_arguments_at_declaration_column(self):
        self.layout_fixture(arguments_indent=4)
        self.rejects("relative layout")

    def test_fragment_rejects_changed_layout_with_same_tokens(self):
        self.layout_fixture(arguments_indent=6)
        self.rejects("relative layout")

    def test_real_compiler_confirms_displayed_layout_regression(self):
        snippet = self.layout_fixture(arguments_indent=4)
        self.rejects("relative layout")
        path = self.root / "app/Main.hs"
        valid = path.read_text()
        begin, end = valid.index("    let config"), valid.index("    print config")
        malformed = valid[:begin] + "".join("    " + line + "\n" for line in snippet.splitlines()) + valid[end:]
        path.write_text(malformed)
        with self.assertRaises(subprocess.CalledProcessError):
            CHECK["execute"](self.root, self.manifest, "cabal.project.ci", "dist")
        self.layout_fixture()
        self.validate()
        self.put("docs/learning/output.txt", "15\n")
        CHECK["execute"](self.root, self.manifest, "cabal.project.ci", "dist")

    def test_unclosed_fence(self):
        self.put("docs/book/src/lesson.md", "```haskell\nx\n")
        self.rejects("unclosed")

    def test_nested_fence_is_not_haskell(self):
        self.put("docs/book/src/other.md", "````text\n```haskell\nnot code\n```\n````\n")
        self.validate()

    def test_markdown_include_inventory(self):
        self.put("docs/book/src/appendix.md", "{{#include ../../outside.md}}\n")
        self.put("docs/outside.md", "```haskell\nx\n```\n")
        self.rejects("unclassified")

    def test_bad_include(self):
        self.put("docs/book/src/lesson.md", "```haskell\n{{#include missing.hs}}\n```\n")
        self.rejects("invalid include")

    def test_out_of_range_include(self):
        self.put("docs/book/src/lesson.md", "```haskell\n{{#include ../../../app/Main.hs:1:999}}\n```\n")
        self.rejects("stale include range")

    def test_include_wrong_context(self):
        self.put("other/Main.hs", (self.root / "app/Main.hs").read_text())
        self.put("docs/book/src/lesson.md", "```haskell\n{{#include ../../../other/Main.hs}}\n```\n")
        self.rejects("outside compiled context")

    def test_runnable_needs_full_source(self):
        self.manifest["fences"]["docs/book/src/lesson.md#1"]["class"] = "runnable"
        self.rejects("complete source context")

    def test_runnable_needs_execution(self):
        self.put("docs/book/src/lesson.md", "```haskell\n{{#include ../../../app/Main.hs}}\n```\n")
        self.manifest["fences"]["docs/book/src/lesson.md#1"]["class"] = "runnable"
        self.manifest["runs"] = []
        self.rejects("no execution")

    def test_pseudocode_needs_reason(self):
        self.manifest["fences"]["docs/book/src/lesson.md#1"] = {"class": "pseudocode", "reason": ""}
        self.rejects("explicit reason")

    def test_stale_pseudocode(self):
        self.manifest["fences"]["docs/book/src/lesson.md#1"] = {"class": "pseudocode", "reason": "A deliberately incomplete conceptual sketch.", "sha256": hashlib.sha256(b"old").hexdigest()}
        self.rejects("stale pseudocode")

    def test_stale_displayed_output(self):
        before = CHECK["receipt"](self.root, self.manifest)
        self.put("docs/learning/output.txt", "unexecuted answer\n")
        self.assertNotEqual(before, CHECK["receipt"](self.root, self.manifest))

    def test_stale_implementation_receipt(self):
        self.put("src/Library.hs", "module Library where\nx = 1\n")
        before = CHECK["receipt"](self.root, self.manifest)
        self.put("src/Library.hs", "module Library where\nx = 2\n")
        self.assertNotEqual(before, CHECK["receipt"](self.root, self.manifest))

    def test_duplicate_output(self):
        self.manifest["runs"].append(copy.deepcopy(self.manifest["runs"][0]))
        self.rejects("duplicate output")

    def test_missing_output(self):
        (self.root / "docs/learning/output.txt").unlink()
        self.rejects("missing reference")

    def test_execution_failure_propagates(self):
        with patch("subprocess.run", side_effect=subprocess.CalledProcessError(1, "cabal")), self.assertRaises(subprocess.CalledProcessError):
            CHECK["execute"](self.root, self.manifest, "cabal.project.ci", "dist")

    def test_real_compiler_rejects_context_error(self):
        self.put("app/Main.hs", 'module Main where\nmain :: IO ()\nmain = False\n')
        with self.assertRaises(subprocess.CalledProcessError):
            CHECK["execute"](self.root, self.manifest, "cabal.project.ci", "dist")

    def test_real_runtime_failure_propagates(self):
        self.put("app/Main.hs", 'module Main where\nmain :: IO ()\nmain = fail "deliberate fixture failure"\n')
        with self.assertRaises(subprocess.CalledProcessError):
            CHECK["execute"](self.root, self.manifest, "cabal.project.ci", "dist")

    def test_real_execution_detects_stale_answer(self):
        self.put("docs/learning/output.txt", "wrong\n")
        with self.assertRaisesRegex(LearningError, "stale executed output"):
            CHECK["execute"](self.root, self.manifest, "cabal.project.ci", "dist")
        CHECK["execute"](self.root, self.manifest, "cabal.project.ci", "dist", write=True)
        self.assertEqual((self.root / "docs/learning/output.txt").read_text(), "checked\n")


if __name__ == "__main__":
    unittest.main()
