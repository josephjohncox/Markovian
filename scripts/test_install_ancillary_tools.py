#!/usr/bin/env python3
"""Check the standalone compiler role without installing tools or changing defaults."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parent.parent


class AncillaryToolInstallationTests(unittest.TestCase):
    def test_pinned_subprocesses_and_caller_selection(self):
        with tempfile.TemporaryDirectory() as directory:
            scratch = Path(directory)
            trace = scratch / "trace.jsonl"
            ghcup = scratch / "ghcup"
            ghcup.write_text(
                "#!/usr/bin/env python3\n"
                "import json, os, sys\n"
                "with open(os.environ['TOOL_TRACE'], 'a') as trace:\n"
                "    trace.write(json.dumps(sys.argv[1:]) + '\\n')\n"
            )
            ghcup.chmod(0o755)
            env = dict(os.environ, GHCUP_BIN=str(ghcup), TOOL_TRACE=str(trace))
            original_path = env["PATH"]
            subprocess.run(
                ["bash", "-c", 'before="$PATH"; bash "$1" "$2" "$3"; '
                 'test "$PATH" = "$before"', "test",
                 str(ROOT / "scripts/install-ancillary-tools"),
                 str(scratch / "bin"), str(scratch / "store")],
                env=env, check=True,
            )
            calls = [json.loads(line) for line in trace.read_text().splitlines()]
            self.assertEqual(calls[0], ["install", "ghc", "9.8.4", "--no-set"])
            self.assertEqual(len(calls), 3)
            for call, tool in zip(calls[1:], ["hlint-3.10", "cabal-fmt-0.1.12"]):
                self.assertEqual(call[:8], ["run", "--quick", "--ghc", "9.8.4",
                                           "--cabal", "3.18.1.0", "--", "cabal"])
                self.assertEqual(call[8:], [f"--store-dir={scratch / 'store'}",
                                           "install", tool, "--ignore-project",
                                           f"--installdir={scratch / 'bin'}",
                                           "--install-method=copy", "--overwrite-policy=always"])
            self.assertEqual(env["PATH"], original_path)

    def test_bootstrap_and_ci_share_installer(self):
        bootstrap = (ROOT / "scripts/bootstrap-tools").read_text()
        workflow = (ROOT / ".github/workflows/ci.yml").read_text()
        self.assertIn('bash "$project_root/scripts/install-ancillary-tools"', bootstrap)
        self.assertIn("bash scripts/install-ancillary-tools", workflow)
        for text in [bootstrap, workflow]:
            self.assertNotIn('"hlint-${MARKOVIAN_HLINT_VERSION}"', text)
            self.assertNotIn('"cabal-fmt-${MARKOVIAN_CABAL_FMT_VERSION}"', text)
        self.assertIn('"ShellCheck-${MARKOVIAN_SHELLCHECK_VERSION}"', workflow)
        self.assertIn('--ghc "$MARKOVIAN_GHC_VERSION"', bootstrap)
        self.assertNotIn('--hls "$MARKOVIAN_HLS_VERSION"', bootstrap)
        self.assertIn('python3 "$project_root/scripts/project-hls.py" --install', bootstrap)

    def test_authoritative_roles(self):
        pins = (ROOT / "toolchain.env").read_text()
        self.assertEqual(pins.count("export MARKOVIAN_TOOL_BOOTSTRAP_GHC_VERSION="), 1)
        self.assertIn("export MARKOVIAN_GHC_VERSION=9.14.1\n", pins)
        self.assertIn("export MARKOVIAN_TOOL_BOOTSTRAP_GHC_VERSION=9.8.4\n", pins)
        self.assertIn("export MARKOVIAN_CABAL_VERSION=3.18.1.0\n", pins)
        self.assertIn("export MARKOVIAN_HLS_VERSION=2.14.0.0\n", pins)


if __name__ == "__main__":
    unittest.main()
