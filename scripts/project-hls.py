#!/usr/bin/env python3
"""Project-local official HLS construction and mandatory guarded LSP launcher.

No global selection, generic-wrapper fallback, relocation, or failed-build reseal.
An existing installation is explicitly reused, never reported as a fresh build.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

HERE = Path(__file__).resolve().parent
PROJECT = HERE.parent


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def recipe():
    pins = json.loads((HERE / "hls-recipe.json").read_text())
    env = (PROJECT / "toolchain.env").read_text()
    for key in ("ghc", "cabal", "hls"):
        values = re.findall(r"^export MARKOVIAN_" + key.upper() +
                            r"_VERSION=(\S+)$", env, re.MULTILINE)
        if values != [pins[key]]:
            raise ValueError("toolchain/official HLS recipe pin mismatch: " + key)
    for name, digest in pins["scripts"].items():
        if Path(name).name != name or sha(HERE / name) != digest:
            raise ValueError("pinned HLS script identity mismatch: " + name)
    return pins


def build_root(pins):
    digest = hashlib.sha256(json.dumps(pins, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    return PROJECT / ".direnv" / f"hls-official-{pins['hls']}-ghc-{pins['ghc']}-{digest}"


def check_inputs(root, pins):
    if root.is_symlink():
        raise ValueError("construction root must not be a symlink")
    receipt = json.loads((root / "build-receipt.json").read_text())
    if receipt.get("installer_sha256") != pins["scripts"]["install-hls-official.py"]:
        raise ValueError("construction used a different installer")
    policy = json.loads((root / "inputs/policy.json").read_text())
    for key, field in (("source_url", "url"), ("source_sha256", "archive_sha256"),
                       ("original_project_sha256", "original_project_sha256"),
                       ("generated_project_sha256", "generated_project_sha256"),
                       ("index_state", "index_state")):
        if policy.get(field) != pins[key]:
            raise ValueError("construction recipe pin mismatch: " + key)
    tools = json.loads((root / "inputs/toolchain.json").read_text())
    compilers = [Path(name) for name in tools if Path(name).name == "ghc-" + pins["ghc"]]
    if len(compilers) != 1:
        raise ValueError("missing unique recorded project compiler")
    return compilers[0]


def install(root, pins, ghc, cabal, archive, jobs):
    fresh = not (root.exists() or root.is_symlink())
    if root.is_symlink():
        raise ValueError("construction root must not be a symlink")
    if fresh:
        if ghc is None or cabal is None:
            raise ValueError("fresh construction requires absolute --ghc and --cabal")
        command = [sys.executable, str(HERE / "install-hls-official.py"),
                   "--root", str(root), "--ghc", str(ghc), "--cabal", str(cabal),
                   "--jobs", str(jobs)]
        if archive is not None:
            command += ["--archive", str(archive)]
        subprocess.run(command, check=True)
        subprocess.run([sys.executable, str(HERE / "seal-hls.py"),
                        "--root", str(root)], check=True)
    compiler = check_inputs(root, pins)
    if ghc is not None and compiler.resolve(strict=True) != ghc.resolve(strict=True):
        raise ValueError("existing installation compiler differs from requested compiler")
    if cabal is not None:
        tools = json.loads((root / "inputs/toolchain.json").read_text())
        if str(cabal.resolve(strict=True)) not in tools:
            raise ValueError("existing installation Cabal differs from requested Cabal")
    subprocess.run([sys.executable, str(HERE / "guard-hls.py"), "--root", str(root),
                    "--ghc", str(compiler), "--check-only"], check=True)
    print("HLS: " + ("fresh isolated construction" if fresh else "reuse of sealed construction") +
          "; live ABI guard passed; project LSP operation is a separate check", file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--install", action="store_true")
    mode.add_argument("--check-only", action="store_true")
    mode.add_argument("--lsp", action="store_true")
    parser.add_argument("--ghc", type=Path)
    parser.add_argument("--cabal", type=Path)
    parser.add_argument("--archive", type=Path)
    parser.add_argument("--jobs", type=int, default=8)
    args = parser.parse_args()
    if not args.install and (args.ghc or args.cabal or args.archive or args.jobs != 8):
        parser.error("construction arguments require --install")
    try:
        pins = recipe()
        root = build_root(pins)
        if args.install:
            install(root, pins, args.ghc, args.cabal, args.archive, args.jobs)
            return 0
        compiler = check_inputs(root, pins)
        command = [sys.executable, str(HERE / "guard-hls.py"),
                   "--root", str(root), "--ghc", str(compiler)]
        if args.check_only:
            command.append("--check-only")
        os.execv(sys.executable, command)
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as exc:
        print("PROJECT HLS BLOCKED: " + str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
