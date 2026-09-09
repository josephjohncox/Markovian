#!/usr/bin/env python3
"""Fresh warning-enabled library installation; never infer success from parent output.

Cabal's default logs-dir uses unit IDs (unlike the documented $pkgid template).
The private library is built but intentionally has no public Haddock interface.
This checkout-only CI gate does not perform release preparation or coverage.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import tarfile
import tempfile
import time

from release_tool import (
    ReleaseError,
    check_haddock_interfaces,
    check_haddock_log,
    parse_manifest,
    sha256_file,
)


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ReleaseError(message)


def source_inventory(root: Path) -> dict:
    paths = subprocess.check_output(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=root,
    ).decode().split("\0")
    return {
        name: {"sha256": sha256_file(root / name),
               "mode": (root / name).stat().st_mode & 0o777}
        for name in sorted(set(filter(None, paths)))
        if (root / name).is_file()
    }


def expected_libraries(root: Path, packages: list) -> set[tuple[str, str]]:
    expected = set()
    for package in packages:
        cabal = root / package.directory / f"{package.name}.cabal"
        headings = re.findall(r"(?m)^library(?:[ \t]+([\w-]+))?[ \t]*$", cabal.read_text())
        require(headings.count("") == 1, f"expected one public library: {cabal}")
        for name in headings:
            key = (package.name, "lib" + (":" + name if name else ""))
            require(key not in expected, f"duplicate library declaration: {key}")
            expected.add(key)
    require(len(packages) == 16 and len(expected) == 17,
            "expected 16 public libraries and one private library")
    require(("markovian-dense-exact", "lib:legacy-finite-ir") in expected,
            "missing declared private legacy-finite-ir library")
    return expected


def verify_install(root: Path, run: Path, binding: dict) -> dict:
    """Check actual plan, source archives, all unit logs and installed interfaces."""
    require(source_inventory(root) == binding["sources"], "source snapshot changed")
    manifest = root / "release/packages.tsv"
    require(sha256_file(manifest) == binding["manifest_sha256"], "manifest changed")
    packages = parse_manifest(manifest)
    expected = expected_libraries(root, packages)
    versions = {p.name: p.version for p in packages}
    directories = {p.name: p.directory for p in packages}
    plan_path = run / "build/cache/plan.json"
    plan = json.loads(plan_path.read_text())
    compiler = binding["compiler"]
    require(plan["compiler-id"] == compiler, "plan compiler mismatch")
    require(plan["cabal-version"] == binding["cabal"], "plan Cabal mismatch")
    compiler_directory = compiler + "-" + plan["compiler-abi"]
    require(re.fullmatch(r"ghc-[0-9.]+-[a-z0-9]+", compiler_directory) is not None,
            "invalid compiler ABI directory")
    require(sha256_file(run / "cabal.config") == binding["config_sha256"],
            "capture config changed")
    libraries = {}
    units = set()
    archives = {}
    for item in plan["install-plan"]:
        component = item.get("component-name", "")
        # Configured executables can be present without being built by install --lib.
        if item.get("type") != "configured" or not (
            component == "lib" or component.startswith("lib:")
        ):
            continue
        key = (item["pkg-name"], component)
        require(key in expected, f"unexpected configured library: {key}")
        require(key not in libraries, f"duplicate planned library: {key}")
        require(item["pkg-version"] == versions[key[0]], f"version mismatch: {key}")
        unit = item["id"]
        require(re.fullmatch(r"[A-Za-z0-9_.+-]+", unit) is not None,
                f"unsafe unit ID: {unit}")
        require(unit not in units, f"duplicate unit ID: {unit}")
        units.add(unit)
        libraries[key] = item
        source = item["pkg-src"]
        require(item.get("style") == "global" and source["type"] == "local-tar",
                f"not a fresh local archive installation: {key}")
        archive = Path(source["path"])
        require(archive.resolve().is_relative_to((run / "build/sdist").resolve()),
                f"archive outside fresh build: {archive}")
        require(sha256_file(archive) == item["pkg-src-sha256"],
                f"archive/plan hash mismatch: {key}")
        if key[0] not in archives:
            with tarfile.open(archive, "r:gz") as tar:
                for member in tar.getmembers():
                    if member.isdir():
                        continue
                    require(member.isfile(), f"non-regular archive source: {member.name}")
                    parts = Path(member.name).parts
                    require(parts[0] == f"{key[0]}-{versions[key[0]]}" and
                            ".." not in parts and len(parts) > 1,
                            f"invalid archive member: {member.name}")
                    original = root / directories[key[0]] / Path(*parts[1:])
                    require(original.is_file() and
                            tar.extractfile(member).read() == original.read_bytes(),
                            f"archive/current source mismatch: {original}")
            archives[key[0]] = {"path": str(archive), "sha256": sha256_file(archive)}
        else:
            require(archives[key[0]]["sha256"] == item["pkg-src-sha256"],
                    f"inconsistent component source: {key}")
    require(set(libraries) == expected, "missing planned libraries")
    expected_logs = {run / "logs" / compiler / f"{unit}.log" for unit in units}
    actual_logs = set((run / "logs").rglob("*.log"))
    require(actual_logs == expected_logs, "missing, duplicate or unexpected component logs")
    records = []
    for key, item in sorted(libraries.items()):
        unit = item["id"]
        log = run / "logs" / compiler / f"{unit}.log"
        require(not log.is_symlink() and log.is_file() and
                log.stat().st_mtime_ns >= binding["started_ns"], f"stale component log: {log}")
        text = check_haddock_log(log)
        name, component = key
        label = "library" if component == "lib" else f"library '{component[4:]}'"
        require(f"Building {label} for {name}-{versions[name]}..." in text,
                f"component identity mismatch: {log}")
        install_label = "library" if component == "lib" else f"internal library {component[4:]}"
        require(re.search(rf"(?m)^Installing {re.escape(install_label)} in "
                          rf"{re.escape(str(run / 'store'))}/"
                          rf"{re.escape(compiler_directory)}/(?:[^\n]*/)?{re.escape(unit)}/lib$", text)
                is not None, f"compiler/store/unit installation mismatch: {log}")
        if component == "lib":
            require(f"Running Haddock on library for {name}-{versions[name]}..." in text
                    and "Documentation created:\n" in text, f"missing Haddock run: {log}")
        records.append({"package": name, "component": component, "unit": unit,
                        "path": str(log), "sha256": sha256_file(log)})
    check_haddock_log(run / "parent.log")
    check_haddock_interfaces(run / "store", packages)
    interfaces = list((run / "store").rglob("*.haddock"))
    require(len(interfaces) == len(packages), "unexpected installed interfaces")
    for key, item in libraries.items():
        if key[1] == "lib":
            require(any(p.name.casefold() == f"{key[0]}.haddock".casefold()
                        and item["id"] in p.parts and compiler_directory in p.parts
                        for p in interfaces),
                    f"interface unit mismatch: {key}")
    return {"plan_sha256": sha256_file(plan_path), "archives": archives,
            "logs": records, "parent_sha256": sha256_file(run / "parent.log"),
            "interfaces": [{"path": str(p), "sha256": sha256_file(p)}
                           for p in sorted(interfaces)]}


def capture(root: Path, output_parent: Path, cabal: str, ghc: str) -> Path:
    root = root.resolve()
    output_parent.mkdir(parents=True, exist_ok=True)
    run = Path(tempfile.mkdtemp(prefix="haddock-install-", dir=output_parent)).resolve()
    print(f"Haddock evidence: {run}", flush=True)
    require(not run.is_relative_to(root), "evidence must be outside source checkout")
    pins = dict(re.findall(r"(?m)^export MARKOVIAN_(GHC|CABAL)_VERSION=(\S+)$",
                           (root / "toolchain.env").read_text()))
    env = dict(os.environ, GHC_ENVIRONMENT="-")
    commands = []

    def execute(args: list[str], log_name: str) -> None:
        with (run / log_name).open("x") as log:
            result = subprocess.run(args, cwd=root, env=env, stdout=log,
                                    stderr=subprocess.STDOUT)
        commands.append({"argv": args, "exit": result.returncode})
        (run / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        require(result.returncode == 0, f"command failed; see {run / log_name}")

    execute([ghc, "--numeric-version"], "ghc-version.log")
    execute([cabal, "--numeric-version"], "cabal-version.log")
    require((run / "ghc-version.log").read_text().strip() == pins["GHC"], "wrong GHC")
    require((run / "cabal-version.log").read_text().strip() == pins["CABAL"], "wrong Cabal")
    config = run / "cabal.config"
    execute([cabal, f"--config-file={config}", "user-config", "init"], "config-init.log")
    # Generate, never copy a host config (which may contain repository credentials).
    with config.open("a") as handle:
        handle.write(f"\nlogs-dir: {run / 'logs'}\nremote-repo-cache: {run / 'cache'}\n"
                     "active-repositories: :none\n")
    packages = parse_manifest(root / "release/packages.tsv")
    expected_libraries(root, packages)
    binding = {"compiler": "ghc-" + pins["GHC"], "cabal": pins["CABAL"],
               "manifest_sha256": sha256_file(root / "release/packages.tsv"),
               "sources": source_inventory(root), "config_sha256": sha256_file(config),
               "started_ns": time.time_ns()}
    (run / "binding.json").write_text(json.dumps(binding, indent=2) + "\n")
    execute([cabal, f"--config-file={config}", f"--store-dir={run / 'store'}",
             "install", "--lib", *[p.name for p in packages],
             "--project-file=cabal.project.ci", f"--builddir={run / 'build'}",
             f"--package-env={run / 'environment'}", f"--with-compiler={ghc}",
             "--offline", "--enable-documentation", "--overwrite-policy=always"], "parent.log")
    result = verify_install(root, run, binding)
    (run / "verified.json").write_text(json.dumps(result, indent=2) + "\n")
    print("PASS: 16 public + 1 private unit logs; 16 unit-bound Haddock interfaces")
    return run


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-parent", type=Path, required=True)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--cabal", default="cabal")
    parser.add_argument("--ghc", default="ghc")
    args = parser.parse_args()
    try:
        capture(args.root, args.output_parent, args.cabal, args.ghc)
    except (ReleaseError, OSError, ValueError, KeyError) as error:
        parser.exit(1, f"Haddock installation gate failed: {error}\n")


if __name__ == "__main__":
    main()
