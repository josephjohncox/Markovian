#!/usr/bin/env python3
"""Bounded, deterministic helpers for non-publishing release preparation."""

from __future__ import annotations

import argparse
import ctypes
import errno
import hashlib
import io
import json
import os
import platform
import re
import shutil
import stat
import subprocess
import sys
import tarfile
from dataclasses import dataclass
from datetime import date, datetime, timezone
from pathlib import Path, PurePosixPath

MAX_PACKAGES = 16
MAX_COMPONENTS = 128
MAX_ENTRIES = 4096
MAX_COMPRESSED_BYTES = 16 * 1024 * 1024
MAX_UNPACKED_BYTES = 64 * 1024 * 1024
ALLOWED_CABAL_NO_INDEX_ADVISORY = (
    "Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal",
    "update' to download it.",
)
REQUIRED_FIELDS = (
    "synopsis",
    "description",
    "category",
    "homepage",
    "bug-reports",
    "license",
    "license-file",
    "author",
    "maintainer",
    "copyright",
    "build-type",
    "stability",
    "tested-with",
    "extra-doc-files",
)
EXPECTED_PUBLIC_SIBLING_DEPENDENCIES = {
    "markovian": frozenset(),
    "markovian-continuous": frozenset(),
    "markovian-continuous-numerical": frozenset(),
    "markovian-reverse": frozenset(),
    "markovian-tensor": frozenset(),
    "markovian-numerical": frozenset({"markovian"}),
    "markovian-dense-exact": frozenset({"markovian"}),
    "markovian-exact-benchmarks": frozenset({"markovian"}),
    "markovian-autodiff": frozenset({"markovian-reverse"}),
    "markovian-neural": frozenset({"markovian-reverse"}),
    "markovian-tensor-reverse": frozenset({"markovian-reverse", "markovian-tensor"}),
    "markovian-safetensors": frozenset({"markovian-tensor"}),
    "markovian-sampling": frozenset({"markovian", "markovian-numerical"}),
    "markovian-gpu": frozenset({"markovian-tensor"}),
    "markovian-neural-bridge": frozenset({"markovian", "markovian-neural"}),
    "markovian-learning": frozenset(
        {"markovian", "markovian-numerical", "markovian-sampling"}
    ),
}
FORBIDDEN_PARTS = {".git", ".direnv", "dist", "dist-newstyle"}
SECRET_NAMES = re.compile(
    r"(^|/)(id_(rsa|dsa|ecdsa|ed25519)|.*\.(pem|p12|pfx|key)|credentials?|secrets?)(/|$)",
    re.IGNORECASE,
)
MODULE = re.compile(r"^[A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*$")
REVISION = re.compile(r"^[0-9a-f]{40}$")
CALVER = re.compile(
    r"^(20[0-9]{2})\.([1-9]|1[0-2])\.([1-9]|[12][0-9]|3[01])\."
    r"(0|[1-9][0-9]*)$"
)
AT_FDCWD = -100
RENAME_NOREPLACE = 1


class ReleaseError(RuntimeError):
    pass


@dataclass(frozen=True)
class Package:
    name: str
    directory: Path
    version: str
    tier: int

    @property
    def archive_stem(self) -> str:
        return f"{self.name}-{self.version}"


@dataclass(frozen=True)
class ArchiveInfo:
    path: Path
    package: Package
    sha256: str
    compressed_bytes: int
    unpacked_bytes: int
    entries: int
    members: tuple[tarfile.TarInfo, ...]


@dataclass(frozen=True)
class Component:
    kind: str
    package: str
    name: str
    required_flags: tuple[str, ...]

    @property
    def cabal_kind(self) -> str:
        return "test" if self.kind == "test" else "bench"

    @property
    def target(self) -> str:
        return f"{self.package}:{self.cabal_kind}:{self.name}"


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def validate_revision(revision: str) -> str:
    if REVISION.fullmatch(revision) is None:
        raise ReleaseError("revision must be a full lowercase 40-character commit object ID")
    return revision


def validate_calver(version: str, *, path: Path, line: int) -> None:
    match = CALVER.fullmatch(version)
    if match is None:
        raise ReleaseError(
            f"{path}:{line}: version must use UTC CalVer YYYY.M.D.N "
            "with canonical decimal components"
        )
    try:
        date(*(int(component) for component in match.groups()[:3]))
    except ValueError as error:
        raise ReleaseError(f"{path}:{line}: invalid CalVer date {version!r}") from error


def finalize_directory(stage: Path, output: Path) -> None:
    """Atomically rename a staged directory without replacing any destination."""
    if stage.parent.resolve() != output.parent.resolve():
        raise ReleaseError("stage and output must have the same parent")
    libc = ctypes.CDLL(None, use_errno=True)
    renameat2 = getattr(libc, "renameat2", None)
    if renameat2 is None:
        raise ReleaseError("race-safe finalization requires renameat2")
    renameat2.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.c_uint]
    renameat2.restype = ctypes.c_int
    result = renameat2(
        AT_FDCWD,
        os.fsencode(stage),
        AT_FDCWD,
        os.fsencode(output),
        RENAME_NOREPLACE,
    )
    if result == 0:
        return
    error_number = ctypes.get_errno()
    if error_number == errno.EEXIST:
        raise ReleaseError(f"output already exists: {output}")
    if error_number in (errno.ENOSYS, errno.EINVAL, errno.ENOTSUP):
        raise ReleaseError("filesystem does not support race-safe no-replace finalization")
    raise ReleaseError(f"cannot finalize {output}: {os.strerror(error_number)}")


def parse_manifest(path: Path) -> list[Package]:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise ReleaseError(f"cannot read package manifest {path}: {error}") from error

    packages: list[Package] = []
    names: set[str] = set()
    directories: set[Path] = set()
    previous_tier = 0
    for number, raw in enumerate(lines, 1):
        if not raw or raw.startswith("#"):
            continue
        fields = raw.split("\t")
        if len(fields) != 4:
            raise ReleaseError(f"{path}:{number}: expected four tab-separated fields")
        name, directory_text, version, tier_text = fields
        if not re.fullmatch(r"[A-Za-z][A-Za-z0-9-]*", name):
            raise ReleaseError(f"{path}:{number}: invalid package name {name!r}")
        directory = Path(directory_text)
        if directory.is_absolute() or ".." in directory.parts or directory_text == "":
            raise ReleaseError(f"{path}:{number}: unsafe package directory {directory_text!r}")
        validate_calver(version, path=path, line=number)
        if not tier_text.isdecimal():
            raise ReleaseError(f"{path}:{number}: invalid dependency tier {tier_text!r}")
        try:
            tier = int(tier_text)
        except ValueError as error:
            raise ReleaseError(
                f"{path}:{number}: invalid dependency tier {tier_text!r}"
            ) from error
        if tier < previous_tier:
            raise ReleaseError(f"{path}:{number}: dependency tiers are not monotone")
        previous_tier = tier
        folded = name.casefold()
        if folded in names:
            raise ReleaseError(f"{path}:{number}: duplicate package name {name!r}")
        if directory in directories:
            raise ReleaseError(f"{path}:{number}: duplicate package directory {directory}")
        names.add(folded)
        directories.add(directory)
        packages.append(Package(name, directory, version, tier))

    if not 1 <= len(packages) <= MAX_PACKAGES:
        raise ReleaseError(f"package count {len(packages)} is outside 1..{MAX_PACKAGES}")
    versions = {package.version for package in packages}
    if len(versions) != 1:
        raise ReleaseError("all packages must use one coordinated CalVer release")
    return packages


def check_ci_manifest(packages: list[Package], path: Path) -> None:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise ReleaseError(f"cannot read CI package manifest {path}: {error}") from error

    rows: list[tuple[str, Path, int]] = []
    for number, raw in enumerate(lines, 1):
        if not raw or raw.startswith("#"):
            continue
        fields = raw.split("\t")
        if len(fields) != 3:
            raise ReleaseError(f"{path}:{number}: expected three tab-separated fields")
        name, directory_text, tier_text = fields
        if not tier_text.isdecimal():
            raise ReleaseError(f"{path}:{number}: invalid dependency tier {tier_text!r}")
        try:
            tier = int(tier_text)
        except ValueError as error:
            raise ReleaseError(
                f"{path}:{number}: invalid dependency tier {tier_text!r}"
            ) from error
        rows.append((name, Path(directory_text), tier))

    expected = [(package.name, package.directory, package.tier) for package in packages]
    if rows != expected:
        raise ReleaseError(
            "CI and release package manifests differ in name, directory, tier, or order"
        )


def check_public_sibling_dependencies(package: Package, actual: set[str]) -> None:
    expected = EXPECTED_PUBLIC_SIBLING_DEPENDENCIES.get(package.name.casefold())
    if expected is None:
        raise ReleaseError(f"{package.name}: package is outside the reviewed public graph")
    normalized = frozenset(name.casefold() for name in actual)
    if normalized != expected:
        expected_text = ", ".join(sorted(expected)) or "base-only"
        actual_text = ", ".join(sorted(normalized)) or "base-only"
        raise ReleaseError(
            f"{package.name}: public sibling dependencies are {actual_text}; "
            f"reviewed graph requires {expected_text}"
        )


def parse_components(path: Path, packages: list[Package]) -> list[Component]:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise ReleaseError(f"cannot read component manifest {path}: {error}") from error

    package_names = {package.name.casefold(): package.name for package in packages}
    components: list[Component] = []
    seen: set[tuple[str, str, str]] = set()
    for number, raw in enumerate(lines, 1):
        if not raw or raw.startswith("#"):
            continue
        fields = raw.split("\t")
        if len(fields) != 4:
            raise ReleaseError(f"{path}:{number}: expected four tab-separated fields")
        kind, package_text, name, flags_text = fields
        if kind not in ("test", "benchmark"):
            raise ReleaseError(f"{path}:{number}: invalid component kind {kind!r}")
        package = package_names.get(package_text.casefold())
        if package is None:
            raise ReleaseError(f"{path}:{number}: unknown package {package_text!r}")
        if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]*", name):
            raise ReleaseError(f"{path}:{number}: invalid component name {name!r}")
        if flags_text == "-":
            flags: tuple[str, ...] = ()
        else:
            raw_flags = flags_text.split(",")
            if any(re.fullmatch(r"\+[A-Za-z][A-Za-z0-9-]*", flag) is None for flag in raw_flags):
                raise ReleaseError(f"{path}:{number}: invalid required flags {flags_text!r}")
            if len(raw_flags) != len(set(raw_flags)):
                raise ReleaseError(f"{path}:{number}: duplicate required flag")
            flags = tuple(raw_flags)
        key = (kind, package.casefold(), name)
        if key in seen:
            raise ReleaseError(f"{path}:{number}: duplicate component {package}:{kind}:{name}")
        seen.add(key)
        components.append(Component(kind, package, name, flags))
        if len(components) > MAX_COMPONENTS:
            raise ReleaseError(f"component count exceeds {MAX_COMPONENTS}")

    if not components:
        raise ReleaseError("component manifest is empty")
    return components


def component_sections(text: str) -> dict[tuple[str, str], str]:
    lines = text.splitlines()
    sections: dict[tuple[str, str], str] = {}
    for index, line in enumerate(lines):
        match = re.fullmatch(r"(test-suite|benchmark)\s+(\S+)", line)
        if match is None:
            continue
        end = next(
            (
                position
                for position in range(index + 1, len(lines))
                if lines[position] and not lines[position][0].isspace()
            ),
            len(lines),
        )
        key = ("test" if match.group(1) == "test-suite" else "benchmark", match.group(2))
        if key in sections:
            raise ReleaseError(f"duplicate Cabal component declaration {key[0]}:{key[1]}")
        sections[key] = "\n".join(lines[index:end])
    return sections


def check_components(root: Path, packages: list[Package], components: list[Component]) -> None:
    expected_by_package: dict[str, dict[tuple[str, str], Component]] = {
        package.name.casefold(): {} for package in packages
    }
    for component in components:
        expected_by_package[component.package.casefold()][
            (component.kind, component.name)
        ] = component

    for package in packages:
        cabal_path = one_cabal_file(root / package.directory)
        text = cabal_path.read_text(encoding="utf-8")
        declared = component_sections(text)
        expected = expected_by_package[package.name.casefold()]
        missing = sorted(set(expected) - set(declared))
        extra = sorted(set(declared) - set(expected))
        if missing:
            raise ReleaseError(f"{cabal_path}: unmanifested declaration mismatch; missing {missing}")
        if extra:
            raise ReleaseError(f"{cabal_path}: unmanifested components {extra}")
        declared_flags = set(re.findall(r"(?m)^flag\s+([A-Za-z][A-Za-z0-9-]*)\s*$", text))
        for key, component in expected.items():
            section = declared[key]
            for flag in component.required_flags:
                bare = flag[1:]
                if bare not in declared_flags:
                    raise ReleaseError(f"{cabal_path}: required flag {flag} is not declared")
                if re.search(rf"\bflag\s*\(\s*{re.escape(bare)}\s*\)", section) is None:
                    raise ReleaseError(
                        f"{cabal_path}: component {component.name} does not use required flag {flag}"
                    )


def archive_project_text(components: list[Component]) -> str:
    package_flags: dict[str, set[str]] = {}
    package_spelling: dict[str, str] = {}
    for component in components:
        folded = component.package.casefold()
        package_spelling[folded] = component.package
        package_flags.setdefault(folded, set()).update(component.required_flags)

    lines = [
        "packages: *",
        "write-ghc-environment-files: never",
        "active-repositories: :none",
        "",
        "package *",
        "  ghc-options: -Werror",
        "  tests: True",
        "  benchmarks: True",
    ]
    for folded in sorted(package_flags):
        flags = sorted(package_flags[folded])
        if not flags:
            continue
        lines.extend(
            [
                "",
                f"package {package_spelling[folded]}",
                f"  flags: {' '.join(flags)}",
            ]
        )
    return "\n".join(lines) + "\n"


def package_dependency_closure(
    root: Path, packages: list[Package], selected_name: str
) -> list[Package]:
    by_name = {package.name.casefold(): package for package in packages}
    selected = by_name.get(selected_name.casefold())
    if selected is None:
        raise ReleaseError(f"unknown package for archive consumer: {selected_name}")

    graph: dict[str, set[str]] = {}
    for package in packages:
        text = one_cabal_file(root / package.directory).read_text(encoding="utf-8")
        dependencies: set[str] = set()
        for sibling_name in by_name:
            if sibling_name == package.name.casefold():
                continue
            spelling = by_name[sibling_name].name
            if re.search(
                rf"(?mi)^\s*,\s*{re.escape(spelling)}(?![A-Za-z0-9-])", text
            ):
                dependencies.add(sibling_name)
        graph[package.name.casefold()] = dependencies

    closure: set[str] = set()
    pending = [selected.name.casefold()]
    while pending:
        name = pending.pop()
        if name in closure:
            continue
        closure.add(name)
        pending.extend(sorted(graph[name] - closure, reverse=True))
    return [package for package in packages if package.name.casefold() in closure]


def archive_consumer_project_text(
    source_root: Path,
    archive_root: Path,
    packages: list[Package],
    components: list[Component],
    selected_name: str,
) -> str:
    closure = package_dependency_closure(source_root, packages, selected_name)
    selected = next(
        package
        for package in packages
        if package.name.casefold() == selected_name.casefold()
    )
    paths = [archive_root.resolve() / package.archive_stem for package in closure]
    missing = [path for path in paths if not path.is_dir()]
    if missing:
        raise ReleaseError(f"archive consumer package directory is missing: {missing[0]}")

    flags = sorted(
        {
            flag
            for component in components
            if component.package.casefold() == selected.name.casefold()
            for flag in component.required_flags
        }
    )
    lines = [
        "packages:",
        *(f"  {path}" for path in paths),
        "write-ghc-environment-files: never",
        "active-repositories: :none",
        "",
        "package *",
        "  ghc-options: -Werror",
        "  tests: False",
        "  benchmarks: False",
        "",
        f"package {selected.name}",
        "  tests: True",
        "  benchmarks: True",
    ]
    if flags:
        lines.append(f"  flags: {' '.join(flags)}")
    return "\n".join(lines) + "\n"


def planned_components(plan: object) -> set[tuple[str, str]]:
    if not isinstance(plan, dict) or not isinstance(plan.get("install-plan"), list):
        raise ReleaseError("Cabal plan has no install-plan array")
    found: set[tuple[str, str]] = set()
    for unit in plan["install-plan"]:
        if not isinstance(unit, dict):
            continue
        package = unit.get("pkg-name")
        if not isinstance(package, str):
            continue
        component_name = unit.get("component-name")
        if isinstance(component_name, str):
            found.add((package.casefold(), component_name))
        unit_components = unit.get("components")
        if isinstance(unit_components, dict):
            found.update(
                (package.casefold(), name)
                for name in unit_components
                if isinstance(name, str)
            )
    return found


def check_test_integration_edges(
    root: Path, packages: list[Package], components: list[Component], manifest: Path
) -> None:
    package_by_name = {package.name.casefold(): package for package in packages}
    declared: list[tuple[str, str, str]] = []
    seen: set[tuple[str, str, str]] = set()
    for number, raw in enumerate(manifest.read_text(encoding="utf-8").splitlines(), 1):
        if not raw or raw.startswith("#"):
            continue
        fields = raw.split("\t")
        if len(fields) != 3:
            raise ReleaseError(f"{manifest}:{number}: expected three fields")
        package_name, component_name, dependency_name = fields
        key = (package_name.casefold(), component_name, dependency_name.casefold())
        if key in seen:
            raise ReleaseError(f"{manifest}:{number}: duplicate test integration edge")
        if key[0] not in package_by_name or key[2] not in package_by_name or key[0] == key[2]:
            raise ReleaseError(f"{manifest}:{number}: invalid test integration edge")
        seen.add(key)
        declared.append(key)

    actual: list[tuple[str, str, str]] = []
    for package in packages:
        cabal_path = one_cabal_file(root / package.directory)
        text = cabal_path.read_text(encoding="utf-8")
        sections = component_sections(text)
        public_dependencies = EXPECTED_PUBLIC_SIBLING_DEPENDENCIES[package.name.casefold()]
        for component in components:
            if component.kind != "test" or component.package.casefold() != package.name.casefold():
                continue
            section = sections.get(("test", component.name), "")
            for dependency in packages:
                folded = dependency.name.casefold()
                if folded == package.name.casefold() or folded in public_dependencies:
                    continue
                if re.search(
                    rf"(?mi)^\s*,?\s*{re.escape(dependency.name)}(?:\s|$)", section
                ):
                    actual.append((package.name.casefold(), component.name, folded))
    if declared != actual:
        raise ReleaseError(
            f"test-only integration edges differ: declared={declared}, actual={actual}"
        )


def check_component_plan(path: Path, components: list[Component]) -> None:
    try:
        plan = json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise ReleaseError(f"cannot read Cabal plan {path}: {error}") from error
    expected = {
        (component.package.casefold(), f"{component.cabal_kind}:{component.name}")
        for component in components
    }
    missing = sorted(expected - planned_components(plan))
    if missing:
        raise ReleaseError(f"Cabal plan omits mandatory components: {missing}")


def component_log_path(root: Path, raw: str) -> Path:
    relative = Path(raw)
    if (
        relative.is_absolute()
        or ".." in relative.parts
        or len(relative.parts) != 2
        or relative.parts[0] != "component-logs"
        or relative.suffix != ".log"
    ):
        raise ReleaseError(f"invalid component execution log path {raw!r}")
    return root / relative


def component_results(
    components: list[Component], manifest_path: Path, receipt_path: Path
) -> dict[str, object]:
    """Bind each zero-exit receipt to the log produced by that invocation."""
    expected = {component.target: component for component in components}
    receipts: dict[str, dict[str, object]] = {}
    for number, raw in enumerate(receipt_path.read_text(encoding="utf-8").splitlines(), 1):
        if not raw or raw.startswith("#"):
            continue
        fields = raw.split("\t")
        if len(fields) != 6:
            raise ReleaseError(f"{receipt_path}:{number}: expected six fields")
        kind, target, compiler, result, log_name, log_digest = fields
        component = expected.get(target)
        if component is None or component.kind != kind:
            raise ReleaseError(f"{receipt_path}:{number}: unmanifested component receipt {target}")
        if target in receipts:
            raise ReleaseError(f"{receipt_path}:{number}: duplicate component receipt {target}")
        if not compiler or result != "passed" or re.fullmatch(r"[0-9a-f]{64}", log_digest) is None:
            raise ReleaseError(f"{receipt_path}:{number}: invalid execution receipt for {target}")
        log_path = component_log_path(receipt_path.parent, log_name)
        if not log_path.is_file():
            raise ReleaseError(f"{receipt_path}:{number}: missing execution log {log_name}")
        if sha256_file(log_path) != log_digest:
            raise ReleaseError(f"{receipt_path}:{number}: execution log digest mismatch for {target}")
        receipts[target] = {
            "target": target,
            "compiler": compiler,
            "result": result,
            "log": log_name,
            "logBytes": log_path.stat().st_size,
            "logSha256": log_digest,
        }
    missing = [component.target for component in components if component.target not in receipts]
    if missing:
        raise ReleaseError(f"component execution receipts are missing: {missing}")
    return {
        "schemaVersion": 3,
        "componentManifestSha256": sha256_file(manifest_path),
        "testSuites": [receipts[item.target] for item in components if item.kind == "test"],
        "benchmarks": [receipts[item.target] for item in components if item.kind == "benchmark"],
    }


def validate_component_results(
    value: object,
    components: list[Component],
    manifest_path: Path,
    evidence_root: Path | None = None,
) -> None:
    if not isinstance(value, dict) or value.get("schemaVersion") != 3:
        raise ReleaseError("invalid component result report schema")
    if value.get("componentManifestSha256") != sha256_file(manifest_path):
        raise ReleaseError("component result report does not bind release/components.tsv")
    root = manifest_path.parent if evidence_root is None else evidence_root
    for field, kind in (("testSuites", "test"), ("benchmarks", "benchmark")):
        expected = [component.target for component in components if component.kind == kind]
        rows = value.get(field)
        if not isinstance(rows, list) or any(not isinstance(row, dict) for row in rows):
            raise ReleaseError(f"invalid component result field {field}")
        actual = [row.get("target") for row in rows]
        invalid_receipt = any(
            row.get("result") != "passed"
            or not isinstance(row.get("compiler"), str)
            or not row.get("compiler")
            or not isinstance(row.get("log"), str)
            or not isinstance(row.get("logBytes"), int)
            or row.get("logBytes", -1) < 0
            or re.fullmatch(r"[0-9a-f]{64}", str(row.get("logSha256"))) is None
            for row in rows
        )
        if actual != expected or invalid_receipt:
            raise ReleaseError(f"component result field {field} does not match the manifest")
        for row in rows:
            log_path = component_log_path(root, str(row["log"]))
            if (
                not log_path.is_file()
                or log_path.stat().st_size != row["logBytes"]
                or sha256_file(log_path) != row["logSha256"]
            ):
                raise ReleaseError(f"component execution log changed for {row['target']}")


def validate_checkout_state(requested: str, resolved: str, head: str, status: str) -> None:
    validate_revision(requested)
    if resolved != requested:
        raise ReleaseError(f"requested revision {requested} does not resolve to that commit")
    if head != requested:
        raise ReleaseError(f"requested revision {requested} is not HEAD {head}")
    if status:
        raise ReleaseError("release preparation requires a clean worktree")


def git_output(root: Path, arguments: list[str]) -> str:
    result = subprocess.run(
        ["git", "-C", os.fspath(root), *arguments],
        check=True,
        text=True,
        capture_output=True,
    )
    return result.stdout.strip()


def check_source_checkout(root: Path, revision: str) -> None:
    validate_revision(revision)
    top = Path(git_output(root, ["rev-parse", "--show-toplevel"])).resolve()
    if top != root.resolve():
        raise ReleaseError(f"release source root is {top}, not {root.resolve()}")
    resolved = git_output(root, ["rev-parse", "--verify", f"{revision}^{{commit}}"])
    head = git_output(root, ["rev-parse", "--verify", "HEAD"])
    status = git_output(root, ["status", "--porcelain=v1", "--untracked-files=normal"])
    validate_checkout_state(revision, resolved, head, status)


def check_haddock_interfaces(store: Path, packages: list[Package]) -> None:
    try:
        interfaces = [path for path in store.rglob("*.haddock") if path.is_file()]
    except OSError as error:
        raise ReleaseError(f"cannot inspect Haddock store {store}: {error}") from error
    for package in packages:
        expected = f"{package.name}.haddock".casefold()
        matches = [path for path in interfaces if path.name.casefold() == expected]
        if len(matches) != 1:
            raise ReleaseError(
                f"expected one Haddock interface for {package.name}, found {len(matches)}"
            )


def check_haddock_log(path: Path) -> str:
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as error:
        raise ReleaseError(f"cannot read Haddock log {path}: {error}") from error

    lines = text.splitlines()
    kept: list[str] = []
    unexpected: list[tuple[int, str]] = []
    index = 0
    while index < len(lines):
        if lines[index] == ALLOWED_CABAL_NO_INDEX_ADVISORY[0]:
            if (
                index + 1 < len(lines)
                and lines[index + 1] == ALLOWED_CABAL_NO_INDEX_ADVISORY[1]
            ):
                index += 2
                continue
            unexpected.append((index + 1, lines[index]))
        elif re.search(r"(^|\s)warning:", lines[index], re.IGNORECASE):
            unexpected.append((index + 1, lines[index]))
        kept.append(lines[index])
        index += 1

    if unexpected:
        details = "; ".join(f"{number}:{line}" for number, line in unexpected)
        raise ReleaseError(f"unexpected build or Haddock warnings in {path}: {details}")
    return "\n".join(kept) + ("\n" if kept and text.endswith("\n") else "")


def cabal_field(text: str, field: str) -> str | None:
    match = re.search(rf"(?mi)^{re.escape(field)}:\s*(.*?)\s*$", text)
    return match.group(1) if match else None


def cabal_section_fields(text: str, heading: str) -> dict[str, str]:
    matches = re.findall(
        rf"(?m)^{re.escape(heading)}[ \t]*\n"
        rf"((?:[ \t]+[^\n]*(?:\n|$))*)",
        text,
    )
    if len(matches) != 1:
        raise ReleaseError(f"expected exactly one {heading!r} section")
    fields: dict[str, str] = {}
    for raw in matches[0].splitlines():
        match = re.fullmatch(r"\s+([a-z-]+):\s*(.*?)\s*", raw)
        if match is None or not match.group(2):
            raise ReleaseError(f"invalid field in {heading!r} section: {raw!r}")
        name, value = match.groups()
        if name in fields:
            raise ReleaseError(f"duplicate {name!r} field in {heading!r} section")
        fields[name] = value
    return fields


def one_cabal_file(directory: Path) -> Path:
    files = sorted(directory.glob("*.cabal"))
    if len(files) != 1:
        raise ReleaseError(f"expected one Cabal file in {directory}, found {len(files)}")
    return files[0]


def library_section(text: str) -> str:
    lines = text.splitlines()
    start = next((index for index, line in enumerate(lines) if line == "library"), None)
    if start is None:
        raise ReleaseError("Cabal file has no library section")
    end = next(
        (index for index in range(start + 1, len(lines)) if lines[index] and not lines[index][0].isspace()),
        len(lines),
    )
    return "\n".join(lines[start:end])


def check_dependency_bounds(cabal_path: Path, text: str, package: Package) -> None:
    dependencies: list[tuple[int, str]] = []
    collecting = False
    for number, line in enumerate(text.splitlines(), 1):
        stripped = line.strip()
        if stripped.startswith("build-depends:"):
            collecting = True
            remainder = stripped.split(":", 1)[1].strip()
            if remainder:
                dependencies.append((number, remainder))
            continue
        if collecting and stripped.startswith(","):
            dependencies.append((number, stripped[1:].strip()))
            continue
        if collecting and not stripped:
            continue
        collecting = False

    if not dependencies:
        raise ReleaseError(f"{cabal_path}: no component dependencies found")
    for number, dependency in dependencies:
        name = dependency.split()[0]
        internal_sublibrary = name.casefold().startswith(package.name.casefold() + ":")
        if re.search(r"(?:\^>=|==|>=|<=|<|>)", dependency) is None and not internal_sublibrary:
            raise ReleaseError(f"{cabal_path}:{number}: unbounded component dependency {dependency!r}")
        if name.casefold() == package.name.casefold() and not re.search(
            rf"^\S+\s+==\s*{re.escape(package.version)}$", dependency
        ):
            raise ReleaseError(
                f"{cabal_path}:{number}: internal component dependency must use =={package.version}"
            )


def exposed_modules(cabal_path: Path) -> list[str]:
    lines = cabal_path.read_text(encoding="utf-8").splitlines()
    in_library = False
    collecting = False
    field_indent = 0
    modules: list[str] = []
    for line in lines:
        if line == "library":
            in_library = True
            collecting = False
            continue
        if in_library and line and not line[0].isspace():
            break
        if not in_library:
            continue
        stripped = line.strip()
        if stripped.startswith("exposed-modules:"):
            collecting = True
            field_indent = len(line) - len(line.lstrip())
            remainder = stripped.split(":", 1)[1].strip()
            if remainder:
                modules.extend(remainder.split())
            continue
        if collecting:
            if not stripped:
                continue
            indent = len(line) - len(line.lstrip())
            if indent <= field_indent or ":" in stripped.split()[0]:
                collecting = False
                continue
            modules.extend(stripped.split())
    if not modules:
        raise ReleaseError(f"no exposed modules found in {cabal_path}")
    invalid = [module for module in modules if not MODULE.fullmatch(module)]
    if invalid:
        raise ReleaseError(f"invalid exposed module names in {cabal_path}: {invalid}")
    if len(modules) != len(set(modules)):
        raise ReleaseError(f"duplicate exposed module in {cabal_path}")
    return modules


def check_proposed_decision_statuses(text: str) -> None:
    for number in range(61, 77):
        decision = f"D-{number:03d}"
        match = re.search(
            rf"(?ms)^### {re.escape(decision)}:.*?^\*\*Status:\*\* ([^\n]+)$",
            text,
        )
        if match is None:
            raise ReleaseError(f"missing decision status for {decision}")
        if match.group(1) != "Proposed":
            raise ReleaseError(
                f"{decision} must remain Proposed until every acceptance gate passes"
            )


def check_metadata(
    root: Path,
    manifest: Path,
    golden_dir: Path,
    ci_manifest: Path | None = None,
    component_manifest: Path | None = None,
) -> list[Package]:
    packages = parse_manifest(manifest)
    if ci_manifest is not None:
        check_ci_manifest(packages, ci_manifest)
    known_versions = {package.name.casefold(): package.version for package in packages}
    known_packages = {package.name.casefold(): package for package in packages}
    manifest_names = frozenset(known_packages)
    if manifest_names != frozenset(EXPECTED_PUBLIC_SIBLING_DEPENDENCIES):
        raise ReleaseError(
            "release package manifest does not match the reviewed 16-package graph"
        )
    project_text = (root / "cabal.project").read_text(encoding="utf-8")
    check_proposed_decision_statuses(
        (root / "docs" / "DECISIONS.md").read_text(encoding="utf-8")
    )
    all_modules: dict[str, str] = {}

    for package in packages:
        directory = root / package.directory
        cabal_path = one_cabal_file(directory)
        text = cabal_path.read_text(encoding="utf-8")
        library_text = library_section(text)
        check_dependency_bounds(cabal_path, text, package)
        if cabal_field(text, "name") != package.name:
            raise ReleaseError(f"manifest/Cabal package name mismatch for {package.name}")
        if cabal_field(text, "version") != package.version:
            raise ReleaseError(f"manifest/Cabal version mismatch for {package.name}")
        if package.directory != Path(".") and not re.search(
            rf"(?m)^\s+{re.escape(package.directory.as_posix())}\s*$", project_text
        ):
            raise ReleaseError(f"{package.name}: package is not registered in cabal.project")
        for field in REQUIRED_FIELDS:
            if cabal_field(text, field) is None:
                raise ReleaseError(f"{cabal_path}: missing required field {field}")
        if cabal_field(text, "license") != "BSD-3-Clause":
            raise ReleaseError(f"{cabal_path}: release profile requires BSD-3-Clause")
        if cabal_field(text, "copyright") != "2024-2026 Joseph Cox":
            raise ReleaseError(f"{cabal_path}: inconsistent copyright range")
        for filename in ("README.md", "CHANGELOG.md", "LICENSE"):
            if not (directory / filename).is_file():
                raise ReleaseError(f"{package.name}: missing {filename}")
        if "source-repository head" not in text or "https://github.com/josephjohncox/Markovian.git" not in text:
            raise ReleaseError(f"{cabal_path}: missing canonical source-repository head")
        if package.directory != Path(".") and f"subdir:   {package.directory.as_posix()}" not in text:
            raise ReleaseError(f"{cabal_path}: missing source-repository subdir")
        expected_repository = {
            "type": "git",
            "location": "https://github.com/josephjohncox/Markovian.git",
            "tag": f"v{package.version}",
        }
        if package.directory != Path("."):
            expected_repository["subdir"] = package.directory.as_posix()
        try:
            release_repository = cabal_section_fields(text, "source-repository this")
        except ReleaseError as error:
            raise ReleaseError(f"{cabal_path}: {error}") from error
        if release_repository != expected_repository:
            raise ReleaseError(
                f"{cabal_path}: source-repository this must identify "
                f"v{package.version} and the package subdirectory"
            )
        if not re.search(r"base\s+>=\s*4\.17\.2\.1\s*&&\s*<\s*4\.20", text):
            raise ReleaseError(f"{cabal_path}: missing evidence-backed full base bounds")

        public_sibling_dependencies: set[str] = set()
        for sibling_folded, version in known_versions.items():
            if sibling_folded == package.name.casefold():
                continue
            sibling_package = known_packages[sibling_folded]
            sibling = sibling_package.name
            has_dependency = re.search(
                rf"(?mi)^\s*,\s*{re.escape(sibling)}(?![A-Za-z0-9-])", library_text
            )
            if has_dependency:
                public_sibling_dependencies.add(sibling_folded)
            if has_dependency and not re.search(
                rf"(?mi)^\s*,\s*{re.escape(sibling)}\s+\^>=\s*{re.escape(version)}\s*$", library_text
            ):
                raise ReleaseError(f"{cabal_path}: {sibling} must use ^>={version}")
            if has_dependency and sibling_package.tier >= package.tier:
                raise ReleaseError(
                    f"{cabal_path}: dependency tier {package.tier} does not follow "
                    f"{sibling} tier {sibling_package.tier}"
                )
        try:
            check_public_sibling_dependencies(package, public_sibling_dependencies)
        except ReleaseError as error:
            raise ReleaseError(f"{cabal_path}: {error}") from error

        modules = exposed_modules(cabal_path)
        golden = golden_dir / f"{package.name}.txt"
        try:
            expected = [line for line in golden.read_text(encoding="utf-8").splitlines() if line]
        except OSError as error:
            raise ReleaseError(f"missing exposed-module golden {golden}: {error}") from error
        if modules != expected:
            raise ReleaseError(f"exposed modules changed for {package.name}; review {golden}")
        for module in modules:
            owner = all_modules.get(module)
            if owner is not None:
                raise ReleaseError(f"exposed module {module} occurs in both {owner} and {package.name}")
            all_modules[module] = package.name

    if component_manifest is not None:
        check_components(root, packages, parse_components(component_manifest, packages))
    return packages


def safe_member_name(name: str) -> PurePosixPath:
    if "\x00" in name or "\\" in name:
        raise ReleaseError(f"unsafe archive member path {name!r}")
    path = PurePosixPath(name)
    if path.is_absolute() or not path.parts or any(part in ("", ".", "..") for part in path.parts):
        raise ReleaseError(f"unsafe archive member path {name!r}")
    if path.as_posix() != name.rstrip("/"):
        raise ReleaseError(f"non-canonical archive member path {name!r}")
    if any(part in FORBIDDEN_PARTS for part in path.parts):
        raise ReleaseError(f"forbidden archive member path {name!r}")
    if SECRET_NAMES.search(path.as_posix()):
        raise ReleaseError(f"credential-like archive member path {name!r}")
    return path


def validate_archive(
    archive: Path,
    package: Package,
    *,
    max_entries: int = MAX_ENTRIES,
    max_compressed_bytes: int = MAX_COMPRESSED_BYTES,
    max_unpacked_bytes: int = MAX_UNPACKED_BYTES,
) -> ArchiveInfo:
    expected_name = f"{package.archive_stem}.tar.gz"
    if archive.name != expected_name:
        raise ReleaseError(f"archive name {archive.name!r} must be {expected_name!r}")
    compressed = archive.stat().st_size
    if compressed > max_compressed_bytes:
        raise ReleaseError(f"archive {archive} exceeds compressed-byte budget")

    seen: set[str] = set()
    members: list[tarfile.TarInfo] = []
    unpacked = 0
    required = {
        f"{package.archive_stem}/{package.name}.cabal",
        f"{package.archive_stem}/LICENSE",
        f"{package.archive_stem}/README.md",
        f"{package.archive_stem}/CHANGELOG.md",
    }
    try:
        with tarfile.open(archive, mode="r:gz") as tar:
            for member in tar:
                canonical = safe_member_name(member.name).as_posix()
                if canonical in seen:
                    raise ReleaseError(f"duplicate archive member {canonical}")
                seen.add(canonical)
                if len(seen) > max_entries:
                    raise ReleaseError(f"archive {archive} exceeds entry-count budget")
                if canonical.split("/", 1)[0] != package.archive_stem:
                    raise ReleaseError(f"archive member is outside {package.archive_stem}: {canonical}")
                if not (member.isdir() or member.isfile()):
                    raise ReleaseError(f"archive member is not a regular file or directory: {canonical}")
                if member.mode & (stat.S_ISUID | stat.S_ISGID | stat.S_ISVTX | stat.S_IWOTH):
                    raise ReleaseError(f"archive member has unsafe mode {oct(member.mode)}: {canonical}")
                if member.size < 0:
                    raise ReleaseError(f"archive member has negative size: {canonical}")
                if member.isfile():
                    unpacked += member.size
                    if unpacked > max_unpacked_bytes:
                        raise ReleaseError(f"archive {archive} exceeds unpacked-byte budget")
                members.append(member)
    except (OSError, tarfile.TarError) as error:
        raise ReleaseError(f"cannot read archive {archive}: {error}") from error

    missing = sorted(required - seen)
    if missing:
        raise ReleaseError(f"archive {archive} is missing required files: {', '.join(missing)}")
    return ArchiveInfo(
        archive,
        package,
        sha256_file(archive),
        compressed,
        unpacked,
        len(members),
        tuple(members),
    )


def extract_archive(info: ArchiveInfo, destination: Path) -> Path:
    destination.mkdir(parents=True, exist_ok=True)
    package_root = destination / info.package.archive_stem
    if package_root.exists():
        raise ReleaseError(f"archive package root already exists: {package_root}")
    root = destination.resolve()
    archive_bytes = info.path.read_bytes()
    if sha256_bytes(archive_bytes) != info.sha256:
        raise ReleaseError(f"archive changed after validation: {info.path}")
    with tarfile.open(fileobj=io.BytesIO(archive_bytes), mode="r:gz") as tar:
        by_name = {member.name.rstrip("/"): member for member in tar.getmembers()}
        for name in sorted(by_name):
            member = by_name[name]
            relative = safe_member_name(member.name)
            target = (destination / Path(*relative.parts)).resolve()
            if root != target and root not in target.parents:
                raise ReleaseError(f"archive extraction escaped destination: {member.name}")
            if member.isdir():
                target.mkdir(parents=True, exist_ok=True)
                target.chmod(member.mode & 0o755)
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            source = tar.extractfile(member)
            if source is None:
                raise ReleaseError(f"cannot read regular archive member {member.name}")
            try:
                with target.open("xb") as output:
                    shutil.copyfileobj(source, output, length=1024 * 1024)
            finally:
                source.close()
            target.chmod(member.mode & 0o755)
    return destination / info.package.archive_stem


def spdx_id(kind: str, name: str) -> str:
    safe = re.sub(r"[^A-Za-z0-9.-]", "-", name).strip("-")
    return f"SPDXRef-{kind}-{safe or 'item'}"


def iso_epoch(epoch: int) -> str:
    return datetime.fromtimestamp(epoch, timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def generate_sbom(info: ArchiveInfo, revision: str, epoch: int) -> dict[str, object]:
    validate_revision(revision)
    package_id = spdx_id("Package", info.package.name)
    files: list[dict[str, object]] = []
    relationships: list[dict[str, str]] = []
    mathjax_files: list[str] = []
    mathjax_verification_hashes: list[str] = []
    verification_hashes: list[str] = []
    with tarfile.open(info.path, mode="r:gz") as tar:
        for member in sorted((item for item in tar.getmembers() if item.isfile()), key=lambda item: item.name):
            handle = tar.extractfile(member)
            if handle is None:
                raise ReleaseError(f"cannot read archive member for SBOM: {member.name}")
            # pi-lens-ignore: python-weak-hash
            sha1 = hashlib.sha1(usedforsecurity=False)
            sha256 = hashlib.sha256()
            with handle:
                while True:
                    chunk = handle.read(1024 * 1024)
                    if not chunk:
                        break
                    sha1.update(chunk)
                    sha256.update(chunk)
            verification_hashes.append(sha1.hexdigest())
            relative = member.name.split("/", 1)[1]
            file_id = spdx_id("File", hashlib.sha256(relative.encode("utf-8")).hexdigest()[:24])
            is_mathjax = relative.startswith("docs/book/theme/vendor/mathjax/")
            license_info = (
                ["Apache-2.0"]
                if is_mathjax
                else (["BSD-3-Clause"] if relative == "LICENSE" else ["NOASSERTION"])
            )
            files.append(
                {
                    "SPDXID": file_id,
                    "fileName": f"./{relative}",
                    "checksums": [
                        {"algorithm": "SHA1", "checksumValue": sha1.hexdigest()},
                        {"algorithm": "SHA256", "checksumValue": sha256.hexdigest()},
                    ],
                    "licenseConcluded": "NOASSERTION",
                    "licenseInfoInFiles": license_info,
                    "copyrightText": "NOASSERTION",
                }
            )
            relationships.append(
                {"spdxElementId": package_id, "relationshipType": "CONTAINS", "relatedSpdxElement": file_id}
            )
            if is_mathjax:
                mathjax_files.append(file_id)
                mathjax_verification_hashes.append(sha1.hexdigest())

    packages: list[dict[str, object]] = [
        {
            "SPDXID": package_id,
            "name": info.package.name,
            "versionInfo": info.package.version,
            "downloadLocation": "NOASSERTION",
            "filesAnalyzed": True,
            "packageVerificationCode": {
                # pi-lens-ignore: python-weak-hash
                "packageVerificationCodeValue": hashlib.sha1(
                    "".join(sorted(verification_hashes)).encode("ascii"),
                    usedforsecurity=False,
                ).hexdigest()
            },
            "checksums": [{"algorithm": "SHA256", "checksumValue": info.sha256}],
            "licenseConcluded": "BSD-3-Clause",
            "licenseDeclared": "BSD-3-Clause",
            "copyrightText": "Copyright 2024-2026 Joseph Cox",
            "sourceInfo": f"Git revision {revision}; generated from validated Cabal sdist",
        }
    ]
    if mathjax_files:
        mathjax_id = "SPDXRef-Package-MathJax"
        packages.append(
            {
                "SPDXID": mathjax_id,
                "name": "MathJax",
                "versionInfo": "3.2.2",
                "downloadLocation": "https://github.com/mathjax/MathJax-src",
                "filesAnalyzed": True,
                "packageVerificationCode": {
                    # pi-lens-ignore: python-weak-hash
                    "packageVerificationCodeValue": hashlib.sha1(
                        "".join(sorted(mathjax_verification_hashes)).encode("ascii"),
                        usedforsecurity=False,
                    ).hexdigest()
                },
                "licenseConcluded": "Apache-2.0",
                "licenseDeclared": "Apache-2.0",
                "copyrightText": "Copyright (c) 2009-2021 The MathJax Consortium",
            }
        )
        relationships.append(
            {"spdxElementId": package_id, "relationshipType": "CONTAINS", "relatedSpdxElement": mathjax_id}
        )
        relationships.extend(
            {"spdxElementId": mathjax_id, "relationshipType": "CONTAINS", "relatedSpdxElement": file_id}
            for file_id in mathjax_files
        )

    return {
        "spdxVersion": "SPDX-2.3",
        "dataLicense": "CC0-1.0",
        "SPDXID": "SPDXRef-DOCUMENT",
        "name": f"{info.package.archive_stem}-source",
        "documentNamespace": f"https://github.com/josephjohncox/Markovian/spdx/{info.package.archive_stem}/{info.sha256}",
        "creationInfo": {"created": iso_epoch(epoch), "creators": ["Tool: Markovian-release-tool-0.1"]},
        "documentDescribes": [package_id],
        "packages": packages,
        "files": files,
        "relationships": relationships,
    }


def write_json(path: Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    encoded = json.dumps(value, sort_keys=True, indent=2, ensure_ascii=True) + "\n"
    path.write_text(encoded, encoding="utf-8")


def tool_version(command: list[str]) -> str:
    result = subprocess.run(command, check=True, text=True, capture_output=True)
    return result.stdout.strip()


def archive_semantic_evidence(info: ArchiveInfo) -> list[dict[str, object]]:
    evidence: list[dict[str, object]] = []
    with tarfile.open(info.path, mode="r:gz") as tar:
        for member in sorted(tar.getmembers(), key=lambda item: item.name):
            marker = "/test/golden/"
            if not member.isfile() or marker not in member.name:
                continue
            handle = tar.extractfile(member)
            if handle is None:
                raise ReleaseError(f"cannot read semantic evidence {member.name}")
            digest = hashlib.sha256()
            size = 0
            with handle:
                while True:
                    chunk = handle.read(1024 * 1024)
                    if not chunk:
                        break
                    digest.update(chunk)
                    size += len(chunk)
            evidence.append(
                {
                    "path": member.name.split("/", 1)[1],
                    "bytes": size,
                    "sha256": digest.hexdigest(),
                }
            )
    return evidence


def generate_artifact_manifest(
    packages: list[Package],
    archive_dir: Path,
    sbom_dir: Path,
    revision: str,
    epoch: int,
    component_manifest: Path | None = None,
    component_result_path: Path | None = None,
) -> dict[str, object]:
    validate_revision(revision)
    artifacts = []
    for package in packages:
        archive = archive_dir / f"{package.archive_stem}.tar.gz"
        sbom = sbom_dir / f"{package.archive_stem}.spdx.json"
        info = validate_archive(archive, package)
        if not sbom.is_file():
            raise ReleaseError(f"missing SBOM for {package.name}: {sbom}")
        try:
            document = json.loads(sbom.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise ReleaseError(f"cannot read SBOM for {package.name}: {error}") from error
        expected_document = generate_sbom(info, revision, epoch)
        if document != expected_document:
            raise ReleaseError(
                f"SBOM semantic mismatch for {package.name}: package identity, "
                "documentDescribes, archive checksum, source revision, file inventory, "
                "verification codes, and licenses must match the archive"
            )
        artifacts.append(
            {
                "name": package.name,
                "version": package.version,
                "dependencyTier": package.tier,
                "archive": archive.name,
                "bytes": info.compressed_bytes,
                "sha256": info.sha256,
                "entries": info.entries,
                "unpackedBytes": info.unpacked_bytes,
                "sbom": sbom.name,
                "sbomSha256": sha256_file(sbom),
                "semanticEvidence": archive_semantic_evidence(info),
            }
        )

    component_evidence: dict[str, object] | None = None
    if (component_manifest is None) != (component_result_path is None):
        raise ReleaseError("component manifest and result report must be supplied together")
    if component_manifest is not None and component_result_path is not None:
        components = parse_components(component_manifest, packages)
        try:
            result_value = json.loads(component_result_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise ReleaseError(f"cannot read component result report: {error}") from error
        validate_component_results(
            result_value, components, component_manifest, component_result_path.parent
        )
        component_evidence = {
            "manifest": component_manifest.name,
            "manifestSha256": sha256_file(component_manifest),
            "results": component_result_path.name,
            "resultsSha256": sha256_file(component_result_path),
            "testSuites": sum(component.kind == "test" for component in components),
            "benchmarks": sum(component.kind == "benchmark" for component in components),
        }

    value: dict[str, object] = {
        "schemaVersion": 1,
        "sourceRevision": revision,
        "sourceDateEpoch": epoch,
        "created": iso_epoch(epoch),
        "toolchain": {
            "ghc": tool_version(["ghc", "--numeric-version"]),
            "cabal": tool_version(["cabal", "--numeric-version"]),
            "os": platform.system(),
            "architecture": platform.machine(),
        },
        "artifacts": artifacts,
        "reproduction": {
            "prepare": f"bash scripts/prepare-release --revision {revision} --output OUTPUT",
            "verify": "(cd BUNDLE_ROOT && sha256sum -c SHA256SUMS)",
            "portableCudaFlag": "disabled",
        },
        "claims": {
            "publicationAuthorized": False,
            "releaseReady": False,
            "scope": "validated source-release preparation artifacts",
        },
    }
    if component_evidence is not None:
        value["componentEvidence"] = component_evidence
    return value


def package_from_args(args: argparse.Namespace) -> Package:
    return Package(args.name, Path("."), args.version, 0)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    check = sub.add_parser("check", help="check package metadata and exposed-module goldens")
    check.add_argument("--root", type=Path, default=Path("."))
    check.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))
    check.add_argument("--goldens", type=Path, default=Path("release/exposed-modules"))
    check.add_argument("--ci-manifest", type=Path, default=Path("ci/packages.tsv"))
    check.add_argument("--components", type=Path, default=Path("release/components.tsv"))
    check.add_argument("--test-integration-edges", type=Path, default=Path("release/test-integration-edges.tsv"))

    source = sub.add_parser("check-source", help="require an exact clean commit checkout")
    source.add_argument("--root", type=Path, default=Path("."))
    source.add_argument("--revision", required=True)

    archive_project = sub.add_parser(
        "archive-project", help="write the archive-only Cabal project with every required flag"
    )
    archive_project.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))
    archive_project.add_argument("--components", type=Path, default=Path("release/components.tsv"))
    archive_project.add_argument("--output", required=True, type=Path)

    consumer_project = sub.add_parser(
        "archive-consumer-project",
        help="write one archive package's isolated dependency-closure project",
    )
    consumer_project.add_argument("--root", type=Path, default=Path("."))
    consumer_project.add_argument("--archive-root", required=True, type=Path)
    consumer_project.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))
    consumer_project.add_argument("--components", type=Path, default=Path("release/components.tsv"))
    consumer_project.add_argument("--package", required=True)
    consumer_project.add_argument("--output", required=True, type=Path)

    targets = sub.add_parser("component-targets", help="print exact mandatory Cabal targets")
    targets.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))
    targets.add_argument("--components", type=Path, default=Path("release/components.tsv"))
    targets.add_argument("--kind", choices=("test", "benchmark"), required=True)
    targets.add_argument("--package")

    plan = sub.add_parser("check-plan", help="require every mandatory component in a Cabal plan")
    plan.add_argument("plan", type=Path)
    plan.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))
    plan.add_argument("--components", type=Path, default=Path("release/components.tsv"))

    results = sub.add_parser(
        "component-results", help="write deterministic receipts after explicit components pass"
    )
    results.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))
    results.add_argument("--components", type=Path, default=Path("release/components.tsv"))
    results.add_argument("--receipts", required=True, type=Path)
    results.add_argument("--output", required=True, type=Path)

    haddock = sub.add_parser(
        "check-haddock-interfaces", help="require one interface for every manifest package"
    )
    haddock.add_argument("store", type=Path)
    haddock.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))

    haddock_log = sub.add_parser(
        "check-haddock-log", help="reject build and Haddock warnings in an installation log"
    )
    haddock_log.add_argument("log", type=Path)
    haddock_log.add_argument("--sanitized-output", type=Path)

    validate = sub.add_parser("validate-archive", help="validate one source archive")
    validate.add_argument("archive", type=Path)
    validate.add_argument("--name", required=True)
    validate.add_argument("--version", required=True)
    validate.add_argument("--extract", type=Path)
    validate.add_argument("--max-entries", type=int, default=MAX_ENTRIES)
    validate.add_argument("--max-compressed-bytes", type=int, default=MAX_COMPRESSED_BYTES)
    validate.add_argument("--max-unpacked-bytes", type=int, default=MAX_UNPACKED_BYTES)

    sbom = sub.add_parser("sbom", help="write a deterministic SPDX 2.3 source SBOM")
    sbom.add_argument("archive", type=Path)
    sbom.add_argument("--name", required=True)
    sbom.add_argument("--version", required=True)
    sbom.add_argument("--revision", required=True)
    sbom.add_argument("--epoch", required=True, type=int)
    sbom.add_argument("--output", required=True, type=Path)

    finalize = sub.add_parser("finalize", help="atomically finalize a staged directory without replacement")
    finalize.add_argument("--stage", required=True, type=Path)
    finalize.add_argument("--output", required=True, type=Path)

    artifact = sub.add_parser("artifact-manifest", help="write the deterministic release artifact manifest")
    artifact.add_argument("--manifest", type=Path, default=Path("release/packages.tsv"))
    artifact.add_argument("--archives", required=True, type=Path)
    artifact.add_argument("--sboms", required=True, type=Path)
    artifact.add_argument("--revision", required=True)
    artifact.add_argument("--epoch", required=True, type=int)
    artifact.add_argument("--components", type=Path)
    artifact.add_argument("--component-results", type=Path)
    artifact.add_argument("--output", required=True, type=Path)

    args = parser.parse_args(argv)
    try:
        if args.command == "check":
            packages = check_metadata(
                args.root,
                args.manifest,
                args.goldens,
                args.ci_manifest,
                args.components,
            )
            components = parse_components(args.components, packages)
            check_test_integration_edges(
                args.root, packages, components, args.test_integration_edges
            )
            print(
                f"release metadata: {len(packages)} packages, "
                f"{sum(item.kind == 'test' for item in components)} suites, "
                f"{sum(item.kind == 'benchmark' for item in components)} benchmarks, "
                "exact public sibling edges, checked test-only integration edges, "
                "Proposed decision statuses, and exposed-module goldens validated"
            )
        elif args.command == "check-source":
            check_source_checkout(args.root, args.revision)
            print(f"release source is the clean immutable revision {args.revision}")
        elif args.command == "archive-project":
            packages = parse_manifest(args.manifest)
            components = parse_components(args.components, packages)
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_text(archive_project_text(components), encoding="utf-8")
            print(f"archive Cabal project written: {args.output}")
        elif args.command == "archive-consumer-project":
            packages = parse_manifest(args.manifest)
            components = parse_components(args.components, packages)
            text = archive_consumer_project_text(
                args.root, args.archive_root, packages, components, args.package
            )
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_text(text, encoding="utf-8")
            print(f"archive consumer project written: {args.output}")
        elif args.command == "component-targets":
            packages = parse_manifest(args.manifest)
            components = parse_components(args.components, packages)
            if args.package is not None and not any(
                package.name.casefold() == args.package.casefold() for package in packages
            ):
                raise ReleaseError(f"unknown component package: {args.package}")
            for component in components:
                if component.kind == args.kind and (
                    args.package is None
                    or component.package.casefold() == args.package.casefold()
                ):
                    print(component.target)
        elif args.command == "check-plan":
            packages = parse_manifest(args.manifest)
            components = parse_components(args.components, packages)
            check_component_plan(args.plan, components)
            print(f"Cabal plan contains all {len(components)} mandatory components")
        elif args.command == "component-results":
            packages = parse_manifest(args.manifest)
            components = parse_components(args.components, packages)
            write_json(args.output, component_results(components, args.components, args.receipts))
            print(f"component result receipts written: {args.output}")
        elif args.command == "check-haddock-interfaces":
            packages = parse_manifest(args.manifest)
            check_haddock_interfaces(args.store, packages)
            print(f"Haddock interfaces validated for {len(packages)} packages")
        elif args.command == "check-haddock-log":
            sanitized = check_haddock_log(args.log)
            if args.sanitized_output is not None:
                args.sanitized_output.write_text(sanitized, encoding="utf-8")
            print(f"Haddock installation log validated: {args.log}")
        elif args.command == "validate-archive":
            info = validate_archive(
                args.archive,
                package_from_args(args),
                max_entries=args.max_entries,
                max_compressed_bytes=args.max_compressed_bytes,
                max_unpacked_bytes=args.max_unpacked_bytes,
            )
            if args.extract is not None:
                extract_archive(info, args.extract)
            print(
                f"archive validated: {info.path.name} entries={info.entries} "
                f"compressed={info.compressed_bytes} unpacked={info.unpacked_bytes} sha256={info.sha256}"
            )
        elif args.command == "sbom":
            info = validate_archive(args.archive, package_from_args(args))
            write_json(args.output, generate_sbom(info, args.revision, args.epoch))
            print(f"SBOM written: {args.output}")
        elif args.command == "finalize":
            finalize_directory(args.stage, args.output)
            print(f"release directory finalized: {args.output}")
        elif args.command == "artifact-manifest":
            packages = parse_manifest(args.manifest)
            value = generate_artifact_manifest(
                packages,
                args.archives,
                args.sboms,
                args.revision,
                args.epoch,
                args.components,
                args.component_results,
            )
            write_json(args.output, value)
            print(f"artifact manifest written: {args.output}")
        return 0
    except (ReleaseError, OSError, subprocess.CalledProcessError, json.JSONDecodeError) as error:
        print(f"release-tool: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
