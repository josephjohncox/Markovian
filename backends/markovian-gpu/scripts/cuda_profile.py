#!/usr/bin/env python3
"""Validate the bounded CUDA profile and same-session evidence receipts."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
import stat
import sys
from pathlib import Path
from typing import Any, NoReturn

SHA256 = re.compile(r"[0-9a-f]{64}\Z")
REVISION = re.compile(r"[0-9a-f]{40}\Z")
SESSION = re.compile(r"[A-Za-z0-9][A-Za-z0-9._:-]{7,255}\Z")
UUID = re.compile(r"gpu-[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}\Z")
VERSION = re.compile(r"[0-9]+(?:\.[0-9]+)+(?:[-+._A-Za-z0-9]*)?\Z")
SAFE_FILE = re.compile(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}\Z")
MAX_PROFILE_JSON_BYTES = 1048576
MAX_PROFILE_JSON_DEPTH = 128

PROFILE_PRECEDENCE = [
    "P001_PROFILE_READ",
    "P002_PROFILE_JSON",
    "P003_PROFILE_ROOT_TYPE",
    "P004_PROFILE_CANONICAL",
    "P005_PROFILE_SCHEMA",
    "P006_PROFILE_VALUE",
    "P007_PROFILE_CONSUMER",
    "P008_PROFILE_GENERATED",
]
RECEIPT_PRECEDENCE = [
    "R001_RECEIPT_READ",
    "R002_RECEIPT_JSON",
    "R003_RECEIPT_ROOT_TYPE",
    "R004_RECEIPT_CANONICAL",
    "R005_RECEIPT_SCHEMA",
    "R006_RECEIPT_BINDING",
    "R007_RECEIPT_OBSERVATION",
    "R008_RECEIPT_RECORD",
    "R009_RECEIPT_ARTIFACT",
    "R010_RECEIPT_LOG",
    "R011_RECEIPT_OUTCOME",
    "R012_RECEIPT_BENCHMARK",
]

TOP_LEVEL_FIELDS = [
    "benchmark",
    "expectedDeviceUuid",
    "observations",
    "profileArtifact",
    "profileSha256",
    "ptxArtifact",
    "ptxSha256",
    "records",
    "result",
    "schemaVersion",
    "sessionId",
    "sourceRevision",
    "workflow",
]
BENCHMARK_FIELDS = ["exactSemanticChecksum", "rawSampleCount"]
OBSERVATION_FIELDS = [
    "deviceQueryLog",
    "deviceQueryLogSha256",
    "driverApiVersion",
    "driverVersion",
    "nativeObservedDeviceUuid",
    "nvidiaSmiDeviceUuid",
    "sanitizerLog",
    "sanitizerLogSha256",
    "sanitizerVersion",
    "toolkitLog",
    "toolkitLogSha256",
    "toolkitVersion",
]
RECORD_FIELDS = [
    "command",
    "commandSha256",
    "executable",
    "executableSha256",
    "exitCode",
    "kind",
    "log",
    "logSha256",
]
WORKFLOW_FIELDS = ["job", "repository", "runAttempt", "runId", "workflow"]
TOP_LEVEL_TYPES = {
    "benchmark": "object",
    "expectedDeviceUuid": "string",
    "observations": "object",
    "profileArtifact": "string",
    "profileSha256": "string",
    "ptxArtifact": "string",
    "ptxSha256": "string",
    "records": "array",
    "result": "string",
    "schemaVersion": "integer",
    "sessionId": "string",
    "sourceRevision": "string",
    "workflow": "object",
}
BENCHMARK_TYPES = {"exactSemanticChecksum": "string", "rawSampleCount": "integer"}
OBSERVATION_TYPES = {
    field: "integer" if field == "driverApiVersion" else "string"
    for field in OBSERVATION_FIELDS
}
RECORD_TYPES = {
    field: "integer" if field == "exitCode" else "string" for field in RECORD_FIELDS
}
WORKFLOW_TYPES = {
    field: "integer" if field == "runAttempt" else "string" for field in WORKFLOW_FIELDS
}
RECEIPT_CONSTRAINTS = {
    "benchmarkSamplePattern": r"^cuda-transfer-inclusive sample ([0-9]{2}): ([+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?) ms$",
    "job": "cuda-hardware",
    "repository": "josephjohncox/Markovian",
    "result": "passed",
    "safeFilePattern": r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$",
    "sessionPattern": r"^[A-Za-z0-9][A-Za-z0-9._:-]{7,255}$",
    "sha256Pattern": r"^[0-9a-f]{64}$",
    "sourceRevisionPattern": r"^[0-9a-f]{40}$",
    "uuidPattern": r"^GPU-[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$",
    "versionPattern": r"^[0-9]+(?:\.[0-9]+)+(?:[-+._A-Za-z0-9]*)?$",
    "workflow": ".github/workflows/cuda-hardware.yml",
}
RECORD_SPECS = [
    {
        "kind": "correctness",
        "command": "cabal test markovian-gpu-test --project-file=cabal.project.ci -fcuda -fcuda-fault-injection --extra-include-dirs=/usr/local/cuda/include --test-show-details=direct && bash backends/markovian-gpu/scripts/check-device-boundary",
        "executable": "test-executable",
        "successMarkers": [
            "markovian-gpu: device contract tests passed",
            "PASS: device plans, host results, executor resources, and endpoints remain protected",
        ],
    },
    {
        "kind": "sanitizer-memcheck",
        "command": "compute-sanitizer --error-exitcode=1 --tool memcheck cuda-evidence/test-executable",
        "executable": "test-executable",
        "successMarkers": ["ERROR SUMMARY: 0 errors"],
    },
    {
        "kind": "sanitizer-initcheck",
        "command": "compute-sanitizer --error-exitcode=1 --tool initcheck cuda-evidence/test-executable",
        "executable": "test-executable",
        "successMarkers": ["ERROR SUMMARY: 0 errors"],
    },
    {
        "kind": "sanitizer-racecheck",
        "command": "compute-sanitizer --error-exitcode=1 --tool racecheck cuda-evidence/test-executable",
        "executable": "test-executable",
        "successMarkers": ["ERROR SUMMARY: 0 errors"],
    },
    {
        "kind": "sanitizer-synccheck",
        "command": "compute-sanitizer --error-exitcode=1 --tool synccheck cuda-evidence/test-executable",
        "executable": "test-executable",
        "successMarkers": ["ERROR SUMMARY: 0 errors"],
    },
    {
        "kind": "benchmark",
        "command": "cabal bench markovian-gpu-bench --project-file=cabal.project.ci -fcuda -fcuda-fault-injection --extra-include-dirs=/usr/local/cuda/include",
        "executable": "benchmark-executable",
        "successMarkers": [
            "Benchmark markovian-gpu-bench: FINISH",
            "exact-semantic-checksum: 49439/128",
        ],
    },
]

EXPECTED_ADMISSION = [
    "complete-driver-symbol-table-before-cuInit",
    "native-device-uuid-match-before-context",
    "compute-capability-literal-match-before-context",
    "module-load",
    "kernel-symbol-lookup",
    "known-answer-self-test",
]
EXPECTED_NUMERIC_POLICY = {
    "cpu": {
        "accumulationOrder": "k-ascending-left-fold",
        "comparisonAbsoluteTolerance": "2e-12",
        "comparisonRelativeTolerance": "2e-12",
        "contraction": "separate-binary64-multiply-then-add",
    },
    "cuda": {
        "accumulationOrder": "k-ascending-left-fold",
        "comparisonAbsoluteTolerance": "2e-12",
        "comparisonRelativeTolerance": "2e-12",
        "contraction": "ptx-fma.rn.f64",
    },
    "exact": {
        "input": "finite-binary64-word-as-exact-dyadic-rational",
        "matmul": "exact-sum-k-of-left[row,k]*right[k,column]",
        "vjpLeft": "exact-matmul(seed,transpose(right))",
        "vjpRight": "exact-matmul(transpose(left),seed)",
    },
}
EXPECTED_DIFFERENTIAL = {
    "left": ["1", "2", "3", "4", "5", "6"],
    "leftShape": [2, 3],
    "right": ["7", "8", "9", "10", "11", "12"],
    "rightShape": [3, 2],
    "seed": ["1/2", "-1", "2", "1/4"],
    "seedShape": [2, 2],
}
EXPECTED_BENCHMARK = {
    "exactSemanticChecksum": "49439/128",
    "leftInput": "row-major ((row+column) mod 7 - 3) / 8",
    "matrixShape": [64, 64],
    "measuredSampleCount": 20,
    "rightInput": "row-major identity-plus-off-diagonal ((3*row+column) mod 5 - 2) / 32",
    "warmupCount": 1,
}


class ProfileError(ValueError):
    def __init__(self, code: str, message: str):
        self.code = code
        super().__init__(f"{code}: {message}")


def fail(code: str, message: str) -> NoReturn:
    raise ProfileError(code, message)


def is_int(value: Any) -> bool:
    return isinstance(value, int) and not isinstance(value, bool)


def object_value(value: Any, label: str, code: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        fail(code, f"{label} must be an object")
    return value


def array_value(value: Any, label: str, code: str) -> list[Any]:
    if not isinstance(value, list):
        fail(code, f"{label} must be an array")
    return value


def string_value(value: Any, label: str, code: str, *, nonempty: bool = True) -> str:
    if not isinstance(value, str) or (nonempty and not value):
        fail(code, f"{label} must be a nonempty string")
    return value


def int_value(value: Any, label: str, code: str, *, minimum: int = 0) -> int:
    if not is_int(value) or value < minimum:
        fail(code, f"{label} must be an integer >= {minimum}")
    return value


def string_array(value: Any, label: str, code: str) -> list[str]:
    items = array_value(value, label, code)
    for index, item in enumerate(items):
        string_value(item, f"{label}[{index}]", code)
    return items


def require_keys(
    value: dict[str, Any], expected: list[str] | set[str], label: str, code: str
) -> None:
    expected_set = set(expected)
    actual = set(value)
    if actual != expected_set:
        fail(
            code,
            f"{label} fields differ: missing={sorted(expected_set - actual)}, unknown={sorted(actual - expected_set)}",
        )


def canonical_json(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True) + "\n").encode()


def read_bytes(
    path: Path, code: str, label: str, *, max_bytes: int | None = None
) -> bytes:
    try:
        with path.open("rb") as handle:
            raw = handle.read() if max_bytes is None else handle.read(max_bytes + 1)
    except OSError as error:
        fail(code, f"cannot read {label}: {error}")
    if max_bytes is not None and len(raw) > max_bytes:
        fail(code, f"{label} exceeds the {max_bytes}-byte limit")
    return raw


def check_json_depth(raw: bytes, max_depth: int, code: str, label: str) -> None:
    depth = 0
    in_string = False
    escaped = False
    for byte in raw:
        if in_string:
            if escaped:
                escaped = False
            elif byte == ord("\\"):
                escaped = True
            elif byte == ord('"'):
                in_string = False
        elif byte == ord('"'):
            in_string = True
        elif byte in (ord("["), ord("{")):
            depth += 1
            if depth > max_depth:
                fail(code, f"{label} JSON exceeds the {max_depth}-level depth limit")
        elif byte in (ord("]"), ord("}")):
            depth -= 1


def read_json(
    path: Path,
    read_code: str,
    json_code: str,
    root_code: str,
    label: str,
    *,
    max_bytes: int,
    max_depth: int,
) -> tuple[dict[str, Any], bytes]:
    raw = read_bytes(path, read_code, label, max_bytes=max_bytes)
    check_json_depth(raw, max_depth, json_code, label)
    try:
        value = json.loads(raw)
    except (UnicodeDecodeError, json.JSONDecodeError, RecursionError) as error:
        fail(json_code, f"invalid {label} JSON: {error}")
    return object_value(value, label, root_code), raw


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def sha256_file(path: Path, code: str = "R009_RECEIPT_ARTIFACT") -> str:
    try:
        digest = hashlib.sha256()
        with path.open("rb") as handle:
            for chunk in iter(lambda: handle.read(1024 * 1024), b""):
                digest.update(chunk)
        return digest.hexdigest()
    except OSError as error:
        fail(code, f"cannot hash {path.name}: {error}")


def validate_profile_types(value: dict[str, Any]) -> None:
    top = [
        "admissionTests",
        "benchmarkFixture",
        "cudaAbi",
        "deviceConstraints",
        "differentialFixture",
        "kernelAbi",
        "numericPolicy",
        "profileId",
        "ptx",
        "receiptSchema",
        "schemaVersion",
        "validationFailurePrecedence",
    ]
    require_keys(value, top, "profile", "P005_PROFILE_SCHEMA")
    int_value(value["schemaVersion"], "schemaVersion", "P005_PROFILE_SCHEMA", minimum=1)
    string_value(value["profileId"], "profileId", "P005_PROFILE_SCHEMA")
    string_array(value["admissionTests"], "admissionTests", "P005_PROFILE_SCHEMA")

    cuda = object_value(value["cudaAbi"], "cudaAbi", "P005_PROFILE_SCHEMA")
    cuda_fields = [
        "compileImage",
        "driverHeaderSha256",
        "driverLibrary",
        "headerVersion",
        "nvcc",
        "requiredSymbols",
        "toolkitRelease",
    ]
    require_keys(cuda, cuda_fields, "cudaAbi", "P005_PROFILE_SCHEMA")
    for field in (
        "compileImage",
        "driverHeaderSha256",
        "driverLibrary",
        "nvcc",
        "toolkitRelease",
    ):
        string_value(cuda[field], f"cudaAbi.{field}", "P005_PROFILE_SCHEMA")
    int_value(
        cuda["headerVersion"], "cudaAbi.headerVersion", "P005_PROFILE_SCHEMA", minimum=1
    )
    string_array(
        cuda["requiredSymbols"], "cudaAbi.requiredSymbols", "P005_PROFILE_SCHEMA"
    )

    device = object_value(
        value["deviceConstraints"], "deviceConstraints", "P005_PROFILE_SCHEMA"
    )
    device_fields = [
        "computeCapabilityMajor",
        "computeCapabilityMinor",
        "maximumThreadsPerBlockAtLeast",
        "uuidBytes",
    ]
    require_keys(device, device_fields, "deviceConstraints", "P005_PROFILE_SCHEMA")
    for field in device_fields:
        int_value(
            device[field],
            f"deviceConstraints.{field}",
            "P005_PROFILE_SCHEMA",
            minimum=1 if field != "computeCapabilityMinor" else 0,
        )

    kernel = object_value(value["kernelAbi"], "kernelAbi", "P005_PROFILE_SCHEMA")
    require_keys(
        kernel,
        ["blockThreads", "indexType", "name", "version"],
        "kernelAbi",
        "P005_PROFILE_SCHEMA",
    )
    int_value(
        kernel["blockThreads"],
        "kernelAbi.blockThreads",
        "P005_PROFILE_SCHEMA",
        minimum=1,
    )
    for field in ("indexType", "name", "version"):
        string_value(kernel[field], f"kernelAbi.{field}", "P005_PROFILE_SCHEMA")

    ptx = object_value(value["ptx"], "ptx", "P005_PROFILE_SCHEMA")
    require_keys(
        ptx,
        ["headerSha256", "isaVersion", "sha256", "target"],
        "ptx",
        "P005_PROFILE_SCHEMA",
    )
    for field in ptx:
        string_value(ptx[field], f"ptx.{field}", "P005_PROFILE_SCHEMA")

    numeric = object_value(
        value["numericPolicy"], "numericPolicy", "P005_PROFILE_SCHEMA"
    )
    require_keys(
        numeric, ["cpu", "cuda", "exact"], "numericPolicy", "P005_PROFILE_SCHEMA"
    )
    floating_fields = [
        "accumulationOrder",
        "comparisonAbsoluteTolerance",
        "comparisonRelativeTolerance",
        "contraction",
    ]
    for layer in ("cpu", "cuda"):
        policy = object_value(
            numeric[layer], f"numericPolicy.{layer}", "P005_PROFILE_SCHEMA"
        )
        require_keys(
            policy, floating_fields, f"numericPolicy.{layer}", "P005_PROFILE_SCHEMA"
        )
        for field in floating_fields:
            string_value(
                policy[field], f"numericPolicy.{layer}.{field}", "P005_PROFILE_SCHEMA"
            )
    exact = object_value(numeric["exact"], "numericPolicy.exact", "P005_PROFILE_SCHEMA")
    require_keys(
        exact,
        ["input", "matmul", "vjpLeft", "vjpRight"],
        "numericPolicy.exact",
        "P005_PROFILE_SCHEMA",
    )
    for field in exact:
        string_value(
            exact[field], f"numericPolicy.exact.{field}", "P005_PROFILE_SCHEMA"
        )

    benchmark = object_value(
        value["benchmarkFixture"], "benchmarkFixture", "P005_PROFILE_SCHEMA"
    )
    require_keys(
        benchmark, list(EXPECTED_BENCHMARK), "benchmarkFixture", "P005_PROFILE_SCHEMA"
    )
    for field in ("exactSemanticChecksum", "leftInput", "rightInput"):
        string_value(
            benchmark[field], f"benchmarkFixture.{field}", "P005_PROFILE_SCHEMA"
        )
    shape = array_value(
        benchmark["matrixShape"], "benchmarkFixture.matrixShape", "P005_PROFILE_SCHEMA"
    )
    for index, dimension in enumerate(shape):
        int_value(
            dimension,
            f"benchmarkFixture.matrixShape[{index}]",
            "P005_PROFILE_SCHEMA",
            minimum=1,
        )
    int_value(
        benchmark["measuredSampleCount"],
        "benchmarkFixture.measuredSampleCount",
        "P005_PROFILE_SCHEMA",
        minimum=1,
    )
    int_value(
        benchmark["warmupCount"],
        "benchmarkFixture.warmupCount",
        "P005_PROFILE_SCHEMA",
        minimum=0,
    )

    differential = object_value(
        value["differentialFixture"], "differentialFixture", "P005_PROFILE_SCHEMA"
    )
    require_keys(
        differential,
        list(EXPECTED_DIFFERENTIAL),
        "differentialFixture",
        "P005_PROFILE_SCHEMA",
    )
    for field in ("left", "right", "seed"):
        string_array(
            differential[field], f"differentialFixture.{field}", "P005_PROFILE_SCHEMA"
        )
    for field in ("leftShape", "rightShape", "seedShape"):
        dimensions = array_value(
            differential[field], f"differentialFixture.{field}", "P005_PROFILE_SCHEMA"
        )
        for index, dimension in enumerate(dimensions):
            int_value(
                dimension,
                f"differentialFixture.{field}[{index}]",
                "P005_PROFILE_SCHEMA",
                minimum=1,
            )

    receipt = object_value(
        value["receiptSchema"], "receiptSchema", "P005_PROFILE_SCHEMA"
    )
    receipt_fields = [
        "benchmarkFields",
        "benchmarkTypes",
        "constraints",
        "maxExecutableBytes",
        "maxJsonBytes",
        "maxJsonDepth",
        "maxLogBytes",
        "observationFields",
        "observationTypes",
        "recordFields",
        "recordTypes",
        "requiredRecords",
        "schemaVersion",
        "topLevelFields",
        "topLevelTypes",
        "workflowFields",
        "workflowTypes",
    ]
    require_keys(receipt, receipt_fields, "receiptSchema", "P005_PROFILE_SCHEMA")
    int_value(
        receipt["schemaVersion"],
        "receiptSchema.schemaVersion",
        "P005_PROFILE_SCHEMA",
        minimum=1,
    )
    int_value(
        receipt["maxExecutableBytes"],
        "receiptSchema.maxExecutableBytes",
        "P005_PROFILE_SCHEMA",
        minimum=1,
    )
    int_value(
        receipt["maxJsonBytes"],
        "receiptSchema.maxJsonBytes",
        "P005_PROFILE_SCHEMA",
        minimum=1,
    )
    int_value(
        receipt["maxJsonDepth"],
        "receiptSchema.maxJsonDepth",
        "P005_PROFILE_SCHEMA",
        minimum=1,
    )
    int_value(
        receipt["maxLogBytes"],
        "receiptSchema.maxLogBytes",
        "P005_PROFILE_SCHEMA",
        minimum=1,
    )
    for field in (
        "benchmarkFields",
        "observationFields",
        "recordFields",
        "topLevelFields",
        "workflowFields",
    ):
        string_array(receipt[field], f"receiptSchema.{field}", "P005_PROFILE_SCHEMA")
    for fields_name, types_name in (
        ("benchmarkFields", "benchmarkTypes"),
        ("observationFields", "observationTypes"),
        ("recordFields", "recordTypes"),
        ("topLevelFields", "topLevelTypes"),
        ("workflowFields", "workflowTypes"),
    ):
        types = object_value(
            receipt[types_name], f"receiptSchema.{types_name}", "P005_PROFILE_SCHEMA"
        )
        require_keys(
            types,
            receipt[fields_name],
            f"receiptSchema.{types_name}",
            "P005_PROFILE_SCHEMA",
        )
        for name, field_type in types.items():
            if field_type not in {"array", "integer", "object", "string"}:
                fail(
                    "P005_PROFILE_SCHEMA",
                    f"receiptSchema.{types_name}.{name} has an unsupported type",
                )
    constraints = object_value(
        receipt["constraints"], "receiptSchema.constraints", "P005_PROFILE_SCHEMA"
    )
    require_keys(
        constraints,
        list(RECEIPT_CONSTRAINTS),
        "receiptSchema.constraints",
        "P005_PROFILE_SCHEMA",
    )
    for name, constraint in constraints.items():
        string_value(
            constraint, f"receiptSchema.constraints.{name}", "P005_PROFILE_SCHEMA"
        )
    records = array_value(
        receipt["requiredRecords"],
        "receiptSchema.requiredRecords",
        "P005_PROFILE_SCHEMA",
    )
    for index, record_value in enumerate(records):
        record = object_value(
            record_value,
            f"receiptSchema.requiredRecords[{index}]",
            "P005_PROFILE_SCHEMA",
        )
        require_keys(
            record,
            ["command", "executable", "kind", "successMarkers"],
            f"receiptSchema.requiredRecords[{index}]",
            "P005_PROFILE_SCHEMA",
        )
        for field in ("command", "executable", "kind"):
            string_value(
                record[field],
                f"receiptSchema.requiredRecords[{index}].{field}",
                "P005_PROFILE_SCHEMA",
            )
        string_array(
            record["successMarkers"],
            f"receiptSchema.requiredRecords[{index}].successMarkers",
            "P005_PROFILE_SCHEMA",
        )

    precedence = object_value(
        value["validationFailurePrecedence"],
        "validationFailurePrecedence",
        "P005_PROFILE_SCHEMA",
    )
    require_keys(
        precedence,
        ["profile", "receipt"],
        "validationFailurePrecedence",
        "P005_PROFILE_SCHEMA",
    )
    string_array(
        precedence["profile"],
        "validationFailurePrecedence.profile",
        "P005_PROFILE_SCHEMA",
    )
    string_array(
        precedence["receipt"],
        "validationFailurePrecedence.receipt",
        "P005_PROFILE_SCHEMA",
    )


def validate_profile_values(value: dict[str, Any]) -> None:
    cuda = value["cudaAbi"]
    ptx = value["ptx"]
    device = value["deviceConstraints"]
    kernel = value["kernelAbi"]
    receipt = value["receiptSchema"]
    if (
        value["schemaVersion"] != 1
        or value["profileId"] != "markovian-cuda-sm121-f64-matmul-v1"
    ):
        fail("P006_PROFILE_VALUE", "unsupported profile identity or schema version")
    if value["admissionTests"] != EXPECTED_ADMISSION:
        fail(
            "P006_PROFILE_VALUE",
            "admission schedule differs from the reviewed schedule",
        )
    if value["numericPolicy"] != EXPECTED_NUMERIC_POLICY:
        fail(
            "P006_PROFILE_VALUE",
            "numeric policy differs from the reviewed exact, CPU, or CUDA policy",
        )
    if value["differentialFixture"] != EXPECTED_DIFFERENTIAL:
        fail(
            "P006_PROFILE_VALUE",
            "differential fixture differs from the reviewed fixture",
        )
    if value["benchmarkFixture"] != EXPECTED_BENCHMARK:
        fail(
            "P006_PROFILE_VALUE",
            "benchmark counts, warmups, inputs, shape, or checksum differ",
        )
    if (
        cuda["headerVersion"] != 13000
        or cuda["toolkitRelease"] != "13.0"
        or cuda["driverLibrary"] != "libcuda.so.1"
    ):
        fail("P006_PROFILE_VALUE", "CUDA ABI differs from the reviewed CUDA 13.0 ABI")
    if not re.fullmatch(
        r"nvidia/cuda:13\.0\.2-devel-ubuntu22\.04@sha256:[0-9a-f]{64}",
        cuda["compileImage"],
    ):
        fail(
            "P006_PROFILE_VALUE",
            "compile image is not the reviewed digest-pinned image",
        )
    if cuda["nvcc"] != "/usr/local/cuda/bin/nvcc":
        fail("P006_PROFILE_VALUE", "nvcc path differs from the reviewed path")
    if len(cuda["requiredSymbols"]) != len(set(cuda["requiredSymbols"])):
        fail("P006_PROFILE_VALUE", "CUDA required symbols contain a duplicate")
    for label, digest in (
        ("driver header", cuda["driverHeaderSha256"]),
        ("PTX", ptx["sha256"]),
        ("PTX header", ptx["headerSha256"]),
    ):
        if SHA256.fullmatch(digest) is None:
            fail("P006_PROFILE_VALUE", f"invalid {label} SHA-256")
    if ptx["target"] != "sm_121" or ptx["isaVersion"] != "9.0":
        fail("P006_PROFILE_VALUE", "PTX target or ISA differs from the reviewed values")
    if kernel != {
        "blockThreads": 128,
        "indexType": "signed-int32",
        "name": "markovian_f64_matmul",
        "version": "markovian-f64-matmul-v1",
    }:
        fail(
            "P006_PROFILE_VALUE", "kernel ABI, index type, name, or block width differs"
        )
    if device != {
        "computeCapabilityMajor": 12,
        "computeCapabilityMinor": 1,
        "maximumThreadsPerBlockAtLeast": 128,
        "uuidBytes": 16,
    }:
        fail("P006_PROFILE_VALUE", "device constraints differ from the reviewed values")
    expected_receipt = {
        "benchmarkFields": BENCHMARK_FIELDS,
        "benchmarkTypes": BENCHMARK_TYPES,
        "constraints": RECEIPT_CONSTRAINTS,
        "maxExecutableBytes": 268435456,
        "maxJsonBytes": 1048576,
        "maxJsonDepth": 128,
        "maxLogBytes": 16777216,
        "observationFields": OBSERVATION_FIELDS,
        "observationTypes": OBSERVATION_TYPES,
        "recordFields": RECORD_FIELDS,
        "recordTypes": RECORD_TYPES,
        "requiredRecords": RECORD_SPECS,
        "schemaVersion": 2,
        "topLevelFields": TOP_LEVEL_FIELDS,
        "topLevelTypes": TOP_LEVEL_TYPES,
        "workflowFields": WORKFLOW_FIELDS,
        "workflowTypes": WORKFLOW_TYPES,
    }
    if receipt != expected_receipt:
        fail(
            "P006_PROFILE_VALUE",
            "complete receipt schema differs from the reviewed schema",
        )
    if value["validationFailurePrecedence"] != {
        "profile": PROFILE_PRECEDENCE,
        "receipt": RECEIPT_PRECEDENCE,
    }:
        fail(
            "P006_PROFILE_VALUE",
            "validation failure precedence differs from the implemented total order",
        )


def load_profile(path: Path) -> tuple[dict[str, Any], str]:
    value, raw = read_json(
        path,
        "P001_PROFILE_READ",
        "P002_PROFILE_JSON",
        "P003_PROFILE_ROOT_TYPE",
        "profile",
        max_bytes=MAX_PROFILE_JSON_BYTES,
        max_depth=MAX_PROFILE_JSON_DEPTH,
    )
    if raw != canonical_json(value):
        fail("P004_PROFILE_CANONICAL", "profile JSON is not in canonical sorted form")
    validate_profile_types(value)
    validate_profile_values(value)
    return value, sha256_bytes(raw)


def generated_files(
    root: Path, profile: dict[str, Any], profile_sha: str
) -> dict[Path, str]:
    cuda = profile["cudaAbi"]
    device = profile["deviceConstraints"]
    kernel = profile["kernelAbi"]
    ptx = profile["ptx"]
    raw_ptx = read_bytes(
        root / "cbits/markovian_dense.ptx", "P007_PROFILE_CONSUMER", "PTX"
    )
    artifact = f'''-- Generated by scripts/cuda_profile.py from profile.json.
module Markovian.Backend.GPU.Artifact where

profileSHA256 :: String
profileSHA256 = "{profile_sha}"

ptxTarget :: String
ptxTarget = "{ptx["target"]}"

ptxISAVersion :: String
ptxISAVersion = "{ptx["isaVersion"]}"

kernelName :: String
kernelName = "{kernel["name"]}"

kernelABI :: String
kernelABI = "{kernel["version"]}"

ptxSHA256 :: String
ptxSHA256 = "{ptx["sha256"]}"

requiredCapabilityMajor :: Integer
requiredCapabilityMajor = {device["computeCapabilityMajor"]}

requiredCapabilityMinor :: Integer
requiredCapabilityMinor = {device["computeCapabilityMinor"]}

requiredThreadsPerBlock :: Integer
requiredThreadsPerBlock = {device["maximumThreadsPerBlockAtLeast"]}

ptxLength :: Integer
ptxLength = {len(raw_ptx)}
'''
    header = f'''#ifndef MARKOVIAN_CUDA_PROFILE_H
#define MARKOVIAN_CUDA_PROFILE_H

/* Generated by scripts/cuda_profile.py from profile.json. */
#define MARKOVIAN_CUDA_PROFILE_SHA256 "{profile_sha}"
#define MARKOVIAN_CUDA_DRIVER_LIBRARY "{cuda["driverLibrary"]}"
#define MARKOVIAN_CUDA_HEADER_VERSION {cuda["headerVersion"]}
#define MARKOVIAN_CUDA_REQUIRED_MAJOR {device["computeCapabilityMajor"]}
#define MARKOVIAN_CUDA_REQUIRED_MINOR {device["computeCapabilityMinor"]}
#define MARKOVIAN_CUDA_REQUIRED_THREADS {device["maximumThreadsPerBlockAtLeast"]}
#define MARKOVIAN_CUDA_UUID_BYTES {device["uuidBytes"]}
#define MARKOVIAN_DENSE_BLOCK_THREADS {kernel["blockThreads"]}U

#endif
'''
    toolchain = "\n".join(
        [
            f"MARKOVIAN_CUDA_VERSION={cuda['toolkitRelease']}",
            f"MARKOVIAN_CUDA_COMPUTE_CAPABILITY={device['computeCapabilityMajor']}{device['computeCapabilityMinor']}",
            f"MARKOVIAN_NVCC={cuda['nvcc']}",
            f"MARKOVIAN_CUDA_DRIVER_HEADER_SHA256={cuda['driverHeaderSha256']}",
            f"MARKOVIAN_CUDA_PTX_SHA256={ptx['sha256']}",
            f"MARKOVIAN_CUDA_PTX_HEADER_SHA256={ptx['headerSha256']}",
            f"MARKOVIAN_CUDA_COMPILE_IMAGE={cuda['compileImage']}",
            f"MARKOVIAN_CUDA_PROFILE_SHA256={profile_sha}",
            "",
        ]
    )
    return {
        root / "cbits/markovian_cuda_profile.h": header,
        root / "src/Markovian/Backend/GPU/Artifact.hs": artifact,
        root / "toolchain.env": toolchain,
    }


def text_file(path: Path, code: str, label: str) -> str:
    raw = read_bytes(path, code, label)
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError as error:
        fail(code, f"{label} is not UTF-8: {error}")


def check_consumers(root: Path, profile: dict[str, Any]) -> None:
    ptx_path = root / "cbits/markovian_dense.ptx"
    header_path = root / "cbits/markovian_dense_ptx.h"
    source = text_file(ptx_path, "P007_PROFILE_CONSUMER", "PTX")
    header = text_file(header_path, "P007_PROFILE_CONSUMER", "PTX header")
    ptx = profile["ptx"]
    kernel = profile["kernelAbi"]
    if (
        sha256_file(ptx_path, "P007_PROFILE_CONSUMER") != ptx["sha256"]
        or sha256_file(header_path, "P007_PROFILE_CONSUMER") != ptx["headerSha256"]
    ):
        fail(
            "P007_PROFILE_CONSUMER",
            "PTX or generated header digest differs from profile authority",
        )
    patterns = [
        rf"^\.version\s+{re.escape(ptx['isaVersion'])}\s*$",
        rf"^\.target\s+{re.escape(ptx['target'])}\s*$",
        rf"^\.visible\s+\.entry\s+{re.escape(kernel['name'])}\(",
    ]
    if any(re.search(pattern, source, re.MULTILINE) is None for pattern in patterns):
        fail(
            "P007_PROFILE_CONSUMER",
            "PTX ISA, target, or kernel entry differs from profile authority",
        )
    header_literals = [
        f'#define MARKOVIAN_DENSE_PTX_TARGET "{ptx["target"]}"',
        f'#define MARKOVIAN_DENSE_PTX_KERNEL "{kernel["name"]}"',
        f'#define MARKOVIAN_DENSE_KERNEL_ABI "{kernel["version"]}"',
        f'#define MARKOVIAN_DENSE_PTX_SHA256 "{ptx["sha256"]}"',
    ]
    if any(literal not in header for literal in header_literals):
        fail(
            "P007_PROFILE_CONSUMER",
            "generated PTX header literals differ from profile authority",
        )
    if (
        profile["numericPolicy"]["cuda"]["contraction"] == "ptx-fma.rn.f64"
        and "fma.rn.f64" not in source
    ):
        fail(
            "P007_PROFILE_CONSUMER",
            "PTX does not contain the required fma.rn.f64 refinement",
        )
    if re.findall(r"\b(?:add|mul)\.[^.\s]+\.f64\b", source):
        fail("P007_PROFILE_CONSUMER", "PTX has a non-FMA F64 accumulation instruction")

    test = text_file(root / "test/Main.lhs", "P007_PROFILE_CONSUMER", "GPU test")
    benchmark = text_file(
        root / "bench/Main.hs", "P007_PROFILE_CONSUMER", "GPU benchmark"
    )
    cpu_path = (
        root.parent.parent
        / "packages/markovian-tensor/src/Markovian/Tensor/Internal.hs"
    )
    if cpu_path.exists():
        cpu_source = text_file(cpu_path, "P007_PROFILE_CONSUMER", "CPU tensor source")
        if "foldl' (\\accumulator (x, y) -> accumulator + x * y) 0" not in cpu_source:
            fail(
                "P007_PROFILE_CONSUMER",
                "CPU matrix operation order differs from profile authority",
            )
    fixture_literals = [
        "leftWords = [1, 2, 3, 4, 5, 6]",
        "rightWords = [7, 8, 9, 10, 11, 12]",
        "seedWords = [0.5, -1, 2, 0.25]",
        f"assertApproxList = assertApproxListWith {profile['numericPolicy']['cpu']['comparisonAbsoluteTolerance']}",
    ]
    exact_literals = [
        "toRational (left !! (row * inner + k)) * toRational (right !! (k * columns + column))",
        "exactMatMul rows columns inner seed (transposeWords inner columns right)",
        "exactMatMul inner rows columns (transposeWords rows inner left) seed",
    ]
    if any(literal not in test for literal in fixture_literals + exact_literals):
        fail(
            "P007_PROFILE_CONSUMER",
            "test fixture, exact equations, or CPU tolerance differs from profile authority",
        )
    correctness_markers = profile["receiptSchema"]["requiredRecords"][0][
        "successMarkers"
    ]
    boundary = text_file(
        root / "scripts/check-device-boundary",
        "P007_PROFILE_CONSUMER",
        "device boundary script",
    )
    if correctness_markers[0] not in test or correctness_markers[1] not in boundary:
        fail(
            "P007_PROFILE_CONSUMER",
            "correctness success markers differ from profile authority",
        )
    count = profile["benchmarkFixture"]["measuredSampleCount"]
    warmups = profile["benchmarkFixture"]["warmupCount"]
    benchmark_literals = [
        "fromIntegral ((row + column) `mod` 7 - 3) / 8",
        "if row == column then 1 else fromIntegral ((row * 3 + column) `mod` 5 - 2) / 32",
        f"shape64 :: SShape '[{profile['benchmarkFixture']['matrixShape'][0]}, {profile['benchmarkFixture']['matrixShape'][1]}]",
        f"replicateM {count}",
        f"unless (length samples == {count})",
        f'putStrLn (label ++ " warmups: {warmups} (excluded)")',
        f"tolerance = {profile['numericPolicy']['cuda']['comparisonAbsoluteTolerance']} + {profile['numericPolicy']['cuda']['comparisonRelativeTolerance']} * max",
        f'putStrLn "exact-semantic-checksum: {profile["benchmarkFixture"]["exactSemanticChecksum"]}"',
    ]
    if any(literal not in benchmark for literal in benchmark_literals):
        fail(
            "P007_PROFILE_CONSUMER",
            "benchmark inputs, policy, count, warmup, or checksum differ from profile authority",
        )

    c_source = text_file(
        root / "cbits/markovian_gpu.c", "P007_PROFILE_CONSUMER", "CUDA bridge"
    )
    symbols = re.findall(r'LOAD_SYMBOL\(driver,\s*\w+,\s*"([^"]+)"\)', c_source)
    if symbols != profile["cudaAbi"]["requiredSymbols"]:
        fail(
            "P007_PROFILE_CONSUMER",
            "CUDA driver symbol table differs from profile authority",
        )
    if "sizeof(int) == 4" not in c_source or "int rows" not in text_file(
        root / "cbits/markovian_dense.cu", "P007_PROFILE_CONSUMER", "CUDA kernel"
    ):
        fail(
            "P007_PROFILE_CONSUMER",
            "kernel signed-int32 index contract differs from profile authority",
        )
    admission_source = c_source.split("void markovian_cuda_executor_create", 1)
    if len(admission_source) != 2:
        fail("P007_PROFILE_CONSUMER", "CUDA admission implementation is missing")
    admission_tokens = [
        "driver_open(&executor->driver",
        "executor->driver.init(0)",
        "executor->driver.device_get_uuid",
        "CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR",
        "executor->driver.ctx_create",
        "executor->driver.module_load_data",
        "executor->driver.module_get_function",
        "self_test(executor)",
    ]
    positions = [admission_source[1].find(token) for token in admission_tokens]
    if any(position < 0 for position in positions) or positions != sorted(positions):
        fail(
            "P007_PROFILE_CONSUMER",
            "CUDA admission implementation differs from the reviewed schedule",
        )

    workflow_path = root.parent.parent / ".github/workflows/cuda-compile.yml"
    if workflow_path.exists():
        workflow = text_file(
            workflow_path, "P007_PROFILE_CONSUMER", "CUDA compile workflow"
        )
        image_name, image_digest = profile["cudaAbi"]["compileImage"].split(
            "@sha256:", 1
        )
        if image_name not in workflow or image_digest not in workflow:
            fail(
                "P007_PROFILE_CONSUMER",
                "compile workflow image differs from profile authority",
            )
    hardware_path = root.parent.parent / ".github/workflows/cuda-hardware.yml"
    if hardware_path.exists():
        hardware = text_file(
            hardware_path, "P007_PROFILE_CONSUMER", "CUDA hardware workflow"
        )
        hardware_requirements = [
            'profile["receiptSchema"]["requiredRecords"]',
            "cuda-evidence/test-executable",
            "cuda-evidence/benchmark-executable",
            "native-observed-device-uuid",
            "actions/attest@1e69f48acb82d1966a394da916b4c1698aa569d6",
        ]
        if any(requirement not in hardware for requirement in hardware_requirements):
            fail(
                "P007_PROFILE_CONSUMER",
                "hardware receipt workflow does not consume the complete profile schema",
            )


def check_profile(root: Path) -> str:
    profile, profile_sha = load_profile(root / "profile.json")
    check_consumers(root, profile)
    for path, expected in generated_files(root, profile, profile_sha).items():
        if text_file(path, "P008_PROFILE_GENERATED", path.name) != expected:
            fail(
                "P008_PROFILE_GENERATED", f"generated profile artifact is stale: {path}"
            )
    return profile_sha


def normalized_uuid(value: str, code: str, label: str) -> str:
    string_value(value, label, code)
    lower = value.lower()
    if UUID.fullmatch(lower) is None:
        fail(code, f"{label} is not canonical NVIDIA UUID text")
    return lower


def safe_file(root: Path, name: str, code: str, label: str, maximum: int) -> Path:
    string_value(name, label, code)
    if SAFE_FILE.fullmatch(name) is None:
        fail(code, f"{label} is not a safe basename")
    path = root / name
    try:
        info = path.stat(follow_symlinks=False)
    except OSError as error:
        fail(code, f"cannot stat {label}: {error}")
    if not stat.S_ISREG(info.st_mode):
        fail(code, f"{label} is not a regular file")
    if info.st_size <= 0 or info.st_size > maximum:
        fail(code, f"{label} byte size is outside 1..{maximum}")
    return path


def validate_declared_type(value: Any, declared: str, label: str) -> None:
    valid = {
        "array": isinstance(value, list),
        "integer": is_int(value),
        "object": isinstance(value, dict),
        "string": isinstance(value, str),
    }[declared]
    if not valid:
        fail("R005_RECEIPT_SCHEMA", f"{label} must have declared type {declared}")


def validate_receipt_types(receipt: dict[str, Any], profile: dict[str, Any]) -> None:
    schema = profile["receiptSchema"]
    require_keys(receipt, schema["topLevelFields"], "receipt", "R005_RECEIPT_SCHEMA")
    for field, declared in schema["topLevelTypes"].items():
        validate_declared_type(receipt[field], declared, f"receipt.{field}")
    int_value(
        receipt["schemaVersion"],
        "receipt.schemaVersion",
        "R005_RECEIPT_SCHEMA",
        minimum=1,
    )
    for field in (
        "expectedDeviceUuid",
        "profileArtifact",
        "profileSha256",
        "ptxArtifact",
        "ptxSha256",
        "result",
        "sessionId",
        "sourceRevision",
    ):
        string_value(receipt[field], f"receipt.{field}", "R005_RECEIPT_SCHEMA")
    benchmark = object_value(
        receipt["benchmark"], "receipt.benchmark", "R005_RECEIPT_SCHEMA"
    )
    require_keys(
        benchmark, schema["benchmarkFields"], "receipt.benchmark", "R005_RECEIPT_SCHEMA"
    )
    for field, declared in schema["benchmarkTypes"].items():
        validate_declared_type(benchmark[field], declared, f"receipt.benchmark.{field}")
    string_value(
        benchmark["exactSemanticChecksum"],
        "receipt.benchmark.exactSemanticChecksum",
        "R005_RECEIPT_SCHEMA",
    )
    int_value(
        benchmark["rawSampleCount"],
        "receipt.benchmark.rawSampleCount",
        "R005_RECEIPT_SCHEMA",
        minimum=1,
    )
    observations = object_value(
        receipt["observations"], "receipt.observations", "R005_RECEIPT_SCHEMA"
    )
    require_keys(
        observations,
        schema["observationFields"],
        "receipt.observations",
        "R005_RECEIPT_SCHEMA",
    )
    for field, declared in schema["observationTypes"].items():
        validate_declared_type(
            observations[field], declared, f"receipt.observations.{field}"
        )
    for field in schema["observationFields"]:
        if field == "driverApiVersion":
            int_value(
                observations[field],
                f"receipt.observations.{field}",
                "R005_RECEIPT_SCHEMA",
                minimum=1,
            )
        else:
            string_value(
                observations[field],
                f"receipt.observations.{field}",
                "R005_RECEIPT_SCHEMA",
            )
    workflow = object_value(
        receipt["workflow"], "receipt.workflow", "R005_RECEIPT_SCHEMA"
    )
    require_keys(
        workflow, schema["workflowFields"], "receipt.workflow", "R005_RECEIPT_SCHEMA"
    )
    for field, declared in schema["workflowTypes"].items():
        validate_declared_type(workflow[field], declared, f"receipt.workflow.{field}")
    for field in ("job", "repository", "runId", "workflow"):
        string_value(
            workflow[field], f"receipt.workflow.{field}", "R005_RECEIPT_SCHEMA"
        )
    int_value(
        workflow["runAttempt"],
        "receipt.workflow.runAttempt",
        "R005_RECEIPT_SCHEMA",
        minimum=1,
    )
    records = array_value(receipt["records"], "receipt.records", "R005_RECEIPT_SCHEMA")
    for index, record_value in enumerate(records):
        record = object_value(
            record_value, f"receipt.records[{index}]", "R005_RECEIPT_SCHEMA"
        )
        require_keys(
            record,
            schema["recordFields"],
            f"receipt.records[{index}]",
            "R005_RECEIPT_SCHEMA",
        )
        for field, declared in schema["recordTypes"].items():
            validate_declared_type(
                record[field], declared, f"receipt.records[{index}].{field}"
            )
        for field in (
            "command",
            "commandSha256",
            "executable",
            "executableSha256",
            "kind",
            "log",
            "logSha256",
        ):
            string_value(
                record[field],
                f"receipt.records[{index}].{field}",
                "R005_RECEIPT_SCHEMA",
            )
        int_value(
            record["exitCode"],
            f"receipt.records[{index}].exitCode",
            "R005_RECEIPT_SCHEMA",
        )


def read_evidence_text(path: Path, code: str, label: str) -> str:
    raw = read_bytes(path, code, label)
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError as error:
        fail(code, f"{label} is not UTF-8: {error}")


def validate_receipt(
    receipt_path: Path,
    profile_path: Path,
    *,
    expected_revision: str | None = None,
    expected_session: str | None = None,
    expected_uuid: str | None = None,
) -> dict[str, Any]:
    profile, profile_sha = load_profile(profile_path)
    receipt, raw = read_json(
        receipt_path,
        "R001_RECEIPT_READ",
        "R002_RECEIPT_JSON",
        "R003_RECEIPT_ROOT_TYPE",
        "receipt",
        max_bytes=profile["receiptSchema"]["maxJsonBytes"],
        max_depth=profile["receiptSchema"]["maxJsonDepth"],
    )
    if raw != canonical_json(receipt):
        fail("R004_RECEIPT_CANONICAL", "receipt JSON is not in canonical sorted form")
    validate_receipt_types(receipt, profile)

    schema = profile["receiptSchema"]
    constraints = schema["constraints"]
    if (
        receipt["schemaVersion"] != schema["schemaVersion"]
        or receipt["result"] != constraints["result"]
    ):
        fail(
            "R006_RECEIPT_BINDING",
            "receipt schema version or result differs from the admitted values",
        )
    if (
        SHA256.fullmatch(receipt["profileSha256"]) is None
        or receipt["profileSha256"] != profile_sha
    ):
        fail("R006_RECEIPT_BINDING", "receipt profile digest differs from authority")
    if (
        SHA256.fullmatch(receipt["ptxSha256"]) is None
        or receipt["ptxSha256"] != profile["ptx"]["sha256"]
    ):
        fail("R006_RECEIPT_BINDING", "receipt PTX digest differs from authority")
    revision = receipt["sourceRevision"]
    session = receipt["sessionId"]
    if REVISION.fullmatch(revision) is None or SESSION.fullmatch(session) is None:
        fail(
            "R006_RECEIPT_BINDING",
            "receipt source revision or session identity is invalid",
        )
    workflow = receipt["workflow"]
    if (
        not workflow["runId"].isdigit()
        or workflow["repository"] != constraints["repository"]
        or workflow["workflow"] != constraints["workflow"]
        or workflow["job"] != constraints["job"]
    ):
        fail(
            "R006_RECEIPT_BINDING",
            "workflow identity differs from the protected workflow",
        )
    expected_session_value = (
        f"github-{workflow['runId']}-{workflow['runAttempt']}-{revision}"
    )
    if session != expected_session_value:
        fail(
            "R006_RECEIPT_BINDING",
            "session identity does not bind workflow run, attempt, and source revision",
        )
    if expected_revision is not None and revision != expected_revision:
        fail(
            "R006_RECEIPT_BINDING",
            "receipt source revision differs from requested revision",
        )
    if expected_session is not None and session != expected_session:
        fail("R006_RECEIPT_BINDING", "receipt session differs from requested session")

    expected_device = normalized_uuid(
        receipt["expectedDeviceUuid"],
        "R007_RECEIPT_OBSERVATION",
        "expected device UUID",
    )
    observations = receipt["observations"]
    native_device = normalized_uuid(
        observations["nativeObservedDeviceUuid"],
        "R007_RECEIPT_OBSERVATION",
        "native-observed device UUID",
    )
    smi_device = normalized_uuid(
        observations["nvidiaSmiDeviceUuid"],
        "R007_RECEIPT_OBSERVATION",
        "nvidia-smi device UUID",
    )
    if native_device != expected_device or smi_device != expected_device:
        fail(
            "R007_RECEIPT_OBSERVATION",
            "configured, nvidia-smi, and native-observed UUIDs differ",
        )
    if expected_uuid is not None and expected_device != normalized_uuid(
        expected_uuid, "R007_RECEIPT_OBSERVATION", "requested UUID"
    ):
        fail(
            "R007_RECEIPT_OBSERVATION",
            "receipt device UUID differs from requested UUID",
        )
    if (
        VERSION.fullmatch(observations["driverVersion"]) is None
        or VERSION.fullmatch(observations["sanitizerVersion"]) is None
    ):
        fail("R007_RECEIPT_OBSERVATION", "driver or sanitizer version is invalid")
    if observations["toolkitVersion"] != profile["cudaAbi"]["toolkitRelease"]:
        fail(
            "R007_RECEIPT_OBSERVATION", "observed toolkit version differs from profile"
        )

    root = receipt_path.parent
    observation_logs = [
        ("deviceQueryLog", "deviceQueryLogSha256", "device query"),
        ("toolkitLog", "toolkitLogSha256", "toolkit version"),
        ("sanitizerLog", "sanitizerLogSha256", "sanitizer version"),
    ]
    observation_text: dict[str, str] = {}
    for log_field, digest_field, label in observation_logs:
        digest = observations[digest_field]
        if SHA256.fullmatch(digest) is None:
            fail("R007_RECEIPT_OBSERVATION", f"{label} log digest is invalid")
        path = safe_file(
            root,
            observations[log_field],
            "R007_RECEIPT_OBSERVATION",
            f"{label} log",
            schema["maxLogBytes"],
        )
        if sha256_file(path, "R007_RECEIPT_OBSERVATION") != digest:
            fail("R007_RECEIPT_OBSERVATION", f"{label} log digest differs")
        observation_text[log_field] = read_evidence_text(
            path, "R007_RECEIPT_OBSERVATION", f"{label} log"
        )
    device_log = observation_text["deviceQueryLog"]
    raw_device = f"device-query-raw: {observations['nvidiaSmiDeviceUuid']}, {observations['driverVersion']}"
    if (
        raw_device not in device_log
        or f"device-query-uuid: {observations['nvidiaSmiDeviceUuid']}" not in device_log
        or f"device-query-driver-version: {observations['driverVersion']}"
        not in device_log
    ):
        fail(
            "R007_RECEIPT_OBSERVATION",
            "device query log does not retain and bind observed UUID and driver",
        )
    if (
        f"release {observations['toolkitVersion']}"
        not in observation_text["toolkitLog"]
    ):
        fail(
            "R007_RECEIPT_OBSERVATION",
            "nvcc log does not contain the observed toolkit release",
        )
    if (
        observations["sanitizerVersion"] not in observation_text["sanitizerLog"]
        or "Compute Sanitizer" not in observation_text["sanitizerLog"]
    ):
        fail(
            "R007_RECEIPT_OBSERVATION",
            "sanitizer log does not contain the observed version",
        )

    records = receipt["records"]
    specs = schema["requiredRecords"]
    if len(records) != len(specs):
        fail("R008_RECEIPT_RECORD", "receipt record count differs from profile")
    for index, (record, spec) in enumerate(zip(records, specs, strict=True)):
        if (
            record["kind"] != spec["kind"]
            or record["command"] != spec["command"]
            or record["executable"] != spec["executable"]
        ):
            fail(
                "R008_RECEIPT_RECORD",
                f"record {index} kind, command, or executable differs from profile",
            )
        expected_command_digest = sha256_bytes(record["command"].encode())
        if (
            SHA256.fullmatch(record["commandSha256"]) is None
            or record["commandSha256"] != expected_command_digest
        ):
            fail("R008_RECEIPT_RECORD", f"record {index} command digest differs")
        if record["exitCode"] != 0:
            fail("R008_RECEIPT_RECORD", f"record {index} exit code is not zero")
        if (
            SHA256.fullmatch(record["executableSha256"]) is None
            or SHA256.fullmatch(record["logSha256"]) is None
        ):
            fail("R008_RECEIPT_RECORD", f"record {index} has an invalid digest")

    for field, expected_name, expected_digest in (
        ("profileArtifact", "profile.json", profile_sha),
        ("ptxArtifact", "markovian_dense.ptx", profile["ptx"]["sha256"]),
    ):
        if receipt[field] != expected_name:
            fail("R009_RECEIPT_ARTIFACT", f"{field} name differs from profile")
        path = safe_file(
            root,
            receipt[field],
            "R009_RECEIPT_ARTIFACT",
            f"{field} artifact",
            schema["maxExecutableBytes"],
        )
        if sha256_file(path, "R009_RECEIPT_ARTIFACT") != expected_digest:
            fail("R009_RECEIPT_ARTIFACT", f"{field} artifact digest differs")

    artifact_digests: dict[str, str] = {}
    for spec, record in zip(specs, records):
        name = spec["executable"]
        path = safe_file(
            root,
            name,
            "R009_RECEIPT_ARTIFACT",
            f"{name} artifact",
            schema["maxExecutableBytes"],
        )
        try:
            mode = path.stat(follow_symlinks=False).st_mode
        except OSError as error:
            fail("R009_RECEIPT_ARTIFACT", f"cannot inspect {name}: {error}")
        if mode & stat.S_IXUSR == 0:
            fail("R009_RECEIPT_ARTIFACT", f"{name} is not executable")
        digest = sha256_file(path, "R009_RECEIPT_ARTIFACT")
        previous = artifact_digests.setdefault(name, digest)
        if previous != digest or record["executableSha256"] != digest:
            fail("R009_RECEIPT_ARTIFACT", f"{name} executable digest differs")

    record_texts: dict[str, str] = {}
    for spec, record in zip(specs, records):
        expected_log = f"{spec['kind']}.log"
        if record["log"] != expected_log:
            fail("R010_RECEIPT_LOG", f"{spec['kind']} log name differs from profile")
        path = safe_file(
            root,
            record["log"],
            "R010_RECEIPT_LOG",
            f"{spec['kind']} log",
            schema["maxLogBytes"],
        )
        if sha256_file(path, "R010_RECEIPT_LOG") != record["logSha256"]:
            fail("R010_RECEIPT_LOG", f"{spec['kind']} log digest differs")
        record_texts[spec["kind"]] = read_evidence_text(
            path, "R010_RECEIPT_LOG", f"{spec['kind']} log"
        )

    common_markers = [
        f"evidence-session-id: {session}",
        f"source-revision: {revision}",
        f"profile-sha256: {profile_sha}",
        f"evidence-device-uuid: {receipt['expectedDeviceUuid']}",
        f"evidence-driver-version: {observations['driverVersion']}",
        f"evidence-toolkit-version: {observations['toolkitVersion']}",
        f"native-observed-device-uuid: {observations['nativeObservedDeviceUuid']}",
        f"native-observed-driver-api-version: {observations['driverApiVersion']}",
    ]
    for spec, record in zip(specs, records):
        text = record_texts[spec["kind"]]
        markers = (
            common_markers
            + [
                f"evidence-record-kind: {spec['kind']}",
                f"evidence-command: {record['command']}",
                f"evidence-command-sha256: {record['commandSha256']}",
                f"evidence-executable-sha256: {record['executableSha256']}",
                "evidence-record-exit: 0",
            ]
            + spec["successMarkers"]
        )
        if any(marker not in text for marker in markers):
            fail(
                "R011_RECEIPT_OUTCOME",
                f"{spec['kind']} log lacks a required command, binding, exit, or success marker",
            )

    benchmark = receipt["benchmark"]
    fixture = profile["benchmarkFixture"]
    if (
        benchmark["exactSemanticChecksum"] != fixture["exactSemanticChecksum"]
        or benchmark["rawSampleCount"] != fixture["measuredSampleCount"]
    ):
        fail(
            "R012_RECEIPT_BENCHMARK",
            "benchmark checksum or sample count differs from profile",
        )
    text = record_texts["benchmark"]
    if (
        f"cuda-transfer-inclusive warmups: {fixture['warmupCount']} (excluded)"
        not in text
        or f"cuda-transfer-inclusive measured samples: {fixture['measuredSampleCount']}"
        not in text
    ):
        fail(
            "R012_RECEIPT_BENCHMARK",
            "benchmark warmup or measured count marker differs",
        )
    sample_lines = [
        line
        for line in text.splitlines()
        if line.startswith("cuda-transfer-inclusive sample ")
    ]
    sample_pattern = re.compile(schema["constraints"]["benchmarkSamplePattern"])
    parsed = [sample_pattern.fullmatch(line) for line in sample_lines]
    if (
        any(match is None for match in parsed)
        or len(parsed) != fixture["measuredSampleCount"]
    ):
        fail(
            "R012_RECEIPT_BENCHMARK",
            "benchmark does not contain exactly the required parseable sample lines",
        )
    indices = [int(match.group(1)) for match in parsed if match is not None]
    values = [float(match.group(2)) for match in parsed if match is not None]
    if indices != list(range(1, fixture["measuredSampleCount"] + 1)) or any(
        not math.isfinite(value) or value < 0 for value in values
    ):
        fail(
            "R012_RECEIPT_BENCHMARK",
            "benchmark sample indices are not ordered and unique or a value is not finite and nonnegative",
        )
    return receipt


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parent.parent
    )
    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("check")
    subparsers.add_parser("generate")
    subparsers.add_parser("digest")
    receipt = subparsers.add_parser("validate-receipt")
    receipt.add_argument("receipt", type=Path)
    receipt.add_argument("--expected-revision")
    receipt.add_argument("--expected-session")
    receipt.add_argument("--expected-uuid")
    args = parser.parse_args(argv)
    root = args.root.resolve()
    try:
        if args.command == "generate":
            profile, profile_sha = load_profile(root / "profile.json")
            check_consumers(root, profile)
            for path, text in generated_files(root, profile, profile_sha).items():
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(text, encoding="utf-8")
            print(f"generated CUDA profile artifacts for {profile_sha}")
        elif args.command == "check":
            print(f"CUDA profile authority validated: {check_profile(root)}")
        elif args.command == "digest":
            print(load_profile(root / "profile.json")[1])
        else:
            validate_receipt(
                args.receipt.resolve(),
                root / "profile.json",
                expected_revision=args.expected_revision,
                expected_session=args.expected_session,
                expected_uuid=args.expected_uuid,
            )
            print(f"CUDA same-session receipt validated: {args.receipt}")
    except ProfileError as error:
        print(f"CUDA profile error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
