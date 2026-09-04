#!/usr/bin/env python3
from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

PACKAGE_ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "cuda_profile", PACKAGE_ROOT / "scripts/cuda_profile.py"
)
assert SPEC is not None and SPEC.loader is not None
cuda_profile = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = cuda_profile
SPEC.loader.exec_module(cuda_profile)


class CUDAProfileTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.profile_path = PACKAGE_ROOT / "profile.json"
        self.profile, self.profile_sha = cuda_profile.load_profile(self.profile_path)
        self.revision = "a" * 40
        self.run_id = "123"
        self.run_attempt = 1
        self.session = f"github-{self.run_id}-{self.run_attempt}-{self.revision}"
        self.uuid = "GPU-ac353d74-ffaf-96d2-7849-b8d03d5cd1a7"
        self.driver = "580.173.02"
        self.toolkit = "13.0"
        self.sanitizer = "2026.1.0"
        self.driver_api = 13000
        (self.root / "profile.json").write_bytes(self.profile_path.read_bytes())
        (self.root / "markovian_dense.ptx").write_bytes(
            (PACKAGE_ROOT / "cbits/markovian_dense.ptx").read_bytes()
        )
        self.test_digest = self.write_executable("test-executable", "test")
        self.benchmark_digest = self.write_executable(
            "benchmark-executable", "benchmark"
        )
        self.write_observation_logs()
        self.records = self.write_record_logs()
        self.receipt = {
            "benchmark": {
                "exactSemanticChecksum": "49439/128",
                "rawSampleCount": 20,
            },
            "expectedDeviceUuid": self.uuid,
            "observations": {
                "deviceQueryLog": "device-query.log",
                "deviceQueryLogSha256": self.digest("device-query.log"),
                "driverApiVersion": self.driver_api,
                "driverVersion": self.driver,
                "nativeObservedDeviceUuid": self.uuid,
                "nvidiaSmiDeviceUuid": self.uuid,
                "sanitizerLog": "sanitizer-version.log",
                "sanitizerLogSha256": self.digest("sanitizer-version.log"),
                "sanitizerVersion": self.sanitizer,
                "toolkitLog": "toolkit-version.log",
                "toolkitLogSha256": self.digest("toolkit-version.log"),
                "toolkitVersion": self.toolkit,
            },
            "profileArtifact": "profile.json",
            "profileSha256": self.profile_sha,
            "ptxArtifact": "markovian_dense.ptx",
            "ptxSha256": self.profile["ptx"]["sha256"],
            "records": self.records,
            "result": "passed",
            "schemaVersion": 2,
            "sessionId": self.session,
            "sourceRevision": self.revision,
            "workflow": {
                "job": "cuda-hardware",
                "repository": "josephjohncox/Markovian",
                "runAttempt": self.run_attempt,
                "runId": self.run_id,
                "workflow": ".github/workflows/cuda-hardware.yml",
            },
        }
        self.receipt_path = self.root / "receipt.json"
        self.write_receipt(self.receipt)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def write_executable(self, name: str, marker: str) -> str:
        path = self.root / name
        path.write_text(f"#!/bin/sh\nprintf '%s\\n' {marker}\n", encoding="utf-8")
        path.chmod(0o755)
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def digest(self, name: str) -> str:
        return hashlib.sha256((self.root / name).read_bytes()).hexdigest()

    def write_observation_logs(self) -> None:
        (self.root / "device-query.log").write_text(
            f"device-query-raw: {self.uuid}, {self.driver}\n"
            f"device-query-uuid: {self.uuid}\n"
            f"device-query-driver-version: {self.driver}\n",
            encoding="utf-8",
        )
        (self.root / "toolkit-version.log").write_text(
            "Cuda compilation tools, release 13.0, V13.0.88\n", encoding="utf-8"
        )
        (self.root / "sanitizer-version.log").write_text(
            f"Compute Sanitizer version {self.sanitizer}\n", encoding="utf-8"
        )

    def common_markers(self) -> list[str]:
        return [
            f"evidence-session-id: {self.session}",
            f"source-revision: {self.revision}",
            f"profile-sha256: {self.profile_sha}",
            f"evidence-device-uuid: {self.uuid}",
            f"evidence-driver-version: {self.driver}",
            f"evidence-toolkit-version: {self.toolkit}",
            f"native-observed-device-uuid: {self.uuid}",
            f"native-observed-driver-api-version: {self.driver_api}",
        ]

    def write_record_logs(self) -> list[dict[str, object]]:
        records: list[dict[str, object]] = []
        for spec in self.profile["receiptSchema"]["requiredRecords"]:
            kind = spec["kind"]
            command = spec["command"]
            command_digest = hashlib.sha256(command.encode()).hexdigest()
            executable_digest = (
                self.benchmark_digest if kind == "benchmark" else self.test_digest
            )
            lines = self.common_markers() + [
                f"evidence-record-kind: {kind}",
                f"evidence-command: {command}",
                f"evidence-command-sha256: {command_digest}",
                f"evidence-executable-sha256: {executable_digest}",
                "evidence-record-exit: 0",
                *spec["successMarkers"],
            ]
            if kind == "benchmark":
                lines.extend(
                    [
                        "cuda-transfer-inclusive warmups: 1 (excluded)",
                        "cuda-transfer-inclusive measured samples: 20",
                        *[
                            f"cuda-transfer-inclusive sample {sample:02d}: 0.250000000 ms"
                            for sample in range(1, 21)
                        ],
                    ]
                )
            log = f"{kind}.log"
            (self.root / log).write_text("\n".join(lines) + "\n", encoding="utf-8")
            records.append(
                {
                    "command": command,
                    "commandSha256": command_digest,
                    "executable": spec["executable"],
                    "executableSha256": executable_digest,
                    "exitCode": 0,
                    "kind": kind,
                    "log": log,
                    "logSha256": self.digest(log),
                }
            )
        return records

    def write_receipt(self, value: object) -> None:
        self.receipt_path.write_text(
            json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )

    def validate(self) -> dict[str, object]:
        return cuda_profile.validate_receipt(
            self.receipt_path,
            self.profile_path,
            expected_revision=self.revision,
            expected_session=self.session,
            expected_uuid=self.uuid,
        )

    def expect_code(self, value: object, code: str) -> None:
        self.write_receipt(value)
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            self.validate()
        self.assertEqual(caught.exception.code, code)

    def test_authority_and_artifact_bound_same_session_receipt_pass(self) -> None:
        self.assertRegex(cuda_profile.check_profile(PACKAGE_ROOT), r"^[0-9a-f]{64}$")
        self.assertEqual(self.validate()["result"], "passed")

    def test_every_authoritative_field_class_rejects_mutation(self) -> None:
        mutations = [
            (
                "numeric tolerance",
                lambda v: v["numericPolicy"]["cpu"].__setitem__(
                    "comparisonAbsoluteTolerance", "999"
                ),
            ),
            (
                "exact equation",
                lambda v: v["numericPolicy"]["exact"].__setitem__("matmul", "nonsense"),
            ),
            (
                "kernel index",
                lambda v: v["kernelAbi"].__setitem__("indexType", "unsigned-int7"),
            ),
            ("kernel width", lambda v: v["kernelAbi"].__setitem__("blockThreads", 99)),
            (
                "benchmark count",
                lambda v: v["benchmarkFixture"].__setitem__("measuredSampleCount", 99),
            ),
            (
                "benchmark warmup",
                lambda v: v["benchmarkFixture"].__setitem__("warmupCount", 42),
            ),
            (
                "benchmark input",
                lambda v: v["benchmarkFixture"].__setitem__("leftInput", "zeros"),
            ),
            ("admission schedule", lambda v: v["admissionTests"].reverse()),
            ("receipt fields", lambda v: v["receiptSchema"]["recordFields"].pop()),
            (
                "receipt type authority",
                lambda v: v["receiptSchema"]["recordTypes"].__setitem__(
                    "exitCode", "string"
                ),
            ),
            (
                "receipt constraints",
                lambda v: v["receiptSchema"]["constraints"].__setitem__(
                    "result", "maybe"
                ),
            ),
            (
                "receipt JSON depth",
                lambda v: v["receiptSchema"].__setitem__("maxJsonDepth", 127),
            ),
            (
                "failure precedence",
                lambda v: v["validationFailurePrecedence"]["receipt"].reverse(),
            ),
        ]
        for label, mutate in mutations:
            with self.subTest(label=label):
                value = copy.deepcopy(self.profile)
                mutate(value)
                path = self.root / "mutated-profile.json"
                path.write_text(
                    json.dumps(value, indent=2, sort_keys=True) + "\n",
                    encoding="utf-8",
                )
                with self.assertRaises(cuda_profile.ProfileError) as caught:
                    cuda_profile.load_profile(path)
                self.assertIn(
                    caught.exception.code, {"P005_PROFILE_SCHEMA", "P006_PROFILE_VALUE"}
                )

    def test_profile_wrong_types_are_stable_schema_failures(self) -> None:
        mutations = [
            lambda v: v.__setitem__("numericPolicy", []),
            lambda v: v.__setitem__("admissionTests", {}),
            lambda v: v.__setitem__("schemaVersion", "1"),
            lambda v: v["ptx"].__setitem__("sha256", 7),
            lambda v: v["receiptSchema"].__setitem__("requiredRecords", "records"),
            lambda v: v["receiptSchema"].__setitem__("topLevelTypes", []),
            lambda v: v["receiptSchema"].__setitem__("maxJsonDepth", False),
            lambda v: v["receiptSchema"]["recordTypes"].__setitem__("exitCode", False),
        ]
        for mutate in mutations:
            value = copy.deepcopy(self.profile)
            mutate(value)
            path = self.root / "wrong-profile.json"
            path.write_text(
                json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            with self.assertRaises(cuda_profile.ProfileError) as caught:
                cuda_profile.load_profile(path)
            self.assertEqual(caught.exception.code, "P005_PROFILE_SCHEMA")

    def test_receipt_wrong_nested_types_never_escape_profile_error(self) -> None:
        mutations = [
            lambda v: v.__setitem__("benchmark", []),
            lambda v: v.__setitem__("observations", []),
            lambda v: v.__setitem__("workflow", []),
            lambda v: v.__setitem__("records", {}),
            lambda v: v["records"].__setitem__(0, "record"),
            lambda v: v.__setitem__("expectedDeviceUuid", 7),
            lambda v: v.__setitem__("profileSha256", 7),
            lambda v: v["benchmark"].__setitem__("rawSampleCount", "20"),
            lambda v: v["records"][0].__setitem__("exitCode", "0"),
            lambda v: v["records"][0].__setitem__("command", 7),
            lambda v: v["observations"].__setitem__("driverApiVersion", "13000"),
        ]
        for index, mutate in enumerate(mutations):
            with self.subTest(index=index):
                value = copy.deepcopy(self.receipt)
                mutate(value)
                self.expect_code(value, "R005_RECEIPT_SCHEMA")

    def test_profile_drift_and_cross_session_receipts_fail(self) -> None:
        value = copy.deepcopy(self.receipt)
        value["profileSha256"] = "0" * 64
        self.expect_code(value, "R006_RECEIPT_BINDING")
        value = copy.deepcopy(self.receipt)
        value["sessionId"] = f"github-999-1-{self.revision}"
        self.expect_code(value, "R006_RECEIPT_BINDING")

    def test_forged_command_digest_and_binding_only_logs_fail(self) -> None:
        value = copy.deepcopy(self.receipt)
        value["records"][0]["command"] = "this command never ran"
        value["records"][0]["commandSha256"] = hashlib.sha256(
            b"this command never ran"
        ).hexdigest()
        self.expect_code(value, "R008_RECEIPT_RECORD")

        log = self.root / "correctness.log"
        log.write_text("\n".join(self.common_markers()) + "\n", encoding="utf-8")
        value = copy.deepcopy(self.receipt)
        value["records"][0]["logSha256"] = self.digest("correctness.log")
        self.expect_code(value, "R011_RECEIPT_OUTCOME")

    def test_synthetic_or_substituted_executable_fails(self) -> None:
        value = copy.deepcopy(self.receipt)
        value["records"][0]["executableSha256"] = "d" * 64
        self.expect_code(value, "R009_RECEIPT_ARTIFACT")
        (self.root / "test-executable").write_text("changed\n", encoding="utf-8")
        (self.root / "test-executable").chmod(0o755)
        self.write_receipt(self.receipt)
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            self.validate()
        self.assertEqual(caught.exception.code, "R009_RECEIPT_ARTIFACT")

    def test_required_success_and_exit_markers_fail_closed(self) -> None:
        log = self.root / "sanitizer-memcheck.log"
        text = log.read_text(encoding="utf-8").replace("ERROR SUMMARY: 0 errors\n", "")
        log.write_text(text, encoding="utf-8")
        value = copy.deepcopy(self.receipt)
        value["records"][1]["logSha256"] = self.digest("sanitizer-memcheck.log")
        self.expect_code(value, "R011_RECEIPT_OUTCOME")

    def test_duplicate_or_nonfinite_benchmark_samples_fail(self) -> None:
        log = self.root / "benchmark.log"
        original = log.read_text(encoding="utf-8")
        changed = original.replace(
            "cuda-transfer-inclusive sample 20: 0.250000000 ms",
            "cuda-transfer-inclusive sample 01: 0.250000000 ms",
        )
        log.write_text(changed, encoding="utf-8")
        value = copy.deepcopy(self.receipt)
        value["records"][-1]["logSha256"] = self.digest("benchmark.log")
        self.expect_code(value, "R012_RECEIPT_BENCHMARK")

        log.write_text(
            original.replace("0.250000000 ms", "nan ms", 1), encoding="utf-8"
        )
        value = copy.deepcopy(self.receipt)
        value["records"][-1]["logSha256"] = self.digest("benchmark.log")
        self.expect_code(value, "R012_RECEIPT_BENCHMARK")

        log.write_text(
            original.replace("cuda-transfer-inclusive sample 20: 0.250000000 ms\n", ""),
            encoding="utf-8",
        )
        value = copy.deepcopy(self.receipt)
        value["records"][-1]["logSha256"] = self.digest("benchmark.log")
        self.expect_code(value, "R012_RECEIPT_BENCHMARK")

    def test_native_observation_and_tool_logs_must_correlate(self) -> None:
        value = copy.deepcopy(self.receipt)
        value["observations"]["nativeObservedDeviceUuid"] = (
            "GPU-00000000-0000-0000-0000-000000000000"
        )
        self.expect_code(value, "R007_RECEIPT_OBSERVATION")
        value = copy.deepcopy(self.receipt)
        value["observations"]["toolkitVersion"] = "99.0"
        self.expect_code(value, "R007_RECEIPT_OBSERVATION")

    def test_failure_precedence_is_frozen_and_malformed_input_is_stable(self) -> None:
        self.assertEqual(
            self.profile["validationFailurePrecedence"]["profile"],
            cuda_profile.PROFILE_PRECEDENCE,
        )
        self.assertEqual(
            self.profile["validationFailurePrecedence"]["receipt"],
            cuda_profile.RECEIPT_PRECEDENCE,
        )
        self.receipt_path.write_text("{", encoding="utf-8")
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            self.validate()
        self.assertEqual(caught.exception.code, "R002_RECEIPT_JSON")

    def test_json_size_and_nesting_limits_fail_closed(self) -> None:
        bounded_profile = self.root / "bounded-profile.json"
        bounded_profile.write_bytes(b" " * cuda_profile.MAX_PROFILE_JSON_BYTES)
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            cuda_profile.load_profile(bounded_profile)
        self.assertEqual(caught.exception.code, "P002_PROFILE_JSON")
        bounded_profile.write_bytes(b" " * (cuda_profile.MAX_PROFILE_JSON_BYTES + 1))
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            cuda_profile.load_profile(bounded_profile)
        self.assertEqual(caught.exception.code, "P001_PROFILE_READ")

        max_receipt_bytes = self.profile["receiptSchema"]["maxJsonBytes"]
        self.receipt_path.write_bytes(b" " * max_receipt_bytes)
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            self.validate()
        self.assertEqual(caught.exception.code, "R002_RECEIPT_JSON")
        self.receipt_path.write_bytes(b" " * (max_receipt_bytes + 1))
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            self.validate()
        self.assertEqual(caught.exception.code, "R001_RECEIPT_READ")

        max_receipt_depth = self.profile["receiptSchema"]["maxJsonDepth"]
        exact_depth = (
            '{"x":'
            + "[" * (max_receipt_depth - 1)
            + "0"
            + "]" * (max_receipt_depth - 1)
            + "}\n"
        )
        self.receipt_path.write_text(exact_depth, encoding="utf-8")
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            self.validate()
        self.assertEqual(caught.exception.code, "R004_RECEIPT_CANONICAL")

        excessive_depth = (
            '{"x":' + "[" * max_receipt_depth + "0" + "]" * max_receipt_depth + "}\n"
        )
        self.receipt_path.write_text(excessive_depth, encoding="utf-8")
        with self.assertRaises(cuda_profile.ProfileError) as caught:
            self.validate()
        self.assertEqual(caught.exception.code, "R002_RECEIPT_JSON")


if __name__ == "__main__":
    unittest.main()
