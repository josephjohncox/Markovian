#!/usr/bin/env python3
"""Negative fixtures for the capability truth/presentation boundary."""

import copy
import json
import shutil
import tempfile
import unittest
from importlib.machinery import SourceFileLoader
from importlib.util import module_from_spec, spec_from_loader
from pathlib import Path

loader = SourceFileLoader("capabilities", str(Path(__file__).with_name("check-capabilities")))
spec = spec_from_loader(loader.name, loader)
assert spec is not None
cap = module_from_spec(spec)
loader.exec_module(cap)


class CapabilityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.current, cls.released = cap.surfaces(cap.ROOT)
        cls.document = json.loads((cap.ROOT / cap.RECORDS).read_text())

    def changed(self, index=0, **changes):
        document = copy.deepcopy(self.document)
        document["capabilities"][index].update(changes)
        return document

    def reward_contract(self, **changes):
        document = self.changed(7, availability="unimplemented", evidenceScope="contract-only",
                                evidence=cap.PROPOSAL_CONTRACTS["EL-04"])
        record = document["capabilities"][7]
        record.pop("module", None)
        record["plannedModule"] = "Markovian.Feedback.Value.Exact"
        record.update(changes)
        return document

    def rejected(self, document, message):
        with self.assertRaisesRegex(cap.CapabilityError, message):
            cap.validate(cap.ROOT, document, self.current, self.released)

    def test_current_records_and_generated_presentation(self):
        self.assertEqual(cap.check(cap.ROOT), 9)

    def test_proposed_implementation_is_not_unimplemented(self):
        records = cap.validate(cap.ROOT, self.document, self.current, self.released)
        self.assertEqual(records[5]["availability"], "unreleased")
        self.assertEqual(records[5]["decisionStatus"], "Proposed")

    def test_paired_proposal_implementation_transition(self):
        record = cap.validate(cap.ROOT, self.document, self.current, self.released)[6]
        self.assertEqual(record["availability"], "unreleased")
        self.assertEqual(record["decisionStatus"], "Proposed")
        self.assertEqual(record["evidenceScope"], "implementation-fixtures")

    def test_reward_jvp_proposal_implementation_transition(self):
        record = cap.validate(cap.ROOT, self.document, self.current, self.released)[7]
        self.assertEqual(record["availability"], "unreleased")
        self.assertEqual(record["decisionStatus"], "Proposed")
        self.assertEqual(record["evidence"], "test/FeedbackRewardJVP.hs")
        self.assertEqual(record["evidenceScope"], "implementation-fixtures")

    def test_aggregation_proposal_implementation_transition(self):
        record = cap.validate(cap.ROOT, self.document, self.current, self.released)[8]
        self.assertEqual(record["availability"], "unreleased")
        self.assertEqual(record["decisionStatus"], "Proposed")
        self.assertEqual(record["module"], "Markovian.Aggregation.Exact")
        self.assertEqual(record["evidence"], "test/AggregationExact.hs")
        self.assertEqual(record["evidenceScope"], "implementation-fixtures")

    def test_aggregation_cannot_claim_release(self):
        self.rejected(self.changed(8, availability="released", evidenceScope="bounded-release"),
                      "not in immutable released membership")

    def test_aggregation_cannot_reuse_contract_evidence(self):
        self.rejected(self.changed(8, evidence=cap.PROPOSAL_CONTRACTS["EL-05"]),
                      "requires implementation evidence")

    def test_reward_jvp_cannot_claim_release(self):
        self.rejected(self.changed(7, availability="released", evidenceScope="bounded-release"),
                      "not in immutable released membership")

    def test_reward_jvp_cannot_reuse_contract_evidence(self):
        self.rejected(self.changed(7, evidence=cap.PROPOSAL_CONTRACTS["EL-04"]),
                      "requires implementation evidence")

    def test_implemented_proposal_cannot_claim_release(self):
        self.rejected(self.changed(6, availability="released", evidenceScope="bounded-release"),
                      "invalid proposal")

    def test_implemented_proposal_cannot_claim_acceptance(self):
        self.rejected(self.changed(6, decisionStatus="Accepted"), "invalid proposal")

    def test_implemented_proposal_cannot_reuse_contract_evidence(self):
        self.rejected(self.changed(6, evidence=cap.PROPOSAL_CONTRACTS["EL-03"]),
                      "requires implementation evidence")

    def test_implemented_proposal_still_checks_contract_module(self):
        self.rejected(self.changed(6, module="Markovian.Continuous.Space"),
                      "contract/module mismatch")

    def test_contract_only_requires_contract_evidence(self):
        self.rejected(self.reward_contract(evidence="test/FeedbackValueExact.hs"),
                      "must name the proposal contract")

    def test_missing_implemented_proposal_contract(self):
        with tempfile.TemporaryDirectory() as directory:
            root = self.fixture(directory)
            (root / cap.PROPOSAL_CONTRACTS["EL-03"]).unlink()
            with self.assertRaisesRegex(cap.CapabilityError, "missing or escaping"):
                cap.check(root)

    def test_unknown_availability(self):
        self.rejected(self.changed(availability="ready"), "invalid availability")

    def test_contract_only_cannot_claim_release_evidence(self):
        self.rejected(self.reward_contract(evidenceScope="bounded-release"), "combination")

    def test_unimplemented_cannot_claim_accepted(self):
        self.rejected(self.reward_contract(decisionStatus="Accepted"), "invalid proposal")

    def test_stale_decision_status(self):
        self.rejected(self.changed(decisionStatus="Proposed"), "status mismatch")

    def test_unknown_package(self):
        self.rejected(self.changed(package="markovian-imaginary"), "unknown package")

    def test_wrong_package_module(self):
        self.rejected(self.changed(package="Markovian"), "not a current public module")

    def test_unknown_module(self):
        self.rejected(self.changed(module="Markovian.NoSuchModule"), "not a current public module")

    def test_current_module_does_not_imply_released_module(self):
        self.rejected(self.changed(5, availability="released", evidenceScope="bounded-release"),
                      "not in immutable released membership")

    def test_no_inference_from_unchanged_package_version(self):
        self.assertIn("Markovian.Feedback.Value.Exact", self.current["Markovian"])
        self.assertNotIn("Markovian.Feedback.Value.Exact", self.released["Markovian"])

    def test_missing_evidence(self):
        self.rejected(self.changed(evidence="test/missing.hs"), "missing or escaping")

    def test_escaping_evidence(self):
        self.rejected(self.changed(evidence="../README.md"), "unsafe reference")

    def test_duplicate_record(self):
        document = copy.deepcopy(self.document)
        document["capabilities"].append(document["capabilities"][0])
        self.rejected(document, "duplicate capability")

    def test_invalid_planned_placement(self):
        self.rejected(self.reward_contract(plannedModule="Markovian.NoSuchModule"), "contract/module mismatch")

    def test_implemented_field_cannot_disguise_unimplemented_placement(self):
        self.rejected(self.reward_contract(module="Markovian.Feedback.Value.Exact"), "invalid record fields")

    def fixture(self, directory):
        root = Path(directory)
        files = [cap.RECORDS, cap.RELEASE_EVIDENCE, cap.OUTPUT,
                 Path("docs/DECISIONS.md"), Path("release/packages.tsv"),
                 Path("release/published-releases.json")]
        files += [Path(r["evidence"]) for r in self.document["capabilities"]]
        files += [Path(p) for p in cap.PROPOSAL_CONTRACTS.values()]
        files += [p.relative_to(cap.ROOT) for p in (cap.ROOT / "release/exposed-modules").glob("*.txt")]
        for package in cap.parse_manifest(cap.ROOT / "release/packages.tsv"):
            files.append(cap.one_cabal_file(cap.ROOT / package.directory).relative_to(cap.ROOT))
        for file in set(files):
            (root / file).parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(cap.ROOT / file, root / file)
        return root

    def test_stale_generated_output(self):
        with tempfile.TemporaryDirectory() as directory:
            root = self.fixture(directory)
            (root / cap.OUTPUT).write_text("stale\n")
            with self.assertRaisesRegex(cap.CapabilityError, "stale capability presentation"):
                cap.check(root)

    def test_current_snapshot_drift(self):
        with tempfile.TemporaryDirectory() as directory:
            root = self.fixture(directory)
            (root / "release/exposed-modules/Markovian.txt").write_text("Markovian.NoSuchModule\n")
            with self.assertRaisesRegex(cap.CapabilityError, "stale current module snapshot"):
                cap.check(root)

    def test_mutable_current_snapshot_cannot_replace_release_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            root = self.fixture(directory)
            evidence = json.loads((root / cap.RELEASE_EVIDENCE).read_text())
            evidence["modules"]["Markovian"] = sorted(self.current["Markovian"])
            (root / cap.RELEASE_EVIDENCE).write_text(json.dumps(evidence))
            with self.assertRaisesRegex(cap.CapabilityError, "immutable release membership evidence changed"):
                cap.check(root)

    def test_archive_only_gate_needs_no_git(self):
        with tempfile.TemporaryDirectory() as directory:
            root = self.fixture(directory)
            self.assertFalse((root / ".git").exists())
            self.assertEqual(cap.check(root), 9)


if __name__ == "__main__":
    unittest.main()
