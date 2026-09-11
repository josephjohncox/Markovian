#!/usr/bin/env python3
"""Negative fixtures for the capability truth/presentation boundary."""

import copy
import hashlib
import json
import re
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

    def test_cuda_receipt_matches_accepted_decision(self):
        decisions = (cap.ROOT / "docs/DECISIONS.md").read_text()
        receipt = (cap.ROOT / "docs/evidence/CUDA-D077-RECEIPTS.md").read_text()
        status = re.search(r"(?ms)^### D-077:.*?^\*\*Status:\*\* ([^\n]+)$", decisions)
        assert status is not None
        self.assertEqual(status[1], "Accepted")
        self.assertEqual(re.findall(r"(?m)^\*\*Decision status:\*\* ([^\n]+)$", receipt),
                         [status[1]])

    def test_accepted_affine_feedback_remains_unreleased(self):
        records = cap.validate(cap.ROOT, self.document, self.current, self.released)
        self.assertEqual(records[5]["decision"], "D-078")
        self.assertEqual(records[5]["availability"], "unreleased")
        self.assertEqual(records[5]["decisionStatus"], "Accepted")
        self.assertEqual(records[5]["evidenceScope"], "implementation-fixtures")
        self.rejected(self.changed(5, availability="released", evidenceScope="bounded-release"),
                      "not in immutable released membership")

    def test_d079_d080_d081_bounded_acceptance_statuses(self):
        decisions = (cap.ROOT / "docs/DECISIONS.md").read_text()
        statuses = dict(re.findall(
            r"(?ms)^### (D-\d+):.*?^\*\*Status:\*\* ([^\n]+)$", decisions))
        for number in range(77, 82):
            self.assertEqual(statuses[f"D-{number:03}"], "Accepted")
        for number in range(82, 86):
            self.assertEqual(statuses[f"D-{number:03}"], "Proposed")
        for decision in ("EL-03", "EL-04", "EL-05"):
            record = next(r for r in self.document["capabilities"]
                          if r["decision"] == decision)
            self.assertEqual(record["decisionStatus"], "Proposed")
            self.assertEqual(record["availability"], "unreleased")

    def test_d079_d080_acceptance_does_not_create_released_membership(self):
        for package, module in (
                ("markovian-continuous", "Markovian.Continuous.Kernel.JointAffine.Exact"),
                ("markovian-autodiff", "Markovian.Autodiff.Quote")):
            self.assertIn(module, self.current[package])
            self.assertNotIn(module, self.released[package])

    def test_d079_d080_current_status_projections(self):
        projections = {
            "D-079": ["README.md", "packages/markovian-continuous/README.md",
                      "docs/book/src/continuous-probability.md"],
            "D-080": ["packages/markovian-autodiff/README.md",
                      "docs/book/src/autodiff-lowering.md"],
        }
        for decision, paths in projections.items():
            for path in paths:
                with self.subTest(decision=decision, path=path):
                    text = (cap.ROOT / path).read_text()
                    self.assertIn(f"{decision} is `Accepted` within its bounded, unreleased scope", text)
                    self.assertNotRegex(text, rf"{decision} remains `?Proposed`?")

    def test_d081_bounded_unreleased_acceptance_record(self):
        text = (cap.ROOT / "docs/evidence/D081-AFFINE-IMPLEMENTATION.md").read_text()
        self.assertIn("**Decision status:** Accepted", text)
        self.assertIn("**Availability:** UNRELEASED", text)
        self.assertIn("cc900878dbf6f7bdc33f95affa9c15d2ea6f97ad", text)
        self.assertIn("47aa263ff42f5691e38016e379150d271343fa685bbcd726b50fb701114901a5", text)
        self.assertIn("Historical strict writer-index byte preservation failed.", text)
        self.assertIn("LINT-PROOF-BRIDGE.md", text)
        self.assertIn("not a physical-allocation theorem", text)
        self.assertIn("D-082 remains Proposed and unimplemented.", text)
        self.assertIn("Its placement is not approved.", text)
        module = "Markovian.Tensor.Affine"
        self.assertIn(module, self.current["markovian-tensor"])
        self.assertNotIn(module, self.released["markovian-tensor"])

    def test_d081_current_status_projections(self):
        for path in ("README.md", "TODO.md", "docs/CONTEXT.md", "docs/ARCHITECTURE.md",
                     "docs/WORKFLOWS.md", "docs/book/src/tensor-runtime.md",
                     "packages/markovian-tensor/README.md"):
            with self.subTest(path=path):
                text = (cap.ROOT / path).read_text()
                self.assertIn("D-081 is `Accepted` within its bounded, unreleased scope", text)
                self.assertIn("D081-AFFINE-IMPLEMENTATION.md", text)
                self.assertNotRegex(text, r"D-081 (?:remains|stays) `?Proposed")
                self.assertNotIn("133 public modules", text)
                self.assertNotIn("still exposes no Affine module", text)
        api = (cap.ROOT / "docs/book/src/api-map.md").read_text()
        self.assertIn("markovian-tensor: Markovian.Tensor.Affine", api)
        self.assertIn("D-081 Accepted with the recorded historical index exception", api)
        decisions = (cap.ROOT / "docs/DECISIONS.md").read_text()
        d082 = decisions.split("### D-082:", 1)[1].split("### D-083:", 1)[0]
        self.assertIn("acceptance prerequisites are satisfied", d082)
        self.assertIn("D-082 remains unimplemented", d082)
        self.assertIn("explicit hardware authority remain pending", d082)
        todo = (cap.ROOT / "TODO.md").read_text()
        r5 = todo.split("#### R5 —", 1)[1].split("#### R6 —", 1)[0]
        self.assertNotIn("- [ ]", r5)
        self.assertIn("**R7 (`NEXT`)", todo)
        self.assertNotIn("Still blocked on separate reviewed acceptance of D-081", todo)

    def test_d081_frozen_contracts_and_historical_proposal(self):
        expected = {
            "docs/plans/D081-AFFINE-VIEWS.md":
                "31bf4e98df950c0f7cd9d738a36b12ca94b207a8e8167346098c36a11e0f8941",
            "docs/plans/D081-MATERIALIZATION-ADDENDUM.md":
                "e9dede17681f241f542163a78dcc011d80d2cab3a64405387aa986e189783d92",
        }
        for path, digest in expected.items():
            self.assertEqual(hashlib.sha256((cap.ROOT / path).read_bytes()).hexdigest(), digest)
        decisions = (cap.ROOT / "docs/DECISIONS.md").read_text()
        historical = decisions.split(
            "#### Historical original proposal and transpose-only prerequisite evidence\n", 1
        )[1].split("### D-082:", 1)[0]
        self.assertEqual(hashlib.sha256(historical.encode()).hexdigest(),
                         "e3a9c852d3e5c05f73356177ae8cb75c8afd0b05b8ac781df2f378b4c1c92d8d")
        digest = hashlib.sha256()
        for path in sorted((cap.ROOT / "docs/evidence/D081-MATERIALIZATION").rglob("*")):
            if path.is_file():
                digest.update(str(path.relative_to(cap.ROOT)).encode() + b"\0"
                              + path.read_bytes() + b"\0")
        self.assertEqual(digest.hexdigest(),
                         "fac53051a4cef18ab8e1b10efa12ea638d04202f74632240350d3fff3f453371")

    def test_d083_frozen_contract_identity_and_placement(self):
        path = "docs/plans/D083-CE-CCE-SOLVERS.md"
        text = (cap.ROOT / path).read_text()
        self.assertEqual(hashlib.sha256((cap.ROOT / path).read_bytes()).hexdigest(),
                         "cfb9301a0ddd454acb7230225fd8c27458081063fe3dd6dcd4a9610212fa1e28")
        self.assertIn("## 3. Frozen public surface", text)
        self.assertIn("Contract freeze is not capability acceptance.", text)
        self.assertIn("60d23acceaeb731c3de713540ec0d051c0aa67d197f496a96bb91daa0f36ac30", text)
        self.assertIn("Markovian.Game.Correlated.Exact", self.current["Markovian"])

    def test_d083_contract_freeze_leaves_implementation_acceptance_pending(self):
        decisions = (cap.ROOT / "docs/DECISIONS.md").read_text()
        d083 = decisions.split("### D-083:", 1)[1].split("### D-084:", 1)[0]
        self.assertIn("**Status:** Proposed", d083)
        self.assertIn("plans/D083-CE-CCE-SOLVERS.md", d083)
        self.assertIn("Contract freeze is not capability acceptance.", d083)
        self.assertIn("CorrelationCompletedSearchWithoutWitness", d083)
        self.assertIn("Arithmetic design review is not executable proof.", d083)
        todo = (cap.ROOT / "TODO.md").read_text()
        r7 = todo.split("#### R7 —", 1)[1].split("#### R8 —", 1)[0]
        self.assertEqual(r7.count("- [x]"), 1)
        self.assertEqual(r7.count("- [ ]"), 4)
        self.assertIn("- [x] Complete placement approval and contract freeze", r7)
        self.assertIn("- [ ] Implement separate bounded exact CE and CCE", r7)
        self.assertIn("private infeasible traversal, and public budget exhaustion", r7)
        self.assertIn("- [ ] Obtain fresh independent review", r7)
        self.assertIn("- [ ] Parent records any acceptance", r7)

    def test_d083_frozen_contract_is_packaged_as_documentation(self):
        text = (cap.ROOT / "Markovian.cabal").read_text()
        entry = "  docs/plans/D083-CE-CCE-SOLVERS.md\n"
        docs = text.split("extra-doc-files:\n", 1)[1].split("extra-source-files:", 1)[0]
        self.assertEqual(text.count(entry), 1)
        self.assertEqual(docs.count(entry), 1)
        self.assertTrue((cap.ROOT / entry.strip()).is_file())

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
