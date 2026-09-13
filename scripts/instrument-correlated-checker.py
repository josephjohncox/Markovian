#!/usr/bin/env python3
"""Add private Rational probes to scratch copies of the constructor and checkers."""

import hashlib
from pathlib import Path
import sys


def once(source, old, new):
    if source.count(old) != 1:
        raise SystemExit(f"checker instrumentation site changed: {old!r}")
    return source.replace(old, new)


def binary(producer, stage, operator, left, right):
    return f'Trace.binary "{producer}" "{stage}" ({operator}) {left} {right}'


def reviewed(source, bodies):
    # Pin the complete reviewed bodies, including traversal and validation.
    # A changed body requires reviewing the hook map and oracle, even if all
    # known replacement sites still exist. Never regenerate these blindly.
    for start, end, expected in bodies:
        if source.count(start) != 1 or source.count(end) != 1:
            raise SystemExit(f"checker coverage boundary changed: {start}")
        body = source.split(start, 1)[1].split(end, 1)[0]
        if hashlib.sha256(body.encode()).hexdigest() != expected:
            raise SystemExit(f"review Rational instrumentation coverage before updating body digest: {start}")


def instrument(source, start, end, edits):
    before, body = source.split(start, 1)
    body, after = body.split(end, 1)
    for old, new in edits:
        body = once(body, old, new)
    return before + start + body + end + after


def generate(destination, mutation):
    relative = Path("Markovian/Game/Correlated/Exact.hs")
    source = (Path("src") / relative).read_text()
    reviewed(source, [
        ("exactCorrelationDevice limits product_ supplied =", "-- | Read the canonical complete table.", "8eda110a33a295e879db916fddfcaa95477090af54cea944327391d4f29a9917"),
        ("checkCorrelatedEquilibrium limits game device@", "-- | One constant pre-recommendation", "a7564e4eea8c4770de559eed0bd4d727326516ff4ce17485c31162b2e2c3323a"),
        ("checkCoarseCorrelatedEquilibrium limits game device@", "{- | Check whether a device", "2f7f74d6ea6e5a2a09bf810ddb8d6dcc14a65e939298cc9dca2a4eb17bf9b278"),
        ("validateDevice limits (ExactCorrelationDevice product_ entries)", "checked ::", "b76b487c1f9978c2296ebed3cb417c6ce1b6c14d93858a324248967fe6667216"),
    ])
    source = once(source, "import Data.List.NonEmpty qualified as NonEmpty",
                  "import Data.List.NonEmpty qualified as NonEmpty\nimport D083Trace qualified as Trace")
    source = instrument(source, "    canonical profile =", "-- | Read the canonical complete table.", [
        ("lookup profile supplied", 'fmap (Trace.mass "actual" "constructor.mass") (lookup profile supplied)'),
        ("accumulator + mass", binary("actual", "constructor.total", "+", "accumulator", "mass")),
    ])
    source = instrument(source, "    addRecommendation accumulator", "-- | One constant pre-recommendation", [
        ("accumulator + mass", binary("actual", "recommendation", "+", "accumulator", "mass")),
        ("incumbent - deviating", binary("actual", "difference", "-", "incumbent", "deviating")),
        ("mass * difference", binary("actual", "weighted", "*", "mass", "difference")),
        ("accumulator + term", binary("actual", "slack", "+", "accumulator", "term")),
    ])
    source = instrument(source, "        Right (CoarseDeviationCheck owner alternative slack)", "{- | Check whether a device", [
        ("incumbent - deviating", binary("actual", "difference", "-", "incumbent", "deviating")),
        ("mass * difference", binary("actual", "weighted", "*", "mass", "difference")),
        ("accumulator + term", binary("actual", "slack", "+", "accumulator", "term")),
    ])
    source = instrument(source, "validateDevice limits (ExactCorrelationDevice product_ entries)", "checked ::", [
        ('checked limits "correlation mass" mass', 'checked limits "correlation mass" (Trace.mass "actual" "validation.mass" mass)'),
        ("accumulator + mass", binary("actual", "validation.total", "+", "accumulator", "mass")),
    ])
    if mutation == "reverse":
        source = once(source, "slack <- foldlM (contribution owner alternative) 0 matching",
                      "slack <- foldlM (contribution owner alternative) 0 (reverse matching)")
    elif mutation == "zero":
        source = once(source, "slack <- foldlM (contribution owner alternative) 0 matching",
                      "slack <- foldlM (contribution owner alternative) 0 (filter ((/= 0) . snd) matching)")
    elif mutation == "operands":
        # Equal offsets leave each difference and the complete report unchanged.
        source = once(source, 'checked limits "CE payoff difference" (Trace.binary "actual" "difference" (-) incumbent deviating)',
                      'checked limits "CE payoff difference" (Trace.binary "actual" "difference" (-) (incumbent + 1) (deviating + 1))')
    elif mutation != "none":
        raise SystemExit(f"unknown mutation: {mutation}")
    target = destination / relative
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(source)

    relative = Path("Markovian/Game/Correlated/Exact/Internal.hs")
    source = (Path("src") / relative).read_text()
    reviewed(source, [
        ("correlatedShadow limits game entries labels =", "sizeOf ::", "37a689860fe6a053f8e7f6038ec676c0dc696b081c0f8de27bd72aabce8898e3"),
    ])
    source = once(source, "import Data.List.NonEmpty qualified as NonEmpty",
                  "import Data.List.NonEmpty qualified as NonEmpty\nimport D083Trace qualified as Trace")
    source = once(source, "observeRational limits phase value = do",
                  'observeRational limits phase rawValue = do\n    let value = Trace.mass "observe" (show phase) rawValue')
    for start, end, stage in [
        ("shadowMasses limits entries = do", "    constructorTotal <-", "constructor.mass"),
        ("    constructorTotal <-", "    deviceTotal <-", "constructor.total"),
        ("    deviceTotal <-", "correlatedShadowRow ::", "validation.total"),
        ("correlatedShadowRow limits game entries label", "coarseShadowRow ::", "recommendation"),
        ("shadowContribution limits game owner alternative accumulator", "sizeOf ::", "contribution"),
    ]:
        edits = []
        if stage in {"constructor.mass", "validation.total"}:
            mass_stage = "validation.mass" if stage == "validation.total" else stage
            edits.append(("observeRational limits CorrelationVerification mass",
                          f'observeRational limits CorrelationVerification (Trace.mass "shadow" "{mass_stage}" mass)'))
        if stage == "contribution":
            edits += [(f"{left} {operator} {right}", binary("shadow", name, operator, left, right))
                      for name, operator, left, right in [("difference", "-", "incumbent", "deviating"),
                                                         ("weighted", "*", "mass", "difference"),
                                                         ("slack", "+", "accumulator", "term")]]
        elif stage != "constructor.mass":
            edits.append(("accumulator + mass", binary("shadow", stage, "+", "accumulator", "mass")))
        source = instrument(source, start, end, edits)
    target = destination / relative
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(source)


if __name__ == "__main__":
    generate(Path(sys.argv[1]), sys.argv[2] if len(sys.argv) > 2 else "none")
