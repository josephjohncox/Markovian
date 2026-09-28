# markovian-neural-bridge

`markovian-neural-bridge` owns bounded integration between root-level sampled or
exact Markovian values and `markovian-neural`. Its exact-support adapter checks
the global action layout, neural head width, per-state availability order,
terminals, work, and output counts before returning a complete mask bridge.

`Markovian.Backend.Neural.Bridge.DQN.Trainer` composes the existing checked DQN
batch update with explicit positive fuel, the repository's resumable generator,
owned FIFO replay, target checkpoints, and a pure environment callback. It
returns a bounded semantic receipt and resume token. It is a deterministic
reference, not a production, device, distributed, convergence, or throughput
API.

The exact-support adapter does not convert exact probabilities to floating
values and does not invent a mask for a terminal state.

`Markovian.Backend.Neural.Bridge.Inspection` requires matching caller-supplied
exact action layouts and checks each layout's width against its frozen head
before attaching action IDs to a numerical audit. The caller remains
responsible for the models' action meanings. Replay entry IDs in probe reports
are provenance within a caller-owned buffer lineage, not checkpoint identifiers.

From the repository root:

```sh
cabal test markovian-neural-bridge-test --project-file=cabal.project.ci \
  --test-show-details=direct
bash backends/markovian-neural-bridge/scripts/check-exact-support-boundary
cabal bench dqn-trainer-bench --project-file=cabal.project.ci
```
