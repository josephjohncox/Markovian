# markovian-neural-bridge

`markovian-neural-bridge` is a bounded adapter between exact Markovian action layouts and `markovian-neural` structural masks. It checks the global action layout, neural head width, per-state availability order, terminals, work, and output counts before returning a complete bridge.

The adapter does not convert exact probabilities to floating values and does not invent a mask for a terminal state. It is one-way and does not add tensor, device, training, or equilibrium semantics.

From the repository root:

```sh
cabal test markovian-neural-bridge-test --project-file=cabal.project.ci \
  --test-show-details=direct
bash backends/markovian-neural-bridge/scripts/check-exact-support-boundary
```
