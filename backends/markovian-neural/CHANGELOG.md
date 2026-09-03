# Revision history for markovian-neural

## 2026.9.3.0 — Unreleased

- Added checked dense reference networks and manual VJPs.
- Added finite owned reverse programs with explicit layouts, ownership, budgets, and opaque stored or recomputed tapes.
- Added categorical information, policy-gradient, replay, target-network, and DQN reference operations.
- Added sized structural action masks and finite-difference and compile-fail evidence.
- Added test-only integration evidence from `markovian-autodiff`: a bounded two-layer tanh fixture compares all manual dense primal, input-VJP, weight-VJP, and bias-VJP coordinates under every output basis seed. This adds no autodiff dependency to the neural library.
