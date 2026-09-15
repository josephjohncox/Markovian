# Revision history for markovian-neural-bridge

## 2026.9.15.1 - 2026-09-15

- Add `Markovian.Backend.Neural.Bridge.DQN.Trainer`, a bounded resumable reference trainer with explicit transition fuel, replay and checkpoint ownership, generator state, cumulative limits, ordered reports, and atomic online updates.
- Record D-084 as Accepted for this pure reference-trainer scope. It makes no production, convergence, device, distributed-training, or scalability claim.
- Add a complete-run benchmark with raw timings and a separate semantic receipt.
- Target GHC 9.14.1 with `base >=4.22 && <4.23`; add coordinated bounds on `markovian-numerical` and `markovian-sampling`, and align the existing `Markovian` and `markovian-neural` bounds.

## 2026.9.3.0 — 2026-09-03

- Added bounded one-way compilation from exact action layouts to sized structural neural masks.
- Added terminal separation, nominal action-role protection, and exact gather/scatter differential tests.
- Added compile-fail evidence for the exact-support boundary.
- Added the bounded resumable sampled DQN trainer bridge with explicit fuel,
  replay, generator, target-checkpoint, cumulative-limit, and semantic-report
  ownership.
