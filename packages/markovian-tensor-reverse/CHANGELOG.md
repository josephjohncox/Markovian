# Changelog

## 2026.9.15.1 - 2026-09-15

- Target GHC 9.14.1 with `base >=4.22 && <4.23` and align `markovian-reverse` and `markovian-tensor` bounds with the coordinated release.

## 2026.9.3.0 — 2026-09-03

- Move the public `Markovian.Tensor.Reverse` closed primitive-tape API from `markovian-tensor` without replacing atomic multi-output allocation with sequential public calls.
- Add the bounded, region-owned host F64 reverse adapter.
- Add closed `tanh` and pointwise-multiplication symbols.
