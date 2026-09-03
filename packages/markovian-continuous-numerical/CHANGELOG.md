# Changelog

## 2026.9.3.0 — 2026-09-03

- Add explicit finite `Double` conversion and rounding reports.
- Add deterministic bounded GK15/7 quadrature.
- Add explicit SplitMix64 sampling and resumable Welford Monte Carlo.
- Reject intervals with nonfinite or nonpositive floating width.
- Reject nonfinite aggregate quadrature values, errors, and tolerance thresholds.
- Expose stable state words and raw draws for pinned SplitMix64 known-answer evidence.
