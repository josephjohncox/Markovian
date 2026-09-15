# Changelog

## 2026.9.15.1 - 2026-09-15

- Add integration coverage for encoding D-081 signed, offset, scalar, and empty affine tensor views with exact shapes, offsets, and IEEE payloads.
- Harden the compile-fail boundary check to select the intended in-place package units in repository and source-archive builds.
- Target GHC 9.14.1 with `base >=4.22 && <4.23` and `bytestring >=0.12.2 && <0.13`; align the `markovian-tensor` bound with the coordinated release.

## 2026.9.3.0 — 2026-09-03

- Add a duplicate-preserving bounded parser for the metadata-free F64 SafeTensors profile.
- Validate complete headers, UTF-8 names, shapes, products, offsets, coverage, payload lengths, and allocation plans before allocation.
- Add canonical name sorting, fixed JSON field order, eight-byte space padding, contiguous offsets, and row-major view materialization.
- Preserve raw IEEE payloads and keep finite refinement separate.
- Add malformed corpus, exact and one-below limits, canonical byte golden, raw payload round trips, archive data, opacity checks, and region-escape checks.
