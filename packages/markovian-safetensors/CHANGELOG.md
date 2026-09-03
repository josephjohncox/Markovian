# Changelog

## 2026.9.3.0 — Unreleased

- Add a duplicate-preserving bounded parser for the metadata-free F64 SafeTensors profile.
- Validate complete headers, UTF-8 names, shapes, products, offsets, coverage, payload lengths, and allocation plans before allocation.
- Add canonical name sorting, fixed JSON field order, eight-byte space padding, contiguous offsets, and row-major view materialization.
- Preserve raw IEEE payloads and keep finite refinement separate.
- Add malformed corpus, exact and one-below limits, canonical byte golden, raw payload round trips, archive data, opacity checks, and region-escape checks.
