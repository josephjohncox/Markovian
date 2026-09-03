# Changelog for markovian-autodiff

## 2026.9.3.0 — Unreleased

- Add a closed typed polynomial and `tanh` language.
- Add bounded lowering to owned reverse programs.
- Add exact formal-polynomial and checked-Double execution.
- Add opaque stored and recomputed tapes.
- Add exact laws, an independent syntax-recursive JVP oracle, all-coordinate finite differences, counterexamples, and compile-fail boundaries.
- Extract the reverse dependency to `markovian-reverse`.
- Add machine-index admission, scalar-work and rational-growth limits, a named Double comparison policy, and a private bounded exact scalar SSA.
- Check exact JVP/VJP pairing for every polynomial primitive and structural constructor, and compare a linear dense primal and both VJPs with the manual neural implementation.
- Add a closed `2 -> 2 tanh -> 2` fixture with two dense layers. For both stored and recomputed tapes and each output basis seed, compare every primal, input VJP, weight VJP, and bias VJP coordinate with the separate neural package and independent central finite differences. Commit deterministic lowering reports and check shape, owner, and nonfinite-perturbation failures.
