# Bounded autodiff lowering

`markovian-autodiff` implements reverse differentiation for a closed finite language.
The package is optional and does not change the root semantic core.

## Shapes and ownership

The language has four shape forms:

- unit with zero coordinates
- scalar with one coordinate
- fixed vector with a type-level length
- an associated product of two shapes

A scalar and `Vector 1` have different layouts.
Product association also remains visible.
Opaque values prevent lists with an incorrect vector length.
Vector construction checks its limit before it consumes the list.
It then consumes at most `n + 1` cells.

A parameter tree has unit leaves, owned leaves, and associated products.
An owned leaf has a type-level `Symbol`.
Independent composition rejects a duplicate owner.
`shareParameters` is the only parameter-sharing combinator.
Its reverse rule adds both owner cotangents.

## Closed fragments

The polynomial fragment contains these operations:

- scalar and vector constants
- owned parameter reads
- scalar negation, addition, and multiplication
- vector addition and Hadamard multiplication
- dot product and vector sum
- product projections
- identity, composition, parallel product, input fanout, and parameter sharing

The smooth fragment adds scalar and vector `tanh`.
The exact compiler accepts only polynomial syntax.
The type checker rejects exact compilation of smooth syntax.

The public API has no primitive callback constructor.
It also has no `Num`, `Fractional`, or `Floating` instance for programs.
Thus, this API does not differentiate arbitrary Haskell.

## Semantics

A program represents a finite map

\\[
F : P \times X \to Y.
\\]

The exact compiler uses `Rational` for polynomial operations.
Its pullback is the transpose of the formal polynomial Jacobian:

\\[
\operatorname{vjp}_F(p,x,\bar y)
=\left(D_pF(p,x)^T\bar y,D_xF(p,x)^T\bar y\right).
\\]

The transpose uses the declared finite coordinate pairing.
It is reverse differentiation only.
It is not a matrix dagger, Bayesian inversion, payoff pullback, feedback, strategic duality, or disintegration.

The Double compilers check all represented coordinates.
They reject nonfinite inputs, parameters, intermediate values, outputs, seeds, and cotangents.
The immutable named comparison policy is `abs <= 2e-10` or `rel <= 2e-8`.
The compiler does not reassociate floating operations.
For example, `(1e16 + -1e16) + 1` differs from `1e16 + (-1e16 + 1)`.

## Lowering and budgets

The compiler lowers each closed primitive to a compiler-owned VJP.
It maps source composition and sharing to the bounded owned reverse-program interpreter.
The target checks these limits before it returns an executable:

1. total syntax nodes
2. primitive nodes
3. syntax depth
4. unique owners
5. primal coordinate extent
6. cotangent coordinate extent
7. layout and ownership structure nodes
8. layout and ownership structure depth
9. conservative scalar work for one forward or reverse execution
10. exact numerator and denominator bit size

Every vector extent must also fit machine indexing before compilation returns an executable. Exact execution revalidates input values and arithmetic results under the executable's active rational limit.

A failed compilation returns no executable.
A failed run returns no run or tape.
A failed pullback returns no gradient.
This atomic rule concerns semantic results, not heap rollback.

Stored tapes retain primitive pullbacks.
Recomputed tapes retain source values and rerun the same primitive.
This policy is not general checkpoint scheduling.
Tapes are opaque, endpoint-indexed, and reusable.

A private bounded scalar SSA admits only earlier-register references. Its exact rewrite pass removes `x + 0`, `0 + x`, `x * 1`, and `1 * x`. It does not reassociate, distribute, or reorder operations and is not used as a floating rewrite justification.

The report contains deterministic structure and conservative work only.
It has no elapsed time or byte estimate.

## Executable evidence

The focused tests include:

- exact polynomial primal and VJP values
- a test-only syntax-recursive exact primal and JVP interpreter that does not call backend primitives
- shared-owner diagonal accumulation
- a missing-diagonal counterexample
- every parameter and input coordinate under central finite differences
- stored and recomputed parity
- exact pairing for every polynomial primitive and each structural constructor
- a linear dense primal, input-VJP, and parameter-VJP differential against the manual neural implementation
- a closed `2 -> 2 tanh -> 2` two-layer fixture, checked for every primal coordinate and every input, weight, and bias VJP coordinate under each output basis seed
- stored and recomputed multilayer tapes compared with the separate manual `markovian-neural` runtime and independent central finite differences
- a committed deterministic lowering report plus exact shape, duplicate-owner, and nonfinite-perturbation failures
- repeated tape application
- exact vector dot-product differentials
- duplicate-owner, machine-index, exact/one-below scalar-work, and rational-growth failures
- nonfinite Double rejection
- a floating reassociation counterexample

The compile-fail script protects constructors, hidden modules, shapes, owners, fragments, wrong seeds, wrong owner trees, cross-fragment use, and tape endpoints.
Finite differences use a coordinate-scaled central step of `1e-6`, absolute tolerance `2e-10`, and relative tolerance `2e-8`. Nonfinite perturbations and objectives are rejected. They corroborate the named fixtures and do not prove all Double derivatives.

The multilayer program is assembled from scalar closed-language combinators. Its manual oracle comes from the test-only dependency on `markovian-neural`, whose library remains independent of `markovian-autodiff`. The explicit parameter permutation reconciles the source owner-tree order with the neural runtime's row-major weights-then-biases order. This is fixture evidence, not arbitrary neural lowering, tensor lowering, or a claim about training.

## Nonclaims

The package does not support recursion, loops, branches, stochastic nodes, division, arbitrary indexing, `abs`, ReLU, tensor execution, devices, or higher derivatives.
It does not differentiate sampling.
It does not claim exact floating algebra or cross-platform transcendental identity.
It does not claim release readiness.

## References

- Cockett et al., “Reverse derivative categories,” *CSL 2020*, DOI 10.4230/LIPIcs.CSL.2020.18.
- Elliott, “The simple essence of automatic differentiation,” *ICFP 2018*, DOI 10.1145/3236765.
- Baydin et al., “Automatic differentiation in machine learning,” *JMLR* 18(153), 2018.
- Griewank and Walther, *Evaluating Derivatives*, second edition, SIAM, 2008.
- Higham, *Accuracy and Stability of Numerical Algorithms*, second edition, SIAM, 2002.
