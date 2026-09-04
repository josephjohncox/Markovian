# markovian-autodiff

This optional package implements automatic reverse differentiation for a closed language.
It does not differentiate arbitrary Haskell functions.

## Supported language

The polynomial fragment has constants, owned parameters, addition, multiplication, negation, products, fixed vectors, dot products, and sums.
The smooth fragment adds `tanh` for checked `Double` execution.

## Explicit first-order quotation

`Quote` adds explicit first-order `let` syntax to the exact polynomial fragment.
`Path` selects a value from the current associated-product environment.
Its constructors and representation are private, and both indexes have nominal roles.
`withQuoteScope` makes a generative lexical scope token.
`pathLeft` and `pathRight` require the token for the matching environment extension.
Paths from independent scopes do not type-check, even when their runtime shapes match.
Use `project` to make a quotation from a path.

`letQuote scope e body` evaluates `e` before `body`, even when `body` does not use the bound value.
The body uses the extended environment `Product environment bound`.
The function stores no Haskell callback.
Its parameter tree is exactly `ParameterProduct (ParameterProduct NoParameters p) q`.

One `QuotationLimits` value bounds traversal, syntax depth, paths, coordinates, machine extent, target size, allocation, and work.
Preflight charges each item before it descends and visits children from left to right.
The successful `QuoteReport` contains the timing-free cumulative ledger.
The execution preflight functions add input and direction traversal to that report.
`lowerQuote` constructs no target `Program` until this preflight succeeds.
`compileExactQuote` uses the same limits for the exact compiler and returns both reports.

`interpretExactQuote` and `interpretExactQuoteJVP` recurse directly over quotation and source syntax.
They do not call the reverse compiler or its primitive VJPs.
Reverse lowering uses `compose (fanout identity e) body` and provides separate VJP evidence.

Programs use static shapes for units, scalars, fixed vectors, and associated products.
Parameter trees keep each type-level owner and each product association.
Independent duplicate owners fail during bounded preparation.
Use `shareParameters` for an explicit parameter diagonal.

## Execution

`compileExactPolynomial` uses literal `Rational` arithmetic.
Its VJP equals the transpose of the formal polynomial Jacobian under the finite coordinate pairing.

`compileDoublePolynomial` and `compileDoubleSmooth` reject nonfinite inputs, outputs, cotangents, and intermediate results.
Their immutable named comparison policy is `abs <= 2e-10` or `rel <= 2e-8`.
Finite differences provide fixture evidence only.

Compiler limits also bound scalar work for each forward or reverse execution and numerator/denominator bits for exact values. Vector extents must fit machine indexing before an executable is returned. Exact input and arithmetic values are checked against the active rational limit.

Both paths lower directly to the owned reverse-program interpreter in `markovian-reverse`.
The compiler creates all primitive VJPs.
Users cannot add callback primitives.
Stored and recomputed tapes are opaque, reusable, and endpoint-indexed. A private bounded scalar SSA performs only exact identity rewrites; it is not used to reassociate floating expressions.

The test suite contains a closed `2 -> 2 tanh -> 2` program with two dense layers. For each output basis seed, it compares every primal, input VJP, weight VJP, and bias VJP coordinate under stored and recomputed tapes with the manual `markovian-neural` runtime and independent central finite differences. The neural dependency is test-only. The fixture is evidence for these coordinates, not a general neural-network lowering API.

## Boundaries

The package has no recursion, branches, loops, stochastic nodes, division, arbitrary indexing, `abs`, ReLU, higher derivatives, tensor runtime, or device lowering.
Quotation is not ordinary Haskell quotation, Template Haskell, a compiler plugin, or tracing.
It has no effects, higher-order values, nested differentiation, or exact transcendental `Double` semantics.
The multilayer fixture uses public closed-language combinators and distinct nominal parameter owners.
It does not add a matrix primitive or an arbitrary callback.
It does not provide matrix dagger, Bayesian inversion, payoff pullback, feedback, strategic duality, or disintegration.
The package makes no release-readiness claim.

Run the focused evidence:

```bash
cabal test markovian-autodiff-test --project-file=cabal.project.ci
bash packages/markovian-autodiff/scripts/check-autodiff-boundary
cabal bench markovian-autodiff-bench --project-file=cabal.project.ci
```
