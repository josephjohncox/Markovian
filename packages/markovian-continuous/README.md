# markovian-continuous

This optional package implements a bounded exact family of continuous laws. A real law is a rational affine combination of compact rational uniform noises with explicit owners. The executable operations are affine pushforward, rational polynomial expectation, affine additive-noise kernel composition, joint moments, exact joint affine kernel materialization and support extrema, and finite affine conditioning.

Bivariate integration uses one cumulative meter. Separate limits bound raw expansion pairs and live canonical monomials. The report counts symbolic products, power merges, comparisons, coefficient operations, cancellations, canonical combinations, moment operations, and final sums. It also reports the largest intermediate rational size. The operation returns no report if a limit fails.

The joint affine kernel has two closed affine input coordinates and one owner table. Each row contains a compact interval and separate reward and successor coefficients. Duplicate owners fail before zero rows are removed. Owner numbers are local to one nominal kernel scope. A partial alpha-renaming preserves that scope. A scope transition requires a complete injective mapping of every retained owner; an empty mapping cannot re-scope a nonempty kernel. Materialization always returns `ExactJointLaw RealBorel RealBorel`. The other phantom parameters are nominal coordinate labels, not space witnesses. One table preserves shared-owner correlation while distinct rows remain independent. Exact extrema select endpoints by coefficient sign. Construction, materialization, support, and renaming use complete bounded preflight and deterministic reports. Gate A does not provide cross-kernel composition.

Measurability is a syntax-directed obligation, not a run-time claim about callbacks. Rational affine real maps are continuous and Borel measurable. Compact uniform sources are Borel probability laws, finite products remain standard Borel, and each output is a finite affine projection. No public joint-kernel constructor accepts a function.

The tests use independent raw and multinomial symbolic oracles for shared and independent owners. They cover exact limits, one-below failures, failure precedence, duplicate monomials, hidden zero-owner duplicates, same-scope and complete-scope renaming, negative signed-owner support corners, and cancellation. One cancellation fixture has a discarded rational maximum larger than its retained zero. The tests also include deterministic accounting goldens and a bounded numerical differential fixture.

`ExactLaw` does not represent every measurable event. The package does not accept arbitrary Haskell functions as measurable maps, provide point conditioning, or provide continuous-to-continuous disintegration. See the repository book chapter `continuous-probability.md`.

Build and test from the repository root:

```sh
cabal test markovian-continuous-test
bash packages/markovian-continuous/scripts/check-continuous-boundary
```
