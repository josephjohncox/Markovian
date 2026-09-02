# markovian-continuous

This optional package implements a bounded exact family of continuous laws. A real law is a rational affine combination of compact rational uniform noises with explicit owners. The executable operations are affine pushforward, rational polynomial expectation, affine additive-noise kernel composition, joint moments, and finite affine conditioning.

Bivariate integration uses one cumulative meter. Separate limits bound raw expansion pairs and live canonical monomials. The report counts symbolic products, power merges, comparisons, coefficient operations, cancellations, canonical combinations, moment operations, and final sums. It also reports the largest intermediate rational size. The operation returns no report if a limit fails.

The tests use an independent raw symbolic oracle for shared and independent owners. They cover exact limits, one-below failures, duplicate monomials, and cancellation. One cancellation fixture has a discarded rational maximum larger than its retained zero. The tests also include a deterministic accounting golden and a bounded numerical differential fixture.

`ExactLaw` does not represent every measurable event. The package does not accept arbitrary Haskell functions as measurable maps, provide point conditioning, or provide continuous-to-continuous disintegration. See the repository book chapter `continuous-probability.md`.

Build and test from the repository root:

```sh
cabal test markovian-continuous-test
bash packages/markovian-continuous/scripts/check-continuous-boundary
```
