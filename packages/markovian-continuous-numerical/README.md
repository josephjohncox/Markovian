# markovian-continuous-numerical

This optional package keeps floating execution separate from exact continuous semantics. It provides explicit rational-to-`Double` conversion reports, deterministic bounded GK15/7 quadrature, an owned SplitMix64 generator, checked uniform/normal/exponential samplers, and resumable Welford Monte Carlo. Interval width and successful quadrature aggregates must be finite. Raw generator words and state words exist only for pinned known-answer evidence and explicit resumption; there is no cross-platform sampler claim.

A GK15/7 error is an estimate, not a certified bound. Monte Carlo standard error is not a deterministic bound. Failure returns no integral estimate, Monte Carlo report, or advanced generator.

```sh
cabal test markovian-continuous-numerical-test
bash packages/markovian-continuous-numerical/scripts/check-continuous-numerical-boundary
```
