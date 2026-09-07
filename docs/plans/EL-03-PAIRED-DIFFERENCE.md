# EL-03 — Exact paired-difference contract

**Status:** Proposed

Frozen and independently reviewed before semantic implementation. Unreleased implementation evidence now lives in `packages/markovian-continuous/test/PairedDifference.hs` and the [executable lesson](../book/src/paired-difference.md); independent implementation review remains a separate gate. This contract asserts no acceptance. Owner: existing `markovian-continuous`, public module `Markovian.Continuous.Measure.Exact`; private machinery stays in `Markovian.Continuous.Internal`. No dependency changes. Related D-070 is Accepted for its older bounded fragment; D-079 remains Proposed.

## Consumer and semantics

The consumer is a one-step affine scenario comparison. Input is one existing `ExactJointLaw RealBorel RealBorel`, not two marginals silently coupled. Reuse `shareAffineSource`, `independentPair`, or `materializeJointAffineKernel` from `Markovian.Continuous.Kernel.JointAffine.Exact` to construct it. The last permits signed coefficients, including `(U,1-U)`. Numeric owner equality and matching intervals determine sharing *within this represented joint*. This report neither creates owners nor infers sharing between different kernel scopes. Existing nominal space/owner witnesses remain opaque.

For coordinates X,Y return E[X], E[Y], Var(X), Var(Y), Cov(X,Y), E[X-Y], Var(X-Y), the independent-marginal baseline Var(X)+Var(Y), and the signed excess Var(X-Y) minus that baseline. `LT`, `EQ`, `GT` mean respectively less, equal, or more variance than independent marginals. No claim of universal variance reduction, policy comparison, trajectory synchronization, new coupling construction, or numerical estimation follows. All quantities are exact Rational; bounded affine-uniform support supplies integrability.

## Complete new public surface

These are the reviewed Haskell declarations, not runnable book examples. Only constructors explicitly listed here will be exported. Existing `ExactLimits(..)` and `ExactError(..)` are reused unchanged.

```haskell
-- Opaque report; no constructor export.
data PairedDifferenceReport

data PairedDifferenceAccounting = PairedDifferenceAccounting
  { pairedWork :: !Natural
  , pairedMomentCount :: !Natural
  , pairedDerivedWork :: !Natural
  , pairedRawExpansionPairs :: !Natural
  , pairedMaximumCanonicalTerms :: !Natural
  , pairedMaximumRationalBits :: !Natural
  } deriving (Eq, Show)

pairedDifferenceReport :: ExactLimits
  -> ExactJointLaw RealBorel RealBorel
  -> Either ExactError PairedDifferenceReport
pairedMeanFirst :: PairedDifferenceReport -> Rational
pairedMeanSecond :: PairedDifferenceReport -> Rational
pairedVarianceFirst :: PairedDifferenceReport -> Rational
pairedVarianceSecond :: PairedDifferenceReport -> Rational
pairedCovariance :: PairedDifferenceReport -> Rational
pairedMeanDifference :: PairedDifferenceReport -> Rational
pairedVarianceDifference :: PairedDifferenceReport -> Rational
pairedIndependentVariance :: PairedDifferenceReport -> Rational
pairedVarianceExcess :: PairedDifferenceReport -> Rational
pairedVarianceComparison :: PairedDifferenceReport -> Ordering
pairedDifferenceAccounting :: PairedDifferenceReport -> PairedDifferenceAccounting
```

`RealBorel` is the existing public continuous-space type, `Natural` is from `Numeric.Natural`. Accessors cannot fail or compute unmetered arithmetic: all fields are retained only after admission. The report is not a coupling witness and has no phantom parameters to relabel.

## Admission, ledger, operation order

Use a single private continuous meter, not five public calls to `expectBivariatePolynomial`. Refactor internal metered expansion/evaluation for reuse without changing that existing public operation's accounting. One support preflight precedes all moment expansion; no moment or derived step resets a budget.

1. Validate `ExactLimits` with existing `validateLimits` precedence (including machine indexing). Bound each retained law spine by `limitTerms`, left then right. Check degree 2, then total five moment input terms against `limitTerms`. These checks apply even for Dirac laws.
2. For n and m retained noise entries, precharge structural work `n+m+(n+m)^2+5` before inspecting owner/interval relationships. Compute this bound with saturated arithmetic at `limitWork+1`, not an overflowing Int product. This conservative reservation pays for canonical owner-order checks, shared interval lookup, union construction, and the fixed moment list. Check each marginal owner count, then joint distinct owner count against `limitNoiseOwners`; reject inconsistent shared intervals. Stored laws already have canonical owner order; do not reorder, rescope, or repair them. Report and reject any violated existing law invariant with the existing `ExactError`.
3. Observe input constants, then each left and right `(coefficient,lower,upper)` in retained order, and the fixed polynomial coefficient 1. Check interval ordering. Check every rational using `max(bits(abs numerator),bits denominator)`, with zero requiring one bit. Account support bounds left then right with the existing four operations per noise entry; even discarded support intermediates contribute to the maximum.
4. Evaluate the five monomials in this order: X, Y, X², Y², XY. Each uses the existing bivariate `expandPolynomial` and `evaluateExpansion` operation order under the *same* meter. Raw Cartesian expansion pairs accumulate across all five; `limitCanonicalExpansionTerms` bounds the largest live canonical expansion, not the sum of finalized moments. No result-dependent elision or caching of moments in this first implementation. Owner/exponent comparisons, merges, insertion visits, coefficient products/additions/cancellations, powers, moment arithmetic/products and summations keep their existing charged units. All their rational intermediates, even later cancelled values, are checked.
5. Compute, in order: `sx=mx*mx`, `vx=mxx-sx`, `sy=my*my`, `vy=myy-sy`, `sxy=mx*my`, `cov=mxy-sxy`, `meanDiff=mx-my`, `baseline=vx+vy`, `twiceCov=2*cov`, `diffVar=baseline-twiceCov`, `excess=diffVar-baseline`. Charge one work unit *before* each of these eleven Rational operations and check its result. Observe the literal 2 before its multiplication. Charge one more unit for `compare diffVar baseline`. Thus `pairedDerivedWork=12` and `pairedMomentCount=5` on every success. These units join `pairedWork`, not a separate allowance.
6. Observe all retained Rational fields in accessor order (no extra work units); only then return the opaque report. The observed-bit maximum includes input, support, symbolic, moment, derived, and retained values. Atomic failure returns only `ExactError`, no partial report/accounting.

At each metered step check work before arithmetic; rational overflow follows that operation's work check. Raw-pair and canonical-growth checks retain the existing bivariate ordering. Work reservations are semantic admission charges, not wall-clock estimates. Report exact successful charges; rejected required counts saturate at the configured limit plus one. Existing positive-limit and machine-bound policies remain in force. Input construction is a separately budgeted operation and is not retroactively included.

## Required evidence before completion

Executed evidence and remaining integration gates are recorded in [the checklist](EXECUTABLE-LEARNING-TODO.md). The required semantic contract below is unchanged.

Use an independent direct affine uniform moment formula, not production expansion, for every reported identity. Include `(U,U)` (difference variance 0), `(U,1-U)` (covariance -1/12 and difference variance 1/3), independent uniforms (baseline and actual variance 1/6), zero variance, negative scales, independent owners, shared owners, and equivalent reordered construction inputs. Check the sign in all three cases. Fix a full deterministic ledger golden, exact and one-below each applicable limit, discarded rational growth, cancellation saturation, and combined-invalid precedence. Preserve old bivariate goldens. Add source archives, current module snapshots if needed, changelog, capability transition, and a runnable lesson at implementation time; never mutate published history.
