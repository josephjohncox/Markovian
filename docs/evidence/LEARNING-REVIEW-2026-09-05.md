# Learning and bounded-extension review — 2026-09-05

## Baseline and authority

Review baseline: `7edc04a2d9a0de6af6fed1ccccb43303442210f5`.

The user requested a durable record of all findings, a durable checklist, and implementation of the recommendations. [The implementation checklist](../plans/EXECUTABLE-LEARNING-TODO.md) controls this work. This record describes the review baseline. Later fixes do not erase these findings.

PR #5 merged the D-081 transpose reverse-equivalence prerequisite repair. Post-merge CI `33935474832`, CUDA compile-only `33935474781`, and Pages `33935474797` all passed. This does not accept D-077 through D-085.

The released tag `v2026.9.3.0` remains immutable. Its tag object is `d746952084e09647e7bcd67b92dd6cef9d0e14c9`. Its target is `fe6abb8db9b3def65ead6602168eef860a79527c`. This task does not authorize publication, new hardware claims, or release-tag changes.

## Findings in current guidance

Line references below identify the baseline, not future line numbers.

| ID | Location | Finding | Required disposition |
| --- | --- | --- | --- |
| F01 | `docs/book/src/api-map.md:3-16` | D-061 appears as both unaccepted and Accepted. | Remove obsolete current migration guidance. |
| F02 | `docs/book/src/api-map.md:3` and feedback table | The map claims the released API but includes post-release `Markovian.Feedback.Value.Exact`. | Separate released and current-development surfaces. |
| F03 | `docs/book/src/tensor-runtime.md:120` | The book claims a GPU dependency on `markovian-tensor-reverse`. Cabal now depends directly on `markovian-tensor`. | Correct the current package graph. |
| F04 | `docs/book/src/tensor-runtime.md:158-159` | D-067 is still described as Proposed. | State its accepted bounded scope without broader lowering claims. |
| F05 | `docs/book/src/introduction.md:77` | All implemented scope is called finite and discrete. | Restrict that description to the root core. Mention the optional represented continuous fragment. |
| F06 | `README.md:14,140` | Accepted continuous release evidence conflicts with a blanket denial of release-readiness evidence. | Separate package release evidence from stronger unsupported semantics. |
| F07 | `docs/book/src/autodiff-lowering.md:184` | Current guidance retains a pre-release readiness denial. | Qualify historical evidence and distinguish released from post-release quotation. |
| F08 | `docs/book/src/probability-kernels.md:63` | `(L >=> K)` labels a sum that executes K before L. | Use conventional left-to-right Kleisli notation `(K >=> L)`. |

Do not rewrite historical decisions or receipts merely because later decisions superseded them. D-037 and D-061 record different stages. Historical CUDA list-backend measurements must remain historical.

## Existing teaching to retain

The book already supplies mathematical primers, a task-to-API selector, exact control, sampling, tabular updates, and detailed model timing. It also explains Bayesian support restrictions, stochastic sharing, open-system boundaries, and several counterexamples. The law catalogue and primary-source reading routes are substantial. The first MDP includes compiled source.

The missing element is not another broad theory chapter. It is a consistent path from a claim to a runnable experiment and checked answer.

At the baseline, 44 book Markdown files contained 70 Haskell fences and four source includes. Three includes were project-document appendices. Only the first-MDP include imported Haskell source. `scripts/check-book` checked links, assets, rendering, and mathematics, not general Haskell snippet compilation. This count is an inventory, not a claim that all fences were incorrect.

## Teaching recommendations

### Executable law laboratory

Each experiment needs a tempting claim, precise hypotheses, a tiny model, two computations, a result or failure witness, and an exercise with a checked solution.

Use existing implementations rather than duplicate demonstration semantics. Cover:

1. One shared stochastic draw versus two independent executions.
2. Matrix transpose versus prior-indexed Bayesian inversion.
3. Joint reward/successor outcomes versus loss of correlation.
4. Terminal payoff versus horizon truncation.
5. Exact equality versus floating reassociation.
6. Logical tensor coordinates versus storage identity, including primitive-on-view versus view-to-base differentiation.
7. Algebraic associativity versus resource admission, which may join the optimization-contract lesson.

For a fair bit, draw-then-copy gives probabilities 1/2 on 00 and 11. Independent execution gives probability 1/4 on each pair. The failed stochastic copy equation is not a failure of the proof-carrying deterministic law.

Classify Haskell snippets as runnable, compiled-source fragments, or explicit pseudocode. Compile and execute runnable examples. Generate displayed outputs from checked fixtures. Reject unclassified snippets and stale generated output. Add regression tests for the checker itself.

Add three reading routes: MDP practice, probability/composition, and differentiation/backends. Add prerequisites, outcomes, and next steps. Add a notation guide for orientation, composition, model timing, exact versus approximate equality, and the distinct reverse operations. Add progressive exercises with hints and checked solutions.

### Capability status

Keep availability, decision status, and evidence scope separate. Availability can be released, unreleased, or unimplemented. Proposal status does not determine whether code exists. A released bounded implementation does not establish a universal claim.

Create a small checked capability record and generated presentation. Validate package/module references against current manifests. Distinguish current snapshots from immutable published API snapshots. Do not infer release availability from unchanged development package versions.

## Bounded feature recommendations

### Exact paired-difference reports

Existing foundations: `Markovian.Continuous.Measure.Exact` exposes shared sources, disjoint-owner independent pairing, marginals, and exact polynomial expectations. The joint-affine kernel retains owner-defined dependence. There is no named operation-wide paired-difference report.

Add exact means, variances, covariance, mean difference, difference variance, and an independent-marginal variance baseline. One cumulative ledger must cover moment evaluation and derived rational arithmetic. Do not assemble an apparently atomic operation from independently reset public budgets.

The identity is `Var(X-Y) = Var(X) + Var(Y) - 2 Cov(X,Y)`.

Positive example: X and Y use the same uniform U, so their difference has zero variance. Negative example: X=U and Y=1-U for U uniform on [0,1]. Covariance is -1/12. Difference variance is 1/3. Independent copies with identical marginals have difference variance 1/6. Common noise doubles variance in this example.

First consumer: a one-step affine scenario-comparison lesson. No general policy comparison, trajectory coupling, arbitrary coupling solver, or universal variance-reduction claim follows.

### Exact feedback sensitivity

Existing foundation: `src/Markovian/Feedback/Value/Exact.hs` solves bounded strict-discount affine feedback coefficients and checks four literal equations. `test/FeedbackValueExact.hs` supplies independent finite-unrolling evidence. No sensitivity API is exposed.

For `z = b + gamma Q z`, a directional derivative solves `(I-gamma Q) dz = db + (dgamma Q + gamma dQ) z`. Start with a frozen topology and a narrowly admitted parameter direction. Prefer a checked directional JVP over a full Jacobian or general autodiff interface.

A retry lesson uses `V = 1 + gamma p V`. At gamma=p=1/2, V=4/3 and dV/dp=8/9. Compare a finite-unrolling derivative with the derivative of the solved fixed point. These are different functions.

Derivatives refer to real parameterized semantics evaluated at rational points. They do not differentiate arbitrary Haskell Rational operations. Probability tangents need lawful row sums and boundary/support rules. Fixed reward directions can avoid those extra gates in the first public slice. Exclude argmax, policy switches, arbitrary callbacks, and differentiation of failure paths. Check differentiated equations independently and meter every added solve and rational operation.

### Resource-aware optimization contract

D-085 promises exact denotation and failure-contract preservation, but cached execution changes cost. Resolve what a work budget measures before cache implementation.

Example: uncached execution needs 100 work units, while a hit needs one. Under budget ten, equal admission requires a source-level accounting policy. Actual-work admission can instead permit only the cached path. These are distinct contracts.

Separate semantic equality, admission, error precedence, and execution cost. Freeze one policy explicitly. Add a worked executable counterexample or model and checked documentation. This task resolves the contract and lesson, not the full D-085 cache implementation.

### Checked state aggregation

Add a checker for a supplied partition of a fixed-policy finite model. Start with a named teaching consumer. Preserve terminal behavior, relevant terminal payoffs, and the joint law of immediate reward and successor block. Preserve any declared observation labels. Return an opaque checked quotient or a distinguishing witness.

Keep horizon and terminal timing explicit. Compare original and lifted quotient finite-horizon evaluations. Do not claim that expected-reward equality preserves reward distributions. Do not search for an optimal partition, solve POMDP abstraction, or infer universal policy preservation from a fixed-policy checker.

## Prior art and confidence

- Glasserman and Yao, *Some Guidelines and Guarantees for Common Random Numbers*, Management Science 38(6), June 1992: <https://business.columbia.edu/sites/default/files-efs/pubfiles/4261/glasserman_yao_guidelines.pdf>. Establishes covariance, synchronization, monotonicity, and qualified benefits of common noise.
- Blondel et al., *Efficient and Modular Implicit Differentiation*, NeurIPS 2022: <https://proceedings.neurips.cc/paper_files/paper/2022/file/228b9279ecf9bbafe582406850c57115-Paper-Conference.pdf>. Establishes implicit root/fixed-point differentiation and its distinction from finite unrolling.
- Givan, Dean, and Greig, *Equivalence Notions and Model Minimization in Markov Decision Processes*: <https://engineering.purdue.edu/~givan/papers/mm.pdf>, published in Artificial Intelligence 147(1-2), 2003, DOI <https://doi.org/10.1016/S0004-3702(02)00376-4>. Establishes reward-respecting stochastic bisimulation and model minimization.

Confidence is high for the verified documentation defects and the mathematical identities. Confidence is moderate for prioritization and implementation effort. Research novelty is unestablished. The proposed contribution is bounded integration, explicit contracts, independent evidence, and executable teaching.
