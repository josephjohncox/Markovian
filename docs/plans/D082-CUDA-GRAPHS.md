# D-082 CUDA multiply-chain graphs

This contract implements the closed fragment recorded in
[D-082](../DECISIONS.md#d-082-add-bounded-cuda-multiply-chain-graphs).
D-077 supplies the unchanged device profile, numeric policy, executor ownership,
and evidence requirements. D-081 supplies admitted immutable affine geometry.

## Scope and schedule

A graph contains typed F64 matrix inputs, lexical references, explicit sharing,
affine views, and matrix multiplication. A prepared graph stores one immutable,
fully admitted topological plan. Inputs with equal bytes or storage identities
remain distinct declarations. Only a lexical reference shares a declaration.

The first implementation uses a transfer-explicit schedule: every multiplication
uses the existing D-077 kernel through one scoped executor. Results are copied
back before their consumers run. Affine gathers, affine pullbacks, transposes,
and cotangent additions execute on the host in fixed order. Every intermediate
and transfer is charged. This deliberately retains the existing kernel ABI,
PTX, profile admission, and maximum three live native allocations per call.

Intermediate transfers increase latency and traffic. This implementation makes no
performance advantage claim.

## Typed public syntax

The new exposed module is `Markovian.Backend.GPU.Graph`. Constructors of refs,
prepared plans, and result vectors remain private; all shape and environment
roles are nominal. `Graph env region rows columns` is closed syntax, with no
stored Haskell functions. Its smart constructors have these signatures:

```haskell
data MatrixRef (env :: [(Nat, Nat)]) (rows :: Nat) (columns :: Nat)
data Graph (env :: [(Nat, Nat)]) region (rows :: Nat) (columns :: Nat)
data PreparedGraph region rows columns
data PreparedGraphVJP region rows columns

matrixHere :: MatrixRef ('(r,c) ': env) r c
matrixThere :: MatrixRef env r c -> MatrixRef ('(a,b) ': env) r c
matrixInput :: FiniteTensor region 'F64 '[r,c] -> Graph env region r c
matrixRef :: MatrixRef env r c -> Graph env region r c
matrixMultiply :: Graph env region r k -> Graph env region k c -> Graph env region r c
matrixLet :: Graph env region a b -> Graph ('(a,b) ': env) region r c -> Graph env region r c
matrixView :: AffineMap map '[r,c] '[p,q] -> Graph env region r c -> Graph env region p q

prepareGraph :: GraphLimits -> Graph '[] region r c -> Either GraphError (PreparedGraph region r c)
prepareGraphVJP :: GraphLimits -> Graph '[] region r c -> FiniteTensor region 'F64 '[r,c] -> Either GraphError (PreparedGraphVJP region r c)
runGraph :: TensorSession region -> BackendRequest -> PreparedGraph region r c -> IO (Either GraphError (GraphResult r c))
runGraphVJP :: TensorSession region -> BackendRequest -> PreparedGraphVJP region r c -> IO (Either GraphError (GraphVJP r c))
```

A ref denotes an already evaluated lexical binding. `matrixLet` evaluates its
bound expression first, even when unused. Preparation visits expression syntax
left-to-right, assigns node numbers only to input/view/multiply nodes, and lowers
refs to existing node numbers. Multiplication appends its node after both inputs.
The prepared plan contains no lexical syntax, callbacks, or arbitrary node IDs.
This makes forward references, cycles, and incompatible matrix dimensions
unrepresentable through public construction. Preparation still checks the
resulting dependency order and represented dimensions before execution.

Results expose copied ordinary Double values, the deterministic plan report,
and actual backend selection. VJP results expose the forward output and one
shape-labelled gradient per input declaration in declaration order, including
zero gradients for inputs used only by an unused binding. Result list lengths
are admitted and finite before publication. Gradients are with respect to each
input's logical coordinates; an input tensor's pre-existing physical aliasing is
not inferred as graph-level sharing.

## Affine admission and tensor hook

`matrixView` accepts only an existing opaque D-081 `AffineMap` from a rank-two
logical base to a rank-two result. Every signed, injective, in-range D-081 map is
admitted, including transformed negative-stride maps. Empty matrix dimensions
are rejected by D-082, consistently with the existing D-077 launch ABI.

Add one pure read-only observer in `Markovian.Tensor.Affine`:

```haskell
affineMapDescriptor :: AffineMap map base view -> (SShape base, SShape view, Integer, [Integer])
```

It returns the already admitted shapes, normalized offset, and strides. It does
not construct a witness, reveal an owner or pointer, or weaken D-081 admission.
D-082 checks dimensions and reserves its complete resource ledger before constructing an
address vector in ascending logical output order. It verifies all represented
addresses and injectivity before retaining that vector. Gather maps base values
to output values. Pullback starts with positive zero in every original logical
base coordinate and assigns the seed at each admitted address. Contributions
from separate graph edges are then added in the declared reverse order.

## Resource ledger and failure order

`graphLimits` takes eight Natural limits in this order: syntax nodes, matrix
dimension, matrix elements, cumulative transfer bytes, cumulative host payload
bytes, peak device payload bytes, scalar work, and launches. All are capped by
the machine Int maximum before arithmetic. The syntax traversal inspects at most
node-limit + 1 constructors and bounds lexical-path traversal under the same
syntax ledger. Shape products must also fit the existing CInt matrix ABI.

Preparation first admits bounded syntax and reference paths, then matrix
dimensions/elements and admitted affine descriptors, then computes the complete forward
or forward-plus-VJP schedule. It checks transfer, host payload, device payload,
work, and launch limits in that order. Only after these limits pass are affine addresses checked for bounds and
injectivity in logical/pair order. No executor admission, tensor reads,
tensor payload allocation, or transfer occurs during preparation. Strictly
forcing the complete flattened plan and report is part of preparation.

For each multiplication of r-by-k by k-by-c, count 2*r*k*c scalar units, one
launch, 8*(r*k+k*c+r*c) transfer bytes and that same device payload peak. Reverse
reserves both gradient products, with shapes (r,c,k) then (k,r,c), after the
forward graph. A graph without matrix nodes performs zero user launches. CPUOnly runs on the
host; RequireCUDA still requires successful device admission and reports that
admission with an empty timing list. PreferCUDA applies the same admission and
fallback policy. Affine operations remain host operations in every case.

The following table freezes the logical resource coefficients. Let e(n) be a
node's elements, I the sum of input elements, O the output elements, N the sum
of every stored node's elements, and S=a+b+z for a matrix call's operand and
result elements. A view has base B and output V elements. All table rows are
summed, including unused bindings. Work counts the listed scalar/geometry units,
not linked-list traversal, GHC evaluation, or total CPU instructions.

| Scheduled event | Host F64 words | Transfer bytes | Device bytes | Work | Launches |
| --- | ---: | ---: | ---: | ---: | ---: |
| Input snapshots, once | I | 0 | 0 | I | 0 |
| Forward input node | 0 | 0 | 0 | 0 | 0 |
| Forward view | V | 0 | 0 | V | 0 |
| Any matrix call (r,k,c) | 5*S | 8*S | 8*S peak | 2*r*k*c | 1 |
| Final forward-value publication | O | 0 | 0 | O | 0 |
| VJP seed snapshot, once | O | 0 | 0 | O | 0 |
| VJP cotangent zero initialization | N | 0 | 0 | N | 0 |
| Seed insertion at output | O | 0 | 0 | O | 0 |
| Reverse view scatter | B | 0 | 0 | B+V | 0 |
| Reverse multiply operand transposes | a+b | 0 | 0 | a+b | 0 |
| Each reverse edge accumulation | parent elements | 0 | 0 | parent elements | 0 |
| Gradient publication | I | 0 | 0 | I | 0 |
| Preparation of each view address map | 0 | 0 | 0 | 4*V+V*(V-1)/2 | 0 |

The matrix staging coefficient 5*S conservatively covers copied operand lists,
finite input tensors, CPU output tensor and copied output, or native FFI staging
and returned values. It is a reserved upper bound, not a measured allocation
count. Final publication means strict validation of ordinary copied values;
sharing already copied lists does not refund the reservation.

Host payload bytes are 8 times (snapshot words plus **twice** all remaining
execution words). Runtime work is likewise input/seed snapshot work plus twice
all execution work, plus preparation-map work. This reserves one failed
pre-launch attempt and one complete CPU fallback. Transfer bytes and launches
reserve one complete CUDA schedule; a permitted CPU fallback transfers nothing.
Device bytes are the maximum graph-call requirement. The context, module, and
fixed admission self-test remain separate D-077 executor admission costs. Existing tensor-session
limits remain a separate cumulative authority and may reject staging; no graph
ledger overrides them. Retry of a separately invoked run is not covered.

Metadata is bounded by the admitted syntax/path count, nodes, and at most one
address per admitted view element; it is not included in F64 payload bytes.
List cells, GHC headers, arbitrary caller thunks, allocator metadata, diagnostics,
and OS resources are excluded. The report exposes input count, plan-node count,
forward and reverse launches, affine coordinates, and accumulation coordinates.
This is not a physical-memory or total-instruction theorem.

All additions and multiplications saturate at the machine limit plus one;
rejected huge products are not materialized. A failure returns neither a plan,
partial result, nor success report. Failure types distinguish syntax budget,
dimension/element errors, affine-map errors, each resource field, tensor runtime
failure, CUDA failure, and nonfinite intermediate/output coordinates.

## Runtime, reverse order, and lifetime

Inputs are read inside their original tensor-session callback, before backend
admission; their immutable logical values are snapshotted once per declaration.
Forward executes increasing node numbers. Reverse starts with the supplied
output seed, visits decreasing node numbers, computes the left matrix gradient
before the right, and adds each contribution into its parent in that order.
For a multiply whose operands reference one node, both contributions are added.
All coordinates are checked finite before the next dependent operation.

CPU execution uses ascending-k separate multiplication/addition. CUDA matrix
execution uses the unchanged D-077 FMA kernel. Affine and accumulation operations
use the same fixed host order on both paths. Exact semantics interpret input and
seed words as Rational dyadics and perform exact matrix, gather, and transpose
pullback operations; floating intermediates do not redefine that denotation.

One `withCUDAExecutor` spans the entire prepared schedule. The graph records
whether any earlier user launch was committed; a later pre-launch-looking
failure is upgraded to committed failure. Cleanup failure always prevents
fallback. `PreferCUDA FallbackBeforeUserLaunch` may rerun the CPU schedule only
when no graph launch was committed and the executor was safely closed. Runtime
host numeric/tensor failures do not trigger fallback. If teardown also fails,
`GraphHostAndCleanupFailure` retains that typed host primary and the CUDA cleanup
error. Final validation and forcing of every copied output, gradient, shape and
timing occurs inside the executor callback, before teardown. Ordinary result
observers do not permit record updates. Device cleanup and
poisoning retain D-077 behavior unchanged. No native buffer survives a successful
matrix call. Tensor runtime allocations and all dependent children must finish
inside their owning session, under D-081's existing lifetime discipline.

## Verification

Tests independently expand small exact Rational graphs; they do not call the
production planner or reverse interpreter. Cover two- and three-product chains,
shared square inputs, unused bindings, reordered declarations, rectangular
signed/sliced/permuted affine views, and all declared input coordinates. Check
both CPU and CUDA results separately against exact values using D-077's numeric
policy. Check every VJP coordinate against independent symbolic differentiation
or exact central differences on fixtures whose relevant degree makes that
oracle exact; also check the full coordinate JVP/VJP pairing.

A fixed plan golden and independent schedule-count fixtures cover exact and
one-below limits for all eight fields, syntax/ref scanning, shared nodes,
malformed competing limits, dimensions, transfer, host/device payload, work,
and forward/reverse launch counts. Nonfinite intermediates return no partial
output. Compile-fail fixtures check constructors, nominal roles, closed scopes,
and incompatible shapes. Runtime tests cover explicit disabled-build failure,
pre-launch fallback, and second-or-later launch, synchronization, copy-back,
free, and teardown faults with no fallback after commitment.

Hardware execution belongs to the root workflow owner: D-077 same-session
correctness, all applicable sanitizer, benchmark, receipt and signature checks
must bind the implementation revision. Existing historical receipts do not
attest D-082. The verified run below supplies that evidence for its named revision. Deployment
and promotion require evidence bound to their actual revision under D-077.

## Protected validation — 2026-09-14

[Run 34884826237, attempt 2](https://github.com/josephjohncox/Markovian/actions/runs/34884826237/attempts/2)
passed at `e9612a3ddfe1b14ff3a471bfceaedfabb4a54232`. Its session is
`github-34884826237-2-e9612a3ddfe1b14ff3a471bfceaedfabb4a54232`.
The graph's exact dyadic, all-coordinate VJP, budget, and fault tests completed
in correctness and each of the four sanitizer logs. `memcheck`, `initcheck`, and
`synccheck` reported zero errors; `racecheck` reported zero hazards, errors, and
warnings. The existing transfer-inclusive kernel benchmark returned checksum
`49439/128` over twenty ordered measured samples. This does not measure graph speedup.

The run used GB10 UUID `GPU-ac353d74-ffaf-96d2-7849-b8d03d5cd1a7`,
GHC 9.14.1, CUDA 13.0, driver 580.173.02, native driver API 13000, and Compute
Sanitizer 2025.3.1.0. The unchanged receipt validator passed with explicit
revision, session, and UUID arguments, binding all six records, commands,
executables, observations, raw samples, profile, and PTX.

**Signature verification:** `gh 2.100.0 attestation verify` passed separately
for all 14 downloaded files, using the same selected Sigstore bundle and default
signature, claims, and transparency checks. Arguments constrained
`--repo josephjohncox/Markovian`, `--source-digest` and `--signer-digest` to the
revision above, `--source-ref refs/heads/feat/remaining-backlog`,
`--cert-oidc-issuer https://token.actions.githubusercontent.com`, and exact
`--cert-identity https://github.com/josephjohncox/Markovian/.github/workflows/cuda-hardware.yml@refs/heads/feat/remaining-backlog`.

The verified certificate independently binds that issuer, repository, source
revision/ref, signer/config revision, self-hosted runner, dispatch event, and
run invocation ending `/34884826237/attempts/2`. The verified Rekor timestamp is
`2026-09-14T19:13:31Z`. The signed predicate agrees with those certificate fields;
its assertions alone were not used to establish identity. The signed subject map
matches exactly the 14 downloaded names and hashes below. A separate reviewer
checked the receipt, subject map, graph markers, and certified bindings.

The verifier binary SHA-256 is
`28a037b967065aa314cb6d539943b55d27ef2f97c523ab2b6023ccf284e1828d`;
the selected bundle SHA-256 is
`99dd5c4afcd535b00aeded1245730897b9c33c05d1719ca33aae5f8632be25ad`.

| Subject | SHA-256 |
| --- | --- |
| `benchmark-executable` | `87003141b9e360afa80ea7b6b96585e0e70b820f4b3c7a4a592ebff93e9f9151` |
| `benchmark.log` | `7a35384f8cff036064ed2c96218dc79739ac91fe1908cf8a4d32d021ed507449` |
| `correctness.log` | `3672406e5fa35dc7ada48bccd6a44ae5547cfe5be8ddfa716535ac15474d1ea2` |
| `device-query.log` | `c5a9074437002e7389bcce69e77b7fc61f9543085b80485258b35f908d903698` |
| `markovian_dense.ptx` | `a01d2c898a78dc4f603a8919c9f84019b79066201bc12430fedc725ac97f6239` |
| `profile.json` | `5fbed61193cf483a2ff5642c7487ad052add4ed52da1a83a110da4711c7480dd` |
| `receipt.json` | `b9b55df4396a955638dbce19257b30cd15a671ce9521b7f9ce021d1ef051de5b` |
| `sanitizer-initcheck.log` | `df9e9b7c200bfbda6ca70a05d09f9358405468aafca26e975a4f3fb50cc26cc7` |
| `sanitizer-memcheck.log` | `7d56a10639eabe24f92f539388fdb86febb9fe2528c5639e8d449e3dfb98c5b7` |
| `sanitizer-racecheck.log` | `6ee59e17e4ef7b2f69182a5e4eed3a8aea528b6a2b688e4c61d5e952ca5e2f14` |
| `sanitizer-synccheck.log` | `1efe9df001befce0020dc195556466936a71a20cc9533e1fd817d993243cf8f0` |
| `sanitizer-version.log` | `958b7c9c5c577e9382369664b0c1704b4dccef3b6d3b9835b0f6d6f1cb280e0d` |
| `test-executable` | `752fc5f11fb1dc4a5c404b323b10b07728c5bf381222fe97bcb6bd0d42bcab42` |
| `toolkit-version.log` | `bdfae8c97a7f2b553bc5fd8891d113ee5a5e09818e3531e9bf58f80392df9a40` |

**Availability:** GitHub artifact `10363778769`, named
`cuda-hardware-e9612a3ddfe1b14ff3a471bfceaedfabb4a54232-2`, contains the raw
14-file evidence set. Its downloaded ZIP SHA-256 matches GitHub metadata:
`bfd90fb66bbfd6957646de64943a308bec0e9909899ccc6f6e1dce93e25a6f56`.
It expires `2026-12-13T19:11:17Z`. This compact record preserves past verification;
raw evidence is still required for any new verification or promotion.

Extraction produced executable modes `0664`. After signature verification,
only owner execute permission was restored in a separate byte-identical copy
(`0764`) for the unchanged receipt validator. No downloaded executable was run.
The ephemeral runner processed this job, removed its registration, and exited
successfully; the subsequent repository API query returned zero runners.
The named environment has no required-reviewer or branch restrictions.

**Failed attempt:** Attempt 1 stopped at racecheck with
`CUDAContextCreate / CUDA_ERROR_OUT_OF_MEMORY`, despite a zero-hazard summary.
It produced no attested receipt. A later local diagnostic passed with the same
binary, but the allocation failure's cause was not established. No records from
attempt 1 or unsigned local runs were combined with attempt 2.

Acceptance is bounded and unreleased. The implementation inputs match
`03493e83bd842d2e9c0b41d0c0de4ab4380e4dc3`; changes through the protected tested
revision are documentation. The subsequent acceptance edit changes only
`README.md`, `TODO.md`, `docs/CONTEXT.md`, `docs/DECISIONS.md`,
`docs/book/src/capabilities.md`, `docs/capabilities/current.json`, this contract,
and `release/API-REVIEW.md`. [PR #13](https://github.com/josephjohncox/Markovian/pull/13)
records the reviewed documentation delta endpoints. The hardware evidence
attests the exact tested revision above, not its descendants.
