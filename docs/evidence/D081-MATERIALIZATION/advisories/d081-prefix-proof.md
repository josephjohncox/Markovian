# Prefix resource derivation

This preliminary derivation supplies detailed expansions for the
[reconciled source proof](../SOURCE-PROOF.md). Use that proof's substitutions
and the materialization addendum's final coefficients when they differ from
this document. Line references use the [historical source basis](../README.md#source-basis).
The units are logical source operations and storage slots.

## 1. Result supplied to the integrator

The following is one deliberately conservative, numerically instantiated **section-A envelope**:

\[
\begin{aligned}
F_W&=2048, &F_H&=448, &F_R&=64,\\
u&=384, &t&=24, &s&=12.
\end{aligned}
\]

For \(e\) successfully admitted inspections, across all scans:

\[
\boxed{
\begin{aligned}
W_A(e)&=2048+384e,\\
H_A(e)&=448+24e,\\
R_A(e)&=64+12e,\\
Q_A(e)&=512+36e,\\
L_A(e)&=16896+3108e.
\end{aligned}}
\]

The construction identity is displayed, not assumed:

\[
8W_A+Q_A
=8(2048+384e)+(512+36e)
=16896+3108e.
\]

The derivations below establish:

- fixed source work at most **1618**, rounded to2048;
- worst admitted-node work at most **323**, rounded to384;
- at most **11 potential-survivor slots per inspection**, rounded to12;
- at most **21 accumulated transient slots per inspection**, rounded to24;
- fixed storage fitting the stated448 transient and64 potential-survivor reservations.

These numbers are sufficient bounds, **not proposed exact measured costs**.

### Composition interface

Let the body lane supply \(W_B,H_B,R_B\), excluding section-A work and objects.

A safe initial composition is:

\[
\begin{aligned}
W_o&=W_A(e_o)+W_B,\\
R_o&=R_A(e_o)+R_B,\\
Q_o&\ge
\max\{Q_A(e_o),\,R_A(e_o)+H_{\mathrm{handoff}}+H_B+R_B\}.
\end{aligned}
\]

The conservative handoff allowance is \(H_{\mathrm{handoff}}\le448\). The variable \(24e_o\) consists of scanner/producer evaluation frames; those evaluations have returned before post-header processing and do **not** need to remain live throughout the body.

An even simpler, looser composition is:

\[
H_o=448+24e_o+H_B,\qquad R_o=64+12e_o+R_B.
\]

That deliberately retains the whole prefix transient allowance in the body peak. It is safe but unnecessarily expensive.

**Do not add another global entry/terminal F.** Runtime transaction, registry, allocation, and publication primitives still require their own source costs; those are not a second generic F.

---

## 2. Elementary expansion convention

This uses P’s elementary slots and primitives. It does not substitute function calls or traversals for elementary allocations.

For reproducibility, the tables use these conservative expansions:

| Notation | Expanded work |
|---|---:|
| Read/project one field |1|
| Inspect a constructor and select its alternative |2: inspection + branch|
| Scalar arithmetic or comparison, including its result assignment |2|
| Additional conditional control transfer |1|
| Select `min`/`max` result |4: comparison/result2 + branch1 + selected-result assignment1|
| Record/tuple with \(k\) payload fields |\(k+1\) slot assignments, including its constructor/control slot|
| List cons plus element |2 slot assignments, as required by P|
| Call with \(a\) explicit arguments |\(a+2\): argument assignments, dispatch, return; **callee added separately**|
| Consume an `Either` bind |3: tag inspection, alternative selection, result-reference extraction|
| `Right ()` |3 slot assignments, conservatively including unit|
| `affineFailure problem` |call3 + `TensorAffineError`2 + `Left`2 =7|

A borrowed existing referent is not copied recursively. New closures are expanded into their code/control and captured-reference slots. Dictionary access is field/reference access; it is not permission to execute an arbitrary instance for constant cost.

### Why each elementary step constructs at most eight slots

The source operations reduce to:

1. **Field/tag inspection:** at most one result slot; a branch may use a two-slot control frame.
2. **Scalar operation:** bounded operands, at most a two-slot widened result and two-slot temporary; even retaining the overwritten bounded result fits eight.
3. **Field assignment:** one assigned slot; allocation of the enclosing record is not an additional bulk primitive.
4. **Call/return:** argument assignments are separate steps. Dispatch/return needs at most one two-slot frame and bounded result/control references.
5. **Constructor/list construction:** every field/link/element assignment is separately charged.
6. **Memoization:** the newly installed reference is a separately charged assignment; it does not recursively copy its referent.

Thus the claim applies to these expanded primitives, not to `affineDebit`, `natVal`, `knownShape`, an entire scanner, or a library list traversal as one step.

---

## 3. Arithmetic and diagnostic callees

### 3.1 Saturated arithmetic

Source: I:453–467.

| Function/branch | Expanded body work | Including argument/return administration |
|---|---:|---:|
| `affineAdd`, successful worst branch | two operand guards6 + subtraction/comparison/branch5 + addition/result2 =13 |17|
| `affineAdd`, over-cap operand | at most6 + sentinel2 =8 |12|
| `affineAdd`, subtraction guard fails |6+5+2 =13|17|
| `affineMultiply`, successful worst branch | zero guards6 + operand guards6 + division/comparison/branch5 + product/result2 =19 |23|
| `affineMultiply`, zero | at most6 + result assignment1 =7 |11|
| `affineMultiply`, operand/product rejection | at most6+6+5+sentinel2 =19 |23|

Short-circuit exits are bounded by the displayed worst branches; no rejected product is formed.

### 3.2 Credit and old-policy checks

Source: I:469–472, 519–527.

`affineCredit`:

- success body: comparison/result2 + branch1 + `Right ()`3 =6;
- failed body: comparison/result2 + branch1 + cap+1/result2 + error-record4 + `affineFailure`7 =16;
- including three-argument call: **11 success,21 failure**.

The scanner’s `oldCheck` has:

- Maybe inspection/branch2;
- selected limits reference1;
- selector call and field access4;
- `min M oldCap`4;
- comparison/branch3;
- success `Right ()`3, or failure cap+1/result2 + shape-error3 + Tensor wrapper2 + Left2.

Including its three-argument call gives **22 success,28 failure**.

The inline old-dimension check additionally converts only its **bounded cap**, not the supplied dimension. Its worst branch is **22**, before the surrounding `Either` bind.

### 3.3 Bounded numeric invariant

Source: I:442–467, 493–527; P §§5,6.3–6.4.

Let \(M=\maxBound::Int\).

- Validated affine policy fields are nonnegative Int conversions, hence at mostM.
- Successful cumulative meters and admitted local Q are at mostM.
- Rejected requirements and diagnostics are at mostM+1.
- `seen` is at most the accepted rank. A rank/length sentinel computes at mostM+1.
- The shape product is exact throughM and otherwise exactlyM+1.
- Product0, including a late zero, overrides the prior M+1 sentinel.
- `affineMultiply` handles zero before division.
- `affineAdd` checks operands before Natural subtraction.
- Old arbitrary Natural caps are compared withM and selected by `min`; no giant converted copy is required.
- The supplied `natVal` result is compared against bounded caps before `fromInteger` creates the scanner’s Natural copy.
- Nil compares count with `M div 8`; it does not first construct an oversized byte product.

The logical treatment of `natVal` is **evidence projection plus a numeric view/reference**, not a bit-complexity theorem about an arbitrary-size supplied number. Arbitrary caller-written instance computation remains excluded. This distinction is essential: the schedule does not certify arbitrary dictionary execution or a physical GHC big-number allocation bound.

---

## 4. Stock witness production and its retention

### 4.1 `knownShape`

Source: S:25–39.

The stock cons instance is:

```haskell
knownShape = SCons Proxy knownShape
```

Its output has a recursive suspended tail. Obtaining the outer constructor does not produce the complete witness.

For one newly demanded stock cons, count:

| Operation | Work | Potentially surviving slots |
|---|---:|---:|
| Select supplied instance evidence |2|0; old referents|
| Method dispatch/return |3|0|
| Build recursive-tail recipe: code and remaining dictionary reference |2|2|
| Produce/share `Proxy`, conservatively allowing a new control slot |1|1|
| Assign `SCons` constructor, KnownNat reference, proxy reference, tail reference |4|4|
| Install memoized result reference |1|1|
| Producer control selection |2|0|
| **Total** |**15**|**8**|

The original dictionaries are not recursively charged again. The **new reference to a dictionary in SCons is charged**.

For stock `SNil`, a constructor/result update and dispatch/return fit **4 work and2 surviving slots**, below the cons allowance.

### 4.2 Dimension evidence access

For the logical evidence projection used by `natVal`, expand:

- two argument/reference assignments plus dispatch/return:4;
- dictionary field access1;
- method selection1;
- borrowed dimension-reference assignment1;
- numeric-view constructor/reference assignments2;
- possible result memoization1;
- proxy/evidence access1.

Total **11 work**, with at most **3 new potentially retained view/memo slots**. No recursive copy of the existing large-number referent is charged or assumed.

Consequently, the stock-witness-plus-evidence survivor bound is:

\[
8+3=11\le12
\]

per admitted inspection. Charging12 also on nil, raw-list nodes, preforced witnesses, and repeated scans is intentional overreservation.

### 4.3 Which nodes may be produced?

A failed debit cannot call the witness producer for the rejected node. A successfully admitted cons may produce its SCons and its suspended **tail recipe**, but not the next SCons.

Therefore:

- rank failure may leave the excess SCons materialized;
- it does not demand that cons’s dimension value;
- the surviving tail recipe’s references are already included above;
- subsequent body use or caller reuse cannot make the materialized prefix disappear from the first attempt’s local survivor theorem.

For map-owned witnesses, successful earlier construction established the admitted witness invariant. Their referents are pre-existing at bind/pullback entry. Their required rescans still consume work coupons; charging12 survivors per inspection remains safe but loose.

### 4.4 `shapeDimensions` is not a header callee

Source: S:47–50; I:493–545.

Neither scanner calls `shapeDimensions`, `shapeRank`, or `shapeElements`.

For the body lanes’ producer inventory, one fully demanded bounded `shapeDimensions` cons can be expanded as:

\[
\begin{split}
&\text{match }2+\text{field accesses }3+\text{stock witness }15\\
&+\text{evidence projection }11+\text{bounded conversion }2\\
&+\text{tail recipe }2+\text{value recipe }3+\text{memo update }1\\
&+\text{cons assignments }2+\text{call administration }3+\text{forcing control }3\\
&=47\le48.
\end{split}
\]

When the header has already produced the witness, remove the stock-production15: **32 per cons** suffices for this schedule. Nil fits8.

A conservative newly surviving dimension-output allowance is **12 slots per axis**, separately covering the two-slot list output, lazy tail/value references, bounded value, and memo/view references. This is a **producer interface**, not an extra prefix charge. The descriptor lane must count its actual uses and overlap with stride production; it must not add both this allowance and an independently expanded duplicate for the same objects.

The stock-witness allowance above and descriptor-list allowance here describe different objects.

---

## 5. Growing-live debit

### 5.1 Prospective source shape

Immutable I:475–491 carries only cells/work and constant live384. D adds a local-live field for the experiment.

The production-shaped schedule being bounded is:

```text
header = (cumulativeCells, cumulativeWork, localQ)

nextCells = satAdd(cumulativeCells, dc)
check cells
nextWork = satAdd(cumulativeWork, dw)
check work
nextLocalQ = satAdd(localQ, dq)
check max(oldSuccessfulPeak, nextLocalQ)
return (nextCells, nextWork, nextLocalQ)
```

The constants derived here are:

\[
dc=3108,\qquad dw=384,\qquad dq=36,\qquad localQ_0=512.
\]

Computing candidate numeric values early is not input inspection, but the error tests must retain cells → work → live precedence.

### 5.2 Successful debit count

| Component | Work |
|---|---:|
| Header match and three field extractions |5|
| Three expanded saturated additions |\(3·17=51\)|
| Three policy-cap accesses and old-peak access |4|
| `max` expansion |4|
| Three successful credit calls |\(3·11=33\)|
| Three `Either` binds |9|
| Three-field header plus `Right` |6|
| Debit call administration |5|
| **Total** |**117 ≤128**|

### 5.3 Failed debit counts

Including failure construction and outer debit call:

- cells failure:
  \[
  5+17+1+21+3+5=52;
  \]
- work failure:
  \[
  5+34+2+(11+21)+6+5=84;
  \]
- live failure:
  \[
  5+51+4+4+(11+11+21)+9+5=121.
  \]

Thus **128 fixed work covers the one rejected debit**. It belongs to F, not to a fictitious admitted inspection.

No second failing debit is executed after propagation begins.

### 5.4 Local Q is not historical peak

The local meter must return `nextLocalQ`, **not** `max(oldPeak,nextLocalQ)`.

For the review’s example:

```text
old    = (3,2,9)
local  = (3,2,4)
step   = (1,1,1)
caps   = (31,31,31)
```

the next local state is `(4,3,5)`, while the live comparison uses9. A second debit advances local live to6, not10.

Historical peak is a successful-path admission comparator. It is not storage newly created by the current attempt, and it must not become the base of the current attempt’s live slope.

---

## 6. Complete per-inspection scanner work

Source: I:493–545. The following sums deliberately include mutually exclusive success/failure continuations where doing so simplifies a uniform bound.

### 6.1 Shape cons

| Component | Worst work |
|---|---:|
| Shape tag/branch and three fields |5|
| Newly demanded stock witness |15|
| Dimension-evidence projection |11|
| Old rank: increment2 + oldCheck28 + bind3 |33|
| Affine rank: increment2 + cap access1 + credit21 + bind3 |27|
| Old dimension branch22 + bind3 |25|
| Affine dimension branch, including repeated cap projections and failing credit |34|
| Admitted Natural conversion |2|
| Zero comparison/short circuit/result |4|
| Product branch, expanded multiply, result transfer |25|
| Recursive arguments/increment and return propagation |15|
| Strict scanner-register inspections |4|
| Debit continuation’s six captured/control slots |6|
| **Non-debit subtotal** |**206**|
| Successful growing-live debit |117|
| **Total** |**323 ≤384**|

The affine-dimension34 is:

\[
1+2+2+1+2+2+21+3=34,
\]

for initial cap access, bounded cap conversion, comparison, branch, repeated cap accesses, sentinel addition, failed credit, and bind.

Failure before a later row simply omits that later work.

### 6.2 Shape nil

A nil inspection has no dimension access or product multiplication.

\[
\begin{aligned}
&\text{nil production }4+\text{strict registers }4+\text{case }2\\
&+\text{old element check/bind }31\\
&+\text{affine element cap/check/bind }25\\
&+\text{machine quotient/compare/branch }5\\
&+\text{machine diagnostic }8+\text{return propagation }6\\
&+\text{debit }117
=202.
\end{aligned}
\]

The alternative success triple plus `Right` costs6, below the machine-diagnostic8 used in this sum.

Thus nil success, old-element failure, affine-element failure, scalar failure, and machine failure all fit384.

### 6.3 Raw-list scan

Source: I:532–545.

A uniform post-debit bound is:

- constructor/branch/tail access:4;
- equality comparison/branch:3;
- long/short diagnostic including sentinel addition:13;
- recursive increment, argument transfer, and propagation:13;
- strict registers:2.

Together:

\[
117+4+3+13+13+2=152\le384.
\]

This includes the exclusive long/short/recursive alternatives conservatively.

Crucially:

- exact nil succeeds;
- short nil reports the actual bounded `seen`;
- excess cons reports `expected+1`;
- excess cons does not inspect its element or tail;
- raw-list element production is not a header operation.

Arbitrary caller computation required to produce a supplied lazy raw-list spine is outside the engine computation theorem. That exclusion does **not** apply to S’s stock framework witness producer, which is explicitly expanded above.

### 6.4 All exit branches

| Exit | Admitted-node work bound |
|---|---:|
| Ordinary nonzero cons |323|
| First zero / later dimension after zero |≤323|
| Product becomes M+1 |≤323|
| Old rank / affine rank failure |≤323; dimension unforced|
| Old dimension / affine dimension failure |≤323|
| Nil success |≤202|
| Old element / affine element / machine nil failure |≤202|
| List ordinary cons, exact nil, short nil, excess cons |≤152|
| Rejected debit |0 admitted-node work; ≤128 in F|

The **384-work coupon is derived from323**, not inferred from the old128 coupon or demand markers.

---

## 7. Iterations, frames, and survivor schedules

### 7.1 Iteration bounds

For a rank-\(r\) shape that reaches nil, exactly \(r+1\) inspections occur.

For a rank sentinel at zero-based axis \(k\), exactly \(k+1\) inspections occur in that scan. The excess dimension is unforced.

For expected list length \(r\):

- exact list: \(r+1\);
- short list with \(t<r\) entries: \(t+1\);
- long list: \(r+1\), ending on the excess cons.

All counts include earlier completed scans. There is no per-scan reset.

Complete valid headers, from I:743–772, 823–934, 1087–1095, 1193–1204:

| Operation | \(e_o\) |
|---|---:|
| New map |\(r_B+2r_V+3\)|
| Permute |\(r_B+2r_S+r_V+4\)|
| Reverse |\(r_B+r_S+r_V+3\)|
| Slice |\(r_B+3r_S+r_V+5\)|
| Bind |\(r_B+r_V+2\)|
| Pullback |\(r_B+2r_V+3\)|

Pullback’s third scan is the retained map view **again**, not a demand of the actual seed wrapper.

### 7.2 Accumulated transient allowance

Do not rely on GHC tail-call optimization or erase source-level `Either` continuations.

Per admitted inspection, conservatively allow simultaneous:

| Transient object | Slots |
|---|---:|
| Debit-result continuation: six captures/control slots plus frame |8|
| Recursive scanner argument registers and frame |7|
| Stock producer activation, dictionary/update-target references |4|
| Return/result shell |2|
| **Total** |**21 ≤24**|

Some of these lifetimes cannot actually accumulate together. Reserving24e deliberately avoids needing that optimization.

Every frame/capture assignment is covered by the expanded call, continuation, and return work above. These are not uncharged allocation objects.

### 7.3 Potential survivors

Per inspection:

- stock witness constructor/proxy/tail recipe/memo reference:8;
- dimension-evidence view/memo references:3.

Hence11≤12.

At failure, these may remain reachable through the caller’s witness root. At success they can remain reachable through that root, through the new map, or both. Count the materialized object once; charge each newly assigned reference separately.

### 7.4 Phase schedule

| Phase | Newly constructed variable objects | Transient live bound | Potential survivor bound |
|---|---|---:|---:|
| Before first debit |None|fixed448|fixed64|
| During admitted inspection \(j\) |Witness/evidence output, debit/scan frames|448+24j|64+12j|
| Rejected next debit |Only fixed bounded arithmetic/error objects; no next node|448+24e|64+12e|
| Semantic scanner failure and unwind |No further input production|448+24e|64+12e|
| All scans returned |No scanner-frame chain remains|≤448|64+12e|
| Body begins |Prefix witnesses and bounded facts remain available|≤448 carried conservatively|64+12e|
| Attempt cutoff |Caller/result/state/diagnostic reachability determines actual survivors|transients released|≤64+12e, plus body survivors|

A repeated attempt can start with previously materialized witness nodes as old input. That gives a smaller actual construction count on the later attempt. It does not refund the earlier attempt or establish a global retry quota.

---

## 8. Fixed policy, startup, formula, and preflight work

### 8.1 Policy/accessor/startup functions

Source: I:402–440.

For `affineLimits`:

- six nonnegative checks, each bounded by20 including its call/bind:
  \(6·20=120\);
- three minimum checks, each bounded by26:
  \(3·26=78\);
- six conversions12 + policy record7 + Right2 =21;
- outer call administration8.

Total:

\[
120+78+21+8=227\le256.
\]

Each minimum-check26 expands as:

\[
\text{call }5+\text{conversion/comparison/branch }5+
\text{error fields/conversion/wrapping }13+\text{bind }3.
\]

All six negative checks precede all minima, as the source requires.

Other fixed functions:

| Function | Expanded upper work |
|---|---:|
| `tensorSessionLimitsWithAffine` |old record case2 + seven copied fields7 + new record9 + Just2 + call4 =24|
| `affineBudget`, including `affineInitialUsage` |20|
| `affineBudgetUsage` |call3 + case2 + selected field1 =6|
| Each bound-view accessor |6; shares existing referent|

The combined conservative policy/setter/budget/usage schedule is:

\[
227+24+20+6=277.
\]

These calls are not all executed inside every affine operation; including their sum in the common fixed bound is conservative.

### 8.2 Runtime entry

Source: I:954–978.

The worst branch includes:

\[
\begin{aligned}
&\text{closed/Maybe entry }6+\text{usage access }1
+\text{storage-cap selection }4\\
&+\text{counter/policy field accesses }13
+\text{ten comparisons }20
+\text{ten branch/short-circuit steps }10\\
&+\text{bounded result/error }8+\text{call }5
=67.
\end{aligned}
\]

Closed and disabled exits execute proper subsets. Counter failure does not force shape/list input.

Changing startup reservations requires changing the corresponding prospective runtime-entry minimum checks consistently; immutable source still tests2432/256/384.

### 8.3 Old payload preflight

Source: I:980–991; callers I:1091–1095 and1199–1204.

Four expanded checks cost \(4·24=96\). Add:

- three saturated additions:51;
- four limit and three state field accesses:7;
- call administration:7;
- pullback’s bounded byte/work argument arithmetic:4.

Total:

\[
96+51+7+7+4=165.
\]

No registry, actual tensor wrapper, seed, or managed payload is inspected here.

### 8.4 Formula expansion

Source: I:549–570.

Count actual saturated-operation calls, including the strict common computations even in branches that do not use their values:

| Branch | `affineAdd` calls | `affineMultiply` calls |
|---|---:|---:|
| New |12|10|
| Transform |13|12|
| Bind |9|9|
| Pullback |16|15|

For the largest branch:

\[
16·17+15·23=617.
\]

The remaining work is:

- zero/parity/pair-factor control and arithmetic13;
- kind dispatch2;
- selected triple construction/projection7;
- charge construction5;
- outer call9;
- aliases2.

Total:

\[
617+13+2+7+5+9+2=655.
\]

`even`, division, subtraction, `min`, and `max` are expanded scalar operations, not opaque library coupons. No V-sized loop occurs in `affinePlan`.

**Integration condition:** these counts cover the retained formula source. Replacement body formulas must be audited against the fixed bound after assembly; arbitrary extra formula traversals are not authorized by the rounding margin.

### 8.5 Full reservation

Source: I:572–582.

\[
\begin{aligned}
&\text{two saturated additions }34
+\text{field accesses }9\\
&+\text{three credit calls, conservatively all failing maxima }63
+\text{binds }9+\max4\\
&+\text{usage/Right construction }6+\text{call }5
=130.
\end{aligned}
\]

The successful stored peak is the maximum of old peak and the full operation’s **local** Q.

### 8.6 Fixed scanner orchestration

Source: I:743–772, 823–934, 1087–1095, 1193–1204.

A worst slice-shaped fixed schedule allows:

- entry map/session/binding/budget destructuring and usage selection:28;
- header start, including its call and three-field construction:9;
- three shape-scan starts/results at20 each:60;
- two list-scan starts/results at17 each:34;
- transform rank comparison, bounded diagnostic, and header-result packaging:33.

Total:

\[
28+9+60+34+33=164.
\]

The recursive work is not hidden here; it is the384e term.

Fixed terminal forwarding allows32:

\[
\text{bounded error fields }5+\text{wrappers }4+
\text{six fixed outer control transitions at3 }18+
\text{result-field forcing }5=32.
\]

Variable recursive unwind is already charged per inspection.

### 8.7 Displayed global fixed sum

\[
\begin{aligned}
F_W^{\rm actual}
&\le277+67+165+655+130+128+164+32\\
&=1618\\
&\le2048.
\end{aligned}
\]

This includes one rejected debit and the complete fixed formula/preflight/reservation path conservatively, even though these paths cannot all occur on one attempt.

---

## 9. Fixed storage and startup reservation

### 9.1 Fixed storage schedule

A conservative fixed transient inventory is:

- entry control/reference registers16;
- constant scanner closures20;
- simultaneously old/current/candidate three-field headers12;
- cumulative/local/next/cap/max registers13;
- shape/rank/cap/product scalar temporaries28;
- formula expression stack: at most31 arithmetic applications ×6 operand/frame/result slots, plus10 bounded local slots =196;
- terminal diagnostic preparation12;
- fixed outer control frames24.

Displayed sum:

\[
16+20+12+13+28+196+12+24=321\le448.
\]

This intentionally sums scanner and formula scratch although their maxima need not overlap.

Potentially surviving fixed slots are bounded by:

- completed-header fact/result references18;
- retained handoff references16;
- charge record5;
- prospective usage record4;
- bounded diagnostic/result slots8.

Thus:

\[
18+16+5+4+8=51\le64.
\]

Body lanes should regard the actual charge/usage objects delivered by section A as already constructed; new report references to them remain body assignments.

### 9.2 Startup-only schedule

The277-work policy/setter/budget/usage sum supports a standalone conservative startup work reservation512. Its fixed objects fit the same448 transient/64 retained envelope.

One compatible **advisory startup tuple** is therefore:

\[
W_0=512,\quad H_0=448,\quad R_0=64,\quad Q_0=512,
\]

\[
L_0=8·512+512=4608.
\]

This is not a requirement to choose these minima. It is an instantiable bound derived from displayed source work/storage, rather than an automatic recertification of2432/256/384.

If selected later, the policy minima, `affineInitialUsage`, runtime-entry checks, meter initialization, diagnostics, and fixtures must change together. This advice grants no authority to perform those changes.

The per-attempt fixed bound2048 is larger than startup’s512 because it also covers formula evaluation, reservation, preflight, and failed-debit handling. Startup and per-attempt F are distinct schedules.

---

## 10. New failure envelope and admission cutoff

For admitted inspection count \(e\), all pre-body failures satisfy:

\[
\boxed{
\begin{aligned}
w_{\rm fail}&\le2048+384e,\\
c_{\rm fail}&\le8(2048+384e),\\
q_{\rm fail}&\le512+36e,\\
r_{\rm fail}&\le64+12e.
\end{aligned}}
\]

Consequently the looser charge-form constructed bound is:

\[
c_{\rm fail}\le L_A(e)=16896+3108e.
\]

The failed candidate debit’s \(Q_A(e+1)\) is an **admission requirement**, not storage actually materialized on that failure.

With starting usage \((C_0,W_0,P_0)\), admission of inspection \(j\ge1\) requires, in order:

\[
\begin{aligned}
C_0+3108j&\le C_{\max},\\
W_0+384j&\le W_{\max},\\
\max(P_0,512+36j)&\le Q_{\max}.
\end{aligned}
\]

All displayed arithmetic is implemented incrementally with saturation atM.

When the starting budget is valid and \(P_0\le Q_{\max}\), the credit-only bound is:

\[
e\le\min\left\{
\left\lfloor\frac{C_{\max}-C_0}{3108}\right\rfloor,
\left\lfloor\frac{W_{\max}-W_0}{384}\right\rfloor,
\left\lfloor\frac{Q_{\max}-512}{36}\right\rfloor
\right\},
\]

with zero admitted nodes if a required baseline inequality fails. Rank/list sentinels can stop earlier.

### Exact failure indexing

| Failure | Value of \(e\) |
|---|---|
| Policy, closed, disabled, counter rejection |0|
| First debit failure |0|
| Rank/dimension error at axis \(k\) |earlier scans + \(k+1\)|
| Element/machine error |earlier scans + rank+1|
| Transform rank mismatch |\(r_B+r_S+r_V+3\)|
| Short list \(t\) |earlier scans + \(t+1\)|
| Long list expected \(t\) |earlier scans + \(t+1\)|
| Partway credit failure |only previously admitted inspections|
| Old payload/full affine reservation failure |complete-header \(e_o\)|

Required diagnostics retain configured cap+1 sentinels. Nil precedence remains old elements → affine elements → machine. The exact machine error remains:

```haskell
Left (TensorShapeError (MachineIndexOverflow (M + 1)))
```

Late zero does not bypass dimension checks or the charged terminating nil.

---

## Diagnosis and drift / contradiction check

1. **Immutable constant-live source does not implement this growing-live schedule.** That is the already-authorized prospective correction, not a new contradiction.
2. The retained experimental profiles establish order/demand distinctions only. They do not supply these resource coefficients.
3. The retained Python oracle’s old-peak/local-Q conflation must be corrected before using it for successive-debit fixtures.
4. A stock witness is not necessarily normalized at entry. S:39 supplies a concrete new-output producer; its output cannot be exempted as arbitrary caller computation.
5. Scanner frames can be bounded conservatively without relying on optimizer tail-call behavior. The24e allowance finishes that part of the schedule rather than assuming constant transient storage.
6. `shapeDimensions` output is a separate post-header producer, not an extra header traversal.
7. No concrete unmapped **in-scope repository traversal** remains in this section. Arbitrary instance execution, physical compiler numeric representation, instrumentation execution, and body/runtime producers are expressly outside this section’s elementary proof—not silently priced as one primitive.

## Recommendation

Use the boxed section-A envelopes as a conservative integration candidate. Audit the assembled replacement formula’s fixed arithmetic, compose prefix survivors with the body’s simultaneous storage, and then derive exact fixtures.

Preserve the distinction between:

- local Q and historical successful peak;
- admitted inspection and rejected candidate debit;
- constructed stock output and existing evidence referents;
- prefix frames that have returned and witness output that survives;
- local failed-attempt cost and successful ledger accounting.

## Risks

- These are logical-machine bounds, not GHC heap, allocation, CPU, or big-number bit-complexity bounds.
- A replacement implementation that introduces another traversal, eagerly evaluates raw elements, executes arbitrary instance work inside the engine theorem, or retains additional closures needs a corresponding schedule change.
- The supplied profile is intentionally loose, especially for already-forced map witnesses and body-time prefix scratch. Tightening it requires source-specific lifetime deductions, not marker counts.
