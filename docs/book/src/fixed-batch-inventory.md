# Fixed-batch execution and newsvendor evidence

This benchmark has two separate calculations:

- `Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact` solves a bounded two-stage finite-horizon control model.
- `Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact` evaluates stationary subsystem equations on an explicit finite Cartesian `R1`/`R2` domain.

Do not use the stationary result as a proof about the finite-horizon oracle. Do not use either result as an unbounded-demand or average-reward result.

## Source and implemented scope

The primary source is M. K. Doğru, G. J. van Houtum, and A. G. de Kok, *Newsboy Characterizations for the Optimal Reorder Levels of Multi-Echelon Inventory Systems with Fixed Batch Sizes*, BETA Working Paper 134, 22 February 2005. Section 2, pages 4--5 states the model and event order. Equations (3) and (9), pages 7 and 10, give stationary costs. Equations (10)--(14), pages 10--12, give subsystem shortfalls and forward differences. Theorem 1, equations (17)--(21), pages 13--14, gives discrete weak and strict inequalities. Page 15 states the unit-batch relation. Theorem 2, equation (22), page 16 is for continuous demand and is not implemented.

The journal version is Doğru, van Houtum, and de Kok, “Newsvendor equations for optimal reorder levels of serial inventory systems with fixed batch sizes,” *Operations Research Letters* 36(5), 2008, pages 551--556, DOI `10.1016/j.orl.2008.06.003`.

The source assumes a serial `N`-stage periodic-review system, positive echelon holding and backlog costs, deterministic lead times, integer-ratio batches, centralized control, iid nonnegative discrete demand with positive mean and `Pr(D=1)>0`, complete backlog, and an infinite-horizon expected average-cost objective. Markovian implements only two stages, `L1=0`, a positive finite `L2`, exact finite-horizon execution, and conditioned finite demand.

## State, batches, and event order

A validated state is `(t,x1,x2,a0,...,a[L2-1])`:

- `t` is periods remaining.
- `x1` is stage-1 net inventory. A negative value is backlog.
- `x2` is stage-2 on-hand inventory.
- `a0` is the known external batch due after the current ordering decision.
- Every pipeline quantity is a multiple of `Q2`.

The implementation requires

\\[
Q_2=nQ_1
\\]

for a positive integer `n`. A physical action `(q1,q2)` satisfies

\\[
q_1=k_1Q_1,\qquad q_2=k_2Q_2,
\\]

\\[
0\leq q_1\leq x_2+a_0,\qquad 0\leq k_2\leq\text{externalBatchCap}.
\\]

One period chooses the action, receives the known due quantity, releases `q1`, observes one demand, and charges cost. The successor is

\\[
x'_2=x_2+a_0-q_1,
\\]

\\[
x'_1=x_1+q_1-D,
\\]

\\[
\text{pipeline}'=\operatorname{tail}(\text{pipeline})\mathbin{++}[q_2].
\\]

The outcome stores this successor with the cost from the same demand draw:

\\[
c=h_2x'_2+(h_1+h_2)\max(x'_1,0)+p\max(-x'_1,0).
\\]

The transition reward is `-c`. There is no state clamping or successor redirection. A state with zero periods remaining has zero payoff and no action. Pipeline after the terminal epoch has no salvage value.

## Conditioned demand and finite layouts

The repository-authored fixture starts from

\\[
\Pr(D=d)=2^{-(d+1)},\qquad d\geq0,
\\]

and conditions on `0<=d<=demandCap`. The report gives

\\[
\text{retainedMass}=1-2^{-(\text{demandCap}+1)},
\\]

\\[
\text{omittedMass}=2^{-(\text{demandCap}+1)},
\\]

and

\\[
1-\text{retainedMass}^{\text{horizon}}.
\\]

These probabilities are not value-error bounds. Demand-cap widening reports exact differences between two conditioned finite models only.

Fixture construction performs complete breadth-first closure. It exposes duplicate-free state and global action layouts. `sameFiniteSet` compares labelled support. `sameFiniteLayout` also compares represented order. Lead time, demand outcomes, states, state-actions, solver work, reorder-grid points, stationary-domain cardinality, and convolution terms all have explicit limits. Budget exhaustion returns no partial result.

## Constant `(R,nQ)` policy and finite oracle

For integer position `y`, reorder level `R`, and positive batch `Q`, the policy request is

\\[
q(y,R,Q)=
\begin{cases}
0,&y>R,\\
Q\left(1+\left\lfloor\dfrac{R-y}{Q}\right\rfloor\right),&y\leq R.
\end{cases}
\\]

The positions are

\\[
IP_1=x_1,
\\]

\\[
IP_2=x_1+x_2+\sum_j a_j.
\\]

The stage-1 request is clipped to physically available material. The external request is clipped to the explicit external batch cap. Both clipping events are reported. Candidate `(R1,R2)` pairs form a duplicate-free canonical finite grid.

The oracle uses exact backward induction with discount one:

\\[
V(s)=\max_a\sum_o p(o)\left(r(o)+V(s'(o))\right).
\\]

Each constant policy is evaluated separately. Exact ties use canonical grid order. The solution records exact oracle return, selected-policy return, regret, represented layouts, checked work, cap witnesses, and grid witnesses.

## Separate stationary subsystem equations

The stationary module uses the conditioned demand law as a finite rational distribution. It introduces independent

\\[
U_1\sim\operatorname{Uniform}[1,\\ldots,Q_1]
\\]

and

\\[
Z_1\sim\operatorname{Uniform}[0,\\ldots,Q_2/Q_1-1].
\\]

For subsystem one,

\\[
B^{(1)}_1=0,
\\]

\\[
B^{(1)}_0=\max(0,D_1-R_1-U_1).
\\]

For subsystem two,

\\[
B^{(2)}_2=0,
\\]

\\[
B^{(2)}_1=\max(0,D^-_2-(R_2-R_1)-Z_1Q_1),
\\]

\\[
B^{(2)}_0=\max(0,B^{(2)}_1+D_1-R_1-U_1).
\\]

Here, `D1` covers `L1+1=1` period and `D2^-` covers `L2` periods. The implementation calculates equation-(9) subsystem costs `C1` and `C2` and exact forward differences

\\[
c_i(\ldots,R_i)=C_i(\ldots,R_i+1)-C_i(\ldots,R_i).
\\]

It checks both Theorem 1 conditions:

- the weak condition is equivalent to `ci>=0`;
- the strict condition is equivalent to `ci>0`.

For discrete demand, these conditions can differ on a plateau. Exact tests include such a plateau. When `Q=1`, page 15 identifies the corresponding base-stock level as

\\[
S=R+1.
\\]

The stationary search domain stores separate finite `R1` and `R2` layouts and declares their Cartesian-product semantics. Sparse or diagonal execution-policy grids are not silently treated as the stationary pair domain. Each solution retains the exact parameters and domain used to produce it. The solver builds each required shortfall law once, shares one lead-demand convolution, and reports the exact number of generated finite-law terms. It rejects an insufficient complete budget before constructing any law. The report checks parameter and domain provenance against the associated execution solution and renders the tested finite/stationary counterexample `(0,-4)` versus `(0,1)` with its parameters and search domain. This is still a finite boundary witness, not an unbounded minimizer. The module documents, but does not implement, Theorem 2's continuous-demand equality.

## Diagnostics and benchmark

The action/grid comparison requires the same non-bound model, a strict external-cap increase, and a strict reorder-grid superset. Stability requires unchanged exact oracle return, selected-policy return, and selected levels, with no widened external-cap or grid witness.

The demand comparison requires a strict demand-cap increase with the same other model and grid. It reports exact return, regret, selected-level, and omitted-mass differences. It does not call these differences error bounds.

Run:

```sh
cabal bench fixed-batch-rnq-bench --project-file=cabal.project.ci
```

The executable excludes one warm-up and measures twenty complete runs. Every measured run must produce the same exact semantic report. Elapsed nanoseconds are local reproducibility data. The benchmark makes no speedup or convergence claim.
