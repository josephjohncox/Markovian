# Finite alternating interaction protocols

Markovian implements one conservative protocol fragment in `Markovian.Game.Arena` and `Markovian.Game.Strategy`. It is finite, exact, acyclic, and deterministic on Player turns. This chapter states what the constructors establish and what they do not establish.

## Checked arenas

An arena has explicit finite layouts for positions and move identities. Every move has one edge with an `Opponent` or `Player` owner. The opaque arena constructor checks:

1. the initial position and every edge endpoint are represented;
2. every represented move has exactly one edge;
3. every represented position is reachable from the initial position;
4. all outgoing edges at one position have one owner;
5. a nonterminal initial position is owned by Opponent;
6. ownership alternates across each edge with a nonterminal target; and
7. the transition graph is acyclic.

Terminal positions have no outgoing moves. Parallel edges are valid when their move identities differ. Unreachable decorative positions are not valid. Construction takes an explicit `ArenaBudget` and uses color-based DFS; it rejects before validation work exceeds the bound.

`LegalHistory` is opaque. `legalHistory` takes a `ReplayBudget`, replays a move list from the initial position, and stores the reached position. A move with the wrong source and a move after a terminal position are errors. History observers expose the represented moves, current position, next owner, legal extensions, and terminal status.

`arenaEquivalent` and `sameArenaLayout` take explicit comparison budgets and return checked results. Exhaustion returns no partial Boolean. The former ignores layout order; the latter also checks represented position and move layouts.

## Plays and ownership reversal

A play from arena `A` to arena `B` uses tagged boundary moves. Domain ownership is reversed and codomain ownership is retained:

\\[
\operatorname{owner}_{A\Rightarrow B}(m)=
\begin{cases}
\operatorname{swap}(\operatorname{owner}_A(m)) & m\text{ is a domain move},\\
\operatorname{owner}_B(m) & m\text{ is a codomain move}.
\end{cases}
\\]

A legal play starts with Opponent and alternates roles. Both endpoint projections must replay as legal arena histories. This is only polarity reversal. It does not add question/answer labels, pointers, or views.

## Checked strategies

A strategy is an explicit finite set of legal plays checked under work and result limits. Its constructor requires:

- the empty play;
- no duplicate play;
- prefix closure;
- every legal Opponent extension at every represented Opponent prefix; and
- exactly one legal Player extension when a Player extension exists.

Thus Opponent branching is exhaustive. Player behavior is total and deterministic. A terminal prefix has no extension. The type has no chance move, probability, mixed strategy, or Player nondeterminism.

`copycatStrategy` mirrors a received move with the same move identity on the other occurrence of one arena. It is a bounded checked construction. The API does not expose an unrestricted `Category` instance.

## Bounded composition

For strategies from `A` to `B` and from `B` to `C`, composition first requires labelled equivalence of the two middle arenas. It then:

1. traverses finite trace pairs in represented order;
2. advances a middle move only when both traces have the same `B` move identity;
3. hides synchronized middle moves;
4. removes duplicate visible `A`/`C` projections; and
5. validates the visible prefix set as a strategy again.

One operation-wide work account charges middle comparison, synchronization, visible deduplication, move replay, duplicate and prefix comparisons, extension generation, membership, and final revalidation. Exhaustion returns an error and no partial strategy. Hiding is not closed for all represented strategies: composition is partial and reports `HiddenInternalDeadlock` when the visible result has no required Player response. A `CompositionReport` records conservative charged work, limits, hidden steps, duplicates, and maximum interaction depth. It has no timing field and is not part of strategy equality.

## Equality and finite law evidence

`observationallyEqual` means bounded checked equality of finite external prefix-closed play sets under labelled-equivalent endpoints. It ignores middle moves and represented play order. `sameStrategyLayout` separately checks endpoint layouts and play order. Both return atomic exhaustion rather than an unchecked Boolean.

The finite fixtures check:

- copycat as left and right identity on a representative strategy;
- representative associativity;
- hidden-middle composition;
- reflexivity, symmetry, transitivity, and representative congruence;
- reordered layouts; and
- deterministic reports and atomic budget failure.

These fixtures are executable finite evidence inside the documented partial-composition domain. They are not quantified proofs for every Haskell callback or a categorical coherence theorem. The API claims neither closure under hiding nor general category laws or unrestricted associativity.

## Why this is not Hyland--Ong or AJM semantics

Hyland and Ong define justified sequences and views in their arena model; see sections 2--3 of their 2000 paper. The AJM model uses a distinct history-free strategy presentation and an extensional quotient. Markovian implements neither set of assumptions.

A concrete counterexample is enough to show the boundary. Suppose the visible label `answer` occurs twice after two occurrences of `question`. Two justified sequences can have the same label list but point the second `answer` to different enabling occurrences. Their views can therefore differ. A `LegalHistory` stores only move identities in a finite acyclic edge graph. It cannot represent this pointer distinction. Equal Markovian move lists cannot establish equality of justified sequences.

The protocol also has no innocence, well-bracketing, exponentials, PCF interpretation, definability, full abstraction, or AJM quotient. Ghani and colleagues' open games add play, coplay, and context-indexed best response. Those data are absent here, so no payoff or equilibrium result follows.

See [Polarity, push-pull duality, and games](polarity-and-games.md) for the broader comparison and [References](references.md#hyland-and-ong-game-semantics) for the primary sources.
