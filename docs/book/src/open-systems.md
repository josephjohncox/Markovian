# Open systems and acyclic networks

An open system separates a subsystem's boundary from its internal topology.

## Typed interfaces

An interface contains named ports and runtime sorts. Interface maps preserve these sorts.

A directed hypergraph supplies the apex of an open system. Each edge has an ordered input list, ordered output list, and label.

The constructor validates vertices, incidence, labels, and interface maps before composition.

## Structured cospans

An open system has this shape:

\\[
X \longrightarrow G \longleftarrow Y.
\\]

`X` is the input boundary. `Y` is the output boundary. `G` is the internal directed hypergraph.

Sequential composition computes a finite pushout over the shared boundary. Tensor uses disjoint union.

Use these operations to connect components without exposing their internal vertex names.

## Boundary reversal

`reverseOpenBoundary` swaps the two cospan legs. It does not reverse edge direction or circuit state orientation.

The reversed value has no automatic stochastic denotation. A new forward interpretation requires new validation.

Boundary reversal is not matrix dagger and is not Bayesian inversion.

## Open-system cells

A cell contains vertical input and output maps plus an apex hypergraph map. The map preserves types, labels, order, and incidence.

Cells describe structure-preserving component maps. Horizontal composition uses the induced pushout map. Vertical composition uses ordinary map composition.

Use cells to state that a component replacement preserves an external interface.

## Acyclic boundary-functional refinement

`AcyclicOpenSystem` validates a supported executable fragment. Every apex vertex must have exactly one producer:

- one input-boundary occurrence, or
- one edge-output occurrence.

The validator rejects missing producers, multiple producers, repeated edge outputs, self-loops, and directed cycles.

## Local circuit semantics

`FiniteValueDomains` assigns a finite value carrier to each runtime sort. A label table assigns one local circuit to each label and ordered endpoint signature.

```haskell
topology <- acyclicOpenSystem rawSystem
domains  <- finiteValueDomains domainEntries
labels   <- labelCircuitTable localCircuits
network  <- acyclicOpenCircuit topology domains labels
```

The exact interpreter executes edges in a stable topological order. Each edge occurrence executes once.

Multiple consumers read one stored output value. Distinct edge occurrences execute independently.

## Live-frontier evaluation

After each edge, the interpreter drops values that no later edge or output can read. It sums over discarded stochastic outputs.

The method avoids construction of the complete apex assignment object. Runtime can still grow exponentially with boundary size and live-frontier width.

Use this fragment for feed-forward sensor networks, fault trees, finite workflows, and acyclic probabilistic circuits.

Do not use it for feedback, recursion, cyclic graphs, or arbitrary hypergraph black-boxing.

## Further reading

- [Baez and Courser: structured cospans](references.md#baez-and-courser-structured-cospans)
- [Fong: decorated cospans](references.md#fong-decorated-cospans)
- [Koller and Friedman: sum-product elimination](references.md#koller-and-friedman-sum-product-elimination)
