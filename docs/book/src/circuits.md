# Stochastic circuits and deterministic compilation

A circuit is typed syntax. An interpreter assigns a denotation to each primitive and structural operation.

The categorical meaning of sequence, tensor, copy, and discard is explained in [Category theory behind the interfaces](category-primer.md). The difference between copied and independently repeated randomness is derived in [Categorical probability: the bridge](categorical-probability.md#copying-sharing-and-independence).

## Purity index

Each circuit has one provenance index:

```haskell
'Deterministic
'Stochastic
```

A deterministic primitive must produce a proof-carrying deterministic matrix. A stochastic primitive must produce a normalized stochastic matrix.

This split prevents a dishonest deterministic tag from enabling invalid rewrites.

## Structural operations

The circuit language supports:

- Identity and composition.
- Tensor product.
- Symmetry, associators, and unitors.
- Copy and discard.
- Deterministic weakening to stochastic syntax.
- Exact convex choice.
- Explicit sharing and fanout.

## Shared and independent randomness

Assume `coin` is a circuit from `()` to `Bool`.

```haskell
shared = shareCircuit coin
independent <- fanoutCircuit coin coin
```

`shared` executes the coin once and copies its result:

\\[
P(F,F)=\frac12,
\qquad
P(T,T)=\frac12.
\\]

`independent` executes both branch occurrences:

\\[
P(F,F)=P(F,T)=P(T,F)=P(T,T)=\frac14.
\\]

Use sharing for a common cause or shared latent value. Use fanout for repeated experiments.

## Exact interpretation

An `ExactPrimitiveInterpreter` supplies deterministic and stochastic primitive denotations.

```haskell
matrix <- interpretExactCircuit primitives circuit
result <- runExactCircuit primitives circuit input
```

The structural fold preserves identity, composition, tensor, and the supported coherence maps.

Dense CPU lowering uses this same exact matrix denotation. It does not define a second circuit semantics.

## Bounded cost reports

`interpretCircuitCost` folds all represented syntax with an explicit raw-node limit. A `CircuitCostInterpreter` assigns an owner and `Natural` work charge to each deterministic primitive, stochastic primitive, and quoted table. The framework separately counts structural operations.

```haskell
report <- interpretCircuitCost limits costInterpreter circuit
```

Convex choice charges every represented branch. It does not weight work by the convex coefficient. Sharing and fanout use the same elaborations as every other circuit algebra. Owner totals retain first-occurrence order.

`maximumLiveLayoutCardinality` is the largest represented finite-object cardinality encountered by the elaborated fold. It is not heap liveness, live-frontier width, runtime, or asymptotic complexity. `maximumRepresentedMatrixCells` is a separate static bound. Primitive callbacks own their internal termination and resource use.

## Exact deterministic rewrite certificates

For a deterministic map, copy naturality has the supported equation

\\[
\mathsf{copy}\circ f=(f\otimes f)\circ\mathsf{copy}.
\\]

The public candidate constructors cover left identity, right identity, composition reassociation, and deterministic fanout changed to one execution followed by output copy.

```haskell
candidate = deduplicateDeterministicRewrite deterministicCircuit
checked <- checkDeterministicRewrite rewriteLimits primitives candidate
comparison <- compareCheckedRewriteCosts costLimits costInterpreter checked
```

The checker first performs bounded analysis. It then requires equal represented source and target layouts, exact deterministic matrix denotation under the supplied exact primitive interpreter, and equal row-major matrix layout. The final matrix-layout check is a redundant representation assertion that detects checker drift after the endpoint and denotation checks. Candidate and checked-witness constructors are private.

A checked witness certifies only this bounded exact matrix interpretation. It does not certify a floating, approximate, stateful, or different primitive interpreter. Cost comparison follows semantic checking. A certificate can be cost-neutral or more expensive under a caller's declared charges. A lower or zero declared cost cannot authorize a rewrite. `chargedLeafOccurrenceReduction` includes both primitive nodes and quoted deterministic-table nodes.

There is no stochastic deduplication constructor. A stochastic primitive remains excluded even when its current denotation is Dirac. For a fair coin, one execution followed by copy has diagonal support, while two executions have off-diagonal mass.

The neural softmax/cross-entropy fusion is separate checked-`Double` code. Explicit-Jacobian, coordinate finite-difference, finite-shift, malformed-input, and underflow fixtures do not produce an exact circuit certificate.

## First-order deterministic compiler

The deterministic term language supports identity, composition, products, pairing, projections, and finite quoted tables.

```haskell
notTerm <-
  quotedTableTerm bits bits
    [ (False, True)
    , (True, False)
    ]

circuit <- compileDeterministicTerm notTerm
matrix  <- denoteDeterministicTerm notTerm
```

Quoted tables provide finite extensional data. The compiler does not compare arbitrary Haskell functions.

Use this compiler for finite controller logic, routing, lookup tables, and backend-independent deterministic circuits.

## Further reading

- [Selinger: graphical languages](references.md#selinger-graphical-languages)
- [Fong and Spivak: applied category theory](references.md#fong-and-spivak-applied-category-theory)
- [Fritz: Markov categories](references.md#fritz-markov-categories)
