# Stochastic circuits and deterministic compilation

A circuit is typed syntax. An interpreter assigns a denotation to each primitive and structural operation.

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

## Deterministic copy rewrite

The circuit optimizer can move a deterministic function through copy:

\\[
\mathsf{copy}\circ f=(f\otimes f)\circ\mathsf{copy}.
\\]

The API accepts only deterministic syntax for this rewrite. It rejects a stochastic circuit at the type level.

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
