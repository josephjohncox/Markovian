# References and further reading

This bibliography favors published papers, standard books, and freely available author copies. A citation explains the mathematical source of a design. It does not imply that Markovian implements every result in that source.

## Guided reading routes

Each route starts with background and ends at the source closest to Markovian's design.

- **Category basics:** [Leinster](#leinster-basic-category-theory) or [Riehl](#riehl-category-theory-in-context) → [Mac Lane](#mac-lane-categories-and-monoidal-coherence) → [Selinger](#selinger-graphical-languages)
- **Measure kernels and conditioning:** [Pollard](#pollard-measure-theoretic-probability) → [Kallenberg](#kallenberg-foundations-of-modern-probability) → [Faden](#faden-regular-conditional-probabilities)
- **Categorical probability:** [Giry](#giry-probability-as-a-monad) → [Moggi](#moggi-kleisli-semantics) → [Fritz](#fritz-markov-categories) → [Cho and Jacobs](#cho-and-jacobs-disintegration-and-bayesian-inversion)
- **Matrix and path algebra:** [Mohri](#mohri-semiring-path-algorithms) → [Droste, Kuich, and Vogler](#droste-kuich-and-vogler-weighted-automata) → [Joyal, Street, and Verity](#joyal-street-and-verity-traced-monoidal-categories)
- **Open composition:** [Fong and Spivak](#fong-and-spivak-applied-category-theory) → [Fong](#fong-decorated-cospans) → [Baez and Courser](#baez-and-courser-structured-cospans)
- **Information theory:** [Shannon](#shannon-information-theory) → [Cover and Thomas](#cover-and-thomas-information-theory) → [Baez, Fritz, and Leinster](#baez-fritz-and-leinster-entropy) → [Perrone](#perrone-markov-categories-and-entropy)
- **Categorical learning and reverse traces:** [Fong, Spivak, and Tuyéras](#fong-spivak-and-tuyeras-backprop) → [Cockett and colleagues](#cockett-and-colleagues-reverse-derivatives) → [Griewank and Walther](#griewank-and-walther-evaluating-derivatives) → [Cruttwell and colleagues](#cruttwell-and-colleagues-gradient-learning)
- **Polarity and game semantics:** [Girard](#girard-linear-logic) → [Andreoli](#andreoli-focusing) → [Hyland and Ong](#hyland-and-ong-game-semantics) → [Laurent](#laurent-polarized-games)
- **Push-pull and compositional games:** [Kozen](#kozen-probabilistic-programs) → [Jacobs and Zanasi](#jacobs-and-zanasi-predicate-state-transformers) → [Riley](#riley-categories-of-optics) → [Ghani and colleagues](#ghani-and-colleagues-compositional-games)
- **Inventory control examples:** [Clark and Scarf](#clark-and-scarf-multi-echelon-inventory) → [Doğru](#dogru-multi-retailer-inventory) → [Doğru, van Houtum, and de Kok](#dogru-van-houtum-and-de-kok-fixed-batches)

The foundation chapters connect these sources to the code. Start with [algebra](algebra-primer.md), [category theory](category-primer.md), and [measure theory](measure-theory-primer.md). Then read [categorical probability](categorical-probability.md), [information theory](information-theory.md), [categorical learning](categorical-learning.md), and [polarity, push-pull duality, and games](polarity-and-games.md).

## Category theory and string diagrams

### Leinster: basic category theory

Tom Leinster. *Basic Category Theory*. Cambridge Studies in Advanced Mathematics 143, Cambridge University Press, 2014. [DOI 10.1017/CBO9781107360068](https://doi.org/10.1017/CBO9781107360068). [Free arXiv edition](https://arxiv.org/abs/1612.09375). [Direct PDF](https://arxiv.org/pdf/1612.09375).

Chapter 1 introduces categories, functors, and natural transformations. Chapters 2–5 develop universal properties, representables, Yoneda, limits, and adjunctions. This is the shortest rigorous starting point used by the book's category primer.

### Riehl: category theory in context

Emily Riehl. *Category Theory in Context*. Dover Publications, 2016. [Author-hosted book page](https://math.jhu.edu/~eriehl/context/). [Direct author PDF](https://emilyriehl.github.io/files/context.pdf).

Chapters 1 and 3 cover categories, functors, natural transformations, limits, and colimits. Chapter 5 develops monads and Kleisli categories and explicitly includes probability monads. Use this text when Leinster's compact treatment needs more examples.

### Mac Lane: categories and monoidal coherence

Saunders Mac Lane. *Categories for the Working Mathematician*. Second edition, Graduate Texts in Mathematics 5, Springer, 1998. [DOI 10.1007/978-1-4757-4721-8](https://doi.org/10.1007/978-1-4757-4721-8).

Chapter I §1 states category identities and composition associativity. Chapter VII §§1–2 develops monoidal categories, associators, unitors, and coherence; later parts treat symmetric structure. Use only the assumptions of the cited section for each circuit law.

### Kelly and Laplaza: compact closed coherence

G. M. Kelly and M. L. Laplaza. “Coherence for compact closed categories.” *Journal of Pure and Applied Algebra* 19, 1980, pages 193–213. [DOI 10.1016/0022-4049(80)90101-2](https://doi.org/10.1016/0022-4049%2880%2990101-2).

This paper gives the standard coherence theory for compact closed categories. It is the primary reference for cups, caps, and snake equations.

### Joyal, Street, and Verity: traced monoidal categories

André Joyal, Ross Street, and Dominic Verity. “Traced monoidal categories.” *Mathematical Proceedings of the Cambridge Philosophical Society* 119(3), 1996, pages 447–468. [DOI 10.1017/S0305004100074338](https://doi.org/10.1017/S0305004100074338). [Author-hosted PDF](https://www.sas.rochester.edu/mth/sites/doug-ravenel/otherpapers/jsv.pdf).

This paper states naturality, dinaturality, vanishing, superposing, and yanking for trace. Markovian tests these equations for finite semiring matrices.

### Selinger: graphical languages

Peter Selinger. “A survey of graphical languages for monoidal categories.” In *New Structures for Physics*, Lecture Notes in Physics 813, Springer, 2011, pages 289–355. [DOI 10.1007/978-3-642-12821-9_4](https://doi.org/10.1007/978-3-642-12821-9_4). [Author-hosted PDF](https://mathstat.dal.ca/~selinger/papers/graphical.pdf).

This survey connects equations in monoidal, traced, compact, and dagger categories with sound string-diagram moves. It is the best single visual reference for the circuit and matrix chapters.

### Fong and Spivak: applied category theory

Brendan Fong and David I. Spivak. *An Invitation to Applied Category Theory: Seven Sketches in Compositionality*. Cambridge University Press, 2019. [Free author PDF](https://dspivak.net/7Sketches.pdf). [arXiv:1803.05316](https://arxiv.org/abs/1803.05316).

Chapters 5 and 6 develop signal-flow diagrams, props, cospans, hypergraph categories, and electrical circuits. The book gives a broader compositional context for Markovian circuits and open systems.

## Information theory and categorical learning

### Shannon: information theory

Claude E. Shannon. “A mathematical theory of communication.” *Bell System Technical Journal* 27, 1948, pages 379–423 and 623–656. [DOI 10.1002/j.1538-7305.1948.tb01338.x](https://doi.org/10.1002/j.1538-7305.1948.tb01338.x). [Direct PDF](https://people.math.harvard.edu/~ctm/home/text/others/shannon/entropy/entropy.pdf).

Sections 6–12 introduce entropy, joint and conditional entropy, and channel capacity. Later sections state the noisy-channel results. This is the primary source for the finite logarithmic quantities used in the information-theory chapter.

### Cover and Thomas: information theory

Thomas M. Cover and Joy A. Thomas. *Elements of Information Theory*. Second edition, Wiley, 2006. [DOI 10.1002/047174882X](https://doi.org/10.1002/047174882X).

Chapters 2 and 4 develop entropy, relative entropy, mutual information, chain rules, sufficient statistics, and the data-processing inequality. Chapter 12 connects information theory to statistical inference. Use it for proofs and equality conditions that the book only sketches.

### Baez, Fritz, and Leinster: entropy

John C. Baez, Tobias Fritz, and Tom Leinster. “A characterization of entropy in terms of information loss.” *Entropy* 13(11), 2011, pages 1945–1957. [DOI 10.3390/e13111945](https://doi.org/10.3390/e13111945). [arXiv:1106.1791](https://arxiv.org/abs/1106.1791).

The paper characterizes Shannon information loss for measure-preserving maps between finite probability spaces using functoriality, convex linearity, and continuity. The hypotheses matter: this is not a uniqueness theorem for every possible information functional.

### Perrone: Markov categories and entropy

Paolo Perrone. “Markov categories and entropy.” *IEEE Transactions on Information Theory* 70(3), 2024, pages 1671–1692. [DOI 10.1109/TIT.2023.3328825](https://doi.org/10.1109/TIT.2023.3328825). [arXiv:2212.11719](https://arxiv.org/abs/2212.11719).

Perrone formulates divergences and derived entropy and mutual-information quantities in enriched Markov categories and proves categorical data-processing results. The finite specialization connects these constructions directly to Markovian channels.

### Fong, Spivak, and Tuyeras: backprop

Brendan Fong, David I. Spivak, and Rémy Tuyéras. “Backprop as functor: a compositional perspective on supervised learning.” *34th Annual ACM/IEEE Symposium on Logic in Computer Science*, 2019. [DOI 10.1109/LICS.2019.8785665](https://doi.org/10.1109/LICS.2019.8785665). [arXiv:1711.10455](https://arxiv.org/abs/1711.10455).

The paper organizes learners into a symmetric monoidal category and shows how gradient descent and backpropagation arise compositionally. It motivates separating parameterized implementation, backward request, and parameter update.

### Cockett and colleagues: reverse derivatives

J. Robin B. Cockett, Geoffrey S. H. Cruttwell, Jonathan Gallagher, Jean-Simon Pacaud Lemay, Benjamin MacAdam, Gordon D. Plotkin, and Dorette Pronk. “Reverse derivative categories.” *Computer Science Logic 2020*, LIPIcs 152, article 18, 2020. [DOI 10.4230/LIPIcs.CSL.2020.18](https://doi.org/10.4230/LIPIcs.CSL.2020.18). [Direct PDF](https://drops.dagstuhl.de/storage/00lipics/lipics-vol152-csl2020/LIPIcs.CSL.2020.18/LIPIcs.CSL.2020.18.pdf).

This paper axiomatizes reverse differentiation and derives reverse chain, copy, and additive-accumulation laws. It is the main reference for interpreting VJPs compositionally without identifying reverse differentiation with inversion.

### Griewank and Walther: evaluating derivatives

Andreas Griewank and Andrea Walther. *Evaluating Derivatives: Principles and Techniques of Algorithmic Differentiation*. Second edition, SIAM, 2008. [DOI 10.1137/1.9780898717761](https://doi.org/10.1137/1.9780898717761).

The book develops forward and reverse accumulation over finite computational traces and explains the storage-versus-recomputation trade-off. Markovian uses it to ground explicit reverse tapes only. The implementation accepts owner-supplied VJPs and does not differentiate arbitrary programs.

### Griewank and Walther: Revolve

Andreas Griewank and Andrea Walther. “Algorithm 799: Revolve: An implementation of checkpointing for the reverse or adjoint mode of computational differentiation.” *ACM Transactions on Mathematical Software* 26(1), 2000, pages 19–45. [DOI 10.1145/347837.347846](https://doi.org/10.1145/347837.347846).

Revolve schedules checkpoints for reverse accumulation under storage constraints. It is a non-implemented contrast: Markovian offers only per-primitive stored-pullback and recomputation policies and makes no checkpoint-optimality claim.

### Cruttwell and colleagues: gradient learning

Geoffrey S. H. Cruttwell, Bruno Gavranović, Neil Ghani, Paul Wilson, and Fabio Zanasi. “Categorical foundations of gradient-based learning.” In *Programming Languages and Systems: ESOP 2022*, LNCS 13240, pages 1–28. [DOI 10.1007/978-3-030-99336-8_1](https://doi.org/10.1007/978-3-030-99336-8_1). [arXiv:2103.01931](https://arxiv.org/abs/2103.01931).

The paper combines parameterization, lenses, and reverse-derivative structure to model gradient-based learners and optimizers compositionally. It provides the broader framework for the bounded parametric reverse interpreter and for any later checked-optimizer extension.

## Polarity, transformers, optics, and games

### Girard: linear logic

Jean-Yves Girard. “Linear logic.” *Theoretical Computer Science* 50(1), 1987, pages 1–101. [DOI 10.1016/0304-3975(87)90045-4](https://doi.org/10.1016/0304-3975(87)90045-4).

Girard introduces resource-sensitive connectives, linear negation, and the proof-theoretic structure behind later polarized systems. Read §§1–2 for the motivating logic and §5 for proof nets. The book uses linear duality only as a scoped comparison, not as a structure on normalized stochastic kernels.

### Andreoli: focusing

Jean-Marc Andreoli. “Logic programming with focusing proofs in linear logic.” *Journal of Logic and Computation* 2(3), 1992, pages 297–347. [DOI 10.1093/logcom/2.3.297](https://doi.org/10.1093/logcom/2.3.297).

Andreoli develops focusing for linear logic. The focused normal form separates invertible and non-invertible proof phases and removes irrelevant proof-search permutations. This is the primary source for the chapter's account of logical polarity.

### Levy: call-by-push-value

Paul Blain Levy. *Call-by-Push-Value: A Functional/Imperative Synthesis*. Kluwer Academic Publishers, 2003. [Author's thesis PDF](https://pblevy.github.io/papers/thesisqmwphd.pdf). [Author's CBPV tutorial](https://pblevy.github.io/papers/cbpvefftt.pdf).

Chapters 2–4 separate value types from computation types and embed call-by-value and call-by-name. Later chapters develop categorical and effectful models. Use this source to understand the value-computation distinction without identifying it with proof polarity or Player-Opponent polarity.

### Kozen: probabilistic programs

Dexter Kozen. “Semantics of probabilistic programs.” *Journal of Computer and System Sciences* 22(3), 1981, pages 328–350. [DOI 10.1016/0022-0000(81)90036-2](https://doi.org/10.1016/0022-0000(81)90036-2). [Author PDF](https://www.cs.cornell.edu/~kozen/Papers/ProbSem.pdf).

Kozen presents probabilistic program semantics through measurable functions and linear operators on measures. The operator view supplies historical grounding for forward state transformation and backward expectation calculations.

### Jacobs and Zanasi: predicate-state transformers

Bart Jacobs and Fabio Zanasi. “A predicate/state transformer semantics for Bayesian learning.” *Electronic Notes in Theoretical Computer Science* 325, 2016, pages 185–200. [DOI 10.1016/j.entcs.2016.09.038](https://doi.org/10.1016/j.entcs.2016.09.038). [Author PDF](https://www.cs.ru.nl/B.Jacobs/PAPERS/A_predicate_state_transformer_semantics.pdf).

The paper distinguishes forward state transformation from backward predicate transformation and relates both to conditioning. It is the closest reference for the finite state-payoff distinction in the polarity chapter.

### Blass: games for linear logic

Andreas Blass. “A game semantics for linear logic.” *Annals of Pure and Applied Logic* 56(1–3), 1992, pages 183–220. [DOI 10.1016/0168-0072(92)90073-9](https://doi.org/10.1016/0168-0072(92)90073-9).

Blass gives an early dialogue-game interpretation of linear logic. The paper proves soundness and additive completeness under its stated strategy model, but its multiplicative interpretation has a known completeness boundary.

### Hyland and Ong: game semantics

J. Martin E. Hyland and C.-H. Luke Ong. “On full abstraction for PCF: I, II, and III.” *Information and Computation* 163(2), 2000, pages 285–408. [DOI 10.1006/inco.2000.2917](https://doi.org/10.1006/inco.2000.2917). [Author publication record](https://www.cs.ox.ac.uk/people/luke.ong/personal/publications/).

The paper models PCF with arenas and innocent strategies and proves full-abstraction and universality results. Read §§2–3 for arenas, justified sequences, views, and strategies before reading the main definability arguments.

### Abramsky, Jagadeesan, and Malacaria: game semantics

Samson Abramsky, Radha Jagadeesan, and Pasquale Malacaria. “Full abstraction for PCF.” *Information and Computation* 163(2), 2000, pages 409–470. [DOI 10.1006/inco.2000.2930](https://doi.org/10.1006/inco.2000.2930). [arXiv:1311.6125](https://arxiv.org/abs/1311.6125).

This independently developed game model uses history-free strategies and obtains full abstraction through an extensional quotient. Compare its strategy conditions with Hyland and Ong rather than treating “game semantics” as one unique construction.

### Laurent: polarized games

Olivier Laurent. “Polarized games.” *17th Annual IEEE Symposium on Logic in Computer Science*, 2002, pages 265–274. [DOI 10.1109/LICS.2002.1029835](https://doi.org/10.1109/LICS.2002.1029835). [Author PDF](https://perso.ens-lyon.fr/olivier.laurent/polgames.pdf).

Laurent develops positive and negative games and models intuitionistic and polarized linear logic. It extends the chapter's separate discussions of proof polarity and game semantics. The chapter does not use Laurent's polarized-game construction as an implemented model.

### Riley: categories of optics

Mitchell Riley. “Categories of optics.” 2018. [arXiv:1809.00738](https://arxiv.org/abs/1809.00738).

Riley gives a general optic construction that includes lenses and prisms and develops composition and lawfulness through categorical structure. Use §§2–3 for the construction. The chapter uses optics only as a common interface shape for typed bidirectional flow.

### Ghani and colleagues: compositional games

Neil Ghani, Jules Hedges, Viktor Winschel, and Philipp Zahn. “Compositional game theory.” *33rd Annual ACM/IEEE Symposium on Logic in Computer Science*, 2018, pages 472–481. [DOI 10.1145/3209108.3209165](https://doi.org/10.1145/3209108.3209165). [arXiv:1603.04641](https://arxiv.org/abs/1603.04641).

Definition 3 gives strategy sets, play and coplay maps, and context-indexed best-response relations. Definition 4 gives the decision open game specialized by `exactMaximizingDecision`. Definitions 9, 10, and 12 give sequential composition, strategy-bijection equivalence, and tensor. Theorems 2 and 3 treat simultaneous and sequential pure-strategy games. Section IX leaves mixed strategies and repeated games outside the paper's implemented scope. Markovian specializes these formulas to explicitly bounded finite carriers, preserves decision-site owner support, and enumerates pure contextual equilibria only.

### Nash: non-cooperative games

John F. Nash. “Non-cooperative games.” *Annals of Mathematics* 54(2), 1951, pages 286–295. [DOI 10.2307/1969529](https://doi.org/10.2307/1969529).

Nash proves existence of an equilibrium in mixed strategies for finite games under the paper's assumptions. Markovian cites this result only to explain why matching pennies can lack a pure equilibrium. It implements no mixed strategy or equilibrium-existence theorem.

### Escardo and Oliva: sequential games

Martin Escardó and Paulo Oliva. “Sequential games and optimal strategies.” *Proceedings of the Royal Society A* 467, 2011, pages 1519–1545. [DOI 10.1098/rspa.2010.0471](https://doi.org/10.1098/rspa.2010.0471).

The paper studies optimal strategies for sequential games through selection functions and explicitly distinguishes the stronger sequential objective from ordinary Nash equilibrium behavior. Markovian uses it only to mark the boundary: finite open-game equilibrium enumeration can retain non-credible threats and is not a subgame-perfect or backward-induction solver.

## Probability kernels, Markov categories, and Bayes

### Giry: probability as a monad

Michèle Giry. “A categorical approach to probability theory.” In *Categorical Aspects of Topology and Analysis*, Lecture Notes in Mathematics 915, Springer, 1982, pages 68–85. [DOI 10.1007/BFb0092872](https://doi.org/10.1007/BFb0092872).

Giry gives the foundational probability monad on measurable spaces. Markovian uses the simpler finite distribution monad, where every integral is a finite sum.

### Moggi: Kleisli semantics

Eugenio Moggi. “Notions of computation and monads.” *Information and Computation* 93(1), 1991, pages 55–92. [DOI 10.1016/0890-5401(91)90052-4](https://doi.org/10.1016/0890-5401%2891%2990052-4).

Moggi explains monads and Kleisli categories as semantics for effectful computation. The exact kernel category is the Kleisli category of the exact finite distribution monad.

### Fritz: Markov categories

Tobias Fritz. “A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics.” *Advances in Mathematics* 370, 2020, article 107239. [DOI 10.1016/j.aim.2020.107239](https://doi.org/10.1016/j.aim.2020.107239). [arXiv:1908.07021](https://arxiv.org/abs/1908.07021).

Read §2 for Markov categories, §3 for deterministic morphisms, §11 for conditionals, and §13 for almost-sure equality. These sections explain why discard is natural for every channel but copy is natural only for deterministic channels.

### Cho and Jacobs: disintegration and Bayesian inversion

Kenta Cho and Bart Jacobs. “Disintegration and Bayesian inversion via string diagrams.” *Mathematical Structures in Computer Science* 29(7), 2019, pages 938–971. [DOI 10.1017/S0960129518000488](https://doi.org/10.1017/S0960129518000488). [arXiv:1709.00322](https://arxiv.org/abs/1709.00322).

Read §3 for disintegration, §4 for almost-everywhere equality and uniqueness, and §5 for Bayesian inversion. Markovian specializes these equations to exact finite positive support.

### Pollard: measure-theoretic probability

David Pollard. *A User's Guide to Measure Theoretic Probability*. Cambridge Series in Statistical and Probabilistic Mathematics 8, Cambridge University Press, 2002. [DOI 10.1017/CBO9780511811555](https://doi.org/10.1017/CBO9780511811555). [Publisher page](https://www.cambridge.org/core/books/users-guide-to-measure-theoretic-probability/A257FE6572A9142FE3B811FFF3FD0171). [Front matter and contents](https://assets.cambridge.org/97805210/02899/frontmatter/9780521002899_frontmatter.pdf).

Chapters 2 and 5 introduce measure theory and conditioning with a probability-first motivation. This is the most approachable bridge from elementary probability to the kernel language used in the measure-theory primer.

### Kallenberg: foundations of modern probability

Olav Kallenberg. *Foundations of Modern Probability*. Third edition, Probability Theory and Stochastic Modelling 99, Springer, 2021. [DOI 10.1007/978-3-030-61871-1](https://doi.org/10.1007/978-3-030-61871-1). [Publisher page](https://link.springer.com/book/10.1007/978-3-030-61871-1).

Chapter 1 develops measurable spaces, measures, integration, product measures, and Fubini's theorem. Chapter 4 treats kernels, products, and disintegration. Chapter 8 treats conditioning and conditional distributions. This is the main measure-theoretic reference for the continuous concepts that Markovian specializes to finite sums.

### Faden: regular conditional probabilities

Arnold M. Faden. “The existence of regular conditional probabilities: necessary and sufficient conditions.” *The Annals of Probability* 13(1), 1985, pages 288–298. [DOI 10.1214/aop/1176993081](https://doi.org/10.1214/aop/1176993081). [Bibliographic record](https://ftp.math.utah.edu/pub/tex/bib/idx/annprobab1980/13/1/288_298.html).

Faden characterizes when regular conditional probabilities exist. The paper grounds Markovian's refusal to claim that disintegration is automatic on arbitrary measurable spaces.

## Semirings, matrices, and convexity

### Mohri: semiring path algorithms

Mehryar Mohri. “Semiring frameworks and algorithms for shortest-distance problems.” *Journal of Automata, Languages and Combinatorics* 7(3), 2002, pages 321–350. [Author-hosted PDF](https://cs.nyu.edu/~mohri/pub/hwa.pdf).

Mohri shows how semiring addition aggregates alternative paths and semiring multiplication combines sequential path segments. This gives an operational reading of raw semiring matrix composition.

### Droste, Kuich, and Vogler: weighted automata

Manfred Droste, Werner Kuich, and Heiko Vogler, editors. *Handbook of Weighted Automata*. Springer, 2009. [DOI 10.1007/978-3-642-01492-5](https://doi.org/10.1007/978-3-642-01492-5).

The introductory chapters cover semirings, formal power series, and weighted automata. Use this book for applications of matrix and path algebra beyond probability.

### Fritz: convex spaces

Tobias Fritz. “Convex spaces I: definition and examples.” 2009. [arXiv:0903.5522](https://arxiv.org/abs/0903.5522).

This paper develops abstract convex combinations and their laws. Markovian uses exact finite convex coefficients to mix stochastic channels.

### Jacobs: convexity and effects

Bart Jacobs. “Convexity, duality and effects.” *Theoretical Computer Science* 412(28), 2011, pages 3523–3536. [DOI 10.1016/j.tcs.2011.04.005](https://doi.org/10.1016/j.tcs.2011.04.005).

Jacobs relates distribution monads, convex sets, and probabilistic effects. It gives broader context for the distinction between convex mixing and raw semiring addition.

## Open systems and compositional networks

### Fong: decorated cospans

Brendan Fong. “Decorated cospans.” *Theory and Applications of Categories* 30(33), 2015, pages 1096–1120. [Journal PDF](https://tac.mta.ca/tac/volumes/30/33/30-33.pdf). [arXiv:1502.00872](https://arxiv.org/abs/1502.00872).

Decorated cospans attach system data to an apex and compose systems by pushout. This paper is a predecessor to the structured-cospan approach.

### Baez and Courser: structured cospans

John C. Baez and Kenny Courser. “Structured cospans.” *Theory and Applications of Categories* 35(48), 2020, pages 1771–1822. [Journal PDF](https://tac.mta.ca/tac/volumes/35/48/35-48.pdf). [arXiv:1911.04630](https://arxiv.org/abs/1911.04630).

The paper constructs categories and symmetric monoidal double categories of structured cospans. It is the primary reference for Markovian interfaces, horizontal composition by pushout, vertical maps, and cells.

### Koller and Friedman: sum-product elimination

Daphne Koller and Nir Friedman. *Probabilistic Graphical Models: Principles and Techniques*. MIT Press, 2009. [Publisher page](https://mitpress.mit.edu/9780262013192/probabilistic-graphical-models/).

Chapters 9 and 10 explain exact inference, variable elimination, clique trees, and the sum-product pattern. This is the standard algorithmic context for the live-frontier acyclic interpreter.

## MDPs, POMDPs, inventory control, and learning

### Synthetic bounded serial fixture provenance

The executable fixture in `Markovian.Benchmark.Inventory.Serial.Exact` is repository-authored and synthetic. Its conditioned geometric demand, one-period supplier delay, event order, costs, caps, and parameter values are stated in [Bounded serial-inventory benchmark](inventory-control.md). Its exact results apply only to that conditional bounded model.

The synthetic fixture is not attributed to any publication. Separate implementations cover the finite-lattice Section III specialization, the multi-retailer balance relaxation, and the fixed-batch model below. None of those citations supplies provenance for the synthetic parameter table.

### Clark and Scarf: multi-echelon inventory

Andrew J. Clark and Herbert Scarf. “Optimal policies for a multi-echelon inventory problem.” *Management Science* 6(4), 1960, pages 475–490. [DOI 10.1287/mnsc.6.4.475](https://doi.org/10.1287/mnsc.6.4.475).

Clark and Scarf derive the echelon decomposition for a serial periodic-review inventory model under their stated assumptions. Pages 476--479 state beginning-of-period purchasing, independent demands, complete backlog, linear internal transport, and echelon-stock natural costs. Pages 481--482 define the Section III two-echelon state `(x1,w1,x2)`, two-period downstream and one-period upstream natural lead times, and equations (11)--(15). Pages 483--484 give equations (20), (21), and (26) and Theorems 1--2. Markovian implements only an exact finite-lattice, finite-demand-sum specialization of those equations. The inspected scan at <http://dido.econ.yale.edu/~hes/pub/echelon1.pdf> has SHA-256 `b64d82098b47dffa7cc4b87a4bbc6c833bb90295ccbede0a1897c8af44956239`. The paper supplies no numerical oracle.

### Arrow, Karlin, and Scarf: inventory background

Kenneth J. Arrow, Samuel Karlin, and Herbert Scarf. *Studies in the Mathematical Theory of Inventory and Production*. Stanford University Press, 1958, Chapter 10. [Stanford University Press record](https://www.sup.org/books/economics-and-finance/studies-mathematical-theory-inventory-and-production).

Clark and Scarf cite this chapter for the single-installation lead-time recursion summarized before their multi-echelon construction. Markovian uses the 1960 paper, not this chapter, as the direct source for the two-echelon state and decomposition equations.

### Dogru: multi-retailer inventory

Mustafa Kemal Doğru. *Optimal Control of One-Warehouse Multi-Retailer Systems: An Assessment of the Balance Assumption*. PhD thesis, Eindhoven University of Technology, 2006. [DOI 10.6100/IR601558](https://doi.org/10.6100/IR601558). [University record](https://research.tue.nl/en/publications/optimal-control-of-one-warehouse-multi-retailer-systems-an-assess/).

The thesis studies centralized periodic-review control of one warehouse and multiple retailers, including the balance assumption, base-stock policies, newsvendor inequalities, and numerical comparisons with dynamic-programming solutions. Equations (2.3)--(2.5) are on printed page 24, and equations (2.6)--(2.7) are on printed page 26; together they define the balance relaxation. Theorem 2.9 on page 30 states the balanced policy class. Section 3.3.4 on page 53 defines the physicalized LB heuristic. Pages 82--87 and equations (4.5)--(4.7) specify the physical timing and allocation constraints. Equation (4.11) on page 92 is the published average-cost objective. Table 4.1 is on page 95, Table 4.3 is on page 98, and the scenario-2 discussion and base stocks are on pages 112--114. These locations supply the implemented scenario row. The balance assumption can permit allocations unavailable in the physical system, so Markovian represents it as an explicit approximation rather than hiding it in a transition kernel.

M. K. Doğru, A. G. de Kok, and G. J. van Houtum. “A numerical study on the effect of the balance assumption in one-warehouse multi-retailer inventory systems.” *Flexible Services and Manufacturing Journal* 21(3--4), 2010, pages 114--147. [DOI 10.1007/s10696-010-9064-1](https://doi.org/10.1007/s10696-010-9064-1).

The journal article supports the later lower-bound and upper-bound numerical study. It does not replace the thesis's Chapter 4 comparison with the physical dynamic-programming optimum.

### Dogru, van Houtum, and de Kok: fixed batches

M. K. Doğru, G. J. van Houtum, and A. G. de Kok. *Newsboy Characterizations for the Optimal Reorder Levels of Multi-Echelon Inventory Systems with Fixed Batch Sizes*. BETA Working Paper 134, Eindhoven University of Technology, 22 February 2005. [University PDF](https://pure.tue.nl/ws/files/2050768/589423.pdf).

Section 2, pages 4--5 states the serial model, integer-ratio batches, initial congruence, iid nonnegative discrete demand, complete backlog, deterministic lead times, positive echelon holding and penalty costs, centralized control, infinite-horizon average-cost objective, and period event order. Equations (3) and (9), pages 7 and 10, define the stationary and subsystem costs. Equations (10)--(14), pages 10--12, define subsystem shortfalls and forward differences. Theorem 1, equations (17)--(21), pages 13--14, gives weak and strict inequalities for discrete demand. Page 15 gives `S=R+1` when all batches equal one. Theorem 2, equation (22), page 16 requires continuous demand and is not implemented.

M. K. Doğru, G. J. van Houtum, and A. G. de Kok. “Newsvendor equations for optimal reorder levels of serial inventory systems with fixed batch sizes.” *Operations Research Letters* 36(5), 2008, pages 551--556. [DOI 10.1016/j.orl.2008.06.003](https://doi.org/10.1016/j.orl.2008.06.003).

Markovian implements a two-stage bounded finite-horizon execution and separate exact finite sums for the stationary subsystem equations. It does not transfer the source's infinite-horizon result to the finite-horizon oracle and does not implement the continuous equality.

### Chen: echelon reorder points

Fangruo Chen. “Echelon reorder points, installation reorder points, and the value of centralized demand information.” *Management Science* 44(12), 1998, pages S221--S234. [DOI 10.1287/mnsc.44.12.S221](https://doi.org/10.1287/mnsc.44.12.S221).

Doğru, van Houtum, and de Kok use Chen's sequential one-dimensional stationary construction and effective reorder points. Markovian uses that result only in the separate stationary evidence module.

Fangruo Chen. “Optimal policies for multi-echelon inventory problems with batch ordering.” *Operations Research* 48(3), 2000, pages 376--389. [DOI 10.1287/opre.48.3.376.12427](https://doi.org/10.1287/opre.48.3.376.12427).

Chen proves an echelon-stock `(R,nQ)` policy result under the paper's infinite-horizon assumptions. That theorem does not establish optimality of Markovian's bounded finite-horizon constant policy.

### Puterman: Markov decision processes

Martin L. Puterman. *Markov Decision Processes: Discrete Stochastic Dynamic Programming*. Wiley, 1994. [DOI 10.1002/9780470316887](https://doi.org/10.1002/9780470316887).

Chapter 4 covers finite-horizon models and backward induction. Chapter 6 covers discounted models, contraction arguments, value iteration, and policy iteration.

### Bellman: dynamic programming

Richard Bellman. *Dynamic Programming*. Princeton University Press, 1957. [Publisher page](https://press.princeton.edu/books/paperback/9780691146683/dynamic-programming).

Bellman gives the principle of optimality and the recursive value equations that underlie finite-horizon and fixed-point evaluation.

### Sutton: temporal-difference learning

Richard S. Sutton. “Learning to predict by the methods of temporal differences.” *Machine Learning* 3, 1988, pages 9–44. [DOI 10.1007/BF00115009](https://doi.org/10.1007/BF00115009).

This paper develops temporal-difference prediction and the TD(0) update. Markovian implements the one-step tabular target for a supplied policy.

### Kaelbling, Littman, and Cassandra: POMDPs

Leslie Pack Kaelbling, Michael L. Littman, and Anthony R. Cassandra. “Planning and acting in partially observable stochastic domains.” *Artificial Intelligence* 101(1–2), 1998, pages 99–134. [DOI 10.1016/S0004-3702(98)00023-X](https://doi.org/10.1016/S0004-3702%2898%2900023-X). [Author-hosted PDF](https://cs.brown.edu/courses/csci2951-k/papers/kaelbling98.pdf).

The paper explains belief states, post-action observation updates, finite-horizon value functions, and the computational difficulty of exact POMDP planning.

### Sutton and Barto: reinforcement learning

Richard S. Sutton and Andrew G. Barto. *Reinforcement Learning: An Introduction*. Second edition, MIT Press, 2018. [Official book page and PDF](http://incompleteideas.net/book/the-book-2nd.html).

Section 2.8 gives gradient-bandit softmax action probabilities. Chapter 3 defines finite MDPs. Chapter 4 covers dynamic programming. Chapter 6 covers temporal-difference learning, Sarsa, and Q-learning. The softmax formula grounds the neural policy consumer; Markovian's exact-support compiler is a repository adapter contract.

### Watkins and Dayan: Q-learning

Christopher J. C. H. Watkins and Peter Dayan. “Q-learning.” *Machine Learning* 8, 1992, pages 279–292. [DOI 10.1007/BF00992698](https://doi.org/10.1007/BF00992698). [Author-hosted PDF](https://www.gatsby.ucl.ac.uk/~dayan/papers/cjch.pdf).

This is the classical convergence paper for tabular Q-learning. Its convergence theorem has visitation and learning-rate assumptions that Markovian does not claim from a bounded example run.

### Rummery and Niranjan: SARSA

Gavin A. Rummery and Mahesan Niranjan. *On-line Q-learning using connectionist systems*. Cambridge University Engineering Department Technical Report CUED/F-INFENG/TR 166, 1994. [University repository record and PDF](https://www.repository.cam.ac.uk/handle/1810/244990).

The report describes the on-policy action-value update later called SARSA. Markovian carries the sampled next behavior action explicitly.

### van Seijen and colleagues: Expected SARSA

Harm van Seijen, Hado van Hasselt, Shimon Whiteson, and Marco Wiering. “A theoretical and empirical analysis of Expected Sarsa.” 2009 IEEE Symposium on Adaptive Dynamic Programming and Reinforcement Learning, pages 177–184. [DOI 10.1109/ADPRL.2009.4927542](https://doi.org/10.1109/ADPRL.2009.4927542).

The paper analyzes the expected action-value target. Markovian uses the expectation under its canonical epsilon-greedy distribution.

### Williams: REINFORCE

Ronald J. Williams. “Simple statistical gradient-following algorithms for connectionist reinforcement learning.” *Machine Learning* 8, 1992, pages 229–256. [DOI 10.1007/BF00992696](https://doi.org/10.1007/BF00992696).

Williams derives score-function reinforcement algorithms. Markovian implements a bounded finite-episode update with an optional detached scalar baseline.

### Sutton and colleagues: policy gradients

Richard S. Sutton, David McAllester, Satinder Singh, and Yishay Mansour. “Policy gradient methods for reinforcement learning with function approximation.” *Advances in Neural Information Processing Systems 12*, 2000, pages 1057–1063. [Publisher paper](https://proceedings.neurips.cc/paper/1999/hash/464d828b85b0bed98e80ade0a5c43b0f-Abstract.html).

This paper states policy-gradient results with function approximation and discusses actor-critic methods. Markovian implements finite linear reference updates, not the paper's convergence claims.

### Konda and Tsitsiklis: actor-critic

Vijay R. Konda and John N. Tsitsiklis. “Actor-critic algorithms.” *Advances in Neural Information Processing Systems 12*, 2000, pages 1008–1014. [Publisher paper](https://proceedings.neurips.cc/paper/1999/hash/6449f44a102fde848669bdd9eb6b76fa-Abstract.html).

This paper develops two-time-scale actor-critic algorithms. Markovian provides one checked simultaneous one-step reference update.

### Lin: experience replay

Long-Ji Lin. “Self-improving reactive agents based on reinforcement learning, planning and teaching.” *Machine Learning* 8, 1992, pages 293–321. [DOI 10.1007/BF00992699](https://doi.org/10.1007/BF00992699).

Lin introduces experience replay for reinforcement learning agents. Markovian implements deterministic bounded FIFO storage and explicit ID selection.

### Mnih and colleagues: DQN

Volodymyr Mnih and colleagues. “Human-level control through deep reinforcement learning.” *Nature* 518, 2015, pages 529–533. [DOI 10.1038/nature14236](https://doi.org/10.1038/nature14236). [Author manuscript](https://storage.googleapis.com/deepmind-media/dqn/DQNNaturePaper.pdf).

This paper combines deep Q-learning, replay, and a target network. Markovian implements one small dense-network batch update, not the Atari training system.

### van Hasselt and colleagues: Double DQN

Hado van Hasselt, Arthur Guez, and David Silver. “Deep reinforcement learning with Double Q-learning.” *Proceedings of the AAAI Conference on Artificial Intelligence* 30(1), 2016. [arXiv:1509.06461](https://arxiv.org/abs/1509.06461). [AAAI paper](https://ojs.aaai.org/index.php/AAAI/article/view/10295).

Section 4 defines Double DQN selection with the online network and evaluation with the target network. Markovian implements this target rule over an explicit action mask. The paper does not define Markovian's exact-layout bridge.

### Lillicrap and colleagues: soft target synchronization

Timothy P. Lillicrap and colleagues. “Continuous control with deep reinforcement learning.” *ICLR*, 2016. [arXiv:1509.02971](https://arxiv.org/abs/1509.02971).

This paper uses the soft target update `target <- tau * online + (1 - tau) * target` implemented by Markovian. The operation is deterministic exponential interpolation and is often called a Polyak update in deep-learning code.

### Polyak and Juditsky: averaged stochastic approximation

Boris T. Polyak and Anatoli B. Juditsky. “Acceleration of stochastic approximation by averaging.” *SIAM Journal on Control and Optimization* 30(4), 1992, pages 838–855. [DOI 10.1137/0330046](https://doi.org/10.1137/0330046).

This paper is the primary source for Polyak–Ruppert iterate averaging. That averaging scheme is historically related terminology, but it is not the fixed-coefficient soft target update implemented here.

## Numerical computation

### Goodfellow, Bengio, and Courville: deep learning

Ian Goodfellow, Yoshua Bengio, and Aaron Courville. *Deep Learning*. MIT Press, 2016. [Publisher book](https://www.deeplearningbook.org/).

Section 6.2.2.3 gives the finite-logit softmax and log-likelihood formulas used to derive the categorical cross-entropy gradient. Markovian uses this only for the formula; checked `Double` execution, underflow behavior, and rejection policy are repository-specific numerical contracts.

### Higham: floating-point stability

Nicholas J. Higham. *Accuracy and Stability of Numerical Algorithms*. Second edition, SIAM, 2002. [DOI 10.1137/1.9780898718027](https://doi.org/10.1137/1.9780898718027).

Chapter 2 covers floating-point arithmetic and rounding error. Use it when deciding which exact algebraic laws can survive a floating interpreter.

### Goldberg: floating-point arithmetic

David Goldberg. “What every computer scientist should know about floating-point arithmetic.” *ACM Computing Surveys* 23(1), 1991, pages 5–48. [DOI 10.1145/103162.103163](https://doi.org/10.1145/103162.103163).

Goldberg gives a practical account of representation, rounding, guard digits, and IEEE arithmetic. It explains why reassociation is not a semantics-preserving floating optimization.

## How the references map to Markovian

| Markovian topic | Primary starting points |
| --- | --- |
| Category basics and universal properties | Leinster, Riehl |
| Measure spaces, kernels, and conditioning | Pollard, Kallenberg, Faden |
| Kernel composition and exact monad laws | Giry, Moggi, Fritz |
| Copy, discard, and deterministic morphisms | Fritz §§2–3 |
| Bayesian inversion and almost-sure equality | Cho and Jacobs §§3–5, Fritz §§11 and 13 |
| Tensor, coherence, dagger, compact structure | Mac Lane Ch. VII, Kelly–Laplaza, Selinger |
| Trace laws | Joyal–Street–Verity, Selinger |
| Semiring matrices | Mohri, *Handbook of Weighted Automata* |
| Convex mixtures | Fritz on convex spaces, Jacobs |
| Structured cospans and double cells | Baez–Courser, Fong |
| Acyclic sum-product interpretation | Koller–Friedman Chs. 9–10 |
| Entropy, KL, mutual information, and data processing | Shannon, Cover–Thomas, Baez–Fritz–Leinster, Perrone |
| Reverse derivatives and compositional learning | Fong–Spivak–Tuyéras, Cockett and colleagues, Cruttwell and colleagues |
| Serial and multi-retailer inventory examples | Clark–Scarf, Doğru, Doğru–van Houtum–de Kok |
| Finite and discounted MDP evaluation and control | Puterman Chs. 4 and 6, Bellman |
| POMDP belief filtering | Kaelbling–Littman–Cassandra |
| TD(0), SARSA, Expected SARSA, and Q-learning | Sutton, Rummery–Niranjan, van Seijen and colleagues, Watkins–Dayan |
| REINFORCE and actor-critic | Williams, Sutton and colleagues, Konda–Tsitsiklis |
| Replay, DQN, Double DQN, and target updates | Lin, Mnih and colleagues, van Hasselt and colleagues, Lillicrap and colleagues |
| Finite-logit softmax and cross entropy | Goodfellow–Bengio–Courville §6.2.2.3 |
| Floating approximation boundaries | Higham Ch. 2, Goldberg |
