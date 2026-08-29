# References and further reading

This bibliography favors published papers, standard books, and freely available author copies. A citation explains the mathematical source of a design. It does not imply that Markovian implements every result in that source.

## Guided reading routes

Each route starts with background and ends at the source closest to Markovian's design.

- **Category basics:** [Leinster](#leinster-basic-category-theory) → [Riehl](#riehl-category-theory-in-context) → [Mac Lane](#mac-lane-categories-and-monoidal-coherence) → [Selinger](#selinger-graphical-languages)
- **Measure kernels and conditioning:** [Pollard](#pollard-measure-theoretic-probability) → [Kallenberg](#kallenberg-foundations-of-modern-probability) → [Faden](#faden-regular-conditional-probabilities)
- **Categorical probability:** [Giry](#giry-probability-as-a-monad) → [Moggi](#moggi-kleisli-semantics) → [Fritz](#fritz-markov-categories) → [Cho and Jacobs](#cho-and-jacobs-disintegration-and-bayesian-inversion)
- **Matrix and path algebra:** [Mohri](#mohri-semiring-path-algorithms) → [Droste, Kuich, and Vogler](#droste-kuich-and-vogler-weighted-automata) → [Joyal, Street, and Verity](#joyal-street-and-verity-traced-monoidal-categories)
- **Open composition:** [Fong and Spivak](#fong-and-spivak-applied-category-theory) → [Fong](#fong-decorated-cospans) → [Baez and Courser](#baez-and-courser-structured-cospans)
- **Information theory:** [Shannon](#shannon-information-theory) → [Cover and Thomas](#cover-and-thomas-information-theory) → [Baez, Fritz, and Leinster](#baez-fritz-and-leinster-entropy) → [Perrone](#perrone-markov-categories-and-entropy)
- **Categorical learning:** [Fong, Spivak, and Tuyéras](#fong-spivak-and-tuyeras-backprop) → [Cockett and colleagues](#cockett-and-colleagues-reverse-derivatives) → [Cruttwell and colleagues](#cruttwell-and-colleagues-gradient-learning)
- **Inventory control examples:** [Clark and Scarf](#clark-and-scarf-multi-echelon-inventory) → [Doğru](#dogru-multi-retailer-inventory) → [Doğru, van Houtum, and de Kok](#dogru-van-houtum-and-de-kok-fixed-batches)

The book chapters [Algebra behind the implementation](algebra-primer.md), [Category theory behind the interfaces](category-primer.md), [Measure theory and the finite specialization](measure-theory-primer.md), [Categorical probability: the bridge](categorical-probability.md), [Information theory for finite stochastic models](information-theory.md), and [Categorical structure of learning and neural networks](categorical-learning.md) explain how these sources connect to the code.

## Category theory and string diagrams

### Leinster: basic category theory

Tom Leinster. *Basic Category Theory*. Cambridge Studies in Advanced Mathematics 143, Cambridge University Press, 2014. [DOI 10.1017/CBO9781107360068](https://doi.org/10.1017/CBO9781107360068). [Free arXiv edition](https://arxiv.org/abs/1612.09375). [Direct PDF](https://arxiv.org/pdf/1612.09375).

Chapter 1 introduces categories, functors, and natural transformations. Chapters 2–5 develop universal properties, representables, Yoneda, limits, and adjunctions. This is the shortest rigorous starting point used by the book's category primer.

### Riehl: category theory in context

Emily Riehl. *Category Theory in Context*. Dover Publications, 2016. [Author-hosted book page](https://math.jhu.edu/~eriehl/context/). [Direct author PDF](https://emilyriehl.github.io/files/context.pdf).

Chapters 1 and 3 cover categories, functors, natural transformations, limits, and colimits. Chapter 5 develops monads and Kleisli categories and explicitly includes probability monads. Use this text when Leinster's compact treatment needs more examples.

### Mac Lane: categories and monoidal coherence

Saunders Mac Lane. *Categories for the Working Mathematician*. Second edition, Graduate Texts in Mathematics 5, Springer, 1998. [DOI 10.1007/978-1-4757-4721-8](https://doi.org/10.1007/978-1-4757-4721-8).

Chapter VII develops monoidal categories, associators, unitors, coherence, and symmetric monoidal structure. Use it for the categorical laws behind tensor, pentagon, triangle, and symmetry.

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

### Cruttwell and colleagues: gradient learning

Geoffrey S. H. Cruttwell, Bruno Gavranović, Neil Ghani, Paul Wilson, and Fabio Zanasi. “Categorical foundations of gradient-based learning.” In *Programming Languages and Systems: ESOP 2022*, LNCS 13240, pages 1–28. [DOI 10.1007/978-3-030-99336-8_1](https://doi.org/10.1007/978-3-030-99336-8_1). [arXiv:2103.01931](https://arxiv.org/abs/2103.01931).

The paper combines parameterization, lenses, and reverse-derivative structure to model gradient-based learners and optimizers compositionally. It provides the broader framework for the proposed parametric-circuit and checked-optimizer extensions.

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

### Clark and Scarf: multi-echelon inventory

Andrew J. Clark and Herbert Scarf. “Optimal policies for a multi-echelon inventory problem.” *Management Science* 6(4), 1960, pages 475–490. [DOI 10.1287/mnsc.6.4.475](https://doi.org/10.1287/mnsc.6.4.475).

Clark and Scarf derive optimal echelon base-stock structure for a serial periodic-review inventory model under their stated demand, lead-time, holding-cost, and shortage-cost assumptions. A Markovian reproduction must encode those timing assumptions explicitly rather than label an arbitrary inventory MDP “Clark--Scarf.”

### Dogru: multi-retailer inventory

Mustafa Kemal Doğru. *Optimal Control of One-Warehouse Multi-Retailer Systems: An Assessment of the Balance Assumption*. PhD thesis, Eindhoven University of Technology, 2006. [DOI 10.6100/IR601558](https://doi.org/10.6100/IR601558). [University record](https://research.tue.nl/en/publications/optimal-control-of-one-warehouse-multi-retailer-systems-an-assess/).

The thesis studies centralized periodic-review control of one warehouse and multiple retailers, including the balance assumption, base-stock policies, newsvendor inequalities, and numerical comparisons with dynamic-programming solutions. The balance assumption can permit allocations unavailable in the physical system, so it must be represented as an explicit approximation rather than hidden in a transition kernel.

### Dogru, van Houtum, and de Kok: fixed batches

M. K. Doğru, G. J. van Houtum, and A. G. de Kok. “Newsvendor equations for optimal reorder levels of serial inventory systems with fixed batch sizes.” *Operations Research Letters* 36(5), 2008, pages 551–556. [DOI 10.1016/j.orl.2008.06.003](https://doi.org/10.1016/j.orl.2008.06.003). [University author copy](https://pure.tue.nl/ws/files/2166749/614894.pdf).

The paper relates optimal echelon-stock `(R,nQ)` reorder levels to newsvendor equalities for continuous demand and inequalities for discrete demand under fixed-batch serial-system assumptions. These results provide a second reproducible inventory benchmark distinct from the unbatched Clark--Scarf model.

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

Chapter 3 defines finite MDPs. Chapter 4 covers dynamic programming. Chapter 6 covers temporal-difference learning, Sarsa, and Q-learning.

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

Double DQN selects the bootstrap action with the online network and evaluates it with the target network. Markovian implements this target rule over an explicit action mask.

### Lillicrap and colleagues: soft target synchronization

Timothy P. Lillicrap and colleagues. “Continuous control with deep reinforcement learning.” *ICLR*, 2016. [arXiv:1509.02971](https://arxiv.org/abs/1509.02971).

This paper uses the soft target update `target <- tau * online + (1 - tau) * target` implemented by Markovian. The operation is deterministic exponential interpolation and is often called a Polyak update in deep-learning code.

### Polyak and Juditsky: averaged stochastic approximation

Boris T. Polyak and Anatoli B. Juditsky. “Acceleration of stochastic approximation by averaging.” *SIAM Journal on Control and Optimization* 30(4), 1992, pages 838–855. [DOI 10.1137/0330046](https://doi.org/10.1137/0330046).

This paper is the primary source for Polyak–Ruppert iterate averaging. That averaging scheme is historically related terminology, but it is not the fixed-coefficient soft target update implemented here.

## Numerical computation

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
| Floating approximation boundaries | Higham Ch. 2, Goldberg |
