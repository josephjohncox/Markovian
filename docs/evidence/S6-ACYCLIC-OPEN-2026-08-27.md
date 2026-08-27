# S6 acyclic open-system evidence — 2026-08-27

## Scope

This record captures the local pre-commit S6 review based on commit `875b1dba1479ab0d1329ac3a8df7b42014736c35`. It does not claim hosted CI evidence.

The implemented fragment is finite, boundary-functional, and acyclic. This record does not cover cyclic graphs, feedback, continuous-time black-boxing, or unrestricted MDP black-boxing.

## Passing gates

The following commands passed during local S6 review:

```sh
for dir in . backends/markovian-gpu backends/markovian-neural; do
  (cd "${dir}" && cabal check)
done
find src app test backends -type f -name '*.hs' -print0 \
  | sort -z \
  | xargs -0 fourmolu --mode check
hlint src backends/*/src test/AcyclicOpenSystems.hs
cabal-fmt --check \
  Markovian.cabal \
  backends/markovian-gpu/markovian-gpu.cabal \
  backends/markovian-neural/markovian-neural.cabal
bash -n \
  scripts/bootstrap-tools \
  scripts/check-refinement-roles \
  scripts/check-circuit-purity \
  scripts/check-acyclic-proof-boundary \
  scripts/check-acyclic-purity \
  backends/markovian-gpu/scripts/build-ptx
cabal build all --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
scripts/check-circuit-purity
scripts/check-refinement-roles
scripts/check-acyclic-proof-boundary
scripts/check-acyclic-purity
cabal build all \
  --project-file=cabal.project.ci \
  --with-compiler=ghc-9.4.8 \
  --builddir=dist-s6-ghc94
cabal test all \
  --project-file=cabal.project.ci \
  --with-compiler=ghc-9.4.8 \
  --builddir=dist-s6-ghc94 \
  --test-show-details=direct
ghcup run --ghc 9.4.8 -- scripts/check-refinement-roles
ghcup run --ghc 9.4.8 -- scripts/check-circuit-purity
ghcup run --ghc 9.4.8 -- scripts/check-acyclic-proof-boundary
ghcup run --ghc 9.4.8 -- scripts/check-acyclic-purity
set -o pipefail
cabal haddock all --project-file=cabal.project.ci \
  --enable-documentation --haddock-all --haddock-hyperlink-source \
  2>&1 | tee haddock-9.8.4.log
! grep -nE '(^|[[:space:]])Warning:' haddock-9.8.4.log
ghcup run --ghc 9.4.8 -- cabal haddock all \
  --project-file=cabal.project.ci \
  --haddock-all --haddock-hyperlink-source \
  2>&1 | tee haddock-9.4.8.log
! grep -nE '(^|[[:space:]])Warning:' haddock-9.4.8.log
cabal build all --project-file=cabal.project.ci \
  --prefer-oldest --builddir=dist-s6-revise-oldest
cabal test all --project-file=cabal.project.ci \
  --prefer-oldest --builddir=dist-s6-revise-oldest \
  --test-show-details=direct
rm -rf sdist-s6-review sdist-s6-review-unpacked
mkdir sdist-s6-review sdist-s6-review-unpacked
cabal sdist all --output-directory=sdist-s6-review
test "$(find sdist-s6-review -maxdepth 1 -type f -name '*.tar.gz' | wc -l)" -eq 3
for tarball in sdist-s6-review/*.tar.gz; do
  package_name="$(basename "${tarball}" .tar.gz)"
  package_root="sdist-s6-review-unpacked/${package_name}"
  mkdir -p "${package_root}"
  tar -xzf "${tarball}" -C "${package_root}" --strip-components=1
  (
    cd "${package_root}"
    rm -f cabal.project cabal.project.local
    cabal check
    cabal build all
    cabal test all --test-show-details=direct
  )
done
git diff --check
```

The project-scoped GHC 9.8.4 run used `-Werror` through `cabal.project.ci`. The separate GHC 9.4.8 build and test used the same project file. The core suite reported all one hundred two named contracts as passing. The disabled CUDA package contract and neural categorical contracts also passed in `cabal test all`. Lower-bound and three-archive unpacked source-distribution loops passed locally. Haddock output was searched for warning lines, and both new public modules retained 100% coverage.

This record contains local evidence only. Hosted workflow results must refer to the commit that contains S6.

## S6-specific evidence

`test/AcyclicOpenSystems.hs` covers:

- all unique-production and cycle rejection paths, including producer-error precedence;
- stable self-loop and multi-edge cycle witnesses;
- duplicate domains, assignment domain/value/object mismatches, key, arity, endpoint, primitive edge-context, represented run, outside-input, and deterministic-denotation paths;
- live-frontier evaluation compared with an independent bounded complete-valuation evaluator;
- exact row normalization, a twelve-edge narrow-chain regression, shared fanout, repeated execution, full and partial discard, diamonds, duplicated observations, empty boundaries, and empty sources;
- successful schedule independence with distinguishable fair and biased ready edges plus an asymmetric downstream consumer;
- renaming and boundary-layout independence;
- nonidentity pushout composition, mismatched-boundary rejection, identity, tensor with unequal factors, noncommuting associations, and nonidentity units;
- direct circuit, `composeStochastic`, and named-assignment-reindexed `tensorStochastic` differential checks.

The compile-fail scripts show that the supplied snippets cannot substitute raw topology, boundary-reversed decorations, or the existing global `OpenCircuit` decoration into the DAG interpreter. The snippets also reject direct constructor access, bypass of the validating `Either`, and stochastic aggregate-purity strengthening. These checks do not cover unsafe coercions, bottoms, or arbitrary unsafe code.
