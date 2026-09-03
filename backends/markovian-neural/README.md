# markovian-neural

`markovian-neural` is a framework-independent numerical reference package for Markovian. It provides checked dense networks, manually supplied VJPs, bounded owned reverse programs, categorical information calculations, structural action masks, small policy-gradient updates, replay, target-network updates, and one DQN batch update.

The package uses finite `Double` values. It does not provide arbitrary-Haskell autodiff, a tensor runtime, a device runtime, an environment runner, convergence evidence, or a production trainer. Reverse primitives own their VJPs. D-067 accepts the extracted reverse-program foundation in `markovian-reverse` only for its bounded scope.

From the repository root:

```sh
cabal test markovian-neural-test --project-file=cabal.project.ci \
  --test-show-details=direct
bash backends/markovian-neural/scripts/check-reverse-program-boundary
```

The optional integration test compares selected updates with the root package and is enabled by `cabal.project.ci`.
