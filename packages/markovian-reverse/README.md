# markovian-reverse

`markovian-reverse` contains the backend-independent reverse core used by the optional autodiff and neural packages.

It provides finite typed reverse circuits and bounded, acyclic owned program interpreters. `Markovian.Reverse.Program` retains pure `Either` execution. This pure API specializes the effect execution core to `Identity`. `Markovian.Reverse.Program.Effect` keeps preparation pure. It runs supplied callbacks through explicit `m (Either error value)` boundaries. Primitive symbols close during preparation. Deterministic reports count attempted callbacks.

Primitive authors still supply primal operations, pullbacks, finite layouts, cotangent-module operations, and ownership metadata. The package does not differentiate arbitrary Haskell and does not claim tensor or device semantics.
