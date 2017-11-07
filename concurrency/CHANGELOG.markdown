Release Notes
=============

All notable changes to this project will be documented in this file.

This project is versioned according to the [Package Versioning Policy](https://pvp.haskell.org), the
*de facto* standard Haskell versioning scheme.


unreleased
----------

### Control.Concurrent.Classy.Async

- New `asyncBound`, `asyncBoundN`, and `withAsyncBound` functions for doing asynchronous actions on
  bound threads. (#126)

### Control.Monad.Conc.Class

- `MonadConc` now supports bound threads with new `forkOS`, `forkOSN`, and `isCurrentThreadBound`
  functions. (#126)

- New `runInBoundThread` and `runInUnboundThread` functions. (#126)

- The `rtsSupportsBoundThreads` definition is now the definition from Control.Concurrent
  re-exported, not just `False`. (#126)

Note that bound threads are only supported if you compile with GHC and link with -threaded.


---------------------------------------------------------------------------------------------------


1.2.2.0
-------

- **Date**    2017-11-05
- **Git tag** [concurrency-1.2.2.0][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.2.2.0

### Control.Monad.Conc.Class

- A new `IsConc` type (and `toIsConc`, `fromIsConc` functions), where a value of type `IsConc m a`
  can only be constructed if `m` has a `MonadConc` instance.  Its `STM` type is `IsSTM (STM m)`.
  (#144)

- The provided transformer instances now use the `modifyCRefCAS_` of the underlying monad, rather
  than the default definition in terms of `modifyCRefCAS`.

### Control.Monad.STM.Class

- A new `IsSTM` type (and `toIsSTM`, `fromIsSTM` functions), where a value of type `IsSTM m a` can
  only be constructed if `m` has a `MonadSTM` instance. (#144)

[concurrency-1.2.2.0]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.2.2.0


---------------------------------------------------------------------------------------------------


1.2.1.2
-------

- **Date**    2017-10-14
- **Git tag** [concurrency-1.2.1.2][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.2.1.2

### Control.Monad.Conc.Class

- Fixes an infinite loop introduced for the `IO` instance of `MonadConc` in `forkWithUnmask` and
  `forkOnWithUnmask` in 1.2.1.1 (#134).

[concurrency-1.2.1.2]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.2.1.2


---------------------------------------------------------------------------------------------------


1.2.1.1
-------

- **Date**    2017-10-11
- **Git tag** [concurrency-1.2.1.1][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.2.1.1

### Control.Monad.Conc.Class

- The `IO` instance of `MonadConc` now names threads with `GHC.Conc.labelThread`, so thread names
  now appear in debugging traces of normal execution.

[concurrency-1.2.1.1]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.2.1.1


---------------------------------------------------------------------------------------------------


1.2.1.0
-------

- **Date**    2017-10-02
- **Git tag** [concurrency-1.2.1.0][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.2.1.0

### Control.Concurrent.Classy.Async

- New named-thread variants of the `async*` functions: `asyncN`, `asyncOnN`, `asyncWithUnmaskN`, and
  `asyncOnWithUnmaskN` (#125).

[concurrency-1.2.1.0]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.2.1.0


---------------------------------------------------------------------------------------------------


1.2.0.0
-------

- **Date**    2017-09-16
- **Git tag** [concurrency-1.2.0.0][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.2.0.0

### Control.Monad.STM.Class

- `MonadSTM` now has a `MonadPlus` constraint.
- The `orElse` and `retry` functions have been promoted to top-level definitions, and are aliases
  for `mplus` and `mzero`.

[concurrency-1.2.0.0]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.2.0.0


---------------------------------------------------------------------------------------------------


1.1.2.1
-------

- **Date**    2017-06-07
- **Git tag** [concurrency-1.1.2.1][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.1.2.1

### Changed

- The `isEmptyMVar` function is now implemented using `tryReadMVar` instead of a combination of
  `tryTakeMVar` and `putMVar`. It no longer modifies the contents of the `MVar` and can no longer
  block.

### Miscellaneous

- There is now a changelog.

[concurrency-1.1.2.1]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.1.2.1


---------------------------------------------------------------------------------------------------


1.1.2.0
-------

- **Date**    2017-04-05
- **Git tag** [concurrency-1.1.2.0][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.1.2.0

### Control.Concurrent.Classy.Async

- New functions:
    - `uninterruptibleCancel` function, which is `cancel` inside an
      uninterruptible mask.
    - `replicateConcurrently` function, which performs an action many
      times in separate threads.
    - `concurrently_`, `mapConcurrently_`, `forConcurrently_`, and
      `replicateConcurrently_` functions, which discard the result of
      the non-_ version.
- New instances:
    - `Semigroup` instance for `Concurrently` when built with base 4.9.
    - `Monoid` instance for `Concurrently`.

### Control.Monad.Conc.Class

- The `mask_` and `uninterruptibleMask_` functions from Control.Monad.Catch are now re-exported.

### Changed

- The `cancel` and the `withAsync` functions now block until the `Async` action terminates, to match
  changes in the main async package.

### Miscellaneous

- Every definition, class, and instance now has a Haddock "@since" annotation.

[concurrency-1.1.2.0]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.1.2.0


---------------------------------------------------------------------------------------------------


1.1.1.0
-------

- **Date**    2017-03-04
- **Git tag** [concurrency-1.1.1.0][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.1.1.0

### Miscellaneous

- The async-dejafu package has been pulled into this package as the Control.Concurrent.Classy.Async
  module. async-dejafu is now __deprecated__.

[concurrency-1.1.1.0]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.1.1.0


---------------------------------------------------------------------------------------------------


1.1.0.0
-------

- **Date**    2017-02-21
- **Git tag** [concurrency-1.1.0.0][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.1.0.0

### Control.Monad.Conc.Class

- The `MonadConc` class now defines `tryReadMVar`, a non-blocking version of `readMVar` akin to
  `tryTakeMVar`.
- The `MonadConc` class no longer defines `_concMessage`, there is no alternative provided, it is
  just gone.

[concurrency-1.1.0.0]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.1.0.0


---------------------------------------------------------------------------------------------------


1.0.0.0
-------

- **Date**    2016-09-10
- **Git tag** [concurrency-1.0.0.0][]
- **Hackage** https://hackage.haskell.org/package/concurrency-1.0.0.0

Initial release. Go read the API docs.

[concurrency-1.0.0.0]: https://github.com/barrucadu/dejafu/releases/tag/concurrency-1.0.0.0
