Release Notes
=============

All notable changes to this project will be documented in this file.

This project is versioned according to the [Package Versioning Policy](https://pvp.haskell.org), the
*de facto* standard Haskell versioning scheme.


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
