concurrency
===========

A typeclass abstraction over much of Control.Concurrent (and some
extras!). If you're looking for a general introduction to Haskell
concurrency, you should check out the excellent
[Parallel and Concurrent Programming in Haskell][parconc], by Simon
Marlow. If you are already familiar with concurrent Haskell, just
change all the imports from Control.Concurrent.* to
Control.Concurrent.Classy.* and fix the type errors.

A brief list of supported functionality:

- Threads: the `forkIO*` and `forkOn*` functions, although bound
  threads are not supported.
- Getting and setting capablities.
- Yielding and delaying.
- Mutable state: STM, `MVar`, and `IORef`.
- Atomic compare-and-swap for `IORef`.
- Exceptions.
- All of the data structures in Control.Concurrent.* and
  Control.Concurrent.STM.* have typeclass-abstracted equivalents.
- A reimplementation of the [async][] package, providing a
  higher-level interface over threads, allowing users to conveniently
  run `MonadConc` operations asynchronously and wait for their
  results.

This is quite a rich set of functionality, although it is not
complete. If there is something else you need, file an issue!

This used to be part of dejafu, but with the dejafu-0.4.0.0 release,
it was split out into its own package.

The documentation of the latest developmental version is
[available online][docs].

Why this and not something else?
--------------------------------

- **Why not base:** like lifted-base, concurrency uses typeclasses to
  make function types more generic. This automatically eliminates
  calls to `lift` in many cases, resulting in clearer and simpler
  code.

- **Why not lifted-base:** fundamentally, lifted-base is still using
  actual threads and actual mutable variables. When using a
  concurrency-specific typeclass, this isn't necessarily the case. The
  dejafu library provides non-IO-based implementations to allow
  testing concurrent programs.

- **Why not IOSpec:** IOSpec provides many of the operations this
  library does, however it uses a free monad to do so, which has extra
  allocation overhead. Furthermore, it does not expose enough of the
  internals in order to accurately test real-execution semantics, such
  as relaxed memory.

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[docs]:    https://docs.barrucadu.co.uk/concurrency/dejafu-0.4
[async]:   https://hackage.haskell.org/package/async
[parconc]: http://chimera.labs.oreilly.com/books/1230000000929
