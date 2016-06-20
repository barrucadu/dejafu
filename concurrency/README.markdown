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

This is quite a rich set of functionality, although it is not
complete. If there is something else you need, file an issue!

This used to be part of dejafu, but with the dejafu-0.4.0.0 release,
it was split out into its own package.

The documentation of the latest developmental version is
[available online][docs].

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[docs]:    https://docs.barrucadu.co.uk/concurrency/dejafu-0.4
[parconc]: http://chimera.labs.oreilly.com/books/1230000000929
