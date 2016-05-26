dejafu [![Build Status][build-status]][build-log]
======

[build-status]: https://travis-ci.org/barrucadu/dejafu.svg?branch=master
[build-log]:    https://travis-ci.org/barrucadu/dejafu

> [Déjà Fu is] A martial art in which the user's limbs move in time as
> well as space, […] It is best described as "the feeling that you
> have been kicked in the head this way before"
>
> -- Terry Pratchett, Thief of Time

Have you ever written a concurrent Haskell program and then, Heaven
forbid, wanted to *test* it? Testing concurrency is normally a hard
problem, because of the nondeterminism of scheduling: you can run your
program ten times and get ten different results if you're unlucky.

Fortunately, there is a solution. By abstracting out the actual
implementation of concurrency through a typeclass, an alternative
implementation can be used for testing, allowing the *systematic*
exploration of the possible results of your program.

This repository contains dejafu, a concurrency testing library based
on a typeclass abstraction of concurrency, and related libraries.

- dejafu ([hackage 0.3.1.1][dejafu]): Overloadable primitives for
  testable, potentially non-deterministic, concurrency.

- dpor ([hackage 0.1.0.1][dpor]): A generic implementation of dynamic
  partial-order reduction (DPOR) for testing arbitrary models of
  concurrency.

- async-dejafu ([hackage 0.1.2.2][async]): Run MonadConc operations
  asynchronously and wait for their results.

- hunit-dejafu ([hackage 0.3.0.1][hunit]): Deja Fu support for the
  HUnit test framework.

- tasty-dejafu ([hackage 0.3.0.1][tasty]): Deja Fu support for the
  Tasty test framework.

There is also dejafu-tests, the test suite for dejafu. This is in a
separate package due to Cabal being bad with test suite transitive
dependencies.

[dejafu]: https://hackage.haskell.org/package/dejafu
[dpor]:   https://hackage.haskell.org/package/dpor
[async]:  https://hackage.haskell.org/package/async-dejafu
[hunit]:  https://hackage.haskell.org/package/hunit-dejafu
[tasty]:  https://hackage.haskell.org/package/tasty-dejafu

Features
--------

dejafu supports most of the GHC concurrency abstraction found in
`Control.Concurrent` and, through the [exceptions][] package,
`Control.Exception`. A brief list of supported functionality:

- Threads: the `forkIO*` and `forkOn*` functions, although bound
  threads are not supported.
- Getting and setting capablities (testing default is two).
- Yielding and delaying.
- Mutable state: STM, `MVar`, and `IORef`.
- Relaxed memory for `IORef` operations: total store order (the
  testing default) and partial store order.
- Atomic compare-and-swap for `IORef`.
- Exceptions.
- All of the data structures in `Control.Concurrent.*` and
  `Control.Concurrent.STM.*` have typeclass-abstracted
  equivalents.

This is quite a rich set of functionality, although it is not
complete. If there is something else you need, file an issue!

[exceptions]: https://hackage.haskell.org/package/exceptions

Usage
-----

The documentation for the latest development version is
[available online][docs].

As a general rule of thumb, to convert some existing code to work with
dejafu:

- Import `Control.Concurrent.Classy.*` instead of `Control.Concurrent.*`
- Change `IO a` to `MonadConc m => m a`
- Change `STM a` to `MonadSTM stm => stm a`
- Parameterise all the types by the monad: `MVar` -> `MVar m`, `TVar`
  -> `TVar stm`, `IORef` -> `CRef m`, etc
- Fix the type errors.

[docs]: https://docs.barrucadu.co.uk/dejafu

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

Bibliography
------------

Each paper has a short name in parentheses, which I use in non-haddock
comments. Haddock comments get the full citation. PDF links are
provided where non-paywalled ones are available.

- [BPOR] *Bounded partial-order reduction*, K. Coons, M. Musuvathi,
  and K. McKinley (2013)
  http://research.microsoft.com/pubs/202164/bpor-oopsla-2013.pdf

- [RDPOR] *Dynamic Partial Order Reduction for Relaxed Memory Models*,
  N. Zhang, M. Kusano, and C. Wang (2015)
  http://www.faculty.ece.vt.edu/chaowang/pubDOC/ZhangKW15.pdf

- [Empirical] *Concurrency Testing Using Schedule Bounding: an
  Empirical Study*, P. Thompson, A. Donaldson, and A. Betts (2014)
  http://www.doc.ic.ac.uk/~afd/homepages/papers/pdfs/2014/PPoPP.pdf

- [RMMVerification] *On the Verification of Programs on Relaxed Memory
  Models*, A. Linden (2014)
  https://orbi.ulg.ac.be/bitstream/2268/158670/1/thesis.pdf

There are also a couple of papers on dejafu itself:

- *Déjà Fu: A Concurrency Testing Library for Haskell*, M. Walker and
  C. Runciman (2015)
  https://www.barrucadu.co.uk/publications/dejafu-hs15.pdf

  This details dejafu-0.1, and was presented at the 2015 Haskell
  Symposium.

- *Déjà Fu: A Concurrency Testing Library for Haskell*, M. Walker and
  C. Runciman (2016)
  https://www.barrucadu.co.uk/publications/YCS-2016-503.pdf

  This is a more in-depth technical report, written between the
  dejafu-0.2 and dejafu-0.3 releases.
