This repository contains dejafu, a concurrency testing library based
on a typeclass abstraction of concurrency, and related libraries.

- dejafu ([hackage 0.3.0.0][dejafu]): Overloadable primitives for
  testable, potentially non-deterministic, concurrency.

- dpor ([hackage 0.1.0.0][dpor]): A generic implementation of dynamic
  partial-order reduction (DPOR) for testing arbitrary models of
  concurrency.

- async-dejafu ([hackage 0.1.1.0][async]): Run MonadConc operations
  asynchronously and wait for their results.

- hunit-dejafu ([hackage 0.2.1.0][hunit]): Deja Fu support for the
  HUnit test framework.

- tasty-dejafu ([hackage 0.2.1.0][tasty]): Deja Fu support for the
  Tasty test framework.

There is also dejafu-tests, the test suite for dejafu. This is in a
separate package due to Cabal being bad with test suite transitive
dependencies.

[dejafu]: http://hackage.haskell.org/package/dejafu
[dpor]:   http://hackage.haskell.org/package/dpor
[async]:  http://hackage.haskell.org/package/async-dejafu
[hunit]:  http://hackage.haskell.org/package/hunit-dejafu
[tasty]:  http://hackage.haskell.org/package/tasty-dejafu

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
