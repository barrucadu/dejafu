dejafu [![Build Status][build-status]][build-log]
======

[build-status]: https://travis-ci.org/barrucadu/dejafu.svg?branch=master
[build-log]:    https://travis-ci.org/barrucadu/dejafu

> [Déjà Fu is] A martial art in which the user's limbs move in time as
> well as space, […] It is best described as "the feeling that you
> have been kicked in the head this way before"
>
> -- Terry Pratchett, Thief of Time

- [Installation](#installation)
- [Quick start guide](#quick-start-guide)
- [Why Déjà Fu?](#why-déjà-fu)
- [Contributing](#contributing)
- [Release notes](#release-notes)
- [Questions, feedback, discussion](#questions-feedback-discussion)
- [Bibliography](#bibliography)
- **[The website!](http://dejafu.readthedocs.io/)**

Déjà Fu is a unit-testing library for concurrent Haskell programs.
Tests are deterministic and expressive, making it easy and convenient
to test your threaded code.  Available on [GitHub][], [Hackage][], and
[Stackage][].

[GitHub]:   https://github.com/barrucadu/dejafu
[Hackage]:  https://hackage.haskell.org/package/dejafu
[Stackage]: https://www.stackage.org/package/dejafu

Features:

- An abstraction over the concurrency functionality in `IO`
- Deterministic testing of nondeterministic code
- Both complete (slower) and incomplete (faster) modes
- A unit-testing-like approach to writing test cases
- A property-testing-like approach to comparing stateful operations
- Testing of potentially nonterminating programs
- Integration with [HUnit][] and [tasty][]

[HUnit]: https://hackage.haskell.org/package/HUnit
[Tasty]: https://hackage.haskell.org/package/tasty

There are a few different packages under the Déjà Fu umbrella:

|   | Version | Summary |
| - | ------- | ------- |
| [concurrency][h:conc]    | 1.4.0.0 | Typeclasses, functions, and data types for concurrency and STM. |
| [dejafu][h:dejafu]       | 1.0.0.2 | Systematic testing for Haskell concurrency. |
| [hunit-dejafu][h:hunit]  | 1.0.1.0 | Deja Fu support for the HUnit test framework. |
| [tasty-dejafu][h:tasty]  | 1.0.1.0 | Deja Fu support for the Tasty test framework. |

Each package has its own README and CHANGELOG in its subdirectory.

There is also dejafu-tests, the test suite for dejafu. This is in a
separate package due to Cabal being bad with test suite transitive
dependencies.

[h:conc]:   https://hackage.haskell.org/package/concurrency
[h:dejafu]: https://hackage.haskell.org/package/dejafu
[h:hunit]:  https://hackage.haskell.org/package/hunit-dejafu
[h:tasty]:  https://hackage.haskell.org/package/tasty-dejafu


Installation
------------

Install from Hackage globally:

```
$ cabal install dejafu
```

Or add it to your cabal file:

```
build-depends: ...
             , dejafu
```

Or to your package.yaml:

```
dependencies:
  ...
  - dejafu
```


Quick start guide
-----------------

Déjà Fu supports unit testing, and comes with a helper function called
`autocheck` to look for some common issues.  Let's see it in action:

```haskell
import Control.Concurrent.Classy

myFunction :: MonadConc m => m String
myFunction = do
  var <- newEmptyMVar
  fork (putMVar var "hello")
  fork (putMVar var "world")
  readMVar var
```

That `MonadConc` is a typeclass abstraction over concurrency, but
we'll get onto that shortly.  First, the result of testing:

```
> autocheck myFunction
[pass] Never Deadlocks
[pass] No Exceptions
[fail] Consistent Result
    "hello" S0----S1--S0--

    "world" S0----S2--S0--
False
```

There are no deadlocks or uncaught exceptions, which is good; but the
program is (as you probably spotted) nondeterministic!

Along with each result, Déjà Fu gives us a representative execution
trace in an abbreviated form.  `Sn` means that thread `n` started
executing, and `Pn` means that thread `n` pre-empted the previously
running thread.


Why Déjà Fu?
------------

Testing concurrent programs is difficult, because in general they are
nondeterministic.  This leads to people using work-arounds like
running their testsuite many thousands of times; or running their
testsuite while putting their machine under heavy load.

These approaches are inadequate for a few reasons:

- **How many runs is enough?** When you are just hopping to spot a bug
  by coincidence, how do you know to stop?
- **How do you know if you've fixed a bug you saw previously?**
  Because the scheduler is a black box, you don't know if the
  previously buggy schedule has been re-run.
- **You won't get that much scheduling variety!** Operating systems
  and language runtimes like to run threads for long periods of time,
  which reduces the variety you get (and so drives up the number of
  runs you need).

Déjà Fu addresses these points by offering *complete* testing.  You
can run a test case and be guaranteed to find all results with some
bounds.  These bounds can be configured, or even disabled!  The
underlying approach used is smarter than merely trying all possible
executions, and will in general explore the state-space quickly.

If your test case is just too big for complete testing, there is also
a random scheduling mode, which is necessarily *incomplete*.  However,
Déjà Fu will tend to produce much more schedule variety than just
running your test case in `IO` the same number of times, and so bugs
will tend to crop up sooner.  Furthermore, as you get execution traces
out, you can be certain that a bug has been fixed by simply following
the trace by eye.


Contributing
------------

See the "contributing" page on [the website](http://dejafu.readthedocs.io/en/latest/contributing.html).

**If you'd like to get involved with Déjà Fu**, check out [the
"good first issue" label on the issue tracker][beginners].

[beginners]: https://github.com/barrucadu/dejafu/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22


Release notes
-------------

See the CHANGELOG.markdown file.


Questions, feedback, discussion
-------------------------------

- For general help talk to me in IRC (barrucadu in #haskell) or shoot
  me an email (mike@barrucadu.co.uk)
- For bugs, issues, or requests, please [file an issue][issues].

[issues]:  https://github.com/barrucadu/dejafu/issues


Bibliography
------------

These libraries wouldn't be possible without prior research, which I
mention in the documentation. Haddock comments get the full citation,
whereas in-line comments just get the shortened name:

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
