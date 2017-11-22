dejafu
======

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


Installation
------------

Install from Hackage globally:

```
$ cabal-install dejafu
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

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).
