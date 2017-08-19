Contributing
============

Bug reports, pull requests, and comments are very welcome!

The general idea (which I'm trying out as of Feb 2017) is
that [master][] should always be at most a minor version ahead of what
is released on hackage, there shouldn't be any backwards-incompatible
changes. Backwards-incompatible changes go on the [next-major][]
branch. This is to make it feasible to fix bugs without also
introducing breaking changes, even if work on the next major version
has already begun.

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[master]:     https://github.com/barrucadu/dejafu/tree/master
[next-major]: https://github.com/barrucadu/dejafu/tree/next-major


Test coverage
-------------

[`hpc`][hpc] can generate a coverage report from the execution of
dejafu-tests:

```
$ stack build --coverage
$ stack exec dejafu-tests
$ stack hpc report --all dejafu-tests.tix
```

This will print some stats and generate an HTML coverage report:

```
Generating combined report
 52% expressions used (4052/7693)
 48% boolean coverage (63/129)
      43% guards (46/106), 31 always True, 9 always False, 20 unevaluated
      68% 'if' conditions (11/16), 2 always True, 3 unevaluated
      85% qualifiers (6/7), 1 unevaluated
 61% alternatives used (392/635)
 80% local declarations used (210/261)
 26% top-level declarations used (280/1063)
The combined report is available at /home/barrucadu/projects/dejafu/.stack-work/install/x86_64-linux/nightly-2016-06-20/8.0.1/hpc/combined/custom/hpc_index.html
```

The highlighted code in the HTML report emphasises branch coverage:

- Red means a branch was evaluated as always false.
- Green means a branch was evaluated as always true.
- Yellow means an expression was never evaluated.

See also the [stack coverage documentation][hpc-stack].

[hpc]:       https://wiki.haskell.org/Haskell_program_coverage
[hpc-stack]: https://docs.haskellstack.org/en/latest/coverage/


Performance
-----------

GHC can generate performance statistics from the execution of
dejafu-tests:

```
$ stack build --profile
$ stack exec  -- dejafu-tests +RTS -p
$ less dejafu-tests.prof
```

This prints a detailed breakdown of where memory and time are being
spent:

```
    Mon Mar 20 19:26 2017 Time and Allocation Profiling Report  (Final)

       dejafu-tests +RTS -p -RTS

    total time  =      105.94 secs   (105938 ticks @ 1000 us, 1 processor)
    total alloc = 46,641,766,952 bytes  (excludes profiling overheads)

COST CENTRE                           MODULE                     %time %alloc

findBacktrackSteps.doBacktrack.idxs'  Test.DejaFu.SCT.Internal    21.9   12.0
==                                    Test.DejaFu.Common          12.4    0.0
yieldCount.go                         Test.DejaFu.SCT             12.1    0.0
dependent'                            Test.DejaFu.SCT              5.1    0.0
runThreads.go                         Test.DejaFu.Conc.Internal    2.7    4.1
[...]
```

dejafu-tests is a good target for profiling, as it is a fairly
representative use: a testsuite where results will be quickly
summarised and printed. It may not be so useful for judging
performance of programs which keep the test results around for a long
time.


Style
-----

There isn't really a prescribed coding style. I've tried [hindent][]
and [brittany][], and didn't like either, maybe I should make my own
formatter. It's not quite the wild west though; keep these four rules
in mind:

1. Be consistent.
2. Use [stylish-haskell][] to format import lists.
3. Use [hlint][] (version 2 at least) and fix all lint messages,
   unless you're *really really sure* there needs to be an exception.
4. The dejafu-tests package really *is* the wild west, hlint and
   stylish-haskell be damned!

The .hlint.yaml and .stylish-haskell.yaml files are enough. You can
use the "lint.sh" and "style.sh" scripts to run the tools for you.

[hindent]:         https://github.com/commercialhaskell/hindent
[brittany]:        https://github.com/lspitzner/brittany
[stylish-haskell]: https://github.com/jaspervdj/stylish-haskell
[hlint]:           https://github.com/ndmitchell/hlint


Documenting your changes
------------------------

**If a top-level definition is introduced or significantly changed**,
whether it is exported or not, give it a descriptive comment:

```haskell
-- | Automatically test a computation. In particular, look for
-- deadlocks, uncaught exceptions, and multiple return values.
--
-- This uses the 'Conc' monad for testing, which is an instance of
-- 'MonadConc'. If you need to test something which also uses
-- 'MonadIO', use 'autocheckIO'.
autocheck :: (Eq a, Show a)
  => (forall t. ConcST t a)
  -- ^ The computation to test
  -> IO Bool
autocheck = autocheckWay defaultWay defaultMemType
```

*Tip:* consider commenting the function arguments. You can skip this
if it's blindingly obvious.

**If an exported definition is introduced or has its API changed**
(behavioural changes don't count for this), it should be given a
`@since` annotation which you can read as "*this definition has had
its current API since version foo*":

```haskell
-- | Variant of 'dejafus' for computations which do 'IO'.
--
-- @since unreleased
dejafusIO :: Show a => ConcIO a -> [(String, Predicate a)] -> IO Bool
dejafusIO = dejafusWayIO defaultWay defaultMemType
```

*Tips:*

- the API for a function is its type, the API for a type is its
  constructors
- moving a definition to a different module is an API change
- version numbers will be filled in when a release is made, using
  "@since unreleased" makes it easy to find the necessary places

**If the change is visible to end-users** add an entry to the relevant
change log in the "unreleased" section. if there is no "unreleased"
section, make one:

```
unreleased
----------

### Test.DejaFu

- All the functions which did take a `Bounds` now take a `Way` instead and support random scheduling
  as well.
```

*Tip:* the change log is a summary of changes, but should still be
intelligible to someone not familiar with the library internals. The
above entry is bad by itself, but ok in context, as another entry in
the 0.5.0.0 changes introduces the `Way` type.
