Unit Testing
============

Writing tests with Déjà Fu is a little different to traditional unit testing, as
your test case may have multiple results.  A "test" is a combination of your
code, and a predicate which says something about the set of allowed results.

Most tests will look something like this:

```haskell
dejafu "Assert the thing holds" myPredicate myAction
```

The `dejafu` function comes from `Test.DejaFu`.  Another useful function is
`dejafuWithSettings` (see [Advanced Usage](./advanced-usage.md)).


Actions
-------

An action is just something with the type `MonadConc m => m a`, or `(MonadConc
m, MonadIO m) => m a` for some `a` that your chosen predicate can deal with.

For example, some users on Reddit found a couple of apparent bugs in the
[auto-update][h:auto] package a while ago ([thread here][auto-thread]).  As the
package is simple and self-contained, I translated it to the `MonadConc`
abstraction and wrote a couple of tests to replicate the bugs.  Here they are:

```haskell
deadlocks :: MonadConc m => m ()
deadlocks = do
  auto <- mkAutoUpdate defaultUpdateSettings
  auto

nondeterministic :: forall m. MonadConc m => m Int
nondeterministic = do
  var <- newIORef 0
  let settings = (defaultUpdateSettings :: UpdateSettings m ())
        { updateAction = atomicModifyIORef var (\x -> (x+1, x)) }
  auto <- mkAutoUpdate settings
  auto
  auto
```

These actions action could be tested with `autocheck`, and the issues would be
revealed.  The use of `ScopedTypeVariables` in the second is an unfortunate
example of what can happen when everything becomes more polymorphic.  But other
than that, note how there is no special mention of Déjà Fu in the actions: it's
just normal concurrent Haskell, simply written against a different interface.

The modified package is included [in the test suite][], if you want to see the
full code.

```admonish note
The predicates in dejafu-tests are a little confusing, as they're the opposite
of what you would normally write!  These predicates are checking that the bug is
found, not that the code is correct.
```

If the RTS supports bound threads (the `-threaded` flag was passed to GHC when
linking), then the main thread of an action given to Déjà Fu will be bound, and
further bound threads can be forked with the `forkOS` functions.  If not, then
attempting to fork a bound thread will raise an error.

[h:auto]: https://hackage.haskell.org/package/auto-update
[auto-thread]: https://www.reddit.com/r/haskell/comments/2i5d7m/updating_autoupdate/
[in the test suite]: https://github.com/barrucadu/dejafu/blob/2a15549d97c2fa12f5e8b92ab918fdb34da78281/dejafu-tests/Examples/AutoUpdate.hs


Conditions
----------

When a concurrent program of type `MonadConc m => m a` is executed, it may
produce a value of type `a`, or it may experience a **condition** such as
deadlock.

A condition does not necessarily cause your test to fail.  It's important to be
aware of what exactly your test is testing, to avoid drawing the wrong
conclusions from a passing (or failing) test.


Setup and Teardown
------------------

Because dejafu drives the execution of the program under test, there are some
tricks available to you which are not possible using normal concurrent Haskell.

If your test does some set-up work which is required for your test to work, but
which is not the actual thing you are testing, you can define that as a **setup
action**:

```haskell
withSetup
  :: Program Basic n x
  -- ^ Setup action
  -> (x -> Program Basic n a)
  -- ^ Main program
  -> Program (WithSetup x) n a
```

dejafu will save the state at the end of the setup action, and efficiently
restore that state in subsequent runs of the same test with a different
schedule.  This can be much more efficient than dejafu running the setup action
normally every single time.

If you want to examine some state you created in your setup action even if your
actual test case deadlocks or something, you can define a **teardown action**:

```haskell
withSetupAndTeardown
  :: Program Basic n x
  -- ^ Setup action
  -> (x -> Either Condition y -> Program Basic n a)
  -- ^ Teardown action
  -> (x -> Program Basic n y)
  -- ^ Main program
  -> Program (WithSetupAndTeardown x y) n a
```

The teardown action is always executed.

Finally, if you want to ensure that some invariant holds over some shared state,
you can define invariants in the setup action, which are checked atomically
during the main action:

```haskell
-- slightly contrived example
let setup = do
      var <- newEmptyMVar
      registerInvariant $ do
        value <- inspectMVar var
        when (x == Just 1) (throwM Overflow)
      pure var
in withSetup setup $ \var -> do
     fork $ putMVar var 0
     fork $ putMVar var 1
     tryReadMVar var
```

If the main action violates the invariant, it is terminated with an
`InvariantFailure` condition, and any teardown action is run.


Predicates
----------

There are a few predicates built in, and some helpers to define your own.


|   |   |
| - | - |
| `abortsNever` | checks that the computation never aborts |
| `abortsAlways` | checks that the computation always aborts |
| `abortsSometimes` | checks that the computation aborts at least once |

An **abort** is where the scheduler chooses to terminate execution early.  If
you see it, it probably means that a test didn't terminate before it hit the
execution length limit.  Aborts are hidden unless you use explicitly enable
them, see (see [Advanced Usage](./advanced-usage.md)).

|   |   |
| - | - |
| `deadlocksNever` | checks that the computation never deadlocks |
| `deadlocksAlways` | checks that the computation always deadlocks |
| `deadlocksSometimes` | checks that the computation deadlocks at least once |

**Deadlocking** is where every thread becomes blocked.  This can be, for
example, if every thread is trying to read from an `MVar` that has been emptied.

|   |   |
| - | - |
| `exceptionsNever` | checks that the main thread is never killed by an exception |
| `exceptionsAlways` | checks that the main thread is always killed by an exception |
| `exceptionsSometimes` | checks that the main thread is killed by an exception at least once |

An uncaught **exception** in the main thread kills the process.  These can be
synchronous (thrown in the main thread) or asynchronous (thrown to it from a
different thread).

|   |   |
| - | - |
| `alwaysSame` | checks that the computation is deterministic and always produces a value |
| `alwaysSameOn f` | is like `alwaysSame`, but transforms the results with `f` first |
| `alwaysSameBy f` | is like `alwaysSame`, but uses `f` instead of `(==)` to compare |
| `notAlwaysSame` | checks that the computation is nondeterministic |
| `notAlwaysSameOn f` | is like `notAlwaysSame`, but transforms the results with `f` first |
| `notAlwaysSameBy f` | is like `notAlwaysSame`, but uses `f` instead of `(==)` to compare |

Checking for **determinism** will also find nondeterministic failures:
deadlocking (for instance) is still a result of a test!

|   |   |
| - | - |
| `alwaysTrue p` | checks that `p` is true for every result |
| `somewhereTrue p` | checks that `p` is true for at least one result |

These can be used to check custom predicates.  For example, you might want all
your results to be less than five.

|   |   |
| - | - |
| `gives xs` |checks that the set of results is exactly `xs` (which may include conditions) |
| `gives' xs` |checks that the set of results is exactly `xs` (which may not include conditions) |

These let you say exactly what you want the results to be.  Your test will fail
if it has any extra results, or misses a result.

You can check multiple predicates against the same collection of results using
the `dejafus` and `dejafusWithSettings` functions.  These avoid recomputing the
results, and so may be faster than multiple `dejafu` / `dejafuWithSettings`
calls.


Using HUnit and Tasty
---------------------

By itself, Déjà Fu has no framework in place for named test groups and parallel
execution or anything like that.  It does one thing and does it well, which is
running test cases for concurrent programs.  [HUnit][] and [tasty][] integration
is provided to get more of the features you'd expect from a testing framework.

The integration is provided by the [hunit-dejafu][] and [tasty-dejafu][]
packages.

There's a simple naming convention used: the `Test.DejaFu` function `dejafuFoo`
is wrapped in the appropriate way and exposed as `testDejafuFoo` from
`Test.HUnit.DejaFu` and `Test.Tasty.DejaFu`.

Our example from the start becomes:

```haskell
testDejafu "Assert the thing holds" myPredicate myAction
```

The `autocheck` function is exposed as `testAuto`.

[HUnit]: https://hackage.haskell.org/package/HUnit
[tasty]: https://hackage.haskell.org/package/tasty
[hunit-dejafu]: https://hackage.haskell.org/package/hunit-dejafu
[tasty-dejafu]: https://hackage.haskell.org/package/tasty-dejafu
