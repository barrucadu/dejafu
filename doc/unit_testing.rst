Unit Testing
============

Writing tests with Déjà Fu is a little different to traditional unit
testing, as your test case may have multiple results.  A "test" is a
combination of your code, and a predicate which says something about
the set of allowed results.

Most tests will look something like this:

.. code-block:: haskell

  dejafu "Assert the thing holds" myPredicate myAction

The ``dejafu`` function comes from ``Test.DejaFu``.  It can't deal
with testcases which need ``MonadIO``, use ``dejafuIO`` for that.


Actions
-------

An action is just something with the type ``MonadConc m => m a``, or
``(MonadConc m, MonadIO m) => m a`` for some ``a`` that your chosen
predicate can deal with.

For example, some users on Reddit found a couple of apparent bugs in
the :hackage:`auto-update` package a while ago (`thread here`__).  As
the package is simple and self-contained, I translated it to the
``MonadConc`` abstraction and wrote a couple of tests to replicate the
bugs.  Here they are:

.. code-block:: haskell

  deadlocks :: MonadConc m => m ()
  deadlocks = do
    auto <- mkAutoUpdate defaultUpdateSettings
    auto

  nondeterministic :: forall m. MonadConc m => m Int
  nondeterministic = do
    var <- newCRef 0
    let settings = (defaultUpdateSettings :: UpdateSettings m ())
          { updateAction = atomicModifyCRef var (\x -> (x+1, x)) }
    auto <- mkAutoUpdate settings
    auto
    auto

.. __: https://www.reddit.com/r/haskell/comments/2i5d7m/updating_autoupdate/

These actions action could be tested with ``autocheck``, and the
issues would be revealed.  The use of ``ScopedTypeVariables`` in the
second is an unfortunate example of what can happen when everything
becomes more polymorphic.  But other than that, note how there is no
special mention of Déjà Fu in the actions: it's just normal concurrent
Haskell, simply written against a different interface.

The modified package is included :github:`in the test suite
<blob/2a15549d97c2fa12f5e8b92ab918fdb34da78281/dejafu-tests/Examples/AutoUpdate.hs>`,
if you want to see the full code. [#]_

.. [#] The predicates in dejafu-tests are a little confusing, as
       they're the opposite of what you would normally write!  These
       predicates are checking that the bug is found, not that the
       code is correct.

If the RTS supports bound threads (the ``-threaded`` flag was passed
to GHC when linking), then the main thread of an action given to Déjà
Fu will be bound, and further bound threads can be forked with the
``forkOS`` functions.  If not, then attempting to fork a bound thread
will raise an error.


Predicates
----------

There are a few predicates built in, and some helpers to define your
own.

.. csv-table::
  :widths: 25, 75

  ``abortsNever``,"checks that the computation never aborts"
  ``abortsAlways``,"checks that the computation always aborts"
  ``abortsSometimes``,"checks that the computation aborts at least once"

An **abort** is where the scheduler chooses to terminate execution
early.  If you see it, it probably means that a test didn't terminate
before it hit the execution length limit.

.. csv-table::
  :widths: 25, 75

  ``deadlocksNever``,"checks that the computation never deadlocks"
  ``deadlocksAlways``,"checks that the computation always deadlocks"
  ``deadlocksSometimes``,"checks that the computation deadlocks at least once"

**Deadlocking** is where every thread becomes blocked.  This can be,
for example, if every thread is trying to read from an ``MVar`` that
has been emptied.

.. csv-table::
  :widths: 25, 75

  ``exceptionsNever``,"checks that the main thread is never killed by an exception"
  ``exceptionsAlways``,"checks that the main thread is always killed by an exception"
  ``exceptionsSometimes``,"checks that the main thread is killed by an exception at least once"

An uncaught **exception** in the main thread kills the process.  These
can be synchronous (thrown in the main thread) or asynchronous (thrown
to it from a different thread).

.. csv-table::
  :widths: 25, 75

  ``alwaysSame``,"checks that the computation is deterministic"
  ``alwaysSameOn f``,"is like ``alwaysSame``, but transforms the results with ``f`` first"
  ``alwaysSameBy f``,"is like ``alwaysSame``, but uses ``f`` instead of ``(==)`` to compare"
  ``notAlwaysSame``,"checks that the computation is nondeterministic"

Checking for **determinism** will also find nondeterministic failures:
deadlocking (for instance) is still a result of a test!

.. csv-table::
  :widths: 25, 75

  ``alwaysTrue p``,"checks that ``p`` is true for every result"
  ``somewhereTrue p``,"checks that ``p`` is true for at least one result"

These can be used to check custom predicates.  For example, you might
want all your results to be less than five.

.. csv-table::
  :widths: 25, 75

  ``gives xs``,"checks that the set of results is exactly ``xs`` (which may include failures)"
  ``gives' xs``,"checks that the set of results is exactly ``xs`` (which may not include failures)"

These let you say exactly what you want the results to be.  Your test
will fail if it has any extra results, or misses a result.

You can check multiple predicates against the same collection of
results using the ``dejafus`` and ``dejafusIO`` functions.  These
avoid recomputing the results, and so may be faster than multiple
``dejafu`` / ``dejafuIO`` calls.  See :ref:`performance`.


Using HUnit and Tasty
---------------------

By itself, Déjà Fu has no framework in place for named test groups and
parallel execution or anything like that.  It does one thing and does
it well, which is running test cases for concurrent programs.
:hackage:`HUnit` and :hackage:`tasty` integration is provided to get
more of the features you'd expect from a testing framework.

The integration is provided by the :hackage:`hunit-dejafu` and
:hackage:`tasty-dejafu` packages.

There's a simple naming convention used: the ``Test.DejaFu`` function
``dejafuFoo`` is wrapped in the appropriate way and exposed as
``testDejafuFoo`` from ``Test.HUnit.DejaFu`` and
``Test.Tasty.DejaFu``.

Our example from the start becomes:

.. code-block:: haskell

  testDejafu "Assert the thing holds" myPredicate myAction

The ``autocheck`` and ``autocheckIO`` functions are exposed as
``testAuto`` and ``testAutoIO``.
