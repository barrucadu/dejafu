Advanced Usage
==============

Déjà Fu tries to have a sensible set of defaults, but there are some
times when the defaults are not suitable.  There are a lot of knobs
provided to tweak how things work.


.. _settings:

Execution settings
------------------

The ``autocheck``, ``dejafu``, and ``dejafus`` functions from
``Test.DejaFu`` all have a variant which lets you specify the **memory
model** used for ``CRef`` operations and the **way** in which
schedules are explored.  These are ``autocheckWay``, ``dejafuWay``,
and ``dejafusWay`` (plus ``IO`` variants).

Memory model
~~~~~~~~~~~~

Threads running under modern multicore processors do not behave as a
simple interleaving of the individual thread actions.  Processors do
all sorts of complex things to increase speed, such as buffering
writes.  For concurrent programs which make use of non-synchronised
functions (such as ``readCRef`` coupled with ``writeCRef``) different
memory models may yield different results.

As an example, consider this program from the ``Data.IORef``
documentation.  Two ``CRef`` variables are created, and two threads
spawned to write to and read from both.  Each thread returns the value
it observes.

.. code-block:: haskell

  example :: MonadConc m => m (Bool, Bool)
  example = do
    r1 <- newCRef False
    r2 <- newCRef False
    x <- spawn $ writeCRef r1 True >> readCRef r2
    y <- spawn $ writeCRef r2 True >> readCRef r1
    (,) <$> readMVar x <*> readMVar y

Under a sequentially consistent memory model the possible results are
``(True, True)``, ``(True, False)``, and ``(False, True)``.  Under
total or partial store order, ``(False, False)`` is also a possible
result, even though there is no interleaving of the threads which can
lead to this.

We can see this by testing with different memory models:

.. code-block:: none

  > autocheckWay defaultWay SequentialConsistency example
  [pass] Never Deadlocks
  [pass] No Exceptions
  [fail] Consistent Result
          (False,True) S0---------S1----S0--S2----S0--

          (True,True) S0---------S1-P2----S1---S0---

          (True,False) S0---------S2----S1----S0---
  False

  > autocheckWay defaultWay TotalStoreOrder example
  [pass] Never Deadlocks
  [pass] No Exceptions
  [fail] Consistent Result
          (False,True) S0---------S1----S0--S2----S0--

          (False,False) S0---------S1--P2----S1--S0---

          (True,False) S0---------S2----S1----S0---

          (True,True) S0---------S1-C-S2----S1---S0---
  False

Traces for non-sequentially-consistent memory models show where
``CRef`` writes are committed, which makes a write visible to all
threads rather than just the one which performed the write.

The default memory model is total store order, as that is how x86
processors behave.

Schedule bounds
~~~~~~~~~~~~~~~

Schedule bounding is an optimisation which only considers schedules
within some bound.  This sacrifices completeness outside of the bound,
but can drastically reduce the number of schedules to test, and is in
fact necessary for non-terminating programs.

There are three supported types of bounds:

Pre-emption bounding
  Restricts the number of pre-emptive context switches.  A context
  switch is pre-emptive if the previously executing thread is not
  blocked and did not explicitly yield.

Fair bounding
  Restricts how many times each thread can yield, by bounding the
  maximum difference between the thread which has yielded the most,
  and the thread which has yielded the least.

Length bounding
  Restricts how long an execution can be, in terms of Déjà Fu's
  "primitive actions".

The standard testing mechanism uses all three bounds.  Pre-emption +
fair bounding is useful for programs which use spinlocks or yield for
control flow, but which are otherwise terminating.  Length bounding
makes it possible to test potentially non-terminating programs.

If you wanted to disable pre-emption bounding, for example, you can do
so like so:

.. code-block:: haskell

  dejafuWay (systematically defaultBounds { boundPreemp = Nothing })
            defaultMemType
            myAction
            ("Assert the thing holds", myPredicate)


Random scheduling
~~~~~~~~~~~~~~~~~

If you don't want to find all executions within the schedule bounds,
and instead want to test a fixed number of executions, you can use
random scheduling.

There are three variants:

``randomly randomGen numExecutions``
  Perform the given number of executions using weighted random
  scheduling.  On creation, a thread is given a random weight, which
  is used to perform a nonuniform random selection amongst the
  enabled (not blocked) threads at every scheduling point.

``uniformly randomGen numExecutions``
  Like ``randomly``, but rather than a weighted selection, it's a
  uniform selection.

``swarmy randomGen numExecutions numUses``
  Like ``randomly``, but each set of thread weights is used for
  ``numUses`` executions.

These are all given as the first argument to ``dejafuWay`` (and its
ilk), like ``systematically``.  So for example you could do this:

.. code-block:: haskell

  dejafuWay (randomly (mkStdGen 42) 1000)
            defaultMemType
            myAction
            ("Assert the thing holds", myPredicate)


.. _performance:

Performance tuning
------------------

* Are you happy to trade space for time?

    Consider computing the results once and running multiple
    predicates over the output: this is what ``dejafus`` /
    ``testDejafus`` / etc does.

* Can you sacrifice completeness?

    Consider using the random testing functionality. See the ``*Way``
    functions and ``Test.DejaFu.SCT.sct{Uniform,Weighted}Random``.

* Would strictness help?

    Consider using the strict functions in ``Test.DejaFu.SCT`` (the
    ones ending with a ``'``).

* Do you just want the set of results, and don't care about traces?

    Consider using ``Test.DejaFu.SCT.resultsSet``.

* Do you know something about the sort of results you care about?

    Consider discarding results you *don't* care about. See the
    ``*Discard`` functions in ``Test.DejaFu``, ``Test.DejaFu.SCT``,
    and ``Test.{HUnit,Tasty}.DejaFu``.

For example, let's say you want to know if your test case deadlocks,
and are going to sacrifice completeness because your possible
state-space is huge.  You could do it like this:

.. code-block:: haskell

  dejafuDiscard
    -- "efa" == "either failure a", discard everything but deadlocks
    (\efa -> if efa == Left Deadlock then Nothing else Just DiscardResultAndTrace)
    -- try 10000 executions with random scheduling
    (randomly (mkStdGen 42) 10000)
    -- use the default memory model
    defaultMemType
    -- your test case
    testCase
    -- the predicate to check (which is a bit redundant in this case)
    ("Never Deadlocks", deadlocksNever)
