Release Notes
=============

This project is versioned according to the PVP_, the *de facto*
standard Haskell versioning scheme.

.. _PVP: https://pvp.haskell.org/


unreleased
----------

Changed
~~~~~~~

* (:issue:`193`) Deterministically assign commit thread IDs.

Fixed
~~~~~

* (:issue:`189`) Remove an incorrect optimisation in systematic
  testing for ``getNumCapabilities`` and ``setNumCapabilities``.

* (:issue:`204`) Fix missed interleavings in systematic testing with
  some uses of STM.

* (:issue:`205`) Fix ``forkOS`` being recorded in an execution trace
  as if it were a ``fork``.


1.0.0.1 (2018-01-19)
--------------------

* Git: :tag:`dejafu-1.0.0.1`
* Hackage: :hackage:`dejafu-1.0.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`concurrency` is <1.5.


1.0.0.0 - The API Friendliness Release (2017-12-23)
---------------------------------------------------

* Git: :tag:`dejafu-1.0.0.0`
* Hackage: :hackage:`dejafu-1.0.0.0`

Added
~~~~~

* ``Test.DejaFu.alwaysSameOn`` and ``alwaysSameBy`` predicate helpers.

* ``Test.DejaFu.SCT.strengthenDiscard`` and ``weakenDiscard``
  functions to combine discard functions.

* (:issue:`124`) The ``Test.DejaFu.ProPredicate`` type, which contains
  both an old-style ``Predicate`` and a discard function.  It is also
  a ``Profunctor``, parameterised by the input and output types.

* (:issue:`124`) ``Test.DejaFu.alwaysNothing`` and
  ``somewhereNothing`` predicate helpers, like ``alwaysTrue`` and
  ``somewhereTrue``, to lift regular functions into a
  ``ProPredicate``.

* (:issue:`137`) The ``Test.DejaFu.Types.Id`` type.

* (:pull:`145`) Thread action and lookahead values for bound threads:

    * ``Test.DejaFu.Types.ForkOS``
    * ``Test.DejaFu.Types.IsCurrentThreadBound``
    * ``Test.DejaFu.Types.WillForkOS``
    * ``Test.DejaFu.Types.WillIsCurrentThreadBound``

* (:issue:`155`) ``Test.DejaFu.Types`` and ``Test.DejaFu.Utils``
  modules, each containing some of what was in ``Test.DejaFu.Common``.

Changed
~~~~~~~

* All testing functions require ``MonadConc``, ``MonadRef``, and
  ``MonadIO`` constraints.  Testing with ``ST`` is no longer possible.

* The ``Test.DejaFu.alwaysSame`` predicate helper gives the simplest
  trace leading to each distinct result.

* The ``MonadIO Test.DejaFu.Conc.ConcIO`` instance is now the more
  general ``MonadIO n => MonadIO (ConcT r n)``.

* (:issue:`121`) The chosen thread is no longer redundantly included
  in trace lookahead.

* (:issue:`123`) All testing functions in ``Test.DejaFu`` take the
  action to run as the final parameter.

* (:issue:`124`) All testing functions in ``Test.DejaFu`` have been
  generalised to take a ``ProPredicate`` instead of a ``Predicate``.

* (:issue:`124`) The ``Test.DejaFu.Predicate`` type is an alias for
  ``ProPredicate a a``.

* (:issue:`124`) The ``Test.DejaFu.Result`` type no longer includes a
  number of cases checked.

* (:issue:`137`) The ``Test.DejaFu.Types.ThreadId``, ``CRefId``,
  ``MVarId``, and ``TVarId`` types are now wrappers for an ``Id``.

* (:pull:`145`) If built with the threaded runtime, the main thread in
  a test is executed as a bound thread.

* (:issue:`155`) The ``Test.DejaFu.SCT.Discard`` type is defined in
  ``Test.DejaFu.Types``, and re-exported from ``Test.DejaFu.SCT``.

* (:issue:`155`) The ``Test.DejaFu.Schedule.tidOf`` and ``decisionOf``
  functions are defined in ``Test.DejaFu.Utils``, but not re-exported
  from ``Test.DejaFu.Schedule``.

Removed
~~~~~~~

* The ``IO`` specific testing functions:

    * ``Test.DejaFu.autocheckIO``
    * ``Test.DejaFu.dejafuIO``
    * ``Test.DejaFu.dejafusIO``
    * ``Test.DejaFu.autocheckWayIO``
    * ``Test.DejaFu.dejafuWayIO``
    * ``Test.DejaFu.dejafusWayIO``
    * ``Test.DejaFu.dejafuDiscardIO``
    * ``Test.DejaFu.runTestM``
    * ``Test.DejaFu.runTestWayM``

* The ``Test.DejaFu.Conc.ConcST`` type alias.

* The ``MonadBaseControl IO Test.DejaFu.Conc.ConcIO`` typeclass instance.

* The ``Test.DejaFu.alwaysTrue2`` function, which had confusing
  behaviour.

* The ``Test.DejaFu.Common.TTrace`` type synonym for ``[TAction]``.

* The ``Test.DejaFu.Common.preEmpCount`` function.

* Re-exports of ``Decision`` and ``NonEmpty`` from
  ``Test.DejaFu.Schedule``.

* (:issue:`155`) The ``Test.DejaFu.Common`` and ``Test.DejaFu.STM``
  modules.

Fixed
~~~~~

* In refinement property testing, a blocking interference function is
  not reported as a deadlocking execution.

Performance
~~~~~~~~~~~

* (:issue:`124`) Passing tests should use substantially less memory.

* (:issue:`168`) Prune some unnecessary interleavings of ``MVar``
  actions in systematic testing.

Miscellaneous
~~~~~~~~~~~~~

* The lower bound on :hackage:`concurrency` is >=1.3.


0.9.1.2 (2017-12-12)
--------------------

* Git: :tag:`dejafu-0.9.1.2`
* Hackage: :hackage:`dejafu-0.9.1.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`leancheck` is <0.8.


0.9.1.1 (2017-12-08)
--------------------

* Git: :tag:`dejafu-0.9.1.1`
* Hackage: :hackage:`dejafu-0.9.1.1`

Fixed
~~~~~

* (:issue:`160`) Fix an off-by-one issue with nested masks during
  systematic testing.


0.9.1.0 (2017-11-26)
--------------------

* Git: :tag:`dejafu-0.9.1.0`
* Hackage: :hackage:`dejafu-0.9.1.0`

Added
~~~~~

* ``MonadFail`` instance for ``Test.DejaFu.Conc.ConcT``.
* ``MonadFail`` instance for ``Test.DejaFu.STM.STMLike``.

Changed
~~~~~~~

* Pretty-printed traces display a pre-emption following a yield with a
  little "p".

Fixed
~~~~~

* Some incorrect Haddock ``@since`` comments.


0.9.0.3 (2017-11-06)
--------------------

* Git: :tag:`dejafu-0.9.0.3`
* Hackage: :hackage:`dejafu-0.9.0.3`

Fixed
~~~~~

* (:issue:`138`) Fix missed interleavings in systematic testing with
  some relaxed memory programs.


0.9.0.2 (2017-11-02)
--------------------

* Git: :tag:`dejafu-0.9.0.2`
* Hackage: :hackage:`dejafu-0.9.0.2`

Changed
~~~~~~~

* A fair bound of 0 prevents yielding or delaying.

Performance
~~~~~~~~~~~

* Prune some unnecessary interleavings of STM transactions in
  systematic testing.


0.9.0.1 (2017-10-28)
--------------------

* Git: :tag:`dejafu-0.9.0.1`
* Hackage: :hackage:`dejafu-0.9.0.1`

Fixed
~~~~~

* (:issue:`139`) Fix double pop of exception handler stack.


0.9.0.0 (2017-10-11)
--------------------

* Git: :tag:`dejafu-0.9.0.0`
* Hackage: :hackage:`dejafu-0.9.0.0`

Added
~~~~~

* Failure predicates (also exported from ``Test.DejaFu``):

    * ``Test.DejaFu.Common.isAbort``
    * ``Test.DejaFu.Common.isDeadlock``
    * ``Test.DejaFu.Common.isIllegalSubconcurrency``
    * ``Test.DejaFu.Common.isInternalError``
    * ``Test.DejaFu.Common.isUncaughtException``

* Thread action and lookahead values for ``threadDelay``:

    * ``Test.DejaFu.Common.ThreadDelay``
    * ``Test.DejaFu.Common.WillThreadDelay``

Changed
~~~~~~~

* The ``UncaughtException`` constructor for
  ``Test.DejaFu.Common.Failure`` now includes the exception value.

* Uses of ``threadDelay`` are no longer reported in the trace as a use
  of ``yield``.

Removed
~~~~~~~

* The ``Bounded``, ``Enum``, and ``Read`` instances for
  ``Test.DejaFu.Common.Failure``.


0.8.0.0 (2017-09-26)
--------------------

* Git: :tag:`dejafu-0.8.0.0`
* Hackage: :hackage:`dejafu-0.8.0.0`

Changed
~~~~~~~

* (:issue:`80`) STM traces now include the ID of a newly-created
  ``TVar``.

* (:issue:`106`) Schedulers are not given the execution trace so far.

* (:issue:`120`) Traces only include a single action of lookahead.

* (:issue:`122`) The ``Test.DejaFu.Scheduler.Scheduler`` type is now a
  newtype, rather than a type synonym.


0.7.3.0 (2017-09-26)
--------------------

* Git: :tag:`dejafu-0.7.3.0`
* Hackage: :hackage:`dejafu-0.7.3.0`

Added
~~~~~

* The ``Test.DejaFu.Common.threadNames`` function.

Fixed
~~~~~

* (:issue:`101`) Named threads which are only started by a pre-emption
  are shown in the pretty-printed trace key.

* (:issue:`118`) Escaping a mask by raising an exception correctly
  restores the masking state (#118).


0.7.2.0 (2017-09-16)
--------------------

* Git: :tag:`dejafu-0.7.2.0`
* Hackage: :hackage:`dejafu-0.7.2.0`

Added
~~~~~

* ``Alternative`` and ``MonadPlus`` instances for
  ``Test.DejaFu.STM.STM``.

Fixed
~~~~~

* The ``Eq`` and ``Ord`` instances for
  ``Test.DejaFu.Common.ThreadId``, ``CRefId``, ``MVarId``, and
  ``TVarId`` are consistent.

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`concurrency` is <1.2.


0.7.1.3 (2017-09-08)
--------------------

* Git: :tag:`dejafu-0.7.1.3`
* Hackage: :hackage:`dejafu-0.7.1.3`

Fixed
~~~~~

* (:issue:`111`) Aborted STM transactions are correctly rolled back.

Performance
~~~~~~~~~~~

* (:issue:`105`) Use a more efficient approach for an internal
  component of the systematic testing.


0.7.1.2 (2017-08-21)
--------------------

* Git: :tag:`dejafu-0.7.1.2`
* Hackage: :hackage:`dejafu-0.7.1.2`

Fixed
~~~~~

* (:issue:`110`) Errors thrown with ``Control.Monad.fail`` are
  correctly treated as asynchronous exceptions.


0.7.1.1 (2017-08-16)
--------------------

* Git: :tag:`dejafu-0.7.1.1`
* Hackage: :hackage:`dejafu-0.7.1.1`

Performance
~~~~~~~~~~~

* (:issue:`64`) Greatly reduce memory usage in systematic testing when
  discarding traces by using an alternative data structure.

    * Old: ``O(max trace length * number of executions)``
    * New: ``O(max trace length * number of traces kept)``


0.7.1.0 - The Discard Release (2017-08-10)
------------------------------------------

* Git: :tag:`dejafu-0.7.1.0`
* Hackage: :hackage:`dejafu-0.7.1.0`

Added
~~~~~

* (:issue:`90`) A way to selectively discard results or traces:

    * Type: ``Test.DejaFu.SCT.Discard``
    * Functions: ``Test.DejaFu.SCT.runSCTDiscard``,
      ``resultsSetDiscard``, ``sctBoundDiscard``,
      ``sctUniformRandomDiscard``, and ``sctWeightedRandomDiscard``.

* (:issue:`90`) Discarding variants of the testing functions:

    * ``Test.DejaFu.dejafuDiscard``
    * ``Test.DejaFu.dejafuDiscardIO``

* (:issue:`90`) ``Test.DejaFu.Defaults.defaultDiscarder``.

Performance
~~~~~~~~~~~

* (:issue:`90`) The ``Test.DejaFu.SCT.resultsSet`` and ``resultsSet'``
  functions discard traces as they are produced, rather than all at
  the end.


0.7.0.2 (2017-06-12)
--------------------

* Git: :tag:`dejafu-0.7.0.2`
* Hackage: :hackage:`dejafu-0.7.0.2`

Changed
~~~~~~~

* Remove unnecessary typeclass constraints from
  ``Test.DejaFu.Refinement.check``, ``check'``, ``checkFor``, and
  ``counterExamples``.

Miscellaneous
~~~~~~~~~~~~~

* Remove an unnecessary dependency on :hackage:`monad-loops`.


0.7.0.1 (2017-06-09)
--------------------

* Git: :tag:`dejafu-0.7.0.1`
* Hackage: :hackage:`dejafu-0.7.0.1`

Performance
~~~~~~~~~~~

* The ``Test.DejaFu.Refinement.check``, ``check'``, and ``checkFor``
  functions no longer need to compute all counterexamples before
  showing only one.

* The above and ``counterExamples`` are now faster even if there is
  only a single counterexample in some cases.


0.7.0.0 - The Refinement Release (2017-06-07)
---------------------------------------------

* Git: :tag:`dejafu-0.7.0.0`
* Hackage: :hackage:`dejafu-0.7.0.0`

Added
~~~~~

* The ``Test.DejaFu.Refinement`` module, re-exported from
  ``Test.DejaFu``.

* The ``Test.DejaFu.SCT.sctUniformRandom`` function for SCT via random
  scheduling.

* Smart constructors for ``Test.DejaFu.SCT.Way`` (also re-exported
  from ``Test.DejaFu``):

    * ``Test.DejaFu.SCT.systematically``, like the old
      ``Systematically``.
    * ``Test.DejaFu.SCT.randomly``, like the old ``Randomly``.
    * ``Test.DejaFu.SCT.uniformly``, a new uniform (as opposed to
      weighted) random scheduler.
    * ``Test.DejaFu.SCT.swarmy``, like the old ``Randomly`` but which
      can use the same weights for multiple executions.

Changed
~~~~~~~

* The ``default*`` values are defined in ``Test.DejaFu.Defaults`` and
  re-exported from ``Test.DejaFu``.

* The ``Test.DejaFu.SCT.sctRandom`` function is now called
  ``sctWeightedRandom`` and can re-use the same weights for multiple
  executions.

Removed
~~~~~~~

* The ``Test.DejaFu.SCT.Way`` type is now abstract, so its
  constructors are no longer exported:

    * ``Test.DejaFu.SCT.Systematically``
    * ``Test.DejaFu.SCT.Randomly``

* The ``Test.DejaFu.SCT.sctPreBound``, ``sctFairBound``, and
  ``sctLengthBound`` functions.

Fixed
~~~~~

* (:issue:`81`) ``Test.DejaFu.Conc.subconcurrency`` no longer re-uses
  IDs.


0.6.0.0 (2017-04-08)
--------------------

* Git: :tag:`dejafu-0.6.0.0`
* Hackage: :hackage:`dejafu-0.6.0.0`

Changed
~~~~~~~

* The ``Test.DejaFu.Conc.Conc n r a`` type is ``ConcT r n a``, and has
  a ``MonadTrans`` instance.

* The ``Test.DejaFu.SCT.Way`` type is a GADT, and does not expose the
  type parameter of the random generator.

Removed
~~~~~~~

* The ``NFData`` instance for ``Test.DejaFu.SCT.Way``.

Miscellaneous
~~~~~~~~~~~~~

* ``Test.DejaFu.Common`` forms part of the public API.

* Every definition, class, and instance now has a Haddock ``@since``
  annotation.


0.5.1.3 (2017-04-05)
--------------------

* Git: :tag:`dejafu-0.5.1.3`
* Hackage: :hackage:`dejafu-0.5.1.3`

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`concurrency` are 1.1.*.


0.5.1.2 (2017-03-04)
--------------------

* Git: :tag:`dejafu-0.5.1.2`
* Hackage: :hackage:`dejafu-0.5.1.2`

**Note:** this version was misnumbered! It should have caused a minor
 version bump!

Added
~~~~~

* ``MonadRef`` and ``MonadAtomicRef`` instances for
  ``Test.DejaFu.Conc.Conc`` using ``CRef``.

Fixed
~~~~~

* A long-standing bug where if the main thread is killed with a
  ``throwTo``, the throwing neither appears in the trace nor correctly
  terminates the execution.

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`concurrency` is <1.1.1.


0.5.1.1 (2017-02-25)
--------------------

* Git: :tag:`dejafu-0.5.1.1`
* Hackage: :hackage:`dejafu-0.5.1.1`

Fixed
~~~~~

* Fix using incorrect correct scheduler state after a `subconcurrency`
  action.

* Fix infinite loop in SCT of subconcurrency.


0.5.1.0 (2017-02-25)
--------------------

* Git: :tag:`dejafu-0.5.1.0`
* Hackage: :hackage:`dejafu-0.5.1.0`

Added
~~~~~

* ``NFData`` instances for:

    * ``Test.DejaFu.Result``
    * ``Test.DejaFu.Common.ThreadId``
    * ``Test.DejaFu.Common.CRefId``
    * ``Test.DejaFu.Common.MVarId``
    * ``Test.DejaFu.Common.TVarId``
    * ``Test.DejaFu.Common.IdSource``
    * ``Test.DejaFu.Common.ThreadAction``
    * ``Test.DejaFu.Common.Lookahead``
    * ``Test.DejaFu.Common.ActionType``
    * ``Test.DejaFu.Common.TAction``
    * ``Test.DejaFu.Common.Decision``
    * ``Test.DejaFu.Common.Failure``
    * ``Test.DejaFu.Common.MemType``
    * ``Test.DejaFu.SCT.Bounds``
    * ``Test.DejaFu.SCT.PreemptionBound``
    * ``Test.DejaFu.SCT.FairBound``
    * ``Test.DejaFu.SCT.LengthBound``
    * ``Test.DejaFu.SCT.Way``
    * ``Test.DejaFu.STM.Result``

* ``Eq``, ``Ord``, and ``Show`` instances for
  ``Test.DejaFu.Common.IdSource``.

* Strict variants of ``Test.DejaFu.SCT.runSCT`` and ``resultsSet``:
  ``runSCT'`` and ``resultsSet'``.


0.5.0.2 (2017-02-22)
--------------------

* Git: :tag:`dejafu-0.5.0.2`
* Hackage: :hackage:`dejafu-0.5.0.2`

**Note:** this version was misnumbered! It should have caused a major
 version bump!

Added
~~~~~

* ``StopSubconcurrency`` constructor for
  ``Test.DejaFu.Common.ThreadAction``.

Changed
~~~~~~~

* A ``Test.DejaFu.Common.StopConcurrency`` action appears in the
  execution trace immediately after the end of a
  ``Test.DejaFu.Conc.subconcurrency`` action.

Fixed
~~~~~

* A ``Test.DejaFu.Conc.subconcurrency`` action inherits the number of
  capabilities from the outer computation.

Miscellaneous
~~~~~~~~~~~~~

- ``Test.DejaFu.SCT`` compiles with ``MonoLocalBinds`` enabled
  (implied by ``GADTs`` and ``TypeFamilies``), which may be relevant
  to hackers.


0.5.0.1 (2017-02-21)
--------------------

* Git: :tag:`dejafu-0.5.0.1`
* Hackage: :hackage:`ps!**`

Fixed
~~~~~

* ``readMVar`` is considered a "release action" for the purposes of
  fair-bounding.


0.5.0.0 - The Way Release (2017-02-21)
--------------------------------------

* Git: :tag:`dejafu-0.5.0.0`
* Hackage: :hackage:`dejafu-0.5.0.0`

Added
~~~~~

* ``Eq`` instances for ``Test.DejaFu.Common.ThreadAction`` and
  ``Lookahead``.

* Thread action and lookahead values for ``tryReadMVar``:

    * ``Test.DejaFu.Common.TryReadMVar``
    * ``Test.DejaFu.Common.WillTryReadMVar``

* The testing-only ``Test.DejaFu.Conc.subconcurrency`` function.

* SCT through weighted random scheduling:
  ``Test.DejaFu.SCT.sctRandom``.

* The ``Test.DejaFu.SCT.Way`` type, used by the new functions
  ``runSCT`` and ``resultsSet``.

Changed
~~~~~~~

* All the functions which took a ``Test.DejaFu.SCT.Bounds`` now take a
  ``Way`` instead.

Fixed
~~~~~

* Some previously-missed ``CRef`` action dependencies are no longer
  missed.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`concurrency` are 1.1.0.*.

* A bunch of things were called "Var" or "Ref", these are now
  consistently "MVar" and "CRef".

* Significant performance improvements in both time and space.

* The :hackage:`dpor` package has been merged back into this, as it
  turned out not to be very generally useful.


0.4.0.0 - The Packaging Release (2016-09-10)
--------------------------------------------

* Git: :tag:`dejafu-0.4.0.0`
* Hackage: :hackage:`dejafu-0.4.0.0`

Added
~~~~~

* The ``Test.DejaFu.runTestM`` and ``runTestM'`` functions.

* The ``Test.DejaFu.Conc.runConcurrent`` function.

* The ``Test.DejaFu.STM.runTransaction`` function.

* The ``Test.DejaFu.Common`` module.

Changed
~~~~~~~

* The ``Control.*`` modules have all been split out into a separate
  :hackage:`concurrency` package.

* The ``Test.DejaFu.Deterministic`` module has been renamed to
  ``Test.DejaFu.Conc``.

* Many definitions from other modules have been moved to the
  ``Test.DejaFu.Common`` module.

* The ``Test.DejaFu.autocheck'`` function takes the schedule bounds as
  a parameter.

* The ``Test.DejaFu.Conc.Conc`` type no longer has the STM type as a
  parameter.

* The ``ST`` specific functions in ``Test.DejaFu.SCT`` are polymorphic
  in the monad.

* The termination of the main thread in execution traces appears as a
  single ``Stop``, rather than the previous ``Lift, Stop``.

* Execution traces printed by the helpful functions in ``Test.DejaFu``
  include a key of thread names.

Removed
~~~~~~~

* The ``Test.DejaFu.runTestIO`` and ``runTestIO'`` functions: use
  ``runTestM`` and ``runTestM'`` instead.

* The ``Test.DejaFu.Conc.runConcST`` and ``runConcIO`` functions: use
  ``runConcurrent`` instead.

* The ``Test.DejaFu.STM.runTransactionST`` and ``runTransactionIO``
  functions: use ``runTransaction`` instead.

* The ``IO`` specific functions in ``Test.DejaFu.SCT``.



0.3.2.1 (2016-07-21)
--------------------

* Git: :tag:`dejafu-0.3.2.1`
* Hackage: :hackage:`dejafu-0.3.2.1`

Fixed
~~~~~

* (:issue:`55`) Fix incorrect detection of deadlocks with some nested
  STM transactions.


0.3.2.0 (2016-06-06)
--------------------

* Git: :tag:`dejafu-0.3.2.0`
* Hackage: :hackage:`dejafu-0.3.2.0`

Fixed
~~~~~

* (:issue:`40`) Fix missing executions with daemon threads with
  uninteresting first actions.  This is significantly faster with
  :hackage:`dpor-0.2.0.0`.

Performance
~~~~~~~~~~~

* When using :hackage:`dpor-0.2.0.0`, greatly improve dependency
  inference of exceptions during systematic testing.

* Improve dependency inference of STM transactions during systematic
  testing.


0.3.1.1 (2016-05-26)
--------------------

* Git: :tag:`dejafu-0.3.1.1`
* Hackage: :hackage:`dejafu-0.3.1.1`

Miscellaneous
~~~~~~~~~~~~~

* Now supports GHC 8.


0.3.1.0 (2016-05-02)
--------------------

* Git: :tag:`dejafu-0.3.1.0`
* Hackage: :hackage:`dejafu-0.3.1.0`

Fixed
~~~~~

* Fix inaccurate counting of pre-emptions in an execution trace when
  relaxed memory commit actions are present.


0.3.0.0 (2016-04-03)
--------------------

* Git: :tag:`dejafu-0.3.0.0`
* Hackage: :hackage:`dejafu-0.3.0.0`

**The minimum supported version of GHC is now 7.10.**

I didn't write proper release notes, and this is so far back I don't
really care to dig through the logs.


0.2.0.0 (2015-12-01)
--------------------

* Git: :tag:`0.2.0.0`
* Hackage: :hackage:`dejafu-0.2.0.0`

I didn't write proper release notes, and this is so far back I don't
really care to dig through the logs.


0.1.0.0 - The Initial Release (2015-08-27)
------------------------------------------

* Git: :tag:`0.1.0.0`
* Hackage: :hackage:`dejafu-0.1.0.0`

Added
~~~~~

* Everything.
