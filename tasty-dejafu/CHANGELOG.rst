Release Notes
=============

This project is versioned according to the PVP_, the *de facto*
standard Haskell versioning scheme.

.. _PVP: https://pvp.haskell.org/


unreleased
----------

Fixed
~~~~~

* Remove inaccurate comment about ```Test.Tasty.DejaFu.testDejafus``
  sharing work.


2.0.0.8 (2021-08-15)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.8`
* Hackage: :hackage:`tasty-dejafu-2.0.0.8`

Miscellaneous
~~~~~~~~~~~~~

* Remove reference to freenode from README.


2.0.0.7 (2020-12-27)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.7`
* Hackage: :hackage:`tasty-dejafu-2.0.0.7`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`tasty` is <1.5.


2.0.0.6 (2020-07-01)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.6`
* Hackage: :hackage:`tasty-dejafu-2.0.0.6`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.5.


2.0.0.5 (2020-06-24)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.5`
* Hackage: :hackage:`tasty-dejafu-2.0.0.5`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`random` is <1.3.


2.0.0.4 (2020-05-14)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.4`
* Hackage: :hackage:`tasty-dejafu-2.0.0.4`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.4


2.0.0.3 (2020-05-10)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.3`
* Hackage: :hackage:`tasty-dejafu-2.0.0.3`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.3


2.0.0.2 (2020-05-10)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.2`
* Hackage: :hackage:`tasty-dejafu-2.0.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`tasty` is <1.4


2.0.0.1 (2019-03-24)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.1`
* Hackage: :hackage:`tasty-dejafu-2.0.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.2


2.0.0.0 (2019-02-12)
--------------------

* Git: :tag:`tasty-dejafu-2.0.0.0`
* Hackage: :hackage:`tasty-dejafu-2.0.0.0`

Added
~~~~~

* Re-exports for the ``Program`` types and their constructors:
    * ``Test.Tasty.DejaFu.Program``
    * ``Test.Tasty.DejaFu.Basic``
    * ``Test.Tasty.DejaFu.ConcT``
    * ``Test.Tasty.DejaFu.ConcIO``
    * ``Test.Tasty.DejaFu.WithSetup``
    * ``Test.Tasty.DejaFu.WithSetupAndTeardown``
    * ``Test.Tasty.DejaFu.withSetup``
    * ``Test.Tasty.DejaFu.withTeardown``
    * ``Test.Tasty.DejaFu.withSetupAndTeardown``

* Re-exports for the ``Invariant`` type and its functions:
    * ``Test.Tasty.DejaFu.Invariant``
    * ``Test.Tasty.DejaFu.registerInvariant``
    * ``Test.Tasty.DejaFu.inspectIORef``
    * ``Test.Tasty.DejaFu.inspectMVar``
    * ``Test.Tasty.DejaFu.inspectTVar``

Changes
~~~~~~~

* Functions which took a ``ConcIO`` now take a ``Program pty IO``:
    * ``Test.Tasty.DejaFu.testAuto``
    * ``Test.Tasty.DejaFu.testAutoWay``
    * ``Test.Tasty.DejaFu.testAutoWithSettings``
    * ``Test.Tasty.DejaFu.testDejafu``
    * ``Test.Tasty.DejaFu.testDejafuWay``
    * ``Test.Tasty.DejaFu.testDejafuWithSettings``
    * ``Test.Tasty.DejaFu.testDejafus``
    * ``Test.Tasty.DejaFu.testDejafusWay``
    * ``Test.Tasty.DejaFu.testDejafusWithSettings``

Removed
~~~~~~~

* The deprecated functions:
    * ``Test.Tasty.DejaFu.testDejafuDiscard``
    * ``Test.Tasty.DejaFu.testDejafusDiscard``

Miscellaneous
~~~~~~~~~~~~~

* The lower bound on :hackage:`dejafu` is >=2.0.


1.2.1.0 (2019-01-20)
--------------------

* Git: :tag:`tasty-dejafu-1.2.1.0`
* Hackage: :hackage:`tasty-dejafu-1.2.1.0`

Added
~~~~~

* Re-export of the ``Condition`` type from :hackage:`dejafu`.  If
  using dejafu < 1.12, this is an alias for ``Failure``.

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.13


1.2.0.8 (2018-12-02)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.8`
* Hackage: :hackage:`tasty-dejafu-1.2.0.8`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`tasty` is <1.3.


1.2.0.7 (2018-07-01)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.7`
* Hackage: :hackage:`tasty-dejafu-1.2.0.7`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.12.


1.2.0.6 (2018-06-17)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.6`
* Hackage: :hackage:`tasty-dejafu-1.2.0.6`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.11.


1.2.0.5 (2018-06-10)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.5`
* Hackage: :hackage:`tasty-dejafu-1.2.0.5`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.10.


1.2.0.4 (2018-06-03)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.4`
* Hackage: :hackage:`tasty-dejafu-1.2.0.4`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.9.


1.2.0.3 (2018-06-03)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.3`
* Hackage: :hackage:`tasty-dejafu-1.2.0.3`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.8.


1.2.0.2 (2018-05-12)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.2`
* Hackage: :hackage:`tasty-dejafu-1.2.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`tasty` is <1.2.


1.2.0.1 (2018-05-11)
--------------------

* Git: :tag:`tasty-dejafu-1.2.0.1`
* Hackage: :hackage:`tasty-dejafu-1.2.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.7.


1.2.0.0 - No More 7.10 (2018-03-28)
-----------------------------------

* Git: :tag:`tasty-dejafu-1.2.0.0`
* Hackage: :hackage:`tasty-dejafu-1.2.0.0`

Miscellaneous
~~~~~~~~~~~~~

* GHC 7.10 support is dropped.  Dependency lower bounds are:

    * :hackage:`base`: 4.9
    * :hackage:`dejafu`: 1.5

* The upper bound on :hackage:`dejafu` is 1.6.


1.1.0.2 (2018-03-17)
--------------------

* Git: :tag:`tasty-dejafu-1.1.0.2`
* Hackage: :hackage:`tasty-dejafu-1.1.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.5.


1.1.0.1 (2018-03-06)
--------------------

* Git: :tag:`tasty-dejafu-1.1.0.1`
* Hackage: :hackage:`tasty-dejafu-1.1.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.4.


1.1.0.0 - The Settings Release (2018-03-06)
-------------------------------------------

* Git: :tag:`tasty-dejafu-1.1.0.0`
* Hackage: :hackage:`tasty-dejafu-1.1.0.0`

Added
~~~~~

* (:pull:`238`) Settings-based test functions:

    * ``Test.Tasty.DejaFu.testAutoWithSettings``
    * ``Test.Tasty.DejaFu.testDejafuWithSettings``
    * ``Test.Tasty.DejaFu.testDejafusWithSettings``

* (:pull:`238`) Re-export of ``Test.DejaFu.Settings``.

Deprecated
~~~~~~~~~~

* (:pull:`238`) ``Test.Tasty.DejaFu.testDejafuDiscard`` and
  ``testDejafusDiscard``.

Removed
~~~~~~~

* (:pull:`238`) The re-export of
  ``Test.DejaFu.Defaults.defaultDiscarder``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=1.2 && <1.3.


1.0.1.1 (2018-02-22)
--------------------

* Git: :tag:`tasty-dejafu-1.0.1.1`
* Hackage: :hackage:`tasty-dejafu-1.0.1.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.2.


1.0.1.0 (2018-02-13)
--------------------

* Git: :tag:`tasty-dejafu-1.0.1.0`
* Hackage: :hackage:`tasty-dejafu-1.0.1.0`

Added
~~~~~

* (:pull:`195`) ``Test.Tasty.DejaFu.testDejafusDiscard`` function.


1.0.0.1 (2018-01-09)
--------------------

* Git: :tag:`tasty-dejafu-1.0.0.1`
* Hackage: :hackage:`tasty-dejafu-1.0.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`tasty` is <1.1.


1.0.0.0 - The API Friendliness Release (2017-12-23)
---------------------------------------------------

* Git: :tag:`tasty-dejafu-1.0.0.0`
* Hackage: :hackage:`tasty-dejafu-1.0.0.0`

Added
~~~~~

* (:issue:`124`) Re-exports of ``Test.DejaFu.Predicate`` and
  ``ProPredicate``.

Changed
~~~~~~~

* All testing functions require ``MonadConc``, ``MonadRef``, and
  ``MonadIO`` constraints.  Testing with ``ST`` is no longer possible.

* (:issue:`123`) All testing functions take the action to run as the
  final parameter.

* (:issue:`124`) All testing functions have been generalised to take a
  ``Test.DejaFu.ProPredicate`` instead of a ``Predicate``.

Removed
~~~~~~~

* The ``Test.DejaFu.Conc.ConcST`` specific functions.

* The orphan ``IsTest`` instance for ``Test.DejaFu.Conc.ConcST t
  (Maybe String)``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=1.0 && <1.1.


0.7.1.1 (2017-11-30)
--------------------

* Git: :tag:`tasty-dejafu-0.7.1.1`
* Hackage: :hackage:`tasty-dejafu-0.7.1.1`

Fixed
~~~~~

* A missing Haddock ``@since`` comments.


0.7.1.0 (2017-11-30)
--------------------

* Git: :tag:`tasty-dejafu-0.7.1.0`
* Hackage: :hackage:`tasty-dejafu-0.7.1.0`

Added
~~~~~

* ``Test.Tasty.DejaFu.testPropertyFor`` function.


0.7.0.3 (2017-11-02)
--------------------

* Git: :tag:`tasty-dejafu-0.7.0.3`
* Hackage: :hackage:`tasty-dejafu-0.7.0.3`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`tasty` is <0.13.


0.7.0.2 (2017-10-11)
--------------------

* Git: :tag:`tasty-dejafu-0.7.0.2`
* Hackage: :hackage:`tasty-dejafu-0.7.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <0.10.


0.7.0.1 (2017-09-26)
--------------------

* Git: :tag:`tasty-dejafu-0.7.0.1`
* Hackage: :hackage:`tasty-dejafu-0.7.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <0.9.


0.7.0.0 - The Discard Release (2017-08-10)
------------------------------------------

* Git: :tag:`tasty-dejafu-0.7.0.0`
* Hackage: :hackage:`tasty-dejafu-0.6.0.0`

Added
~~~~~

* Re-export for ``Test.DejaFu.SCT.Discard`` and
  ``Test.DejaFu.Defaults.defaultDiscarder``.

* ``Test.Tasty.DejaFu.testDejafuDiscard`` and ``testDejafuDiscardIO``
  functions.

Miscellaneous
~~~~~~~~~~~~~

* The lower bound on :hackage:`dejafu` is >=0.7.1.


0.6.0.0 - The Refinement Release (2017-04-08)
---------------------------------------------

* Git: :tag:`tasty-dejafu-0.6.0.0`
* Hackage: :hackage:`tasty-dejafu-0.6.0.0`

Added
~~~~~

* ``Test.Tasty.DejaFu.testProperty`` function

* Re-exports for ``Test.DejaFu.SCT.systematically``, ``randomly``,
  ``uniformly``, and ``swarmy``.

* Re-exports for ``Test.DejaFu.Defaults.defaultWay``,
  ``defaultMemType``, and ``defaultBounds``.

Removed
~~~~~~~

* Re-exports of the ``Test.DejaFu.SCT.Way`` constructors:
  ``Systematically`` and ``Randomly``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=0.7 && <0.8.


0.5.0.0 - The Way Release (2017-04-08)
--------------------------------------

* Git: :tag:`tasty-dejafu-0.5.0.0`
* Hackage: :hackage:`tasty-dejafu-0.5.0.0`

Changed
~~~~~~~

* Due to changes in :hackage:`dejafu`, the ``Way`` type no longer
  takes a parameter; it is now a GADT.

Miscellaneous
~~~~~~~~~~~~~

* Every definition, class, and instance now has a Haddock ``@since``
  annotation.

* The version bounds on :hackage:`dejafu` are >=0.6 && <0.7.


0.4.0.0 (2017-02-21)
--------------------

* Git: :tag:`tasty-dejafu-0.4.0.0`
* Hackage: :hackage:`tasty-dejafu-0.4.0.0`

Added
~~~~~

* Re-export of ``Test.DejaFu.SCT.Way``.

* Orphan ``IsOption`` instance for ``Test.DejaFu.SCT.Way``.
  Command-line parameters are:

    * "systematically": systematic testing with the default bounds
    * "randomly": 100 executions with a fixed random seed

Changed
~~~~~~~

* All the functions which took a ``Test.DejaFu.SCT.Bounds`` now take a
  ``Way``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=0.5 && <0.6.

* Dependency on :hackage:`random` with bounds >=1.0 && <1.2.


0.3.0.2 (2016-09-10)
--------------------

* Git: :tag:`tasty-dejafu-0.3.0.2`
* Hackage: :hackage:`tasty-dejafu-0.3.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <0.5.


0.3.0.1 (2016-05-26)
--------------------

* Git: :tag:`tasty-dejafu-0.3.0.1`
* Hackage: :hackage:`tasty-dejafu-0.3.0.1`

Miscellaneous
~~~~~~~~~~~~~


* The lower bound on :hackage:`base` is >=4.8.

* The upper bound on :hackage:`dejafu` is <0.4.


0.3.0.0 (2016-04-28)
--------------------

* Git: :tag:`tasty-dejafu-0.3.0.0`
* Hackage: :hackage:`tasty-dejafu-0.3.0.0`

Added
~~~~~

* Orphan ``IsTest`` instances for ``Test.DejaFu.Conc.ConcST t (Maybe
  String)`` and ``ConcIO (Maybe String)``.

* Orphan ``IsOption`` instances for ``Test.DejaFu.SCT.Bounds`` and
  ``MemType``.  Command-line parameters are:

    * "sc": sequential consistency
    * "tso": total store order
    * "pso": partial store order

* Re-export ``Test.DejaFu.SCT.Bounds``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=0.2


0.1.1.0 (2016-04-03)
--------------------

* Git: :tag:`tasty-dejafu-0.1.1.0`

**Note:** this was misnumbered (it should have been 0.2.1.0) *and* was
 never pushed to Hackage, whoops!

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are 0.3.*.


0.2.0.0 - The Initial Release (2015-12-01)
------------------------------------------

* Git: :tag:`0.2.0.0`
* Hackage: :hackage:`tasty-dejafu-0.2.0.0`

Added
~~~~~

* Everything.
