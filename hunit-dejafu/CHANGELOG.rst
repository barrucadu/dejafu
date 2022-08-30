Release Notes
=============

This project is versioned according to the PVP_, the *de facto*
standard Haskell versioning scheme.

.. _PVP: https://pvp.haskell.org/


2.0.0.6 (2022-08-30)
--------------------

* Git: :tag:`hunit-dejafu-2.0.0.6`
* Hackage: :hackage:`hunit-dejafu-2.0.0.6`

Fixed
~~~~~

* Remove inaccurate comment about ``Test.HUnit.DejaFu.testDejafus``
  sharing work.


2.0.0.5 (2021-08-15)
--------------------

* Git: :tag:`hunit-dejafu-2.0.0.5`
* Hackage: :hackage:`hunit-dejafu-2.0.0.5`

Miscellaneous
~~~~~~~~~~~~~

* Remove reference to freenode in README.


2.0.0.4 (2020-07-01)
--------------------

* Git: :tag:`hunit-dejafu-2.0.0.4`
* Hackage: :hackage:`hunit-dejafu-2.0.0.4`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.5


2.0.0.3 (2020-05-10)
--------------------

* Git: :tag:`hunit-dejafu-2.0.0.3`
* Hackage: :hackage:`hunit-dejafu-2.0.0.3`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.4


2.0.0.2 (2020-05-10)
--------------------

* Git: :tag:`hunit-dejafu-2.0.0.2`
* Hackage: :hackage:`hunit-dejafu-2.0.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.3


2.0.0.1 (2019-03-24)
--------------------

* Git: :tag:`hunit-dejafu-2.0.0.1`
* Hackage: :hackage:`hunit-dejafu-2.0.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <2.2


2.0.0.0 (2019-02-12)
--------------------

* Git: :tag:`hunit-dejafu-2.0.0.0`
* Hackage: :hackage:`hunit-dejafu-2.0.0.0`

Added
~~~~~

* Re-exports for the ``Program`` types and their constructors:
    * ``Test.HUnit.DejaFu.Program``
    * ``Test.HUnit.DejaFu.Basic``
    * ``Test.HUnit.DejaFu.ConcT``
    * ``Test.HUnit.DejaFu.ConcIO``
    * ``Test.HUnit.DejaFu.WithSetup``
    * ``Test.HUnit.DejaFu.WithSetupAndTeardown``
    * ``Test.HUnit.DejaFu.withSetup``
    * ``Test.HUnit.DejaFu.withTeardown``
    * ``Test.HUnit.DejaFu.withSetupAndTeardown``

* Re-exports for the ``Invariant`` type and its functions:
    * ``Test.HUnit.DejaFu.Invariant``
    * ``Test.HUnit.DejaFu.registerInvariant``
    * ``Test.HUnit.DejaFu.inspectIORef``
    * ``Test.HUnit.DejaFu.inspectMVar``
    * ``Test.HUnit.DejaFu.inspectTVar``

Changed
~~~~~~~

* Functions which took a ``ConcIO`` now take a ``Program pty IO``:
    * ``Test.HUnit.DejaFu.testAuto``
    * ``Test.HUnit.DejaFu.testAutoWay``
    * ``Test.HUnit.DejaFu.testAutoWithSettings``
    * ``Test.HUnit.DejaFu.testDejafu``
    * ``Test.HUnit.DejaFu.testDejafuWay``
    * ``Test.HUnit.DejaFu.testDejafuWithSettings``
    * ``Test.HUnit.DejaFu.testDejafus``
    * ``Test.HUnit.DejaFu.testDejafusWay``
    * ``Test.HUnit.DejaFu.testDejafusWithSettings``

Removed
~~~~~~~

* The deprecated functions:
    * ``Test.HUnit.DejaFu.testDejafuDiscard``
    * ``Test.HUnit.DejaFu.testDejafusDiscard``

Miscellaneous
~~~~~~~~~~~~~

* The lower bound on :hackage:`dejafu` is >=2.0.


1.2.1.0 (2019-01-20)
--------------------

* Git: :tag:`hunit-dejafu-1.2.1.0`
* Hackage: :hackage:`hunit-dejafu-1.2.1.0`

Added
~~~~~

* Re-export of the ``Condition`` type from :hackage:`dejafu`.  If
  using dejafu < 1.12, this is an alias for ``Failure``.

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.13


1.2.0.6 (2018-07-01)
--------------------

* Git: :tag:`hunit-dejafu-1.2.0.6`
* Hackage: :hackage:`hunit-dejafu-1.2.0.6`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.12.


1.2.0.5 (2018-06-17)
--------------------

* Git: :tag:`hunit-dejafu-1.2.0.5`
* Hackage: :hackage:`hunit-dejafu-1.2.0.5`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.11.


1.2.0.4 (2018-06-10)
--------------------

* Git: :tag:`hunit-dejafu-1.2.0.4`
* Hackage: :hackage:`hunit-dejafu-1.2.0.4`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.10.


1.2.0.3 (2018-06-03)
--------------------

* Git: :tag:`hunit-dejafu-1.2.0.3`
* Hackage: :hackage:`hunit-dejafu-1.2.0.3`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.9.


1.2.0.2 (2018-06-03)
--------------------

* Git: :tag:`hunit-dejafu-1.2.0.2`
* Hackage: :hackage:`hunit-dejafu-1.2.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.8.


1.2.0.1 (2018-05-11)
--------------------

* Git: :tag:`hunit-dejafu-1.2.0.1`
* Hackage: :hackage:`hunit-dejafu-1.2.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.7.


1.2.0.0 - No More 7.10 (2018-03-28)
-----------------------------------

* Git: :tag:`hunit-dejafu-1.2.0.0`
* Hackage: :hackage:`hunit-dejafu-1.2.0.0`

Miscellaneous
~~~~~~~~~~~~~

* GHC 7.10 support is dropped.  Dependency lower bounds are:

    * :hackage:`base`: 4.9
    * :hackage:`dejafu`: 1.5
    * :hackage:`HUnit`: 1.3.1

* The upper bound on :hackage:`dejafu` is 1.6.


1.1.0.3 (2018-03-17)
--------------------

* Git: :tag:`hunit-dejafu-1.1.0.3`
* Hackage: :hackage:`hunit-dejafu-1.1.0.3`

Miscellaneous
~~~~~~~~~~~~~

* (:pull:`251`) The upper bound on :hackage:`dejafu` is <1.5.


1.1.0.2 (2018-03-11)
--------------------

* Git: :tag:`hunit-dejafu-1.1.0.2`
* Hackage: :hackage:`hunit-dejafu-1.1.0.2`

Miscellaneous
~~~~~~~~~~~~~

* (:pull:`245`) The upper bound on :hackage:`exceptions` is <0.11.


1.1.0.1 (2018-03-06)
--------------------

* Git: :tag:`hunit-dejafu-1.1.0.1`
* Hackage: :hackage:`hunit-dejafu-1.1.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.4.


1.1.0.0 - The Settings Release (2018-03-06)
-------------------------------------------

* Git: :tag:`hunit-dejafu-1.1.0.0`
* Hackage: :hackage:`hunit-dejafu-1.1.0.0`

Added
~~~~~

* (:pull:`238`) Settings-based test functions:

    * ``Test.HUnit.DejaFu.testAutoWithSettings``
    * ``Test.HUnit.DejaFu.testDejafuWithSettings``
    * ``Test.HUnit.DejaFu.testDejafusWithSettings``

* (:pull:`238`) Re-export of ``Test.DejaFu.Settings``.

Deprecated
~~~~~~~~~~

* (:pull:`238`) ``Test.HUnit.DejaFu.testDejafuDiscard`` and
  ``testDejafusDiscard``.

Removed
~~~~~~~

* (:pull:`238`) The re-export of
  ``Test.DejaFu.Defaults.defaultDiscarder``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=1.2 && <1.3.


1.0.1.2 (2018-02-26)
--------------------

* Git: :tag:`hunit-dejafu-1.0.1.2`
* Hackage: :hackage:`hunit-dejafu-1.0.1.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`exceptions` is <0.10.


1.0.1.1 (2018-02-22)
--------------------

* Git: :tag:`hunit-dejafu-1.0.1.1`
* Hackage: :hackage:`hunit-dejafu-1.0.1.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <1.2.


1.0.1.0 (2018-02-13)
--------------------

* Git: :tag:`hunit-dejafu-1.0.1.0`
* Hackage: :hackage:`hunit-dejafu-1.0.1.0`

Added
~~~~~

* (:pull:`200`) ``Test.HUnit.DejaFu.testDejafusDiscard`` function.


1.0.0.0 - The API Friendliness Release (2017-12-23)
---------------------------------------------------

* Git: :tag:`hunit-dejafu-1.0.0.0`
* Hackage: :hackage:`hunit-dejafu-1.0.0.0`

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

* The orphan ``Testable`` and ``Assertable`` instances for
  ``Test.DejaFu.Conc.ConcST t ()``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=1.0 && <1.1.


0.7.1.1 (2017-11-30)
--------------------

* Git: :tag:`hunit-dejafu-0.7.1.1`
* Hackage: :hackage:`hunit-dejafu-0.7.1.1`

Fixed
~~~~~

* A missing Haddock ``@since`` comments.


0.7.1.0 (2017-11-30)
--------------------

* Git: :tag:`hunit-dejafu-0.7.1.0`
* Hackage: :hackage:`hunit-dejafu-0.7.1.0`

Added
~~~~~

* ``Test.HUnit.DejaFu.testPropertyFor`` function.


0.7.0.2 (2017-10-11)
--------------------

* Git: :tag:`hunit-dejafu-0.7.0.2`
* Hackage: :hackage:`hunit-dejafu-0.7.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <0.10.


0.7.0.1 (2017-09-26)
--------------------

* Git: :tag:`hunit-dejafu-0.7.0.1`
* Hackage: :hackage:`hunit-dejafu-0.7.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <0.9.


0.7.0.0 - The Discard Release (2017-08-10)
------------------------------------------

* Git: :tag:`hunit-dejafu-0.7.0.0`
* Hackage: :hackage:`hunit-dejafu-0.7.0.0`

Added
~~~~~

* Re-export for ``Test.DejaFu.SCT.Discard`` and
  ``Test.DejaFu.Defaults.defaultDiscarder``.

* ``Test.HUnit.DejaFu.testDejafuDiscard`` and ``testDejafuDiscardIO``
  functions.

Miscellaneous
~~~~~~~~~~~~~

* The lower bound on :hackage:`dejafu` is >=0.7.1.


0.6.0.0 - The Refinement Release (2017-06-07)
---------------------------------------------

* Git: :tag:`hunit-dejafu-0.6.0.0`
* Hackage: :hackage:`hunit-dejafu-0.6.0.0`

Added
~~~~~

* ``Test.HUnit.DejaFu.testProperty`` function

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

* Git: :tag:`hunit-dejafu-0.5.0.0`
* Hackage: :hackage:`hunit-dejafu-0.5.0.0`

Changed
~~~~~~~

* Due to changes in :hackage:`dejafu`, the ``Way`` type no longer
  takes a parameter; it is now a GADT.

Miscellaneous
~~~~~~~~~~~~~

* Every definition, class, and instance now has a Haddock ``@since``
  annotation.

* The version bounds on :hackage:`dejafu` are >=0.6 && <0.7.

* Remove an unnecessary dependency on :hackage:`random`.


0.4.0.1 (2017-03-20)
--------------------

* Git: :tag:`hunit-dejafu-0.4.0.1`
* Hackage: :hackage:`hunit-dejafu-0.4.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`HUnit` is <1.7.


0.4.0.0 (2017-02-21)
--------------------

* Git: :tag:`hunit-dejafu-0.4.0.0`
* Hackage: :hackage:`hunit-dejafu-0.4.0.0`

Added
~~~~~

* Re-export of ``Test.DejaFu.SCT.Way``.

Changed
~~~~~~~

* All the functions which took a ``Test.DejaFu.SCT.Bounds`` now take a
  ``Way``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=0.5 && <0.6.

* Dependency on :hackage:`random` with bounds >=1.0 && <1.2.


0.3.0.3 (2016-10-22)
--------------------

* Git: :tag:`hunit-dejafu-0.3.0.3`
* Hackage: :hackage:`hunit-dejafu-0.3.0.3`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`HUnit` is <1.6.


0.3.0.2 (2016-09-10)
--------------------

* Git: :tag:`hunit-dejafu-0.3.0.2`
* Hackage: :hackage:`hunit-dejafu-0.3.0.2`

Miscellaneous
~~~~~~~~~~~~~

* The upper bound on :hackage:`dejafu` is <0.5.


0.3.0.1 (2016-05-26)
--------------------

* Git: :tag:`hunit-dejafu-0.3.0.1`
* Hackage: :hackage:`hunit-dejafu-0.3.0.1`

Miscellaneous
~~~~~~~~~~~~~

* The lower bound on :hackage:`base` is >=4.8.

* The upper bound on :hackage:`dejafu` is <0.4.


0.3.0.0 (2016-04-28)
--------------------

* Git: :tag:`hunit-dejafu-0.3.0.0`
* Hackage: :hackage:`hunit-dejafu-0.3.0.0`

Added
~~~~~

* Orphan ``Assertable`` and ``Testable`` instances for
  ``Test.DejaFu.Conc.ConcST t ()`` and ``ConcIO ()``.

* Re-export ``Test.DejaFu.SCT.Bounds``.

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are >=0.2


0.2.1.0 (2016-04-03)
--------------------

* Git: :tag:`hunit-dejafu-0.2.1.0`

**Note:** this was never pushed to Hackage, whoops!

Miscellaneous
~~~~~~~~~~~~~

* The version bounds on :hackage:`dejafu` are 0.3.*.


0.2.0.0 - The Initial Release (2015-12-01)
------------------------------------------

* Git: :tag:`0.2.0.0`
* Hackage: :hackage:`hunit-dejafu-0.2.0.0`

Added
~~~~~

* Everything.
