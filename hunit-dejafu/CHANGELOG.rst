Release Notes
=============

This project is versioned according to the PVP_, the *de facto*
standard Haskell versioning scheme.

.. _PVP: https://pvp.haskell.org/

unreleased
----------

Added
~~~~~

* (:issue:`221`) Settings-based test functions:

    * ``Test.HUnit.DejaFu.testAutoWithSettings``
    * ``Test.HUnit.DejaFu.testDejafuWithSettings``
    * ``Test.HUnit.DejaFu.testDejafusWithSettings``

* (:issue:`221`) Re-export of ``Test.DejaFu.Settings``.

Deprecated
~~~~~~~~~~

* (:issue:`221`) ``Test.HUnit.DejaFu.testDejafuDiscard`` and
  ``testDejafusDiscard``.

Removed
~~~~~~~

* (:issue:`221`) The re-export of
  ``Test.DejaFu.Defaults.defaultDiscarder``.


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
