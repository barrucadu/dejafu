Release Notes
=============

This project is versioned according to the PVP_, the *de facto*
standard Haskell versioning scheme.

.. _PVP: https://pvp.haskell.org/


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
