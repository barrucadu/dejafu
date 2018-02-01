0.x to 1.x
==========

:hackage:`dejafu-1.0.0.0` is a super-major release which breaks
compatibility with :hackage:`dejafu-0.x <dejafu-0.9.1.1>` quite
significantly, but brings with it support for bound threads, and
significantly improves memory usage in the general case.

Highlights reel:

* Most predicates now only need to keep around the failures, rather
  than all results.
* Support for bound threads (with :hackage:`concurrency-1.3.0.0`).
* The ``ST`` / ``IO`` interface duplication is gone, everything is now
  monadic.
* Function parameter order is closer to other testing libraries.
* Much improved API documentation.

See the changelogs for the full details.


``ST`` and ``IO`` functions
---------------------------

There is only one set of functions now.  Testing bound threads
requires being able to fork actual threads, so testing with ``ST`` is
no longer possible.  The ``ConcST`` type is gone, there is only
``ConcIO``.

For :hackage:`dejafu` change:

* ``autocheckIO`` to ``autocheck``
* ``dejafuIO`` to ``dejafu``
* ``dejafusIO`` to ``dejafus``
* ``autocheckWayIO`` to ``autocheckWay``
* ``dejafuWayIO`` to ``dejafuWay``
* ``dejafusWayIO`` to ``dejafusWay``
* ``dejafuDiscardIO`` to ``dejafuDiscard``
* ``runTestM`` to ``runTest``
* ``runTestWayM`` to ``runTestWay``

If you relied on being able to get a pure result from the ``ConcST``
functions, you can no longer do this.

For :hackage:`hunit-dejafu` and :hackage:`tasty-dejafu` change:

* ``testAutoIO`` to ``testAuto``
* ``testDejafuIO`` to ``testDejafu``
* ``testDejafusIO`` to ``testDejafus``
* ``testAutoWayIO`` to ``testAutoWay``
* ``testDejafuWayIO`` to ``testDejafuWay``
* ``testDejafusWayIO`` to ``testDejafusWay``
* ``testDejafuDiscardIO`` to ``testDejafuDiscard``


Function parameter order
------------------------

Like :hackage:`HUnit`, the monadic action to test is now the last
parameter of the testing functions.  This makes it convenient to write
tests without needing to define the action elsewhere.

For :hackage:`dejafu` change:

* ``dejafu ma (s, p)`` to ``dejafu s p ma``
* ``dejafus ma ps`` to ``dejafus ps ma``
* ``dejafuWay way mem ma (s, p)`` to ``dejafuWay way mem s p ma``
* ``dejafusWay way mem ma ps`` to ``dejafuWay way mem ps ma``
* ``dejafuDiscard d way mem ma (s, p)`` to ``dejafuDiscard d way mem s p ma``

For :hackage:`hunit-dejafu` and :hackage:`tasty-dejafu` change:

* ``testDejafu ma s p`` to ``testDejafu s p ma``
* ``testDejafus ma ps`` to ``testDejafus ps ma``
* ``testDejafuWay way mem ma s p`` to ``testDejafuWay way mem s p ma``
* ``testDejafusWay way mem ma ps`` to ``testDejafusWay way mem ps ma``
* ``testDejafuDiscard d way mem ma s p`` to ``testDejafuDiscard d way mem s p ma``


Predicates
----------

The ``Predicate a`` type is now an alias for ``ProPredicate a a``,
defined like so:

.. code-block:: haskell

  data ProPredicate a b = ProPredicate
    { pdiscard :: Either Failure a -> Maybe Discard
    -- ^ Selectively discard results before computing the result.
    , peval :: [(Either Failure a, Trace)] -> Result b
    -- ^ Compute the result with the un-discarded results.
    }

If you use the predicate helper functions to construct a predicate,
you do not need to change anything (and should get a nice reduction in
your resident memory usage).  If you supply a function directly, you
can recover the old behaviour like so:

.. code-block:: haskell

  old :: ([(Either Failure a, Trace)] -> Result a) -> ProPredicate a a
  old p = ProPredicate
    { pdiscard = const Nothing
    , peval = p
    }

The ``alwaysTrue2`` helper function is gone.  If you use it, use
``alwaysSameOn`` or ``alwaysSameBy`` instead.


Need help?
----------

* For general help talk to me in IRC (barrucadu in #haskell) or shoot
  me an email (mike@barrucadu.co.uk)
* For bugs, issues, or requests, please :issue:`file an issue <>`.
