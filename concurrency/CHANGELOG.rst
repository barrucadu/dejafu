Release Notes
=============

This project is versioned according to the PVP_, the *de facto*
standard Haskell versioning scheme.

.. _PVP: https://pvp.haskell.org/


1.4.0.0 (2018-01-19)
--------------------

* Git: :tag:`concurrency-1.4.0.0`
* Hackage: :hackage:`concurrency-1.4.0.0`

Changed
~~~~~~~

* ``Control.Monad.Conc.Class.peekTicket'`` has a more concrete type,
  to make deriving newtype instances of ``MonadConc`` possible:

    * Old: ``MonadConc m => proxy m -> Ticket m a -> a``
    * New: ``MonadConc m => Proxy m -> Ticket m a -> a``


1.3.0.0 - The Bound Thread Release (2017-12-23)
-----------------------------------------------

* Git: :tag:`concurrency-1.3.0.0`
* Hackage: :hackage:`concurrency-1.3.0.0`

**Note:** bound threads are only supported if you compile with GHC and
link with -threaded.

Added
~~~~~

* (:pull:`145`) Bound thread variants of the ``withAsync`` functions:

    * ``Control.Concurrent.Classy.Async.asyncBound``
    * ``Control.Concurrent.Classy.Async.asyncBoundN``
    * ``Control.Concurrent.Classy.Async.withAsyncBound``
    * ``Control.Concurrent.Classy.Async.withAsyncBoundN``

* (:pull:`145`) Bound thread functions in ``MonadConc``:

    * ``Control.Monad.Conc.Class.forkOS``
    * ``Control.Monad.Conc.Class.forkOSN``
    * ``Control.Monad.Conc.Class.isCurrentThreadBound``

* (:pull:`145`) Helper functions for bound threads:

    * ``Control.Monad.Conc.Class.runInBoundThread``
    * ``Control.Monad.Conc.Class.runInUnboundThread``

Changed
~~~~~~~

* (:pull:`145`) ``Control.Monad.Conc.Class.rtsSupportsBoundThreads``
  is a re-export from ``Control.Concurrent``.


1.2.3.0 (2017-11-30)
--------------------

* Git: :tag:`concurrency-1.2.3.0`
* Hackage: :hackage:`concurrency-1.2.3.0`

Added
~~~~~

* (:issue:`148`) Named thread variants of the ``withAsync`` functions:

    * ``Control.Concurrent.Classy.Async.withAsyncN``
    * ``Control.Concurrent.Classy.Async.withAsyncOnN``
    * ``Control.Concurrent.Classy.Async.withAsyncWithUnmaskN``
    * ``Control.Concurrent.Classy.Async.withAsyncOnWithUnmaskN``


1.2.2.0 (2017-11-05)
--------------------

* Git: :tag:`concurrency-1.2.2.0`
* Hackage: :hackage:`concurrency-1.2.2.0`

Added
~~~~~

* (:issue:`144`) ``IsConc`` and ``IsSTM`` wrapper types:

    * ``Control.Monad.Conc.Class.IsConc`` (constructor unexported)
    * ``Control.Monad.Conc.Class.toIsConc``
    * ``Control.Monad.Conc.Class.fromIsConc``
    * ``Control.Monad.STM.Class.IsSTM`` (constructor unexported)
    * ``Control.Monad.STM.Class.toIsSTM``
    * ``Control.Monad.STM.Class.fromIsSTM``

Changed
~~~~~~~

* ``Control.Monad.Conc.Class.modifyCRefCAS_`` for transformer
  instances delegates to the underlying monad, rather than using the
  default definition in terms of ``modifyCRefCAS``.


1.2.1.2 (2017-10-14)
--------------------

* Git: :tag:`concurrency-1.2.1.2`
* Hackage: :hackage:`concurrency-1.2.1.2`

Fixed
~~~~~

* (:issue:`134`) ``Control.Monad.Conc.Class.forkWithUnmask`` and
  ``forkOnWithUnmask`` for the ``IO`` instance does not infinitely
  loop (bug introduced in :tag:`concurrency-1.2.1.1`).


1.2.1.1 (2017-10-11)
--------------------

* Git: :tag:`concurrency-1.2.1.1`
* Hackage: :hackage:`concurrency-1.2.1.1`

Changed
~~~~~~~

* Named threads for ``IO`` are implemented with
  ``GHC.Conc.labelThread``.


1.2.1.0 (2017-10-02)
--------------------

* Git: :tag:`concurrency-1.2.1.0`
* Hackage: :hackage:`concurrency-1.2.1.0`

Added
~~~~~

* (:pull:`125`) Named thread variants of the ``async`` functions:

    * ``Control.Concurrent.Classy.Async.asyncN``
    * ``Control.Concurrent.Classy.Async.asyncOnN``
    * ``Control.Concurrent.Classy.Async.asyncWithUnmaskN``
    * ``Control.Concurrent.Classy.Async.asyncOnWithUnmaskN``


1.2.0.0 (2017-09-16)
--------------------

* Git: :tag:`concurrency-1.2.0.0`
* Hackage: :hackage:`concurrency-1.2.0.0`

Changed
~~~~~~~

* ``MonadPlus`` is a superclass of ``MonadSTM``.

* ``Control.Monad.STM.Class.orElse`` is a top-level alias for
  ``mplus``.

* ``Control.Monad.STM.Class.retry`` is a top-level alias for
  ``mzero``.


1.1.2.1 (2017-06-07)
--------------------

* Git: :tag:`concurrency-1.1.2.1`
* Hackage: :hackage:`concurrency-1.1.2.1`

Changed
~~~~~~~

* ``Control.Concurrent.Classy.MVar.isEmptyMVar`` does not briefly
  empties the ``MVar``, and does not block.


1.1.2.0 (2017-04-05)
--------------------

* Git: :tag:`concurrency-1.1.2.0`
* Hackage: :hackage:`concurrency-1.1.2.0`

Added
~~~~~

* Missing functions copied from :hackage:`async`:

    * ``Control.Concurrent.Classy.Async.uninterruptibleCancel``
    * ``Control.Concurrent.Classy.Async.replicateConcurrently``
    * ``Control.Concurrent.Classy.Async.concurrently_``
    * ``Control.Concurrent.Classy.Async.mapConcurrently_``
    * ``Control.Concurrent.Classy.Async.forConcurrently_``
    * ``Control.Concurrent.Classy.Async.replicateConcurrently_``

* ``Control.Concurrent.Classy.Async.Concurrently`` has a ``Semigroup``
  instance when built with :hackage:`base` >= 4.9.

* ``Control.Concurrent.Classy.Async.Concurrently`` has a ``Monoid``
  instance.

* ``Control.Monad.Conc.Class`` re-exports
  ``Control.Monad.Catch.mask_`` and ``uninterruptibleMask_``.

Changed
~~~~~~~

* (:pull:`77`) To match changes in :hackage:`async`,
  ``Control.Concurrent.Classy.Async.cancel`` and ``withAsync`` block
  until the ``Async`` is killed.

Miscellaneous
~~~~~~~~~~~~~

* Every definition, class, and instance now has a Haddock ``@since``
  annotation.


1.1.1.0 - The Async Release (2017-03-04)
----------------------------------------

* Git: :tag:`concurrency-1.1.1.0`
* Hackage: :hackage:`concurrency-1.1.1.0`

Added
~~~~~

* The ``Control.Concurrent.Classy.Async`` module.


1.1.0.0 (2017-02-21)
--------------------

* Git: :tag:`concurrency-1.1.0.0`
* Hackage: :hackage:`concurrency-1.1.0.0`

Added
~~~~~

* ``Control.Monad.Conc.Class.tryReadMVar``

Removed
~~~~~~~

* ``Control.Monad.Conc.Class._concMessage``


1.0.0.0 - The Initial Release (2016-09-10)
------------------------------------------

* Git: :tag:`concurrency-1.0.0.0`
* Hackage: :hackage:`concurrency-1.0.0.0`

Added
~~~~~

* Everything.
