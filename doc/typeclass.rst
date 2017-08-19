Typeclasses
===========

We don't use the regular ``Control.Concurrent`` and
``Control.Exception`` modules, we use typeclass-generalised ones
instead from the `concurrency`_ and `exceptions`_ packages.

.. _concurrency: https://hackage.haskell.org/package/concurrency
.. _exceptions: https://hackage.haskell.org/package/exceptions


Porting guide
-------------

If you want to test some existing code, you'll need to port it to the
appropriate typeclass.  The typeclass is necessary, because we can't
peek inside ``IO`` and ``STM`` values, so we need to able to plug in
an alternative implementation when testing.

Fortunately, this tends to be a fairly mechanical and type-driven
process:

1. Import ``Control.Concurrent.Classy.*`` instead of
   ``Control.Concurrent.*``

2. Import ``Control.Monad.Catch`` instead of ``Control.Exception``

3. Change your monad type:

   * ``IO a`` becomes ``MonadConc m => m a``
   * ``STM a`` becomes ``MonadSTM stm => stm a``

4. Parameterise your state types by the monad:

   * ``TVar`` becomes ``TVar stm``
   * ``MVar`` becomes ``MVar m``
   * ``IORef`` becomes ``CRef m`` [#]_

5. Some functions are renamed:

   * ``*IORef*`` becomes ``*CRef*``
   * ``forkIO*`` becomes ``fork*``
   * ``atomicModifyIORefCAS*`` becomes ``modifyCRefCAS*``

6. Fix the type errors

If you're lucky enough to be starting a new concurrent Haskell
project, you can just program against the ``MonadConc`` interface.

.. [#] I felt that calling it ``IORef`` when there was no I/O involved
        would be confusing, but this was perhaps a mistake.


What if I really need I/O?
--------------------------

You can use ``MonadIO`` and ``liftIO`` with ``MonadConc``, for
instance if you need to talk to a database (or just use some existing
library which needs real I/O).

To test ``IO``-using code, there are some rules you need to follow:

1. Given the same set of scheduling decisions, your ``IO`` code must
   be deterministic [#]_

2. As ``IO`` values can't be broken up into smaller chunks, they
   should be kept small; otherwise dejafu may miss buggy interleavings

3. You absolutely cannot block on the action of another thread inside
   ``IO``, or the test execution will just deadlock.

.. [#] This is only essential if you're using the systematic testing
       (the default).  Nondeterministic ``IO`` won't break the random
       testing, it'll just make things more confusing.
