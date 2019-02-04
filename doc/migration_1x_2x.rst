1.x to 2.x
==========

:hackage:`dejafu-2.0.0.0` is a super-major release which breaks
compatibility with :hackage:`dejafu-1.x <dejafu-1.12.0.0>`.

Highlights reel:

* Test cases are written in terms of a new ``Program`` type.
* The ``Failure`` type has been replaced with a ``Condition`` type
  (actually in 1.12).
* Random testing takes an optional length bound.
* Atomically-checked invariants over shared mutable state.

See the changelogs for the full details.


The ``Program`` type
--------------------

The ``ConcT`` type is now an alias for ``Program Basic``.

A ``Program Basic`` has all the instances ``ConcT`` did, defined using
the ``~`` instance trick, so this shouldn't be a breaking change:

.. code-block:: haskell

  instance (pty ~ Basic)            => MonadTrans (Program pty)
  instance (pty ~ Basic)            => MonadCatch (Program pty n)
  instance (pty ~ Basic)            => MonadThrow (Program pty n)
  instance (pty ~ Basic)            => MonadMask  (Program pty n)
  instance (Monad n,   pty ~ Basic, Monad   n) => MonadConc (Program pty n)
  instance (MonadIO n, pty ~ Basic, MonadIO n) => MonadIO   (Program pty n)

The ``dontCheck`` function has been removed in favour of
``withSetup``:

.. code-block:: haskell

  do x <- dontCheck setup
     action x

  -- becomes

  withSetup setup action

The ``subconcurrency`` function has been removed in favour of
``withSetupAndTeardown``:

.. code-block:: haskell

  do x <- setup
     y <- subconcurrency (action x)
     teardown x y

  -- becomes

  withSetupAndTeardown setup teardown action

The ``dontCheck`` and ``subconcurrency`` functions throw runtime
errors if nested.  This is not possible with ``withSetup`` and
``withSetupAndTeardown`` due to their types:

.. code-block:: haskell

  withSetup
    :: Program Basic n x
    -- ^ Setup action
    -> (x -> Program Basic n a)
    -- ^ Main program
    -> Program (WithSetup x) n a

  withSetupAndTeardown
    :: Program Basic n x
    -- ^ Setup action
    -> (x -> Either Condition y -> Program Basic n a)
    -- ^ Teardown action
    -> (x -> Program Basic n y)
    -- ^ Main program
    -> Program (WithSetupAndTeardown x y) n a

Previously, multiple calls to ``subconcurrency`` could be sequenced in
the same test case.  This is not possible using
``withSetupAndTeardown``.  If you rely on this behaviour, please
:issue:`file an issue <>`.


The ``Condition`` type
----------------------

This is a change in :hackage:`dejafu-1.12.0.0`, but the alias
``Failure = Condition`` is removed in :hackage:`dejafu-2.0.0.0`.

* The ``STMDeadlock`` and ``Deadlock`` constructors have been merged.
* Internal errors have been split into the ``Error`` type and are
  raised as exceptions, instead of being returned as conditions.

The name "failure" has been a recurring source of confusion, because
an individual execution can "fail" without the predicate as a whole
failing.  My hope is that the more neutral "condition" will prevent
this confusion.


Deprecated functions
--------------------

All the deprecated special-purpose functions have been removed.  Use
more general ``*WithSettings`` functions instead.


Need help?
----------

* For general help talk to me in IRC (barrucadu in #haskell) or shoot
  me an email (mike@barrucadu.co.uk)
* For bugs, issues, or requests, please :issue:`file an issue <>`.
