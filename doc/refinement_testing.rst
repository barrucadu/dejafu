Refinement Testing
==================

Déjà Fu also supports a form of property-testing where you can check
things about the side-effects of stateful operations.  For example, we
can assert that ``readMVar`` is equivalent to sequencing ``takeMVar``
and ``putMVar`` like so:

.. code-block:: haskell

  prop_mvar_read_take_put =
    sig readMVar `equivalentTo` sig (\v -> takeMVar v >>= putMVar v)

Given the signature function, ``sig``, defined in the next section.
If we check this, our property fails!

.. code-block:: none

  > check prop_mvar_read_take_put
  *** Failure: (seed Just 0)
      left:  [(Nothing,Just 0)]
      right: [(Nothing,Just 0),(Just Deadlock,Just 0)]
  False

This is because ``readMVar`` is atomic, whereas sequencing
``takeMVar`` with ``putMVar`` is not, and so another thread can
interfere with the ``MVar`` in the middle.  The ``check`` and
``equivalentTo`` functions come from ``Test.DejaFu.Refinement`` (also
re-exported from ``Test.DejaFu``).


Signatures
----------

A signature tells the property-tester something about the state your
operation acts upon, it has a few components:

.. code-block:: haskell

  data Sig s o x = Sig
    { initialise ::      x -> ConcIO s
    , observe    :: s -> x -> ConcIO o
    , interfere  :: s -> x -> ConcIO ()
    , expression :: s      -> ConcIO ()
    }

* ``s`` is the **state type**, it's the thing which your operations
  mutate.  For ``readMVar``, the state is some ``MVar a``.

* ``o`` is the **observation type**, it's some pure (and comparable)
  proxy for a snapshot of your mutable state.  For ``MVar a``, the
  observation is probably a ``Maybe a``.

* ``x`` is the **seed type**, it's some pure value used to construct
  the initial mutable state.  For ``MVar a``, the seed is probably a
  ``Maybe a``.

* ``ConcIO`` is just one of the instances of ``MonadConc`` that Déjà
  Fu defines for testing purposes.  Just write code polymorphic in the
  monad as usual, and all will work.

The ``initialise``, ``observe``, and ``expression`` functions should
be self-explanatory, but the ``interfere`` one may not be.  It's the
job of the ``interfere`` function to change the state in some way;
it's run concurrently with the expression, to simulate the
nondeterministic action of other threads.

Here's a concrete example for our ``MVar`` example:

.. code-block:: haskell

  sig :: (MVar ConcIO Int -> ConcIO a) -> Sig (MVar ConcIO Int) (Maybe Int) (Maybe Int)
  sig e = Sig
  { initialise = maybe newEmptyMVar newMVar
  , observe    = \v _ -> tryTakeMVar v
  , interfere  = \v s -> tryTakeMVar v >> maybe (pure ()) (\x -> void $ tryPutMVar v (x * 1000)) s
  , expression = void . e
  }

The ``observe`` function should be deterministic, but as it is run
after the normal execution ends, it may have side-effects on the
state.  The ``interfere`` function can do just about anything [#]_,
but a poor one may result in the property-checker being unable to
distinguish between atomic and nonatomic expressions.

.. [#] There are probably some concrete rules for a good function, but
       I haven't figured them out yet.


Properties
----------

A property is a pair of signatures linked by one of three provided
functions.  These functions are:

.. csv-table::
   :header: "Function", "Operator", "Checks that..."

   "``equivalentTo``",    "``===``", "... the left and right have exactly the same behaviours"
   "``refines``",         "``=>=``", "... every behaviour of the left is also a behaviour of the right"
   "``strictlyRefines``", "``->-``", "... ``left =>= right`` holds but ``left === right`` does not"

The signatures can have different state types, as long as the seed and
observation types are the same.  This lets you compare different
implementations of the same idea: for example, comparing a concurrent
stack implemented using ``MVar`` with one implemented using ``IORef``.

Properties can have parameters, given in the obvious way:

.. code-block:: haskell

  check $ \a b c -> sig1 ... `op` sig2 ...

Under the hood, seed and parameter values are generated using the
:hackage:`leancheck` package, an enumerative property-based testing
library.  This means that any types you use will need to have a
``Listable`` instance.

You can also think about the three functions in terms of sets of
results, where a result is a ``(Maybe Failure, o)`` value.  A
``Failure`` is something like deadlocking, or being killed by an
exception; ``o`` is the observation type.  An observation is always
made, even if execution of the expression fails.

.. csv-table::
   :header: "Function", "Result-set operation"

   "``refines``",         "For all seed and parameter assignments, subset-or-equal"
   "``strictlyRefines``", "For at least one seed and parameter assignment, proper subset; for all others, subset-or-equal"
   "``equivalentTo``",    "For all seed and parameter assignments, equality"

Finally, there is an ``expectFailure`` function, which inverts the
expected result of a property.

The Déjà Fu testsuite has :github:`a collection of refinement
properties
<blob/2a15549d97c2fa12f5e8b92ab918fdb34da78281/dejafu-tests/Cases/Refinement.hs>`,
which may help you get a feel for this sort of testing.


Using HUnit and Tasty
---------------------

As for unit testing, :hackage:`HUnit` and :hackage:`tasty` integration
is provided for refinement testing in the :hackage:`hunit-dejafu` and
:hackage:`tasty-dejafu` packages.

The ``testProperty`` function is used to check properties.  Our example from the start becomes:

.. code-block:: haskell

  testProperty "Read is equivalent to Take then Put" prop_mvar_read_take_put
