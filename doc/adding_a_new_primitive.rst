Adding a New Primitive
======================

Déjà Fu is fairly well written (or so I like to tell myself), so
adding a new primitive doesn't have to be a great undertaking.  Let's
add this function:

.. code-block:: haskell

  -- | Atomically set the value of an @MVar@.
  setMVar :: MonadConc m => MVar m a -> Maybe a -> m ()

Before we get started, take a moment to look at the existing ``MVar``
functions and convince yourself that this really is a new primitive.
Specifically, if the ``MVar`` already contains a value, there's no way
to atomically clear it and put the new value in.  The best we can do
is something like this:

.. code-block:: haskell

  setMVar :: MonadConc m => MVar m a -> Maybe a -> m ()
  setMVar mvar (Just a) = go where
    go = do
      tryTakeMVar mvar
      flag <- tryPutMVar mvar a
      unless flag go
  setMVar mvar Nothing = void (tryTakeMVar mvar)

We can't actually implement this for ``IO``, but as the point of this
exercise is to learn the internals of the dejafu library, this is
fine.

Normally it's a bad idea to add primitives which only work when
testing, as they can't be used in ``IO`` code.


Trace elements
--------------

Every primitive has a corresponding constructor in the
``ThreadAction`` and ``Lookahead`` types, which appear in execution
traces.

These types live in ``Test.DejaFu.Common``:

.. code-block:: haskell

  data ThreadAction =
      Fork ThreadId
    -- ^ Start a new thread.
    | MyThreadId
    -- ^ Get the 'ThreadId' of the current thread.
    | GetNumCapabilities Int
    -- ^ Get the number of Haskell threads that can run simultaneously.
    | SetNumCapabilities Int
    -- ^ Set the number of Haskell threads that can run simultaneously.
    | Yield
    -- ^ Yield the current thread.
    | NewMVar MVarId
    -- ^ Create a new 'MVar'.
    | PutMVar MVarId [ThreadId]
    -- ^ Put into a 'MVar', possibly waking up some threads.
    | BlockedPutMVar MVarId
    -- ^ Get blocked on a put.
    | TryPutMVar MVarId Bool [ThreadId]
    -- ^ Try to put into a 'MVar', possibly waking up some threads.
    | ReadMVar MVarId
    -- ^ Read from a 'MVar'.
    | TryReadMVar MVarId Bool
    -- ^ Try to read from a 'MVar'.
    | BlockedReadMVar MVarId
    -- ^ Get blocked on a read.
    | TakeMVar MVarId [ThreadId]
    -- ^ Take from a 'MVar', possibly waking up some threads.
    | BlockedTakeMVar MVarId
    -- ^ Get blocked on a take.
    | TryTakeMVar MVarId Bool [ThreadId]
    -- ^ Try to take from a 'MVar', possibly waking up some threads.
    | NewCRef CRefId
    -- ^ Create a new 'CRef'.
    | ReadCRef CRefId
    -- ^ Read from a 'CRef'.
    | ReadCRefCas CRefId
    -- ^ Read from a 'CRef' for a future compare-and-swap.
    | ModCRef CRefId
    -- ^ Modify a 'CRef'.
    | ModCRefCas CRefId
    -- ^ Modify a 'CRef' using a compare-and-swap.
    | WriteCRef CRefId
    -- ^ Write to a 'CRef' without synchronising.
    | CasCRef CRefId Bool
    -- ^ Attempt to to a 'CRef' using a compare-and-swap, synchronising
    -- it.
    | CommitCRef ThreadId CRefId
    -- ^ Commit the last write to the given 'CRef' by the given thread,
    -- so that all threads can see the updated value.
    | STM TTrace [ThreadId]
    -- ^ An STM transaction was executed, possibly waking up some
    -- threads.
    | BlockedSTM TTrace
    -- ^ Got blocked in an STM transaction.
    | Catching
    -- ^ Register a new exception handler
    | PopCatching
    -- ^ Pop the innermost exception handler from the stack.
    | Throw
    -- ^ Throw an exception.
    | ThrowTo ThreadId
    -- ^ Throw an exception to a thread.
    | BlockedThrowTo ThreadId
    -- ^ Get blocked on a 'throwTo'.
    | Killed
    -- ^ Killed by an uncaught exception.
    | SetMasking Bool MaskingState
    -- ^ Set the masking state. If 'True', this is being used to set the
    -- masking state to the original state in the argument passed to a
    -- 'mask'ed function.
    | ResetMasking Bool MaskingState
    -- ^ Return to an earlier masking state.  If 'True', this is being
    -- used to return to the state of the masked block in the argument
    -- passed to a 'mask'ed function.
    | LiftIO
    -- ^ Lift an IO action. Note that this can only happen with
    -- 'ConcIO'.
    | Return
    -- ^ A 'return' or 'pure' action was executed.
    | Stop
    -- ^ Cease execution and terminate.
    | Subconcurrency
    -- ^ Start executing an action with @subconcurrency@.
    | StopSubconcurrency
    -- ^ Stop executing an action with @subconcurrency@.
    deriving (Eq, Show)

We can look at the other ``MVar`` actions to get some idea of what to
include.  How about this?

.. code-block:: haskell

  | SetMVar MVarId [ThreadId]
  -- ^ Set the value of an 'MVar', possibly waking up some threads.

We also need a ``Lookahead`` equivalent:

.. code-block:: haskell

  | WillSetMVar MVarId
  -- ^ Will set the value of a 'MVar', possibly waking up some threads.

Both ``ThreadAction`` and ``Lookahead`` have ``NFData`` instances,
don't forget to add the extra cases in those.

The ``rewind`` function converts between ``ThreadAction`` and
``Lookahead`` values, so we need to add a case to that as well:

.. code-block:: haskell

  rewind (SetMVar c _) = Just (WillSetMVar c)

Finally, we need to make sure the systematic testing will treat our
new primitive correctly.  As setting the value of an ``MVar`` may
cause previously blocked threads to be unblocked, it is a *release*
action.  Furthermore, as it writes to an ``MVar`` it is a
*synchronised write*:

.. code-block:: haskell

  willRelease (WillSetMVar _) = True

  ...

  simplifyLookahead (WillSetMVar c) = SynchronisedWrite c

**Summary**:

* Add a new ``ThreadAction`` constructor, and update the ``NFData``
  instance
* Add a new ``Lookahead`` constructor, and update the ``NFData``
  instance
* Add a new case to ``rewind``, connecting the two new values
* If the action can enable threads, add a case to ``willRelease``
* if the action enforces a (partial) memory barrier, add a case to
  ``simplifyLookahead``


Actions
-------

Now jump to the ``Test.DejaFu.Conc.Internal.Common`` module (yes,
another "common").  The ``Action`` type defines the actual primitive
actions which are used to implement all the concurrency primitives.
An ``Action`` value contains the information needed to perform that
action and a continuation to call when it is done:

.. code-block:: haskell

  data Action n r =
      AFork  String ((forall b. M n r b -> M n r b) -> Action n r) (ThreadId -> Action n r)
    | AMyTId (ThreadId -> Action n r)

    | AGetNumCapabilities (Int -> Action n r)
    | ASetNumCapabilities Int (Action n r)

    | forall a. ANewMVar String (MVar r a -> Action n r)
    | forall a. APutMVar     (MVar r a) a (Action n r)
    | forall a. ATryPutMVar  (MVar r a) a (Bool -> Action n r)
    | forall a. AReadMVar    (MVar r a) (a -> Action n r)
    | forall a. ATryReadMVar (MVar r a) (Maybe a -> Action n r)
    | forall a. ATakeMVar    (MVar r a) (a -> Action n r)
    | forall a. ATryTakeMVar (MVar r a) (Maybe a -> Action n r)

    | forall a.   ANewCRef String a (CRef r a -> Action n r)
    | forall a.   AReadCRef    (CRef r a) (a -> Action n r)
    | forall a.   AReadCRefCas (CRef r a) (Ticket a -> Action n r)
    | forall a b. AModCRef     (CRef r a) (a -> (a, b)) (b -> Action n r)
    | forall a b. AModCRefCas  (CRef r a) (a -> (a, b)) (b -> Action n r)
    | forall a.   AWriteCRef   (CRef r a) a (Action n r)
    | forall a.   ACasCRef     (CRef r a) (Ticket a) a ((Bool, Ticket a) -> Action n r)

    | forall e.   Exception e => AThrow e
    | forall e.   Exception e => AThrowTo ThreadId e (Action n r)
    | forall a e. Exception e => ACatching (e -> M n r a) (M n r a) (a -> Action n r)
    | APopCatching (Action n r)
    | forall a. AMasking MaskingState ((forall b. M n r b -> M n r b) -> M n r a) (a -> Action n r)
    | AResetMask Bool Bool MaskingState (Action n r)

    | forall a. AAtom (STMLike n r a) (a -> Action n r)
    | ALift (n (Action n r))
    | AYield  (Action n r)
    | AReturn (Action n r)
    | ACommit ThreadId CRefId
    | AStop (n ())

    | forall a. ASub (M n r a) (Either Failure a -> Action n r)
    | AStopSub (Action n r)

Again we can look at the existing ``MVar`` actions for inspiration.
The ``setMVar`` function will need an action very much like
``APutMVar``, but which takes a ``Maybe`` value instead:

.. code-block:: haskell

  | forall a. ASetMVar (MVar r a) (Maybe a) (Action n r)

The only other thing we need to change in this file is the
``lookahead`` function, which converts between ``Action`` and
``Lookahead`` values:

.. code-block:: haskell

  lookahead' (ASetMVar (MVar c _) _ k) = WillSetMVar c : lookahead' k

**Summary**:

* Add a new ``Action`` constructor
* Add a new case to ``lookahead``, connecting the ``Action`` to its
  ``Lookahead``


Implementation
--------------

Now we have all that we need to implement the behaviour of the action.
Check out the huge ``stepThread`` function in
``Test.DejaFu.Conc.Internal``.  It has one case for every ``Action``
so, you guessed it, we're going to add another case which is similar
to the one for ``APutMVar``.

Here's the solution:

.. code-block:: haskell

  -- atomically set the value of an @MVar@.
  ASetMVar cvar@(MVar cvid ref) ma c -> synchronised $ do
    (_, threads', woken) <- case ma of
      Just a -> do
        writeRef ref Nothing
        putIntoMVar cvar a c tid (cThreads ctx)
      Nothing ->
        tryTakeFromMVar cvar (const c) tid (cThreads ctx)
    simple threads' $ SetMVar cvid woken

Let's break this down a bit.

.. code-block:: haskell

  -- atomically set the value of an @MVar@.
  ASetMVar cvar@(MVar cvid ref) ma c -> synchronised $ do

"cvar" means "concurrent variable", and "cvid" means "concurrent
variable ID", this is a naming convention from the past which I
haven't updated yet.

The tricky bit here is ``synchronised``.  It means that this action
imposes a *memory barrier*: any uncommitted ``CRef`` writes get
flushed when this action is performed.  Pretty much everything other
than a couple of ``CRef`` operations impose a memory barrier.
Incidentally, this is what the ``SynchronisedWrite`` we mentioned
above refers to.

.. code-block:: haskell

    (_, threads', woken) <- case ma of
      Just a -> do
        writeRef ref Nothing
        putIntoMVar cvar a c tid (cThreads ctx)
      Nothing ->
        tryTakeFromMVar cvar (const c) tid (cThreads ctx)

Now we update the value inside the ``MVar``, using the pre-existing
functions to do that.  We have two cases: (1) if we're setting the
value in the ``MVar`` to something new; and (2) if we're unsetting it.

1. In this case, we unconditionally empty the ``MVar``, then we write
   the new value.  As each primitive action is executed atomically,
   this is fine.
2. In this case, we just re-use the ``tryTakeMVar`` logic.

Both ``putIntoMVar`` and ``tryTakeFromMVar`` are implemented in
``Test.DejaFu.Conc.Internal.Memory``, in terms of more general
functions called ``mutMVar`` and ``seeMVar``.  They're pretty short,
so go have a read if you like.  Each takes the ``MVar`` to update, the
continuation to call, the current thread ID, and the collection of
threads (from the global context object).  They return an indicator of
success, an updated collection of threads, and a list of woken
threads.

.. code-block:: haskell

    simple threads' $ SetMVar cvid woken

Finally, we produce a new context by saying that this is a "simple"
action (one which only updates the threads), and giving the
``ThreadAction`` value.  This action also updates the relaxed memory
state, but ``synchronised`` handles that bit.

Our final task is to actually define the ``setMVar`` function, which
I'll put in ``Test.DejaFu.Conc``:

.. code-block:: haskell

  setMVar :: MVar r a -> Maybe a -> ConcT r n ()
  setMVar var a = toConc (\c -> ASetMVar var a (c ()))

And we're done!


Testing
-------

Now we want to make sure it works.  In particular, we want to write a
test which will fail if we use the non-atomic version from the start,
but pass with the atomic version.  I can think of two such tests:

.. code-block:: haskell

  -- | An intermediate state shouldn't be observable
  setMVarIntermediate :: Monad n => ConcT r n Bool
  setMVarIntermediate = do
    v <- newMVar 1
    fork (setMVar v (Just 2))
    isNothing <$> tryReadMVar v

This should never return ``True``.

.. code-block:: haskell

  -- | It should terminate
  setMVarTerminate :: Monad n => ConcT r n Bool
  setMVarTerminate = do
    v <- newMVar 1
    let loop = putMVar v 2 >> loop
    fork loop
    setMVar v (Just 3)

This should always terminate.

Let's just try these in ghci with our new primitive:

.. code-block:: none

  > let way = systematically defaultBounds { boundPreemp = Nothing }

  > resultsSet way defaultMemType setMVarIntermediate
  fromList [Right False]

  > resultsSet way defaultMemType setMVarTerminate
  fromList [Right ()]

We're not using ``defaultWay`` because any pre-emption bound would
prevent an infinite loop caused by thread switching from being
observed.  And now with the non-atomic version:

.. code-block:: none

  > resultsSet way defaultMemType setMVarIntermediate
  fromList [Right False,Right True]

  > resultsSet way defaultMemType setMVarTerminate
  fromList [Left Abort,Right ()]

Great!  Now that wasn't so bad, was it?
