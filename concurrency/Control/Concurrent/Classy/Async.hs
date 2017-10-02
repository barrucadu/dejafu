{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | This module is a version of the
-- <https://hackage.haskell.org/package/async async> package. It
-- provides a set of operations for running @MonadConc@ operations
-- asynchronously and waiting for their results.
--
-- For example, assuming a suitable @getURL@ function, we can fetch
-- the contents of two web pages at the same time:
--
-- > withAsync (getURL url1) $ \a1 -> do
-- > withAsync (getURL url2) $ \a2 -> do
-- > page1 <- wait a1
-- > page2 <- wait a2
-- > ...
--
-- The 'withAsync' function starts an operation in a separate thread,
-- and kills it if the inner action finishes before it completes.
--
-- There are a few deviations from the regular async package:
--
--   * 'asyncBound' and 'withAsyncBound' are missing as @MonadConc@
--   does not support bound threads.
--
--   * The @Alternative@ instance for 'Concurrently' uses @forever
--   yield@ in the definition of @empty@, rather than @forever
--   (threadDelay maxBound)@.
module Control.Concurrent.Classy.Async
  ( -- * Asynchronous actions
    Async

  -- * Spawning
  , async
  , asyncN
  , asyncOn
  , asyncOnN
  , asyncWithUnmask
  , asyncWithUnmaskN
  , asyncOnWithUnmask
  , asyncOnWithUnmaskN

  -- * Spawning with automatic 'cancel'ation
  , withAsync
  , withAsyncOn
  , withAsyncWithUnmask
  , withAsyncOnWithUnmask

  -- * Querying 'Async's
  , wait, waitSTM
  , poll, pollSTM
  , waitCatch, waitCatchSTM
  , cancel
  , uninterruptibleCancel
  , cancelWith
  , asyncThreadId

  -- * Waiting for multiple 'Async's
  , waitAny, waitAnySTM
  , waitAnyCatch, waitAnyCatchSTM
  , waitAnyCancel
  , waitAnyCatchCancel
  , waitEither, waitEitherSTM
  , waitEitherCatch, waitEitherCatchSTM
  , waitEitherCancel
  , waitEitherCatchCancel
  , waitEither_, waitEitherSTM_
  , waitBoth, waitBothSTM

  -- * Linking
  , link
  , link2

  -- * Convenient utilities
  , race
  , race_
  , concurrently, concurrently_
  , mapConcurrently, mapConcurrently_
  , forConcurrently, forConcurrently_
  , replicateConcurrently, replicateConcurrently_
  , Concurrently(..)
  ) where

import           Control.Applicative
import           Control.Concurrent.Classy.STM.TMVar (newEmptyTMVar, putTMVar,
                                                      readTMVar)
import           Control.Exception                   (AsyncException(ThreadKilled),
                                                      BlockedIndefinitelyOnSTM(..),
                                                      Exception, SomeException)
import           Control.Monad
import           Control.Monad.Catch                 (finally, onException, try)
import           Control.Monad.Conc.Class
import           Control.Monad.STM.Class
import           Data.Foldable                       (foldMap)

#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup                      (Semigroup(..))
#endif

-----------------------------------------------------------------------------------------
-- Asynchronous and Concurrent Actions

-- | An asynchronous action spawned by 'async' or
-- 'withAsync'. Asynchronous actions are executed in a separate
-- thread, and operations are provided for waiting for asynchronous
-- actions to complete and obtaining their results (see e.g. 'wait').
--
-- Note that, unlike the \"async\" package, 'Async' here does not have
-- an 'Ord' instance. This is because 'MonadConc' 'ThreadId's do not
-- necessarily have one.
--
-- @since 1.1.1.0
data Async m a = Async
  { asyncThreadId :: !(ThreadId m)
  , _asyncWait :: STM m (Either SomeException a)
  }

-- | @since 1.1.1.0
instance MonadConc m => Eq (Async m a) where
  Async t1 _ == Async t2 _ = t1 == t2

-- | @since 1.1.1.0
instance MonadConc m => Functor (Async m) where
  fmap f (Async t w) = Async t $ fmap f <$> w

-- | A value of type @Concurrently m a@ is a @MonadConc@ operation
-- that can be composed with other @Concurrently@ values, using the
-- @Applicative@ and @Alternative@ instances.
--
-- Calling @runConcurrently@ on a value of type @Concurrently m a@
-- will execute the @MonadConc@ operations it contains concurrently,
-- before delivering the result of type @a@.
--
-- For example
--
-- > (page1, page2, page3)
-- >   <- runConcurrently $ (,,)
-- >   <$> Concurrently (getURL "url1")
-- >   <*> Concurrently (getURL "url2")
-- >   <*> Concurrently (getURL "url3")
--
-- @since 1.1.1.0
newtype Concurrently m a = Concurrently { runConcurrently :: m a }

-- | @since 1.1.1.0
instance MonadConc m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

-- | @since 1.1.1.0
instance MonadConc m => Applicative (Concurrently m) where
  pure = Concurrently . pure

  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

-- | @since 1.1.1.0
instance MonadConc m => Alternative (Concurrently m) where
  empty = Concurrently $ forever yield

  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

#if MIN_VERSION_base(4,9,0)
-- | Only defined for base >= 4.9.0.0
--
-- @since 1.1.2.0
instance (MonadConc m, Semigroup a) => Semigroup (Concurrently m a) where
  (<>) = liftA2 (<>)
#endif

-- | @since 1.1.2.0
instance (MonadConc m, Monoid a) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-------------------------------------------------------------------------------
-- Spawning

-- | Spawn an asynchronous action in a separate thread.
--
-- @since 1.1.1.0
async :: MonadConc m => m a -> m (Async m a)
async = asyncUsing fork

-- | Like 'async', but using a named thread for better debugging information.
--
-- @since 1.2.1.0
asyncN :: MonadConc m => String -> m a -> m (Async m a)
asyncN name = asyncUsing (forkN name)

-- | Like 'async' but using 'forkOn' internally.
--
-- @since 1.1.1.0
asyncOn :: MonadConc m => Int -> m a -> m (Async m a)
asyncOn = asyncUsing . forkOn

-- | Like 'asyncOn' but using a named thread for better debugging information.
--
-- @since 1.2.1.0
asyncOnN :: MonadConc m => String -> Int -> m a -> m (Async m a)
asyncOnN name = asyncUsing . (forkOnN name)

-- | Like 'async' but using 'forkWithUnmask' internally.
--
-- @since 1.1.1.0
asyncWithUnmask :: MonadConc m => ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncWithUnmask = asyncUnmaskUsing forkWithUnmask

-- | Like 'asyncWithUnmask' but using a named thread for better debugging information.
--
-- @since 1.2.1.0
asyncWithUnmaskN :: MonadConc m => String -> ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncWithUnmaskN name = asyncUnmaskUsing (forkWithUnmaskN name)

-- | Like 'asyncOn' but using 'forkOnWithUnmask' internally.
--
-- @since 1.1.1.0
asyncOnWithUnmask :: MonadConc m => Int -> ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncOnWithUnmask i = asyncUnmaskUsing (forkOnWithUnmask i)

-- | Like 'asyncOnWithUnmask' but using a named thread for better debugging information.
--
-- @since 1.2.1.0
asyncOnWithUnmaskN :: MonadConc m => String -> Int -> ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncOnWithUnmaskN name i = asyncUnmaskUsing (forkOnWithUnmaskN name i)

-- | Fork a thread with the given forking function
asyncUsing :: MonadConc m => (m () -> m (ThreadId m)) -> m a -> m (Async m a)
asyncUsing doFork action = do
  var <- atomically newEmptyTMVar
  tid <- mask $ \restore -> doFork $ try (restore action) >>= atomically . putTMVar var
  pure (Async tid (readTMVar var))

-- | Fork a thread with the given forking function and give it an
-- action to unmask exceptions
asyncUnmaskUsing :: MonadConc m => (((forall b. m b -> m b) -> m ()) -> m (ThreadId m)) -> ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncUnmaskUsing doFork action = do
  var <- atomically newEmptyTMVar
  tid <- doFork $ \restore -> try (action restore) >>= atomically . putTMVar var
  pure (Async tid (readTMVar var))

-- | Spawn an asynchronous action in a separate thread, and pass its
-- @Async@ handle to the supplied function. When the function returns
-- or throws an exception, 'uninterruptibleCancel' is called on the @Async@.
--
-- > withAsync action inner = bracket (async action) uninterruptiblCancel inner
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
-- Since 'uninterruptibleCancel' may block, 'withAsync' may also
-- block; see 'uninterruptibleCancel' for details.
--
-- @since 1.1.1.0
withAsync :: MonadConc m => m a -> (Async m a -> m b) -> m b
withAsync = withAsyncUsing fork

-- | Like 'withAsync' but uses 'forkOn' internally.
--
-- @since 1.1.1.0
withAsyncOn :: MonadConc m => Int -> m a -> (Async m a -> m b) -> m b
withAsyncOn = withAsyncUsing . forkOn

-- | Like 'withAsync' bit uses 'forkWithUnmask' internally.
--
-- @since 1.1.1.0
withAsyncWithUnmask :: MonadConc m => ((forall x. m x -> m x) -> m a) -> (Async m a -> m b) -> m b
withAsyncWithUnmask = withAsyncUnmaskUsing forkWithUnmask

-- | Like 'withAsyncOn' bit uses 'forkOnWithUnmask' internally.
--
-- @since 1.1.1.0
withAsyncOnWithUnmask :: MonadConc m => Int -> ((forall x. m x -> m x) -> m a) -> (Async m a -> m b) -> m b
withAsyncOnWithUnmask i = withAsyncUnmaskUsing (forkOnWithUnmask i)

-- | Helper for 'withAsync' and 'withAsyncOn': fork a thread with the
-- given forking function and kill it when the inner action completes.
withAsyncUsing :: MonadConc m => (m () -> m (ThreadId m)) -> m a -> (Async m a -> m b) -> m b
withAsyncUsing doFork action inner = do
  var <- atomically newEmptyTMVar
  tid <- mask $ \restore -> doFork $ try (restore action) >>= atomically . putTMVar var
  withAsyncDo (Async tid (readTMVar var)) inner

-- | Helper for 'withAsyncWithUnmask' and 'withAsyncOnWithUnmask':
-- fork a thread with the given forking function, give it an action to
-- unmask exceptions, and kill it when the inner action completed.
withAsyncUnmaskUsing :: MonadConc m => (((forall x. m x -> m x) -> m ()) -> m (ThreadId m)) -> ((forall x. m x -> m x) -> m a) -> (Async m a -> m b) -> m b
withAsyncUnmaskUsing doFork action inner = do
  var <- atomically newEmptyTMVar
  tid <- doFork $ \restore -> try (action restore) >>= atomically . putTMVar var
  withAsyncDo (Async tid (readTMVar var)) inner

-- | Helper for 'withAsyncUsing' and 'withAsyncUnmaskUsing': run the
-- inner action and kill the async thread when done.
withAsyncDo :: MonadConc m => Async m a -> (Async m a -> m b) -> m b
withAsyncDo a inner = do
  res <- inner a `catchAll` (\e -> uninterruptibleCancel a >> throw e)
  cancel a
  pure res

catchAll :: MonadConc m => m a -> (SomeException -> m a) -> m a
catchAll = catch

-------------------------------------------------------------------------------
-- Querying

-- | Wait for an asynchronous action to complete, and return its
-- value. If the asynchronous value threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
--
-- @since 1.1.1.0
wait :: MonadConc m => Async m a -> m a
wait = atomically . waitSTM

-- | A version of 'wait' that can be used inside a @MonadSTM@ transaction.
--
-- @since 1.1.1.0
waitSTM :: MonadConc m => Async m a -> STM m a
waitSTM a = do
 r <- waitCatchSTM a
 either throwSTM pure r

-- | Check whether an 'Async' has completed yet. If it has not
-- completed yet, then the result is @Nothing@, otherwise the result
-- is @Just e@ where @e@ is @Left x@ if the @Async@ raised an
-- exception @x@, or @Right a@ if it returned a value @a@.
--
-- > poll = atomically . pollSTM
--
-- @since 1.1.1.0
poll :: MonadConc m => Async m a -> m (Maybe (Either SomeException a))
poll = atomically . pollSTM

-- | A version of 'poll' that can be used inside a @MonadSTM@ transaction.
--
-- @since 1.1.1.0
pollSTM :: MonadConc m => Async m a -> STM m (Maybe (Either SomeException a))
pollSTM (Async _ w) = (Just <$> w) `orElse` pure Nothing

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- @since 1.1.1.0
waitCatch :: MonadConc m => Async m a -> m (Either SomeException a)
waitCatch = tryAgain . atomically . waitCatchSTM where
  -- See: https://github.com/simonmar/async/issues/14
  tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f

-- | A version of 'waitCatch' that can be used inside a @MonadSTM@ transaction.
--
-- @since 1.1.1.0
waitCatchSTM :: MonadConc m => Async m a -> STM m (Either SomeException a)
waitCatchSTM (Async _ w) = w

-- | Cancel an asynchronous action by throwing the @ThreadKilled@
-- exception to it, and waiting for the 'Async' thread to quit. Has no
-- effect if the 'Async' has already completed.
--
-- > cancel a = throwTo (asyncThreadId a) ThreadKilled <* waitCatch a
--
-- Note that 'cancel' will not terminate until the thread the 'Async'
-- refers to has terminated. This means that 'cancel' will block for
-- as long as said thread blocks when receiving an asynchronous
-- exception.
--
-- An asynchronous 'cancel' can of course be obtained by wrapping
-- 'cancel' itself in 'async'.
--
-- @since 1.1.1.0
cancel :: MonadConc m => Async m a -> m ()
cancel a@(Async tid _) = throwTo tid ThreadKilled <* waitCatch a

-- | Cancel an asynchronous action.
--
-- This is a variant of 'cancel' but it is not interruptible.
--
-- @since 1.1.2.0
uninterruptibleCancel :: MonadConc m => Async m a -> m ()
uninterruptibleCancel = uninterruptibleMask_ . cancel

-- | Cancel an asynchronous action by throwing the supplied exception
-- to it.
--
-- > cancelWith a e = throwTo (asyncThreadId a) e
--
-- The notes about the synchronous nature of 'cancel' also apply to
-- 'cancelWith'.
--
-- @since 1.1.1.0
cancelWith :: (MonadConc m, Exception e) => Async m a -> e -> m ()
cancelWith (Async tid _) = throwTo tid


-------------------------------------------------------------------------------
-- Waiting for multiple 'Async's

-- | Wait for any of the supplied 'Async's to complete.  If the first
-- to complete throws an exception, then that exception is re-thrown
-- by 'waitAny'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
-- @since 1.1.1.0
waitAny :: MonadConc m => [Async m a] -> m (Async m a, a)
waitAny = atomically . waitAnySTM

-- | A version of 'waitAny' that can be used inside a @MonadSTM@
-- transaction.
--
-- @since 1.1.1.0
waitAnySTM :: MonadConc m => [Async m a] -> STM m (Async m a, a)
waitAnySTM = foldr (orElse . (\a -> do r <- waitSTM a; pure (a, r))) retry

-- | Wait for any of the supplied asynchronous operations to complete.
-- The value returned is a pair of the 'Async' that completed, and the
-- result that would be returned by 'wait' on that 'Async'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
-- @since 1.1.1.0
waitAnyCatch :: MonadConc m => [Async m a] -> m (Async m a, Either SomeException a)
waitAnyCatch = atomically . waitAnyCatchSTM

-- | A version of 'waitAnyCatch' that can be used inside a @MonadSTM@
-- transaction.
--
-- @since 1.1.1.0
waitAnyCatchSTM :: MonadConc m => [Async m a] -> STM m (Async m a, Either SomeException a)
waitAnyCatchSTM = foldr (orElse . (\a -> do r <- waitCatchSTM a; pure (a, r))) retry

-- | Like 'waitAny', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
-- @since 1.1.1.0
waitAnyCancel :: MonadConc m => [Async m a] -> m (Async m a, a)
waitAnyCancel asyncs = waitAny asyncs `finally` mapM_ cancel asyncs

-- | Like 'waitAnyCatch', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
-- @since 1.1.1.0
waitAnyCatchCancel :: MonadConc m => [Async m a] -> m (Async m a, Either SomeException a)
waitAnyCatchCancel asyncs = waitAnyCatch asyncs `finally` mapM_ cancel asyncs

-- | Wait for the first of two @Async@s to finish.  If the @Async@
-- that finished first raised an exception, then the exception is
-- re-thrown by 'waitEither'.
--
-- @since 1.1.1.0
waitEither :: MonadConc m => Async m a -> Async m b -> m (Either a b)
waitEither left right = atomically $ waitEitherSTM left right

-- | A version of 'waitEither' that can be used inside a @MonadSTM@
-- transaction.
--
-- @since 1.1.1.0
waitEitherSTM :: MonadConc m => Async m a -> Async m b -> STM m (Either a b)
waitEitherSTM left right =
  (Left <$> waitSTM left) `orElse` (Right <$> waitSTM right)

-- | Wait for the first of two @Async@s to finish.
--
-- @since 1.1.1.0
waitEitherCatch :: MonadConc m => Async m a -> Async m b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch left right = atomically $ waitEitherCatchSTM left right

-- | A version of 'waitEitherCatch' that can be used inside a
-- @MonadSTM@ transaction.
--
-- @since 1.1.1.0
waitEitherCatchSTM :: MonadConc m => Async m a -> Async m b
  -> STM m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchSTM left right =
  (Left <$> waitCatchSTM left) `orElse` (Right <$> waitCatchSTM right)

-- | Like 'waitEither', but also 'cancel's both @Async@s before
-- returning.
--
-- @since 1.1.1.0
waitEitherCancel :: MonadConc m => Async m a -> Async m b -> m (Either a b)
waitEitherCancel left right =
  waitEither left right `finally` (cancel left >> cancel right)

-- | Like 'waitEitherCatch', but also 'cancel's both @Async@s before
-- returning.
--
-- @since 1.1.1.0
waitEitherCatchCancel :: MonadConc m => Async m a -> Async m b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel left right =
  waitEitherCatch left right `finally` (cancel left >> cancel right)

-- | Like 'waitEither', but the result is ignored.
--
-- @since 1.1.1.0
waitEither_ :: MonadConc m => Async m a -> Async m b -> m ()
waitEither_ left right = atomically $ waitEitherSTM_ left right

-- | A version of 'waitEither_' that can be used inside a @MonadSTM@
-- transaction.
--
-- @since 1.1.1.0
waitEitherSTM_:: MonadConc m => Async m a -> Async m b -> STM m ()
waitEitherSTM_ left right = void $ waitEitherSTM left right

-- | Waits for both @Async@s to finish, but if either of them throws
-- an exception before they have both finished, then the exception is
-- re-thrown by 'waitBoth'.
--
-- @since 1.1.1.0
waitBoth :: MonadConc m => Async m a -> Async m b -> m (a, b)
waitBoth left right = atomically $ waitBothSTM left right

-- | A version of 'waitBoth' that can be used inside a @MonadSTM@
-- transaction.
--
-- @since 1.1.1.0
waitBothSTM :: MonadConc m => Async m a -> Async m b -> STM m (a, b)
waitBothSTM left right = do
  a <- waitSTM left `orElse` (waitSTM right >> retry)
  b <- waitSTM right
  pure (a, b)


-------------------------------------------------------------------------------
-- Linking

-- | Link the given @Async@ to the current thread, such that if the
-- @Async@ raises an exception, that exception will be re-thrown in
-- the current thread.
--
-- @since 1.1.1.0
link :: MonadConc m => Async m a -> m ()
link (Async _ w) = do
  me <- myThreadId
  void $ forkRepeat $ do
    r <- atomically w
    case r of
      Left e -> throwTo me e
      _ -> pure ()

-- | Link two @Async@s together, such that if either raises an
-- exception, the same exception is re-thrown in the other @Async@.
--
-- @since 1.1.1.0
link2 :: MonadConc m => Async m a -> Async m b -> m ()
link2 left@(Async tl _)  right@(Async tr _) =
  void $ forkRepeat $ do
    r <- waitEitherCatch left right
    case r of
      Left  (Left e) -> throwTo tr e
      Right (Left e) -> throwTo tl e
      _ -> pure ()

-- | Fork a thread that runs the supplied action, and if it raises an
-- exception, re-runs the action.  The thread terminates only when the
-- action runs to completion without raising an exception.
forkRepeat :: MonadConc m => m a -> m (ThreadId m)
forkRepeat action = mask $ \restore ->
  let go = do
        r <- (try :: MonadConc m => m a -> m (Either SomeException a)) $ restore action
        case r of
          Left _ -> go
          _      -> pure ()
  in fork go


-------------------------------------------------------------------------------
-- Convenient Utilities

-- | Run two @MonadConc@ actions concurrently, and return the first to
-- finish. The loser of the race is 'cancel'led.
--
-- > race left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitEither a b
--
-- @since 1.1.1.0
race :: MonadConc m => m a -> m b -> m (Either a b)
race left right = concurrently' left right collect where
  collect m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right r -> pure r

-- | Like 'race', but the result is ignored.
--
-- > race_ left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitEither_ a b
--
-- @since 1.1.1.0
race_ :: MonadConc m => m a -> m b -> m ()
race_ a b = void $ race a b

-- | Run two @MonadConc@ actions concurrently, and return both
-- results. If either action throws an exception at any time, then the
-- other action is 'cancel'led, and the exception is re-thrown by
-- 'concurrently'.
--
-- > concurrently left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitBoth a b
--
-- @since 1.1.1.0
concurrently :: MonadConc m => m a -> m b -> m (a, b)
concurrently left right = concurrently' left right (collect []) where
  collect [Left a, Right b] _ = pure (a, b)
  collect [Right b, Left a] _ = pure (a, b)
  collect xs m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right r -> collect (r:xs) m

-- | 'concurrently_' is 'concurrently' but ignores the return values.
--
-- @since 1.1.2.0
concurrently_ :: MonadConc m => m a -> m b -> m ()
concurrently_ left right = concurrently' left right (collect 0) where
  collect 2 _ = pure ()
  collect i m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right _ -> collect (i+1::Int) m

-- Run two things concurrently. Faster than the 'Async' version.
concurrently' :: MonadConc m => m a -> m b
  -> (MVar m (Either SomeException (Either a b)) -> m r)
  -> m r
concurrently' left right collect = do
  done <- newEmptyMVar
  mask $ \restore -> do
    lid <- fork $ restore (left >>= putMVar done . Right . Left)
          `catch` (putMVar done . Left)

    rid <- fork $ restore (right >>= putMVar done . Right . Right)
          `catch` (putMVar done . Left)

    -- See: https://github.com/simonmar/async/issues/27
    let stop = killThread rid >> killThread lid

    r <- restore (collect done) `onException` stop

    stop

    pure r

-- | Maps a @MonadConc@-performing function over any @Traversable@
-- data type, performing all the @MonadConc@ actions concurrently, and
-- returning the original data structure with the arguments replaced
-- by the results.
--
-- For example, @mapConcurrently@ works with lists:
--
-- > pages <- mapConcurrently getURL ["url1", "url2", "url3"]
--
-- @since 1.1.1.0
mapConcurrently :: (Traversable t, MonadConc m) => (a -> m b) -> t a -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- | `forConcurrently` is `mapConcurrently` with its arguments flipped
--
-- > pages <- forConcurrently ["url1", "url2", "url3"] $ \url -> getURL url
--
-- @since 1.1.1.0
forConcurrently :: (Traversable t, MonadConc m) => t a -> (a -> m b)-> m (t b)
forConcurrently = flip mapConcurrently

-- | 'mapConcurrently_' is 'mapConcurrently' with the return value
-- discarded, just like 'mapM_'.
--
-- @since 1.1.2.0
mapConcurrently_ :: (Foldable f, MonadConc m) => (a -> m b) -> f a -> m ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)

-- | 'forConcurrently_' is 'forConcurrently' with the return value
-- discarded, just like 'forM_'.
--
-- @since 1.1.2.0
forConcurrently_ :: (Foldable f, MonadConc m) => f a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_

-- | Perform the action in the given number of threads.
--
-- @since 1.1.2.0
replicateConcurrently :: MonadConc m => Int -> m a -> m [a]
replicateConcurrently i = runConcurrently . sequenceA . replicate i . Concurrently

-- | 'replicateConcurrently_' is 'replicateConcurrently' with the
-- return values discarded.
--
-- @since 1.1.2.0
replicateConcurrently_ :: MonadConc m => Int -> m a -> m ()
replicateConcurrently_ i = void . replicateConcurrently i
