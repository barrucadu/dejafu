{-# LANGUAGE CPP        #-}
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
  , asyncOn
  , asyncWithUnmask
  , asyncOnWithUnmask

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
  , concurrently
  , mapConcurrently
  , forConcurrently
  , Concurrently(..)
  ) where

import Control.Applicative
import Control.Concurrent.Classy.STM.TMVar (newEmptyTMVar, putTMVar, readTMVar)
import Control.Exception (AsyncException(ThreadKilled), BlockedIndefinitelyOnSTM(..), Exception, SomeException)
import Control.Monad
import Control.Monad.Catch (finally, try, onException)
import Control.Monad.Conc.Class
import Control.Monad.STM.Class

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable
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
data Async m a = Async
  { asyncThreadId :: !(ThreadId m)
  , _asyncWait :: STM m (Either SomeException a)
  }

instance MonadConc m => Eq (Async m a) where
  Async t1 _ == Async t2 _ = t1 == t2

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
newtype Concurrently m a = Concurrently { runConcurrently :: m a }

instance MonadConc m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance MonadConc m => Applicative (Concurrently m) where
  pure = Concurrently . return

  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

instance MonadConc m => Alternative (Concurrently m) where
  empty = Concurrently $ forever yield

  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs


-------------------------------------------------------------------------------
-- Spawning

-- | Spawn an asynchronous action in a separate thread.
async :: MonadConc m => m a -> m (Async m a)
async = asyncUsing fork

-- | Like 'async' but using 'forkOn' internally.
asyncOn :: MonadConc m => Int -> m a -> m (Async m a)
asyncOn = asyncUsing . forkOn

-- | Like 'async' but using 'forkWithUnmask' internally.
asyncWithUnmask :: MonadConc m => ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncWithUnmask = asyncUnmaskUsing forkWithUnmask

-- | Like 'asyncOn' but using 'forkOnWithUnmask' internally.
asyncOnWithUnmask :: MonadConc m => Int -> ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncOnWithUnmask i = asyncUnmaskUsing (forkOnWithUnmask i)

-- | Fork a thread with the given forking function
asyncUsing :: MonadConc m => (m () -> m (ThreadId m)) -> m a -> m (Async m a)
asyncUsing doFork action = do
  var <- atomically newEmptyTMVar
  tid <- mask $ \restore -> doFork $ try (restore action) >>= atomically . putTMVar var

  return $ Async tid (readTMVar var)

-- | Fork a thread with the given forking function and give it an
-- action to unmask exceptions
asyncUnmaskUsing :: MonadConc m => (((forall b. m b -> m b) -> m ()) -> m (ThreadId m)) -> ((forall b. m b -> m b) -> m a) -> m (Async m a)
asyncUnmaskUsing doFork action = do
  var <- atomically newEmptyTMVar
  tid <- doFork $ \restore -> try (action restore) >>= atomically . putTMVar var

  return $ Async tid (readTMVar var)

-- | Spawn an asynchronous action in a separate thread, and pass its
-- @Async@ handle to the supplied function. When the function returns
-- or throws an exception, 'cancel' is called on the @Async@.
--
-- > withAsync action inner = bracket (async action) cancel inner
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
-- Since 'cancel' may block, 'withAsync' may also block; see 'cancel'
-- for details.
withAsync :: MonadConc m => m a -> (Async m a -> m b) -> m b
withAsync = withAsyncUsing fork

-- | Like 'withAsync' but uses 'forkOn' internally.
withAsyncOn :: MonadConc m => Int -> m a -> (Async m a -> m b) -> m b
withAsyncOn = withAsyncUsing . forkOn

-- | Like 'withAsync' bit uses 'forkWithUnmask' internally.
withAsyncWithUnmask :: MonadConc m => ((forall x. m x -> m x) -> m a) -> (Async m a -> m b) -> m b
withAsyncWithUnmask = withAsyncUnmaskUsing forkWithUnmask

-- | Like 'withAsyncOn' bit uses 'forkOnWithUnmask' internally.
withAsyncOnWithUnmask :: MonadConc m => Int -> ((forall x. m x -> m x) -> m a) -> (Async m a -> m b) -> m b
withAsyncOnWithUnmask i = withAsyncUnmaskUsing (forkOnWithUnmask i)

-- | Fork a thread with the given forking function and kill it when
-- the inner action completes.
--
-- The 'bracket' version appears to hang, even with just IO stuff and
-- using the normal async package. Curious.
withAsyncUsing :: MonadConc m => (m () -> m (ThreadId m)) -> m a -> (Async m a -> m b) -> m b
withAsyncUsing doFork action inner = do
  var <- atomically newEmptyTMVar
  tid <- mask $ \restore -> doFork $ try (restore action) >>= atomically . putTMVar var

  let a = Async tid (readTMVar var)

  res <- inner a `catchAll` (\e -> cancel a >> throw e)
  cancel a

  return res

-- | Fork a thread with the given forking function, give it an action
-- to unmask exceptions, and kill it when the inner action completed.
withAsyncUnmaskUsing :: MonadConc m => (((forall x. m x -> m x) -> m ()) -> m (ThreadId m)) -> ((forall x. m x -> m x) -> m a) -> (Async m a -> m b) -> m b
withAsyncUnmaskUsing doFork action inner = do
  var <- atomically newEmptyTMVar
  tid <- doFork $ \restore -> try (action restore) >>= atomically . putTMVar var

  let a = Async tid (readTMVar var)

  res <- inner a `catchAll` (\e -> cancel a >> throw e)
  cancel a

  return res

catchAll :: MonadConc m => m a -> (SomeException -> m a) -> m a
catchAll = catch

-------------------------------------------------------------------------------
-- Querying

-- | Wait for an asynchronous action to complete, and return its
-- value. If the asynchronous value threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
wait :: MonadConc m => Async m a -> m a
wait = atomically . waitSTM

-- | A version of 'wait' that can be used inside a @MonadSTM@ transaction.
waitSTM :: MonadConc m => Async m a -> STM m a
waitSTM a = do
 r <- waitCatchSTM a
 either throwSTM return r

-- | Check whether an 'Async' has completed yet. If it has not
-- completed yet, then the result is @Nothing@, otherwise the result
-- is @Just e@ where @e@ is @Left x@ if the @Async@ raised an
-- exception @x@, or @Right a@ if it returned a value @a@.
--
-- > poll = atomically . pollSTM
poll :: MonadConc m => Async m a -> m (Maybe (Either SomeException a))
poll = atomically . pollSTM

-- | A version of 'poll' that can be used inside a @MonadSTM@ transaction.
pollSTM :: MonadConc m => Async m a -> STM m (Maybe (Either SomeException a))
pollSTM (Async _ w) = (Just <$> w) `orElse` return Nothing

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
waitCatch :: MonadConc m => Async m a -> m (Either SomeException a)
waitCatch = tryAgain . atomically . waitCatchSTM where
  -- See: https://github.com/simonmar/async/issues/14
  tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f

-- | A version of 'waitCatch' that can be used inside a @MonadSTM@ transaction.
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
cancel :: MonadConc m => Async m a -> m ()
cancel a@(Async tid _) = throwTo tid ThreadKilled <* waitCatch a

-- | Cancel an asynchronous action by throwing the supplied exception
-- to it.
--
-- > cancelWith a e = throwTo (asyncThreadId a) e
--
-- The notes about the synchronous nature of 'cancel' also apply to
-- 'cancelWith'.
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
waitAny :: MonadConc m => [Async m a] -> m (Async m a, a)
waitAny = atomically . waitAnySTM

-- | A version of 'waitAny' that can be used inside a @MonadSTM@
-- transaction.
waitAnySTM :: MonadConc m => [Async m a] -> STM m (Async m a, a)
waitAnySTM = foldr (orElse . (\a -> do r <- waitSTM a; return (a, r))) retry

-- | Wait for any of the supplied asynchronous operations to complete.
-- The value returned is a pair of the 'Async' that completed, and the
-- result that would be returned by 'wait' on that 'Async'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
waitAnyCatch :: MonadConc m => [Async m a] -> m (Async m a, Either SomeException a)
waitAnyCatch = atomically . waitAnyCatchSTM

-- | A version of 'waitAnyCatch' that can be used inside a @MonadSTM@
-- transaction.
waitAnyCatchSTM :: MonadConc m => [Async m a] -> STM m (Async m a, Either SomeException a)
waitAnyCatchSTM = foldr (orElse . (\a -> do r <- waitCatchSTM a; return (a, r))) retry

-- | Like 'waitAny', but also cancels the other asynchronous
-- operations as soon as one has completed.
waitAnyCancel :: MonadConc m => [Async m a] -> m (Async m a, a)
waitAnyCancel asyncs = waitAny asyncs `finally` mapM_ cancel asyncs

-- | Like 'waitAnyCatch', but also cancels the other asynchronous
-- operations as soon as one has completed.
waitAnyCatchCancel :: MonadConc m => [Async m a] -> m (Async m a, Either SomeException a)
waitAnyCatchCancel asyncs = waitAnyCatch asyncs `finally` mapM_ cancel asyncs

-- | Wait for the first of two @Async@s to finish.  If the @Async@
-- that finished first raised an exception, then the exception is
-- re-thrown by 'waitEither'.
waitEither :: MonadConc m => Async m a -> Async m b -> m (Either a b)
waitEither left right = atomically $ waitEitherSTM left right

-- | A version of 'waitEither' that can be used inside a @MonadSTM@
-- transaction.
waitEitherSTM :: MonadConc m => Async m a -> Async m b -> STM m (Either a b)
waitEitherSTM left right =
  (Left <$> waitSTM left) `orElse` (Right <$> waitSTM right)

-- | Wait for the first of two @Async@s to finish.
waitEitherCatch :: MonadConc m => Async m a -> Async m b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch left right = atomically $ waitEitherCatchSTM left right

-- | A version of 'waitEitherCatch' that can be used inside a
-- @MonadSTM@ transaction.
waitEitherCatchSTM :: MonadConc m => Async m a -> Async m b
  -> STM m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchSTM left right =
  (Left <$> waitCatchSTM left) `orElse` (Right <$> waitCatchSTM right)

-- | Like 'waitEither', but also 'cancel's both @Async@s before
-- returning.
waitEitherCancel :: MonadConc m => Async m a -> Async m b -> m (Either a b)
waitEitherCancel left right =
  waitEither left right `finally` (cancel left >> cancel right)

-- | Like 'waitEitherCatch', but also 'cancel's both @Async@s before
-- returning.
waitEitherCatchCancel :: MonadConc m => Async m a -> Async m b
  -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel left right =
  waitEitherCatch left right `finally` (cancel left >> cancel right)

-- | Like 'waitEither', but the result is ignored.
waitEither_ :: MonadConc m => Async m a -> Async m b -> m ()
waitEither_ left right = atomically $ waitEitherSTM_ left right

-- | A version of 'waitEither_' that can be used inside a @MonadSTM@
-- transaction.
waitEitherSTM_:: MonadConc m => Async m a -> Async m b -> STM m ()
waitEitherSTM_ left right = void $ waitEitherSTM left right

-- | Waits for both @Async@s to finish, but if either of them throws
-- an exception before they have both finished, then the exception is
-- re-thrown by 'waitBoth'.
waitBoth :: MonadConc m => Async m a -> Async m b -> m (a, b)
waitBoth left right = atomically $ waitBothSTM left right

-- | A version of 'waitBoth' that can be used inside a @MonadSTM@
-- transaction.
waitBothSTM :: MonadConc m => Async m a -> Async m b -> STM m (a, b)
waitBothSTM left right = do
  a <- waitSTM left `orElse` (waitSTM right >> retry)
  b <- waitSTM right
  return (a, b)


-------------------------------------------------------------------------------
-- Linking

-- | Link the given @Async@ to the current thread, such that if the
-- @Async@ raises an exception, that exception will be re-thrown in
-- the current thread.
--
link :: MonadConc m => Async m a -> m ()
link (Async _ w) = do
  me <- myThreadId
  void $ forkRepeat $ do
    r <- atomically w
    case r of
      Left e -> throwTo me e
      _ -> return ()

-- | Link two @Async@s together, such that if either raises an
-- exception, the same exception is re-thrown in the other @Async@.
link2 :: MonadConc m => Async m a -> Async m b -> m ()
link2 left@(Async tl _)  right@(Async tr _) =
  void $ forkRepeat $ do
    r <- waitEitherCatch left right
    case r of
      Left  (Left e) -> throwTo tr e
      Right (Left e) -> throwTo tl e
      _ -> return ()

-- | Fork a thread that runs the supplied action, and if it raises an
-- exception, re-runs the action.  The thread terminates only when the
-- action runs to completion without raising an exception.
forkRepeat :: MonadConc m => m a -> m (ThreadId m)
forkRepeat action = mask $ \restore ->
  let go = do
        r <- (try :: MonadConc m => m a -> m (Either SomeException a)) $ restore action
        case r of
          Left _ -> go
          _      -> return ()
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
race :: MonadConc m => m a -> m b -> m (Either a b)
race left right = concurrently' left right collect where
  collect m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right r -> return r

-- | Like 'race', but the result is ignored.
--
-- > race_ left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitEither_ a b
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
concurrently :: MonadConc m => m a -> m b -> m (a, b)
concurrently left right = concurrently' left right (collect []) where
  collect [Left a, Right b] _ = return (a, b)
  collect [Right b, Left a] _ = return (a, b)
  collect xs m = do
    e <- takeMVar m
    case e of
      Left ex -> throw ex
      Right r -> collect (r:xs) m

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

    return r

-- | Maps a @MonadConc@-performing function over any @Traversable@
-- data type, performing all the @MonadConc@ actions concurrently, and
-- returning the original data structure with the arguments replaced
-- by the results.
--
-- For example, @mapConcurrently@ works with lists:
--
-- > pages <- mapConcurrently getURL ["url1", "url2", "url3"]
--
mapConcurrently :: (Traversable t, MonadConc m) => (a -> m b) -> t a -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- | `forConcurrently` is `mapConcurrently` with its arguments flipped
--
-- > pages <- forConcurrently ["url1", "url2", "url3"] $ \url -> getURL url
--
forConcurrently :: (Traversable t, MonadConc m) => t a -> (a -> m b)-> m (t b)
forConcurrently = flip mapConcurrently
