{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Deterministic traced execution of concurrent computations which
-- don't do @IO@.
--
-- This works by executing the computation on a single thread, calling
-- out to the supplied scheduler after each step to determine which
-- thread runs next.
module Test.DejaFu.Deterministic
  ( -- * The @Conc@ Monad
    Conc
  , Failure(..)
  , runConc
  , fork
  , myThreadId
  , spawn
  , atomically
  , throw
  , catch

  -- * Communication: CVars
  , CVar
  , newEmptyCVar
  , putCVar
  , tryPutCVar
  , readCVar
  , takeCVar
  , tryTakeCVar

  -- * Testing
  , _concNoTest

  -- * Execution traces
  , Trace
  , Decision(..)
  , ThreadAction(..)
  , CVarId
  , showTrace

  -- * Scheduling
  , module Test.DejaFu.Deterministic.Schedule
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Exception (Exception, SomeException(..))
import Control.Monad.Cont (cont, runCont)
import Control.Monad.ST (ST, runST)
import Control.State (Wrapper(..), refST)
import Data.STRef (STRef, newSTRef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule
import Test.DejaFu.STM (STMLike, runTransactionST)

import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.Conc.Class as C

-- | The @Conc@ monad itself. This uses the same
-- universally-quantified indexing state trick as used by 'ST' and
-- 'STRef's to prevent mutable references from leaking out of the
-- monad.
newtype Conc t a = C { unC :: M (ST t) (STRef t) (STMLike t) a } deriving (Functor, Applicative, Monad)

instance Ca.MonadCatch (Conc t) where
  catch = catch

instance Ca.MonadThrow (Conc t) where
  throwM = throw

instance C.MonadConc (Conc t) where
  type CVar     (Conc t) = CVar t
  type STMLike  (Conc t) = STMLike t (ST t) (STRef t)
  type ThreadId (Conc t) = Int

  fork         = fork
  myThreadId   = myThreadId
  newEmptyCVar = newEmptyCVar
  putCVar      = putCVar
  tryPutCVar   = tryPutCVar
  readCVar     = readCVar
  takeCVar     = takeCVar
  tryTakeCVar  = tryTakeCVar
  atomically   = atomically
  _concNoTest  = _concNoTest

fixed :: Fixed (ST t) (STRef t) (STMLike t)
fixed = Wrapper refST $ \ma -> cont (\c -> ALift $ c <$> ma)

-- | The concurrent variable type used with the 'Conc' monad. One
-- notable difference between these and 'MVar's is that 'MVar's are
-- single-wakeup, and wake up in a FIFO order. Writing to a @CVar@
-- wakes up all threads blocked on reading it, and it is up to the
-- scheduler which one runs next. Taking from a @CVar@ behaves
-- analogously.
newtype CVar t a = V { unV :: R (STRef t) a } deriving Eq

-- | Run the provided computation concurrently, returning the result.
spawn :: Conc t a -> Conc t (CVar t a)
spawn = C.spawn

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> Conc t a
readCVar cvar = C $ cont $ AGet $ unV cvar

-- | Run the provided computation concurrently.
fork :: Conc t () -> Conc t ThreadId
fork (C ma) = C $ cont $ AFork (runCont ma $ const AStop)

-- | Get the 'ThreadId' of the current thread.
myThreadId :: Conc t ThreadId
myThreadId = C $ cont AMyTId

-- | Run the provided 'MonadSTM' transaction atomically. If 'retry' is
-- called, it will be blocked until any of the touched 'CTVar's have
-- been written to.
atomically :: STMLike t (ST t) (STRef t) a -> Conc t a
atomically stm = C $ cont $ AAtom stm

-- | Create a new empty 'CVar'.
newEmptyCVar :: Conc t (CVar t a)
newEmptyCVar = C $ cont lifted where
  lifted c = ANew $ \cvid -> c <$> newEmptyCVar' cvid
  newEmptyCVar' cvid = (\ref -> V (cvid, ref)) <$> newSTRef Nothing

-- | Block on a 'CVar' until it is empty, then write to it.
putCVar :: CVar t a -> a -> Conc t ()
putCVar cvar a = C $ cont $ \c -> APut (unV cvar) a $ c ()

-- | Put a value into a 'CVar' if there isn't one, without blocking.
tryPutCVar :: CVar t a -> a -> Conc t Bool
tryPutCVar cvar a = C $ cont $ ATryPut (unV cvar) a

-- | Block on a 'CVar' until it is full, then read from it (with
-- emptying).
takeCVar :: CVar t a -> Conc t a
takeCVar cvar = C $ cont $ ATake $ unV cvar

-- | Read a value from a 'CVar' if there is one, without blocking.
tryTakeCVar :: CVar t a -> Conc t (Maybe a)
tryTakeCVar cvar = C $ cont $ ATryTake $ unV cvar

-- | Raise an exception in the 'Conc' monad. The exception is raised
-- when the action is run, not when it is applied. It short-citcuits
-- the rest of the computation:
--
-- > throw e >> x == throw e
throw :: Exception e => e -> Conc t a
throw e = C $ cont $ \_ -> AThrow (SomeException e)

-- | Catch an exception raised by 'throw'. This __cannot__ catch
-- errors, such as evaluating 'undefined', or division by zero. If you
-- need that, use Control.Exception.catch and 'ConcIO'.
catch :: Exception e => Conc t a -> (e -> Conc t a) -> Conc t a
catch ma h = C $ cont $ ACatching (unC . h) (unC ma)

-- | Run the argument in one step. If the argument fails, the whole
-- computation will fail.
_concNoTest :: Conc t a -> Conc t a
_concNoTest ma = C $ cont $ \c -> ANoTest (unC ma) c

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
--
-- Note how the @t@ in 'Conc' is universally quantified, what this
-- means in practice is that you can't do something like this:
--
-- > runConc (\s _ (x:_) -> (x, s)) () newEmptyCVar
--
-- So 'CVar's cannot leak out of the 'Conc' computation. If this is
-- making your head hurt, check out the \"How @runST@ works\" section
-- of <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
runConc :: Scheduler s -> s -> (forall t. Conc t a) -> (Either Failure a, s, Trace)
runConc sched s ma = runST $ runFixed fixed runTransactionST sched s $ unC ma
