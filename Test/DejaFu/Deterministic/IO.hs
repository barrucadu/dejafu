{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Deterministic traced execution of concurrent computations which
-- may do @IO@.
--
-- __Caution!__ Blocking on the action of another thread in 'liftIO'
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep @IO@ blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
module Test.DejaFu.Deterministic.IO
  ( -- * The @ConcIO@ Monad
    ConcIO
  , Failure(..)
  , runConcIO
  , liftIO
  , fork
  , spawn
  , atomically

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
import Control.Monad.Cont (cont, runCont)
import Control.State (Wrapper(..), refIO)
import Data.IORef (IORef, newIORef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule
import Test.DejaFu.STM (STMLike, runTransactionIO)

import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO

-- | The 'IO' variant of Test.DejaFu.Deterministic's @Conc@ monad.
newtype ConcIO t a = C { unC :: M IO IORef (STMLike t) a } deriving (Functor, Applicative, Monad)

instance IO.MonadIO (ConcIO t) where
  liftIO = liftIO

instance C.MonadConc (ConcIO t) where
  type CVar    (ConcIO t) = CVar t
  type STMLike (ConcIO t) = STMLike t IO IORef

  fork         = fork
  newEmptyCVar = newEmptyCVar
  putCVar      = putCVar
  tryPutCVar   = tryPutCVar
  readCVar     = readCVar
  takeCVar     = takeCVar
  tryTakeCVar  = tryTakeCVar
  atomically   = atomically
  _concNoTest  = _concNoTest

fixed :: Fixed IO IORef (STMLike t)
fixed = Wrapper refIO $ unC . liftIO

-- | The concurrent variable type used with the 'ConcIO' monad. These
-- behave the same as @Conc@'s @CVar@s
newtype CVar t a = V { unV :: R IORef a } deriving Eq

-- | Lift an 'IO' action into the 'ConcIO' monad.
liftIO :: IO a -> ConcIO t a
liftIO ma = C $ cont lifted where
  lifted c = ALift $ c <$> ma

-- | Run the provided computation concurrently, returning the result.
spawn :: ConcIO t a -> ConcIO t (CVar t a)
spawn = C.spawn

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> ConcIO t a
readCVar cvar = C $ cont $ AGet $ unV cvar

-- | Run the provided computation concurrently.
fork :: ConcIO t () -> ConcIO t ()
fork (C ma) = C $ cont $ \c -> AFork (runCont ma $ const AStop) $ c ()

-- | Run the provided 'MonadSTM' transaction atomically. If 'retry' is
-- called, it will be blocked until any of the touched 'CTVar's have
-- been written to.
atomically :: STMLike t IO IORef a -> ConcIO t a
atomically stm = C $ cont $ AAtom stm

-- | Create a new empty 'CVar'.
newEmptyCVar :: ConcIO t (CVar t a)
newEmptyCVar = C $ cont lifted where
  lifted c = ANew $ \cvid -> c <$> newEmptyCVar' cvid
  newEmptyCVar' cvid = (\ref -> V (cvid, ref)) <$> newIORef Nothing

-- | Block on a 'CVar' until it is empty, then write to it.
putCVar :: CVar t a -> a -> ConcIO t ()
putCVar cvar a = C $ cont $ \c -> APut (unV cvar) a $ c ()

-- | Put a value into a 'CVar' if there isn't one, without blocking.
tryPutCVar :: CVar t a -> a -> ConcIO t Bool
tryPutCVar cvar a = C $ cont $ ATryPut (unV cvar) a

-- | Block on a 'CVar' until it is full, then read from it (with
-- emptying).
takeCVar :: CVar t a -> ConcIO t a
takeCVar cvar = C $ cont $ ATake $ unV cvar

-- | Read a value from a 'CVar' if there is one, without blocking.
tryTakeCVar :: CVar t a -> ConcIO t (Maybe a)
tryTakeCVar cvar = C $ cont $ ATryTake $ unV cvar

-- | Run the argument in one step. If the argument fails, the whole
-- computation will fail.
_concNoTest :: ConcIO t a -> ConcIO t a
_concNoTest ma = C $ cont $ \c -> ANoTest (unC ma) c

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning an failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
runConcIO :: Scheduler s -> s -> (forall t. ConcIO t a) -> IO (Either Failure a, s, Trace)
runConcIO sched s ma = runFixed fixed runTransactionIO sched s $ unC ma
