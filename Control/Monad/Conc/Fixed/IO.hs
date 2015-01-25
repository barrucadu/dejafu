{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}

-- | Concurrent monads with a fixed scheduler which can do IO.
--
-- Caution! Blocking on the action of another thread in 'liftIO'
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep 'IO' blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
module Control.Monad.Conc.Fixed.IO
  ( -- * The Conc Monad
    Conc
  , Trace
  , ThreadAction(..)
  , runConc
  , runConc'
  , liftIO
  , spawn
  , fork

  -- * Communication: CVars
  , CVar
  , newEmptyCVar
  , putCVar
  , tryPutCVar
  , readCVar
  , takeCVar
  , tryTakeCVar

  -- * Schedulers
  , module Control.Monad.Conc.Fixed.Schedulers
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.Conc.Fixed.Internal
import Control.Monad.Conc.Fixed.Schedulers
import Control.Monad.Cont (cont, runCont)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO

-- | The @Conc@ monad itself. This uses the same
-- universally-quantified indexing state trick as used by 'ST' and
-- 'STRef's to prevent mutable references from leaking out of the
-- monad. See 'runConc' for an example of what this means.
newtype Conc t a = C { unC :: M IO IORef a } deriving (Functor, Applicative, Monad)

instance IO.MonadIO (Conc t) where
  liftIO = liftIO

instance C.ConcFuture (CVar t) (Conc t) where
  spawn    = spawn
  readCVar = readCVar

instance C.ConcCVar (CVar t) (Conc t) where
  fork         = fork
  newEmptyCVar = newEmptyCVar
  putCVar      = putCVar
  tryPutCVar   = tryPutCVar
  takeCVar     = takeCVar
  tryTakeCVar  = tryTakeCVar

-- This is horrible, but it makes the types work
fixed :: Fixed Conc IO IORef t
fixed = F
  { newRef    = newIORef
  , readRef   = readIORef
  , writeRef  = writeIORef
  , liftN     = liftIO
  , getCont   = unC
  }

-- | The concurrent variable type used with the 'Conc' monad. One
-- notable difference between these and 'MVar's is that 'MVar's are
-- single-wakeup, and wake up in a FIFO order. Writing to a @CVar@
-- wakes up all threads blocked on reading it, and it is up to the
-- scheduler which one runs next. Taking from a @CVar@ behaves
-- analogously.
newtype CVar t a = V { unV :: R IORef a } deriving Eq

-- | Lift an 'IO' action into the 'Conc' monad.
liftIO :: IO a -> Conc t a
liftIO ma = C $ cont lifted where
  lifted c = ALift $ c <$> ma

-- | Run the provided computation concurrently, returning the result.
spawn :: Conc t a -> Conc t (CVar t a)
spawn = C.defaultSpawn

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> Conc t a
readCVar cvar = C $ cont $ AGet $ unV cvar

-- | Run the provided computation concurrently.
fork :: Conc t () -> Conc t ()
fork (C ma) = C $ cont $ \c -> AFork (runCont ma $ const AStop) $ c ()

-- | Create a new empty 'CVar'.
newEmptyCVar :: Conc t (CVar t a)
newEmptyCVar = C $ cont lifted where
  lifted c = ANew $ c <$> newEmptyCVar'
  newEmptyCVar' = V <$> newIORef (Nothing, [])

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

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning 'Just' if it terminates, and 'Nothing' if a
-- deadlock is detected.
--
-- Note how the @t@ in 'Conc' is universally quantified, what this
-- means in practice is that you can't do something like this:
--
-- > runConc (\s _ (x:_) -> (x, s)) () newEmptyCVar
--
-- So 'CVar's cannot leak out of the 'Conc' computation. If this is
-- making your head hurt, check out the \"How @runST@ works\" section
-- of <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
runConc :: Scheduler s -> s -> (forall t. Conc t a) -> IO (Maybe a)
runConc sched s ma = (\(a,_,_) -> a) <$> runConc' sched s ma

-- | Variant of 'runConc' which returns the final state of the
-- scheduler and an execution trace.
runConc' :: Scheduler s -> s -> (forall t. Conc t a) -> IO (Maybe a, s, Trace)
-- Note: Don't eta-reduce, the forall t messes up type inference.
runConc' sched s ma = runFixed' fixed sched s ma
