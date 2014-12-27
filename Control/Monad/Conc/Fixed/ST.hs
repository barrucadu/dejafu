{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Concurrent monads with a fixed scheduler which can do ST.
module Control.Monad.Conc.Fixed.ST
  ( -- * The Conc Monad
    Conc
  , Trace
  , ThreadAction(..)
  , runConc
  , runConc'
  , runConcST
  , liftST
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
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.Conc.Fixed.Internal
import Control.Monad.Cont (cont, runCont)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.ST.Class as ST

-- | The @Conc@ monad itself. Under the hood, this uses continuations
-- so it's able to interrupt and resume a monadic computation at any
-- point where a primitive is used.
--
-- This uses the same universally-quantified indexing state trick as
-- used by 'ST' and 'STRef's to prevent mutable references from
-- leaking out of the monad. See 'runConc' for an example of what this
-- means.
newtype Conc t a = C (M (ST t) (STRef t) t a) deriving (Functor, Applicative, Monad)

instance ST.MonadST (Conc t) where
  type World (Conc t) = t
  liftST = liftST

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

fixed :: Fixed Conc (ST t) (STRef t) t
fixed = F
  { newRef    = newSTRef
  , readRef   = readSTRef
  , writeRef  = writeSTRef
  , liftN     = liftST
  , unC       = \(C c) -> c
  }

-- | The concurrent variable type used with the 'Conc'
-- monad. Internally, these are implemented as 'STRef's, but they are
-- structured to behave fairly similarly to 'MVar's. One notable
-- difference is that 'MVar's are single-wakeup, and wake up in a FIFO
-- order. Writing to a @CVar@ wakes up all threads blocked on reading
-- it, and it is up to the scheduler which one runs next. Taking from
-- a @CVar@ behaves analogously.
newtype CVar t a = V { fidget :: R (STRef t) a } deriving Eq

-- | Lift an 'ST' action into the 'Conc' monad.
liftST :: ST t a -> Conc t a
liftST ma = C $ cont lifted where
  lifted c = ALift $ c <$> ma

-- | Run the provided computation concurrently, returning the result.
spawn :: Conc t a -> Conc t (CVar t a)
spawn ma = do
  cvar <- newEmptyCVar
  fork $ ma >>= putCVar cvar
  return cvar

-- | Block on a 'CVar' until it is full, then read from it (without
-- emptying).
readCVar :: CVar t a -> Conc t a
readCVar cvar = C $ cont $ AGet $ fidget cvar

-- | Run the provided computation concurrently.
fork :: Conc t () -> Conc t ()
fork (C ma) = C $ cont $ \c -> AFork (runCont ma $ const AStop) $ c ()

-- | Create a new empty 'CVar'.
newEmptyCVar :: Conc t (CVar t a)
newEmptyCVar = liftST $ do
  stref <- newSTRef (Nothing, [])
  return $ V stref

-- | Block on a 'CVar' until it is empty, then write to it.
putCVar :: CVar t a -> a -> Conc t ()
putCVar cvar a = C $ cont $ \c -> APut (fidget cvar) a $ c ()

-- | Put a value into a 'CVar' if there isn't one, without blocking.
tryPutCVar :: CVar t a -> a -> Conc t Bool
tryPutCVar cvar a = C $ cont $ ATryPut (fidget cvar) a

-- | Block on a 'CVar' until it is full, then read from it (with
-- emptying).
takeCVar :: CVar t a -> Conc t a
takeCVar cvar = C $ cont $ ATake $ fidget cvar

-- | Read a value from a 'CVar' if there is one, without blocking.
tryTakeCVar :: CVar t a -> Conc t (Maybe a)
tryTakeCVar cvar = C $ cont $ ATryTake $ fidget cvar

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning `Just result` if it terminates, and `Nothing` if a
-- deadlock is detected.
--
-- Note how the `t` in 'Conc' is universally quantified, what this
-- means in practice is that you can't do something like this:
--
-- > runConc (\s _ (x:_) -> (x, s)) () $ new >>= return
--
-- So 'CVar's cannot leak out of the 'Conc' computation. If this is
-- making your head hurt, check out the \"How `runST` works\" section
-- of <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
runConc :: Scheduler s -> s -> (forall t. Conc t a) -> Maybe a
runConc sched s ma = let (a,_,_) = runConc' sched s ma in a

-- | Variant of 'runConc' which returns the final state of the
-- scheduler and an execution trace.
runConc' :: Scheduler s -> s -> (forall t. Conc t a) -> (Maybe a, s, Trace)
runConc' sched s ma = runST $ runConcST sched s ma

-- | Variant of 'runConc' keeping the result in ST, allowing for
-- further processing.
runConcST :: Scheduler s -> s -> Conc t a -> ST t (Maybe a, s, Trace)
runConcST = runFixed' fixed
