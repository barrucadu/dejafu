{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- |
-- Module      : Test.DejaFu.Deterministic
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeSynonymInstances
--
-- Deterministic traced execution of concurrent computations.
--
-- This works by executing the computation on a single thread, calling
-- out to the supplied scheduler after each step to determine which
-- thread runs next.
module Test.DejaFu.Deterministic
  ( -- * The @Conc@ Monad
    Conc
  , ConcST
  , ConcIO

  -- * Executing computations
  , Failure(..)
  , MemType(..)
  , runConcST
  , runConcIO

  -- * Execution traces
  , Trace
  , Decision(..)
  , ThreadId(..)
  , ThreadAction(..)
  , Lookahead(..)
  , MVarId
  , CRefId
  , MaskingState(..)
  , showTrace
  , showFail

  -- * Scheduling
  , module Test.DPOR.Schedule
  ) where

import Control.Exception (MaskingState(..))
import Control.Monad.ST (ST, runST)
import Data.Dynamic (toDyn)
import Data.IORef (IORef)
import Data.STRef (STRef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.STM (STMLike, STMIO, STMST, runTransactionIO, runTransactionST)
import Test.DejaFu.STM.Internal (TVar(..))
import Test.DPOR.Schedule

import qualified Control.Monad.Base as Ba
import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.Conc.Class as C
import qualified Control.Monad.IO.Class as IO

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use const"    :: String) #-}

newtype Conc n r s a = C { unC :: M n r s a } deriving (Functor, Applicative, Monad)

-- | A 'MonadConc' implementation using @ST@, this should be preferred
-- if you do not need 'liftIO'.
type ConcST t = Conc (ST t) (STRef t) (STMST t)

-- | A 'MonadConc' implementation using @IO@.
type ConcIO = Conc IO IORef STMIO

toConc :: ((a -> Action n r s) -> Action n r s) -> Conc n r s a
toConc = C . cont

wrap :: (M n r s a -> M n r s a) -> Conc n r s a -> Conc n r s a
wrap f = C . f . unC

instance IO.MonadIO ConcIO where
  liftIO ma = toConc (\c -> ALift (fmap c ma))

instance Ba.MonadBase IO ConcIO where
  liftBase = IO.liftIO

instance Ca.MonadCatch (Conc n r s) where
  catch ma h = toConc (ACatching (unC . h) (unC ma))

instance Ca.MonadThrow (Conc n r s) where
  throwM e = toConc (\_ -> AThrow e)

instance Ca.MonadMask (Conc n r s) where
  mask                mb = toConc (AMasking MaskedInterruptible   (\f -> unC $ mb $ wrap f))
  uninterruptibleMask mb = toConc (AMasking MaskedUninterruptible (\f -> unC $ mb $ wrap f))

instance Monad n => C.MonadConc (Conc n r (STMLike n r)) where
  type MVar     (Conc n r (STMLike n r)) = MVar r
  type CRef     (Conc n r (STMLike n r)) = CRef r
  type Ticket   (Conc n r (STMLike n r)) = Ticket
  type STM      (Conc n r (STMLike n r)) = STMLike n r
  type ThreadId (Conc n r (STMLike n r)) = ThreadId

  -- ----------

  forkWithUnmaskN   n ma = toConc (AFork n (\umask -> runCont (unC $ ma $ wrap umask) (\_ -> AStop (pure ()))))
  forkOnWithUnmaskN n _  = C.forkWithUnmaskN n

  -- This implementation lies and returns 2 until a value is set. This
  -- will potentially avoid special-case behaviour for 1 capability,
  -- so it seems a sane choice.
  getNumCapabilities      = toConc AGetNumCapabilities
  setNumCapabilities caps = toConc (\c -> ASetNumCapabilities caps (c ()))

  myThreadId = toConc AMyTId

  yield = toConc (\c -> AYield (c ()))

  -- ----------

  newCRefN n a = toConc (\c -> ANewRef n a c)

  readCRef   ref = toConc (AReadRef    ref)
  readForCAS ref = toConc (AReadRefCas ref)

  peekTicket tick = toConc (APeekTicket tick)

  writeCRef ref      a = toConc (\c -> AWriteRef ref a (c ()))
  casCRef   ref tick a = toConc (ACasRef ref tick a)

  atomicModifyCRef ref f = toConc (AModRef    ref f)
  modifyCRefCAS    ref f = toConc (AModRefCas ref f)

  -- ----------

  newEmptyMVarN n = toConc (\c -> ANewVar n c)

  putMVar  var a = toConc (\c -> APutVar var a (c ()))
  readMVar var   = toConc (AReadVar var)
  takeMVar var   = toConc (ATakeVar var)

  tryPutMVar  var a = toConc (ATryPutVar  var a)
  tryTakeMVar var   = toConc (ATryTakeVar var)

  -- ----------

  throwTo tid e = toConc (\c -> AThrowTo tid e (c ()))

  -- ----------

  atomically = toConc . AAtom

  -- ----------

  _concKnowsAbout (Left  (MVar cvarid  _)) = toConc (\c -> AKnowsAbout (Left  cvarid)  (c ()))
  _concKnowsAbout (Right (TVar (ctvarid, _))) = toConc (\c -> AKnowsAbout (Right ctvarid) (c ()))

  _concForgets (Left  (MVar cvarid  _)) = toConc (\c -> AForgets (Left  cvarid)  (c ()))
  _concForgets (Right (TVar (ctvarid, _))) = toConc (\c -> AForgets (Right ctvarid) (c ()))

  _concAllKnown = toConc (\c -> AAllKnown (c ()))

  _concMessage msg = toConc (\c -> AMessage (toDyn msg) (c ()))

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
--
-- Note how the @t@ in 'Conc' is universally quantified, what this
-- means in practice is that you can't do something like this:
--
-- > runConc roundRobinSched SequentialConsistency () newEmptyMVar
--
-- So mutable references cannot leak out of the 'Conc' computation. If
-- this is making your head hurt, check out the \"How @runST@ works\"
-- section of
-- <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
--
-- __Note:__ In order to prevent computation from hanging, the runtime
-- will assume that a deadlock situation has arisen if the scheduler
-- attempts to (a) schedule a blocked thread, or (b) schedule a
-- nonexistent thread. In either of those cases, the computation will
-- be halted.
runConcST :: Scheduler ThreadId ThreadAction Lookahead s -> MemType -> s -> (forall t. ConcST t a) -> (Either Failure a, s, Trace ThreadId ThreadAction Lookahead)
runConcST sched memtype s ma = runST $ runFixed runTransactionST sched memtype s $ unC ma

-- | Run a concurrent computation in the @IO@ monad with a given
-- 'Scheduler' and initial state, returning a failure reason on
-- error. Also returned is the final state of the scheduler, and an
-- execution trace.
--
-- __Warning:__ Blocking on the action of another thread in 'liftIO'
-- cannot be detected! So if you perform some potentially blocking
-- action in a 'liftIO' the entire collection of threads may deadlock!
-- You should therefore keep @IO@ blocks small, and only perform
-- blocking operations with the supplied primitives, insofar as
-- possible.
--
-- __Note:__ In order to prevent computation from hanging, the runtime
-- will assume that a deadlock situation has arisen if the scheduler
-- attempts to (a) schedule a blocked thread, or (b) schedule a
-- nonexistent thread. In either of those cases, the computation will
-- be halted.
runConcIO :: Scheduler ThreadId ThreadAction Lookahead s -> MemType -> s -> ConcIO a -> IO (Either Failure a, s, Trace ThreadId ThreadAction Lookahead)
runConcIO sched memtype s ma = runFixed runTransactionIO sched memtype s $ unC ma
