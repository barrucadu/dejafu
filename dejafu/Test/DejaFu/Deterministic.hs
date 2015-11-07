{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
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
  , MemType(..)
  , runConc
  , runConc'

  -- * Execution traces
  , Trace
  , Trace'
  , Decision(..)
  , ThreadAction(..)
  , Lookahead(..)
  , CVarId
  , CRefId
  , MaskingState(..)
  , toTrace
  , showTrace
  , showFail

  -- * Scheduling
  , module Test.DejaFu.Deterministic.Schedule
  ) where

import Control.Exception (MaskingState(..))
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef)
import Test.DejaFu.Deterministic.Internal
import Test.DejaFu.Deterministic.Schedule
import Test.DejaFu.Internal (refST)
import Test.DejaFu.STM (STMST, runTransactionST)
import Test.DejaFu.STM.Internal (CTVar(..))

import qualified Control.Monad.Catch as Ca
import qualified Control.Monad.Conc.Class as C

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use const"    :: String) #-}

-- | The @Conc@ monad itself. This uses the same
-- universally-quantified indexing state trick as used by 'ST' and
-- 'STRef's to prevent mutable references from leaking out of the
-- monad.
newtype Conc t a = C { unC :: M (ST t) (STRef t) (STMST t) a } deriving (Functor, Applicative, Monad)

toConc :: ((a -> Action (ST t) (STRef t) (STMST t)) -> Action (ST t) (STRef t) (STMST t)) -> Conc t a
toConc = C . cont

wrap :: (M (ST t) (STRef t) (STMST t) a -> M (ST t) (STRef t) (STMST t) a) -> Conc t a -> Conc t a
wrap f = C . f . unC

fixed :: Fixed (ST t) (STRef t) (STMST t)
fixed = refST $ \ma -> cont (\c -> ALift $ c <$> ma)

-- | The concurrent variable type used with the 'Conc' monad. One
-- notable difference between these and 'MVar's is that 'MVar's are
-- single-wakeup, and wake up in a FIFO order. Writing to a @CVar@
-- wakes up all threads blocked on reading it, and it is up to the
-- scheduler which one runs next. Taking from a @CVar@ behaves
-- analogously.
newtype CVar t a = Var (V (STRef t) a) deriving Eq

-- | The mutable non-blocking reference type. These are like 'IORef's,
-- but don't have the potential re-ordering problem mentioned in
-- Data.IORef.
newtype CRef t a = Ref (R (STRef t) a) deriving Eq

instance Ca.MonadCatch (Conc t) where
  catch ma h = toConc (ACatching (unC . h) (unC ma))

instance Ca.MonadThrow (Conc t) where
  throwM e = toConc (\_ -> AThrow e)

instance Ca.MonadMask (Conc t) where
  mask                mb = toConc (AMasking MaskedInterruptible   (\f -> unC $ mb $ wrap f))
  uninterruptibleMask mb = toConc (AMasking MaskedUninterruptible (\f -> unC $ mb $ wrap f))

instance C.MonadConc (Conc t) where
  type CVar     (Conc t) = CVar t
  type CRef     (Conc t) = CRef t
  type STMLike  (Conc t) = STMST t
  type ThreadId (Conc t) = Int

  -- ----------

  forkWithUnmask  ma = toConc (AFork (\umask -> runCont (unC $ ma $ wrap umask) (\_ -> AStop)))
  forkOnWithUnmask _ = C.forkWithUnmask

  -- This implementation lies and always returns 2. There is no way to
  -- verify in the computation that this is a lie, and will
  -- potentially avoid special-case behaviour for 1 capability, so it
  -- seems a sane choice.
  getNumCapabilities = return 2

  myThreadId = toConc AMyTId

  yield = toConc (\c -> AYield (c ()))

  -- ----------

  newCRef a = toConc (\c -> ANewRef a (c . Ref))

  readCRef   (Ref ref)   = toConc (AReadRef ref)
  writeCRef  (Ref ref) a = toConc (\c -> AWriteRef ref a (c ()))
  modifyCRef (Ref ref) f = toConc (AModRef ref f)

  -- ----------

  newEmptyCVar = toConc (\c -> ANewVar (c . Var))

  putCVar  (Var var) a = toConc (\c -> APutVar var a (c ()))
  readCVar (Var var)   = toConc (AReadVar var)
  takeCVar (Var var)   = toConc (ATakeVar var)

  tryPutCVar  (Var var) a = toConc (ATryPutVar  var a)
  tryTakeCVar (Var var)   = toConc (ATryTakeVar var)

  -- ----------

  throwTo tid e = toConc (\c -> AThrowTo tid e (c ()))

  -- ----------

  atomically = toConc . AAtom

  -- ----------

  _concKnowsAbout (Left  (Var (cvarid,  _))) = toConc (\c -> AKnowsAbout (Left  cvarid)  (c ()))
  _concKnowsAbout (Right (V   (ctvarid, _))) = toConc (\c -> AKnowsAbout (Right ctvarid) (c ()))

  _concForgets (Left  (Var (cvarid,  _))) = toConc (\c -> AForgets (Left  cvarid)  (c ()))
  _concForgets (Right (V   (ctvarid, _))) = toConc (\c -> AForgets (Right ctvarid) (c ()))

  _concAllKnown = toConc (\c -> AAllKnown (c ()))

-- | Run a concurrent computation with a given 'Scheduler' and initial
-- state, returning a failure reason on error. Also returned is the
-- final state of the scheduler, and an execution trace.
--
-- Note how the @t@ in 'Conc' is universally quantified, what this
-- means in practice is that you can't do something like this:
--
-- > runConc roundRobinSched () newEmptyCVar
--
-- So mutable references cannot leak out of the 'Conc' computation. If
-- this is making your head hurt, check out the \"How @runST@ works\"
-- section of
-- <https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html>
--
-- This uses the 'SequentialConsistency' memory model.
runConc :: Scheduler s -> s -> (forall t. Conc t a) -> (Either Failure a, s, Trace)
runConc sched s ma =
  let (r, s', t') = runConc' sched SequentialConsistency s ma
  in  (r, s', toTrace t')

-- | Variant of 'runConc' which produces a 'Trace''.
runConc' :: Scheduler s -> MemType -> s -> (forall t. Conc t a) -> (Either Failure a, s, Trace')
runConc' sched memtype s ma = runST $ runFixed fixed runTransactionST sched memtype s $ unC ma
