{-# LANGUAGE ImpredicativeTypes #-}

-- | This is a separate module because of the need for
-- ImpredicativeTypes, which breaks things elsewhere in the main
-- SearchParty module.
module Examples.SearchParty.Impredicative where

import Control.Concurrent.Classy.STM.TMVar (TMVar, newTMVar)
import Control.Monad.Conc.Class
import Unsafe.Coerce (unsafeCoerce)

-- | A unit of work in a monad @m@ which will produce a final result
-- of type @a@.
newtype WorkItem m a = WorkItem { unWrap :: forall x. WorkItem' m x a }

instance Functor (WorkItem m) where
  fmap f (WorkItem w) = workItem (_result w) (f . _mapped w) (_killme w)

-- | A unit of work in a monad @m@ producing a result of type @x@,
-- which will then be transformed into a value of type @a@.
data WorkItem' m x a = WorkItem'
  { _result :: TMVar (STM m) (Maybe x)
  -- ^ The future result of the computation.
  , _mapped :: x -> a
  -- ^ Some post-processing to do.
  , _killme :: m ()
  -- ^ Fail the computation, if it's still running.
  }

-- | The possible states that a work item may be in.
data WorkState = StillComputing | HasFailed | HasSucceeded
  deriving (Eq)

-- | Construct a 'WorkItem'.
workItem :: TMVar (STM m) (Maybe x) -> (x -> a) -> m () -> WorkItem m a
workItem res mapp kill = wrap $ WorkItem' res mapp kill where
  -- Really not nice, but I have had difficulty getting GHC to unify
  -- @WorkItem' m x a@ with @forall x. WorkItem' m x a@
  --
  -- This needs ImpredicativeTypes in GHC 7.8.
  wrap :: WorkItem' m x a -> WorkItem m a
  wrap = WorkItem . unsafeCoerce

-- | Construct a 'WorkItem' containing a result.
workItem' :: MonadConc m => Maybe a -> m (WorkItem m a)
workItem' a = (\v -> workItem v id $ pure ()) <$> atomically (newTMVar a)
