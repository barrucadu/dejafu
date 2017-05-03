module Utils where

import Control.Exception (ArithException, ArrayException, SomeException)
import Control.Monad (void)
import qualified Control.Monad.Catch as C
import Control.Monad.Conc.Class (MonadConc, readMVar, spawn)

catchArithException :: C.MonadCatch m => m a -> (ArithException -> m a) -> m a
catchArithException = C.catch

catchArrayException :: C.MonadCatch m => m a -> (ArrayException -> m a) -> m a
catchArrayException = C.catch

catchSomeException :: C.MonadCatch m => m a -> (SomeException -> m a) -> m a
catchSomeException = C.catch

(|||) :: MonadConc m => m a -> m b -> m ()
a ||| b = do
  j <- spawn a
  void b
  void (readMVar j)
