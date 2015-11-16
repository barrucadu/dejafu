module Cases.SingleThreaded (tests) where

import Control.Exception (ArithException(..), ArrayException(..))
import Test.DejaFu (Failure(..), gives, gives')
import Test.HUnit (Test, test)
import Test.HUnit.DejaFu (testDejafu)

import Control.Concurrent.CVar
import Control.Monad.Conc.Class
import Control.Monad.STM.Class

import Utils

tests :: Test
tests = test
  [ testDejafu emptyCVarTake "emptyCVarTake" $ gives' [True]
  , testDejafu emptyCVarPut  "emptyCVarPut"  $ gives' [()]
  , testDejafu fullCVarPut   "fullCVarPut"   $ gives' [True]
  , testDejafu fullCVarTake  "fullCVarTake"  $ gives' [True]
  , testDejafu fullCVarRead  "fullCVarRead"  $ gives' [True]

  , testDejafu crefRead       "crefRead"       $ gives' [True]
  , testDejafu crefWrite      "crefWrite"      $ gives' [True]
  , testDejafu crefModify     "crefModify"     $ gives' [True]
  , testDejafu crefTicketPeek "crefTicketPeek" $ gives' [True]
  , testDejafu crefCas1       "crefCas1"       $ gives' [(True, True)]
  , testDejafu crefCas2       "crefCas2"       $ gives' [(False, False)]

  , testDejafu stmWrite    "stmWrite"    $ gives' [True]
  , testDejafu stmPreserve "stmPreserve" $ gives' [True]
  , testDejafu stmRetry    "stmRetry"    $ gives  [Left STMDeadlock]
  , testDejafu stmOrElse   "stmOrElse"   $ gives' [True]
  , testDejafu stmCatch1   "stmCatch1"   $ gives' [True]
  , testDejafu stmCatch2   "stmCatch2"   $ gives' [True]

  , testDejafu excCatch    "excCatch"    $ gives' [True]
  , testDejafu excNest     "excNest"     $ gives' [True]
  , testDejafu excEscape   "excEscape"   $ gives  [Left UncaughtException]
  , testDejafu excCatchAll "excCatchAll" $ gives' [(True, True)]
  , testDejafu excSTM      "excSTM"      $ gives' [True]

  ]

--------------------------------------------------------------------------------
-- @CVar@s

-- | An empty @CVar@ cannot be taken from.
emptyCVarTake :: MonadConc m => m Bool
emptyCVarTake = do
  var <- newEmptyCVar
  res <- tryTakeCVar var

  return $ (res :: Maybe ()) == Nothing

-- | An empty @CVar@ can be put into.
emptyCVarPut :: MonadConc m => m ()
emptyCVarPut = do
  var <- newEmptyCVar
  putCVar var ()

-- | A full @CVar@ cannot be put into.
fullCVarPut :: MonadConc m => m Bool
fullCVarPut = do
  var <- newCVar ()
  not <$> tryPutCVar var ()

-- | A full @CVar@ can be taken from.
fullCVarTake :: MonadConc m => m Bool
fullCVarTake = do
  var <- newCVar ()
  (() ==) <$> takeCVar var

-- | A full @CVar@ can be read from.
fullCVarRead :: MonadConc m => m Bool
fullCVarRead = do
  var <- newCVar ()
  (() ==) <$> readCVar var

--------------------------------------------------------------------------------
-- @CRef@s

-- | A @CRef@ can be read from.
crefRead :: MonadConc m => m Bool
crefRead = do
  ref <- newCRef (5::Int)
  (5==) <$> readCRef ref

-- | A @CRef@ can be written to.
crefWrite :: MonadConc m => m Bool
crefWrite = do
  ref <- newCRef (5::Int)
  writeCRef ref 6
  (6==) <$> readCRef ref

-- | A @CRef@ can be modified.
crefModify :: MonadConc m => m Bool
crefModify = do
  ref <- newCRef (5::Int)
  modifyCRef ref (\i -> (i+1, ()))
  (6==) <$> readCRef ref

-- | A @Ticket@ contains the value as of when it was created.
crefTicketPeek :: MonadConc m => m Bool
crefTicketPeek = do
  ref  <- newCRef (5::Int)
  tick <- readForCAS ref
  writeCRef ref 6

  (5==) <$> peekTicket tick

-- | A compare-and-swap can be done on a @CRef@ which hasn't been
-- modified.
crefCas1 :: MonadConc m => m (Bool, Bool)
crefCas1 = do
  ref  <- newCRef (5::Int)
  tick <- readForCAS ref

  (suc, _) <- casCRef ref tick 6
  val <- readCRef ref
  return (suc, 6 == val)

-- | A compare-and-swap cannot be done on a @CRef@ which has been
-- modified.
crefCas2 :: MonadConc m => m (Bool, Bool)
crefCas2 = do
  ref  <- newCRef (5::Int)
  tick <- readForCAS ref
  writeCRef ref 6

  (suc, _) <- casCRef ref tick 7
  val <- readCRef ref
  return (suc, 7 == val)

--------------------------------------------------------------------------------
-- STM

-- | A @CTVar@ can be written to.
stmWrite :: MonadConc m => m Bool
stmWrite =
  (6==) <$> atomically (do { v <- newCTVar (5::Int); writeCTVar v 6; readCTVar v })

-- | A @CTVar@ preserves its value between transactions.
stmPreserve :: MonadConc m => m Bool
stmPreserve = do
  ctv <- atomically $ newCTVar (5::Int)
  (5==) <$> atomically (readCTVar ctv)

-- | A transaction can be aborted, which blocks the thread.
stmRetry :: MonadConc m => m ()
stmRetry = atomically retry

-- | An abort can be caught by an @orElse@.
stmOrElse :: MonadConc m => m Bool
stmOrElse = do
  ctv <- atomically $ newCTVar (5::Int)
  atomically $ orElse retry (writeCTVar ctv 6)

  (6==) <$> atomically (readCTVar ctv)

-- | An exception can be caught by an appropriate handler.
stmCatch1 :: MonadConc m => m Bool
stmCatch1 = do
  ctv <- atomically $ newCTVar (5::Int)
  atomically $ catchArithException
                 (throwSTM Overflow)
                 (\_ -> writeCTVar ctv 6)

  (6==) <$> atomically (readCTVar ctv)

-- | Nested exception handlers can catch different types of exception.
stmCatch2 :: MonadConc m => m Bool 
stmCatch2 = do
  ctv <- atomically $ newCTVar (5::Int)
  atomically $ catchArithException
                 (catchArrayException
                   (throwSTM Overflow)
                   (\_ -> writeCTVar ctv 0))
                 (\_ -> writeCTVar ctv 6)

  (6==) <$> atomically (readCTVar ctv)

--------------------------------------------------------------------------------
-- Exceptions

-- | An exception can be caught by an appropriate handler.
excCatch :: MonadConc m => m Bool
excCatch = catchArithException
  (throw Overflow)
  (\_ -> return True)

-- | Nested exception handlers can catch different types of exception.
excNest :: MonadConc m => m Bool
excNest = catchArithException
  (catchArrayException
    (throw Overflow)
    (\_ -> return False))
  (\_ -> return True)

-- | Exceptions of the wrong type kill the computation
excEscape :: MonadConc m => m ()
excEscape = catchArithException
  (throw $ IndexOutOfBounds "")
  (\_ -> return undefined)

-- | @SomeException@ matches all exception types.
excCatchAll :: MonadConc m => m (Bool, Bool)
excCatchAll = do
  a <- catchSomeException
        (throw Overflow)
        (\_ -> return True)
  b <- catchSomeException
        (throw $ IndexOutOfBounds "")
        (\_ -> return True)

  return (a, b)

-- | Exceptions thrown from STM can be caught.
excSTM :: MonadConc m => m Bool
excSTM = catchArithException
  (atomically $ throwSTM Overflow)
  (\_ -> return True)
