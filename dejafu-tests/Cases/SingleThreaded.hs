{-# LANGUAGE CPP #-}

module Cases.SingleThreaded where

import Control.Exception (ArithException(..), ArrayException(..))
import Test.DejaFu (Failure(..), gives, gives')

import Control.Concurrent.Classy
import Test.DejaFu.Conc (subconcurrency)

import Common

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

tests :: [Test]
tests =
  [ testGroup "MVar"
    [ emptyMVarPut
    , emptyMVarTryPut
    , emptyMVarTake
    , emptyMVarTryTake
    , emptyMVarRead
    , emptyMVarTryRead
    , fullMVarPut
    , fullMVarTryPut
    , fullMVarTake
    , fullMVarTryTake
    , fullMVarRead
    , fullMVarTryRead
    ]

  , testGroup "CRef"
    [ crefRead
    , crefWrite
    , crefModify
    , crefTicketPeek
    , crefTicketPeek2
    , crefCas1
    , crefCas2
    ]

  , testGroup "STM"
    [ stmWrite
    , stmPreserve
    , stmRetry
    , stmOrElse
    , stmCatch1
    , stmCatch2
    , stmMFail
    ]

  , testGroup "Exceptions"
    [ excCatch
    , excNest
    , excEscape
    , excCatchAll
    , excSTM
    , excToMain1
    , excToMain2
    , excMFail
    ]

  , testGroup "Capabilities"
    [ capsGet
    , capsSet
    ]

  , testGroup "Subconcurrency"
    [ scDeadlock1
    , scDeadlock2
    , scSuccess
    ]
  ]

--------------------------------------------------------------------------------
-- @MVar@s

emptyMVarTake :: Test
emptyMVarTake = djfu "Taking from an empty MVar blocks" (gives [Left Deadlock]) $ do
  var <- newEmptyMVarInt
  takeMVar var

emptyMVarTryTake :: Test
emptyMVarTryTake = djfu "Non-blockingly taking from an empty MVar gives nothing" (gives' [Nothing]) $ do
  var <- newEmptyMVarInt
  tryTakeMVar var

emptyMVarPut :: Test
emptyMVarPut = djfu "Putting into an empty MVar updates it" (gives' [True]) $ do
  var <- newEmptyMVarInt
  putMVar var 7
  (==7) <$> readMVar var

emptyMVarTryPut :: Test
emptyMVarTryPut = djfu "Non-blockingly putting into an empty MVar updates it" (gives' [True]) $ do
  var <- newEmptyMVarInt
  _   <- tryPutMVar var 7
  (==7) <$> readMVar var

emptyMVarRead :: Test
emptyMVarRead = djfu "Reading an empty MVar blocks" (gives [Left Deadlock]) $ do
  var <- newEmptyMVarInt
  readMVar var

emptyMVarTryRead :: Test
emptyMVarTryRead = djfu "Non-blockingly reading an empty MVar gives nothing" (gives' [Nothing]) $ do
  var <- newEmptyMVarInt
  tryReadMVar var

fullMVarPut :: Test
fullMVarPut = djfu "Putting into a full MVar blocks" (gives [Left Deadlock]) $ do
  var <- newMVarInt 7
  putMVar var 10

fullMVarTryPut :: Test
fullMVarTryPut = djfu "Non-blockingly putting into a full MVar fails" (gives' [False]) $ do
  var <- newMVarInt 7
  tryPutMVar var 10

fullMVarTake :: Test
fullMVarTake = djfu "Taking from a full MVar works" (gives' [True]) $ do
  var <- newMVarInt 7
  (==7) <$> takeMVar var

fullMVarTryTake :: Test
fullMVarTryTake = djfu "Non-blockingly taking from a full MVar works" (gives' [True]) $ do
  var <- newMVarInt 7
  (==Just 7) <$> tryTakeMVar var

fullMVarRead :: Test
fullMVarRead = djfu "Reading a full MVar works" (gives' [True]) $ do
  var <- newMVarInt 7
  (==7) <$> readMVar var

fullMVarTryRead :: Test
fullMVarTryRead = djfu "Non-blockingly reading a full MVar works" (gives' [True]) $ do
  var <- newMVarInt 7
  (==Just 7) <$> tryReadMVar var

--------------------------------------------------------------------------------
-- @CRef@s

crefRead :: Test
crefRead = djfu "Reading a non-updated CRef gives its initial value" (gives' [True]) $ do
  ref <- newCRefInt 5
  (5==) <$> readCRef ref

crefWrite :: Test
crefWrite = djfu "Reading an updated CRef gives its new value" (gives' [True]) $ do
  ref <- newCRefInt 5
  writeCRef ref 6
  (6==) <$> readCRef ref

crefModify :: Test
crefModify = djfu "Updating a CRef by a function changes its value" (gives' [True]) $ do
  ref <- newCRefInt 5
  atomicModifyCRef ref (\i -> (i+1, ()))
  (6==) <$> readCRef ref

crefTicketPeek :: Test
crefTicketPeek = djfu "A ticket contains the value of the CRef at the time of its creation" (gives' [True]) $ do
  ref  <- newCRefInt 5
  tick <- readForCAS ref
  writeCRef ref 6
  (5==) <$> peekTicket tick

crefTicketPeek2 :: Test
crefTicketPeek2 = djfu "Compare-and-swap returns a ticket containing the new value" (gives' [True]) $ do
  ref  <- newCRefInt 5
  tick <- readForCAS ref
  (_, tick') <- casCRef ref tick 6
  (6==) <$> peekTicket tick'

crefCas1 :: Test
crefCas1 = djfu "Compare-and-swap on an unmodified CRef succeeds" (gives' [True]) $ do
  ref  <- newCRefInt 5
  tick <- readForCAS ref
  (suc, _) <- casCRef ref tick 6
  val <- readCRef ref
  return (suc && (6 == val))

crefCas2 :: Test
crefCas2 = djfu "Compare-and-swap on a modified CRef fails" (gives' [True]) $ do
  ref  <- newCRefInt 5
  tick <- readForCAS ref
  writeCRef ref 6
  (suc, _) <- casCRef ref tick 7
  val <- readCRef ref
  return (not suc && not (7 == val))

--------------------------------------------------------------------------------
-- STM

stmWrite :: Test
stmWrite = djfu "When a TVar is updated in a transaction, its new value is visible later in the transaction" (gives' [True]) $
  (6==) <$> atomically (do { v <- newTVarInt 5; writeTVar v 6; readTVar v })

stmPreserve :: Test
stmPreserve = djfu "When a TVar is updated, its new value is visible in a later transaction" (gives' [True]) $ do
  ctv <- atomically $ newTVarInt 5
  (5==) <$> atomically (readTVar ctv)

stmRetry :: Test
stmRetry = djfu "Aborting a transaction blocks the thread" (gives [Left STMDeadlock]) $
  (atomically retry :: MonadConc m => m ()) -- avoid an ambiguous type

stmOrElse :: Test
stmOrElse = djfu "Aborting a transaction can be caught and recovered from" (gives' [True]) $ do
  ctv <- atomically $ newTVarInt 5
  atomically $ orElse retry (writeTVar ctv 6)
  (6==) <$> atomically (readTVar ctv)

stmCatch1 :: Test
stmCatch1 = djfu "An exception thrown in a transaction can be caught" (gives' [True]) $ do
  ctv <- atomically $ newTVarInt 5
  atomically $ catchArithException
                 (throwSTM Overflow)
                 (\_ -> writeTVar ctv 6)
  (6==) <$> atomically (readTVar ctv)

stmCatch2 :: Test
stmCatch2 = djfu "Nested exception handlers in transactions work" (gives' [True]) $ do
  ctv <- atomically $ newTVarInt 5
  atomically $ catchArithException
                 (catchArrayException
                   (throwSTM Overflow)
                   (\_ -> writeTVar ctv 0))
                 (\_ -> writeTVar ctv 6)
  (6==) <$> atomically (readTVar ctv)

stmMFail :: Test
stmMFail = djfu "MonadSTM is a MonadFail" (gives [Left UncaughtException]) $
  (atomically $ fail "hello world" :: MonadConc m => m ())

--------------------------------------------------------------------------------
-- Exceptions

excCatch :: Test
excCatch = djfu "An exception thrown can be caught" (gives' [True]) $
  catchArithException
    (throw Overflow)
    (\_ -> return True)

excNest :: Test
excNest = djfu "Nested exception handlers work" (gives' [True]) $
  catchArithException
    (catchArrayException
      (throw Overflow)
      (\_ -> return False))
    (\_ -> return True)

excEscape :: Test
excEscape = djfu "Uncaught exceptions kill the computation" (gives [Left UncaughtException]) $
  catchArithException
    (throw $ IndexOutOfBounds "")
    (\_ -> return False)

excCatchAll :: Test
excCatchAll = djfu "SomeException matches all exception types" (gives' [True]) $ do
  a <- catchSomeException
        (throw Overflow)
        (\_ -> return True)
  b <- catchSomeException
        (throw $ IndexOutOfBounds "")
        (\_ -> return True)
  return (a && b)

excSTM :: Test
excSTM = djfu "Exceptions thrown in a transaction can be caught outside it" (gives' [True]) $
  catchArithException
    (atomically $ throwSTM Overflow)
    (\_ -> return True)

excToMain1 :: Test
excToMain1 = djfu "Throwing an unhandled exception to the main thread kills it" (gives [Left UncaughtException]) $ do
  tid <- myThreadId
  throwTo tid Overflow

excToMain2 :: Test
excToMain2 = djfu "Throwing a handled exception to the main thread does not kill it" (gives' [True]) $ do
  tid <- myThreadId
  catchArithException (throwTo tid Overflow >> pure False) (\_ -> pure True)

excMFail :: Test
excMFail = djfu "MonadConc is a MonadFail" (gives [Left UncaughtException]) $
  (fail "hello world" :: MonadConc m => m ())

--------------------------------------------------------------------------------
-- Capabilities

capsGet :: Test
capsGet = djfu "Reading the capabilities twice without update gives the same result" (gives' [True]) $ do
  c1 <- getNumCapabilities
  c2 <- getNumCapabilities
  return (c1 == c2)

capsSet :: Test
capsSet = djfu "Getting the updated capabilities gives the new value" (gives' [True]) $ do
  caps <- getNumCapabilities
  setNumCapabilities (caps + 1)
  (== caps + 1) <$> getNumCapabilities

--------------------------------------------------------------------------------
-- Subconcurrency

scDeadlock1 :: Test
scDeadlock1 = djfu "Failures in subconcurrency can be observed" (gives' [True]) $ do
  x <- subconcurrency (newEmptyMVar >>= readMVar)
  pure (either (==Deadlock) (const False) x)

scDeadlock2 :: Test
scDeadlock2 = djfu "Actions after a failing subconcurrency still happen" (gives' [True]) $ do
  var <- newMVarInt 0
  x <- subconcurrency (putMVar var 1)
  y <- readMVar var
  pure (either (==Deadlock) (const False) x && y == 0)

scSuccess :: Test
scSuccess = djfu "Non-failing subconcurrency returns the final result" (gives' [True]) $ do
  var <- newMVarInt 3
  x <- subconcurrency (takeMVar var)
  pure (either (const False) (==3) x)
