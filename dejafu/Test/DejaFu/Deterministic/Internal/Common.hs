{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- | Common types and utility functions for deterministic execution of
-- 'MonadConc' implementations.
module Test.DejaFu.Deterministic.Internal.Common where

import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, MaskingState(..))
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.List (sort, nub, intercalate)
import Data.List.Extra
import Test.DejaFu.Internal

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif

--------------------------------------------------------------------------------
-- * The @Conc@ Monad

-- | The underlying monad is based on continuations over 'Action's.
--
-- One might wonder why the return type isn't reflected in 'Action',
-- and a free monad formulation used. This would remove the need for a
-- @Lift@ action as the penultimate action of thread 0 used to
-- communicate back the result, and be more pleasing in a
-- sense. However, this makes the current expression of threads and
-- exception handlers very difficult (perhaps even not possible
-- without significant reworking), so I abandoned the attempt.
newtype M n r s a = M { runM :: (a -> Action n r s) -> Action n r s }

instance Functor (M n r s) where
    fmap f m = M $ \ c -> runM m (c . f)

instance Applicative (M n r s) where
    pure x  = M $ \c -> AReturn $ c x
    f <*> v = M $ \c -> runM f (\g -> runM v (c . g))

instance Monad (M n r s) where
    return  = pure
    m >>= k = M $ \c -> runM m (\x -> runM (k x) c)

-- | The concurrent variable type used with the 'Conc' monad. One
-- notable difference between these and 'MVar's is that 'MVar's are
-- single-wakeup, and wake up in a FIFO order. Writing to a @CVar@
-- wakes up all threads blocked on reading it, and it is up to the
-- scheduler which one runs next. Taking from a @CVar@ behaves
-- analogously.
data CVar r a = CVar
  { _cvarId   :: CVarId
  , _cvarVal  :: r (Maybe a)
  }

-- | The mutable non-blocking reference type. These are like 'IORef's.
--
-- @CRef@s are represented as a unique numeric identifier and a
-- reference containing (a) any thread-local non-synchronised writes
-- (so each thread sees its latest write), (b) a commit count (used in
-- compare-and-swaps), and (c) the current value visible to all
-- threads.
data CRef r a = CRef
  { _crefId   :: CRefId
  , _crefVal  :: r (Map ThreadId a, Integer, a)
  }

-- | The compare-and-swap proof type.
--
-- @Ticket@s are represented as just a wrapper around the identifier
-- of the 'CRef' it came from, the commit count at the time it was
-- produced, and an @a@ value. This doesn't work in the source package
-- (atomic-primops) because of the need to use pointer equality. Here
-- we can just pack extra information into 'CRef' to avoid that need.
data Ticket a = Ticket
  { _ticketCRef   :: CRefId
  , _ticketWrites :: Integer
  , _ticketVal    :: a
  }

-- | Dict of methods for implementations to override.
type Fixed n r s = Ref n r (M n r s)

-- | Construct a continuation-passing operation from a function.
cont :: ((a -> Action n r s) -> Action n r s) -> M n r s a
cont = M

-- | Run a CPS computation with the given final computation.
runCont :: M n r s a -> (a -> Action n r s) -> Action n r s
runCont = runM

--------------------------------------------------------------------------------
-- * Primitive Actions

-- | Scheduling is done in terms of a trace of 'Action's. Blocking can
-- only occur as a result of an action, and they cover (most of) the
-- primitives of the concurrency. 'spawn' is absent as it is
-- implemented in terms of 'newEmptyCVar', 'fork', and 'putCVar'.
data Action n r s =
    AFork  String ((forall b. M n r s b -> M n r s b) -> Action n r s) (ThreadId -> Action n r s)
  | AMyTId (ThreadId -> Action n r s)

  | AGetNumCapabilities (Int -> Action n r s)
  | ASetNumCapabilities Int (Action n r s)

  | forall a. ANewVar String (CVar r a -> Action n r s)
  | forall a. APutVar     (CVar r a) a (Action n r s)
  | forall a. ATryPutVar  (CVar r a) a (Bool -> Action n r s)
  | forall a. AReadVar    (CVar r a) (a -> Action n r s)
  | forall a. ATakeVar    (CVar r a) (a -> Action n r s)
  | forall a. ATryTakeVar (CVar r a) (Maybe a -> Action n r s)

  | forall a.   ANewRef String a (CRef r a -> Action n r s)
  | forall a.   AReadRef    (CRef r a) (a -> Action n r s)
  | forall a.   AReadRefCas (CRef r a) (Ticket a -> Action n r s)
  | forall a.   APeekTicket (Ticket a) (a -> Action n r s)
  | forall a b. AModRef     (CRef r a) (a -> (a, b)) (b -> Action n r s)
  | forall a b. AModRefCas  (CRef r a) (a -> (a, b)) (b -> Action n r s)
  | forall a.   AWriteRef   (CRef r a) a (Action n r s)
  | forall a.   ACasRef     (CRef r a) (Ticket a) a ((Bool, Ticket a) -> Action n r s)

  | forall e.   Exception e => AThrow e
  | forall e.   Exception e => AThrowTo ThreadId e (Action n r s)
  | forall a e. Exception e => ACatching (e -> M n r s a) (M n r s a) (a -> Action n r s)
  | APopCatching (Action n r s)
  | forall a. AMasking MaskingState ((forall b. M n r s b -> M n r s b) -> M n r s a) (a -> Action n r s)
  | AResetMask Bool Bool MaskingState (Action n r s)

  | AKnowsAbout (Either CVarId CTVarId) (Action n r s)
  | AForgets    (Either CVarId CTVarId) (Action n r s)
  | AAllKnown   (Action n r s)

  | forall a. AAtom (s a) (a -> Action n r s)
  | ALift (n (Action n r s))
  | AYield  (Action n r s)
  | AReturn (Action n r s)
  | ACommit ThreadId CRefId
  | AStop

--------------------------------------------------------------------------------
-- * Identifiers

-- | Every live thread has a unique identitifer.
data ThreadId = ThreadId (Maybe String) Int
  deriving Eq

instance Ord ThreadId where
  compare (ThreadId _ i) (ThreadId _ j) = compare i j

instance Show ThreadId where
  show (ThreadId (Just n) _) = n
  show (ThreadId Nothing  i) = show i

instance NFData ThreadId where
  rnf (ThreadId n i) = rnf (n, i)

-- | The ID of the initial thread.
initialThread :: ThreadId
initialThread = ThreadId (Just "main") 0

-- | Every @CVar@ has a unique identifier.
data CVarId = CVarId (Maybe String) Int
  deriving Eq

instance Ord CVarId where
  compare (CVarId _ i) (CVarId _ j) = compare i j

instance Show CVarId where
  show (CVarId (Just n) _) = n
  show (CVarId Nothing  i) = show i

instance NFData CVarId where
  rnf (CVarId n i) = rnf (n, i)

-- | Every @CRef@ has a unique identifier.
data CRefId = CRefId (Maybe String) Int
  deriving Eq

instance Ord CRefId where
  compare (CRefId _ i) (CRefId _ j) = compare i j

instance Show CRefId where
  show (CRefId (Just n) _) = n
  show (CRefId Nothing  i) = show i

instance NFData CRefId where
  rnf (CRefId n i) = rnf (n, i)

-- | Every @CTVar@ has a unique identifier.
data CTVarId = CTVarId (Maybe String) Int
  deriving Eq

instance Ord CTVarId where
  compare (CTVarId _ i) (CTVarId _ j) = compare i j

instance Show CTVarId where
  show (CTVarId (Just n) _) = n
  show (CTVarId Nothing  i) = show i

instance NFData CTVarId where
  rnf (CTVarId n i) = rnf (n, i)

-- | The number of ID parameters was getting a bit unwieldy, so this
-- hides them all away.
data IdSource = Id
  { _nextCRId  :: Int
  , _nextCVId  :: Int
  , _nextCTVId :: Int
  , _nextTId   :: Int
  , _usedCRNames  :: [String]
  , _usedCVNames  :: [String]
  , _usedCTVNames :: [String]
  , _usedTNames   :: [String] }

-- | Get the next free 'CRefId'.
nextCRId :: String -> IdSource -> (IdSource, CRefId)
nextCRId name idsource = (idsource { _nextCRId = newid, _usedCRNames = newlst }, CRefId newname newid) where
  newid  = _nextCRId idsource + 1
  newlst
    | null name = _usedCRNames idsource
    | otherwise = name : _usedCRNames idsource
  newname
    | null name       = Nothing
    | occurrences > 0 = Just (name ++ "-" ++ show occurrences)
    | otherwise       = Just name
  occurrences = length . filter (==name) $ _usedCRNames idsource

-- | Get the next free 'CVarId'.
nextCVId :: String -> IdSource -> (IdSource, CVarId)
nextCVId name idsource = (idsource { _nextCVId = newid, _usedCVNames = newlst }, CVarId newname newid) where
  newid  = _nextCVId idsource + 1
  newlst
    | null name = _usedCVNames idsource
    | otherwise = name : _usedCVNames idsource
  newname
    | null name       = Nothing
    | occurrences > 0 = Just (name ++ "-" ++ show occurrences)
    | otherwise       = Just name
  occurrences = length . filter (==name) $ _usedCVNames idsource

-- | Get the next free 'CTVarId'.
nextCTVId :: String -> IdSource -> (IdSource, CTVarId)
nextCTVId name idsource = (idsource { _nextCTVId = newid, _usedCTVNames = newlst }, CTVarId newname newid) where
  newid  = _nextCTVId idsource + 1
  newlst
    | null name = _usedCTVNames idsource
    | otherwise = name : _usedCTVNames idsource
  newname
    | null name       = Nothing
    | occurrences > 0 = Just (name ++ "-" ++ show occurrences)
    | otherwise       = Just name
  occurrences = length . filter (==name) $ _usedCTVNames idsource

-- | Get the next free 'ThreadId'.
nextTId :: String -> IdSource -> (IdSource, ThreadId)
nextTId name idsource = (idsource { _nextTId = newid, _usedTNames = newlst }, ThreadId newname newid) where
  newid  = _nextTId idsource + 1
  newlst
    | null name = _usedTNames idsource
    | otherwise = name : _usedTNames idsource
  newname
    | null name       = Nothing
    | occurrences > 0 = Just (name ++ "-" ++ show occurrences)
    | otherwise       = Just name
  occurrences = length . filter (==name) $ _usedTNames idsource

-- | The initial ID source.
initialIdSource :: IdSource
initialIdSource = Id 0 0 0 0 [] [] [] []

--------------------------------------------------------------------------------
-- * Scheduling & Traces

-- | A @Scheduler@ maintains some internal state; @s@, takes the trace
-- so far; the 'ThreadId' and 'ThreadAction' of the last thread
-- scheduled (or 'Nothing' if this is the first decision); and the
-- list of runnable threads including a lookahead (as far as can be
-- determined). It produces a 'ThreadId' to schedule, and a new state.
--
-- __Note:__ In order to prevent computation from hanging, the runtime
-- will assume that a deadlock situation has arisen if the scheduler
-- attempts to (a) schedule a blocked thread, or (b) schedule a
-- nonexistent thread. In either of those cases, the computation will
-- be halted.
type Scheduler s = s -> [(Decision, ThreadAction)] -> Maybe (ThreadId, ThreadAction) -> NonEmpty (ThreadId, NonEmpty Lookahead) -> (Maybe ThreadId, s)

-- | One of the outputs of the runner is a @Trace@, which is a log of
-- decisions made, alternative decisions (including what action would
-- have been performed had that decision been taken), and the action a
-- thread took in its step.
type Trace = [(Decision, [(Decision, Lookahead)], ThreadAction)]

-- | Like a 'Trace', but gives more lookahead (where possible) for
-- alternative decisions.
type Trace' = [(Decision, [(Decision, NonEmpty Lookahead)], ThreadAction)]

-- | Throw away information from a 'Trace'' to get just a 'Trace'.
toTrace :: Trace' -> Trace
toTrace = map go where
  go (_, alters, CommitRef t c) = (Commit, goA alters, CommitRef t c)
  go (dec, alters, act) = (dec, goA alters, act)

  goA = map $ \x -> case x of
    (_, WillCommitRef t c:|_) -> (Commit, WillCommitRef t c)
    (d, a:|_) -> (d, a)

-- | Pretty-print a trace, including a key of the thread IDs. Each
-- line of the key is indented by two spaces.
showTrace :: Trace -> String
showTrace trc = intercalate "\n" $ trace "" 0 trc : (map ("  "++) . sort . nub $ mapMaybe toKey trc) where
  trace prefix num ((Start    (ThreadId _ i),_,_):ds) = thread prefix num ++ trace ("S" ++ show i) 1 ds
  trace prefix num ((SwitchTo (ThreadId _ i),_,_):ds) = thread prefix num ++ trace ("P" ++ show i) 1 ds
  trace prefix num ((Continue,_,_):ds) = trace prefix (num + 1) ds
  trace prefix num ((Commit,_,_):ds)   = thread prefix num ++ trace "C" 1 ds
  trace prefix num [] = thread prefix num

  thread prefix num = prefix ++ replicate num '-'

  toKey (Start (ThreadId (Just name) i), _, _) = Just $ show i ++ ": " ++ name
  toKey _ = Nothing

-- | Scheduling decisions are based on the state of the running
-- program, and so we can capture some of that state in recording what
-- specific decision we made.
data Decision =
    Start ThreadId
  -- ^ Start a new thread, because the last was blocked (or it's the
  -- start of computation).
  | Continue
  -- ^ Continue running the last thread for another step.
  | SwitchTo ThreadId
  -- ^ Pre-empt the running thread, and switch to another.
  | Commit
  -- ^ Commit a 'CRef' write action so that every thread can see the
  -- result.
  deriving (Eq, Show)

instance NFData Decision where
  rnf (Start    tid) = rnf tid
  rnf (SwitchTo tid) = rnf tid
  rnf d = d `seq` ()

-- | All the actions that a thread can perform.
data ThreadAction =
    Fork ThreadId
  -- ^ Start a new thread.
  | MyThreadId
  -- ^ Get the 'ThreadId' of the current thread.
  | GetNumCapabilities Int
  -- ^ Get the number of Haskell threads that can run simultaneously.
  | SetNumCapabilities Int
  -- ^ Set the number of Haskell threads that can run simultaneously.
  | Yield
  -- ^ Yield the current thread.
  | NewVar CVarId
  -- ^ Create a new 'CVar'.
  | PutVar CVarId [ThreadId]
  -- ^ Put into a 'CVar', possibly waking up some threads.
  | BlockedPutVar CVarId
  -- ^ Get blocked on a put.
  | TryPutVar CVarId Bool [ThreadId]
  -- ^ Try to put into a 'CVar', possibly waking up some threads.
  | ReadVar CVarId
  -- ^ Read from a 'CVar'.
  | BlockedReadVar CVarId
  -- ^ Get blocked on a read.
  | TakeVar CVarId [ThreadId]
  -- ^ Take from a 'CVar', possibly waking up some threads.
  | BlockedTakeVar CVarId
  -- ^ Get blocked on a take.
  | TryTakeVar CVarId Bool [ThreadId]
  -- ^ Try to take from a 'CVar', possibly waking up some threads.
  | NewRef CRefId
  -- ^ Create a new 'CRef'.
  | ReadRef CRefId
  -- ^ Read from a 'CRef'.
  | ReadRefCas CRefId
  -- ^ Read from a 'CRef' for a future compare-and-swap.
  | PeekTicket CRefId
  -- ^ Extract the value from a 'Ticket'.
  | ModRef CRefId
  -- ^ Modify a 'CRef'.
  | ModRefCas CRefId
  -- ^ Modify a 'CRef' using a compare-and-swap.
  | WriteRef CRefId
  -- ^ Write to a 'CRef' without synchronising.
  | CasRef CRefId Bool
  -- ^ Attempt to to a 'CRef' using a compare-and-swap, synchronising
  -- it.
  | CommitRef ThreadId CRefId
  -- ^ Commit the last write to the given 'CRef' by the given thread,
  -- so that all threads can see the updated value.
  | STM TTrace [ThreadId]
  -- ^ An STM transaction was executed, possibly waking up some
  -- threads.
  | BlockedSTM TTrace
  -- ^ Got blocked in an STM transaction.
  | Catching
  -- ^ Register a new exception handler
  | PopCatching
  -- ^ Pop the innermost exception handler from the stack.
  | Throw
  -- ^ Throw an exception.
  | ThrowTo ThreadId
  -- ^ Throw an exception to a thread.
  | BlockedThrowTo ThreadId
  -- ^ Get blocked on a 'throwTo'.
  | Killed
  -- ^ Killed by an uncaught exception.
  | SetMasking Bool MaskingState
  -- ^ Set the masking state. If 'True', this is being used to set the
  -- masking state to the original state in the argument passed to a
  -- 'mask'ed function.
  | ResetMasking Bool MaskingState
  -- ^ Return to an earlier masking state.  If 'True', this is being
  -- used to return to the state of the masked block in the argument
  -- passed to a 'mask'ed function.
  | Lift
  -- ^ Lift an action from the underlying monad. Note that the
  -- penultimate action in a trace will always be a @Lift@, this is an
  -- artefact of how the runner works.
  | Return
  -- ^ A 'return' or 'pure' action was executed.
  | KnowsAbout
  -- ^ A '_concKnowsAbout' annotation was processed.
  | Forgets
  -- ^ A '_concForgets' annotation was processed.
  | AllKnown
  -- ^ A '_concALlKnown' annotation was processed.
  | Stop
  -- ^ Cease execution and terminate.
  deriving (Eq, Show)

instance NFData ThreadAction where
  rnf (Fork t) = rnf t
  rnf (GetNumCapabilities i) = rnf i
  rnf (SetNumCapabilities i) = rnf i
  rnf (NewVar c) = rnf c
  rnf (PutVar c ts) = rnf (c, ts)
  rnf (BlockedPutVar c) = rnf c
  rnf (TryPutVar c b ts) = rnf (c, b, ts)
  rnf (ReadVar c) = rnf c
  rnf (BlockedReadVar c) = rnf c
  rnf (TakeVar c ts) = rnf (c, ts)
  rnf (BlockedTakeVar c) = rnf c
  rnf (TryTakeVar c b ts) = rnf (c, b, ts)
  rnf (NewRef c) = rnf c
  rnf (ReadRef c) = rnf c
  rnf (ReadRefCas c) = rnf c
  rnf (PeekTicket c) = rnf c
  rnf (ModRef c) = rnf c
  rnf (ModRefCas c) = rnf c
  rnf (WriteRef c) = rnf c
  rnf (CasRef c b) = rnf (c, b)
  rnf (CommitRef t c) = rnf (t, c)
  rnf (STM s ts) = rnf (s, ts)
  rnf (BlockedSTM s) = rnf s
  rnf (ThrowTo t) = rnf t
  rnf (BlockedThrowTo t) = rnf t
  rnf (SetMasking b m) = b `seq` m `seq` ()
  rnf (ResetMasking b m) = b `seq` m `seq` ()
  rnf a = a `seq` ()

-- | Check if a @ThreadAction@ immediately blocks.
isBlock :: ThreadAction -> Bool
isBlock (BlockedThrowTo  _) = True
isBlock (BlockedTakeVar _) = True
isBlock (BlockedReadVar _) = True
isBlock (BlockedPutVar  _) = True
isBlock (BlockedSTM _) = True
isBlock _ = False

-- | A trace of an STM transaction is just a list of actions that
-- occurred, as there are no scheduling decisions to make.
type TTrace = [TAction]

-- | All the actions that an STM transaction can perform.
data TAction =
    TNew
  -- ^ Create a new @CTVar@
  | TRead  CTVarId
  -- ^ Read from a @CTVar@.
  | TWrite CTVarId
  -- ^ Write to a @CTVar@.
  | TRetry
  -- ^ Abort and discard effects.
  | TOrElse TTrace (Maybe TTrace)
  -- ^ Execute a transaction until it succeeds (@STMStop@) or aborts
  -- (@STMRetry@) and, if it aborts, execute the other transaction.
  | TThrow
  -- ^ Throw an exception, abort, and discard effects.
  | TCatch TTrace (Maybe TTrace)
  -- ^ Execute a transaction until it succeeds (@STMStop@) or aborts
  -- (@STMThrow@). If the exception is of the appropriate type, it is
  -- handled and execution continues; otherwise aborts, propagating
  -- the exception upwards.
  | TStop
  -- ^ Terminate successfully and commit effects.
  | TLift
  -- ^ Lifts an action from the underlying monad. Note that the
  -- penultimate action in a trace will always be a @Lift@, this is an
  -- artefact of how the runner works.
  deriving (Eq, Show)

instance NFData TAction where
  rnf (TRead  v) = rnf v
  rnf (TWrite v) = rnf v
  rnf (TCatch  s m) = rnf (s, m)
  rnf (TOrElse s m) = rnf (s, m)
  rnf a = a `seq` ()

-- | A one-step look-ahead at what a thread will do next.
data Lookahead =
    WillFork
  -- ^ Will start a new thread.
  | WillMyThreadId
  -- ^ Will get the 'ThreadId'.
  | WillGetNumCapabilities
  -- ^ Will get the number of Haskell threads that can run
  -- simultaneously.
  | WillSetNumCapabilities Int
  -- ^ Will set the number of Haskell threads that can run
  -- simultaneously.
  | WillYield
  -- ^ Will yield the current thread.
  | WillNewVar
  -- ^ Will create a new 'CVar'.
  | WillPutVar CVarId
  -- ^ Will put into a 'CVar', possibly waking up some threads.
  | WillTryPutVar CVarId
  -- ^ Will try to put into a 'CVar', possibly waking up some threads.
  | WillReadVar CVarId
  -- ^ Will read from a 'CVar'.
  | WillTakeVar CVarId
  -- ^ Will take from a 'CVar', possibly waking up some threads.
  | WillTryTakeVar CVarId
  -- ^ Will try to take from a 'CVar', possibly waking up some threads.
  | WillNewRef
  -- ^ Will create a new 'CRef'.
  | WillReadRef CRefId
  -- ^ Will read from a 'CRef'.
  | WillPeekTicket CRefId
  -- ^ Will extract the value from a 'Ticket'.
  | WillReadRefCas CRefId
  -- ^ Will read from a 'CRef' for a future compare-and-swap.
  | WillModRef CRefId
  -- ^ Will modify a 'CRef'.
  | WillModRefCas CRefId
  -- ^ Will nodify a 'CRef' using a compare-and-swap.
  | WillWriteRef CRefId
  -- ^ Will write to a 'CRef' without synchronising.
  | WillCasRef CRefId
  -- ^ Will attempt to to a 'CRef' using a compare-and-swap,
  -- synchronising it.
  | WillCommitRef ThreadId CRefId
  -- ^ Will commit the last write by the given thread to the 'CRef'.
  | WillSTM
  -- ^ Will execute an STM transaction, possibly waking up some
  -- threads.
  | WillCatching
  -- ^ Will register a new exception handler
  | WillPopCatching
  -- ^ Will pop the innermost exception handler from the stack.
  | WillThrow
  -- ^ Will throw an exception.
  | WillThrowTo ThreadId
  -- ^ Will throw an exception to a thread.
  | WillSetMasking Bool MaskingState
  -- ^ Will set the masking state. If 'True', this is being used to
  -- set the masking state to the original state in the argument
  -- passed to a 'mask'ed function.
  | WillResetMasking Bool MaskingState
  -- ^ Will return to an earlier masking state.  If 'True', this is
  -- being used to return to the state of the masked block in the
  -- argument passed to a 'mask'ed function.
  | WillLift
  -- ^ Will lift an action from the underlying monad. Note that the
  -- penultimate action in a trace will always be a @Lift@, this is an
  -- artefact of how the runner works.
  | WillReturn
  -- ^ Will execute a 'return' or 'pure' action.
  | WillKnowsAbout
  -- ^ Will process a '_concKnowsAbout' annotation.
  | WillForgets
  -- ^ Will process a '_concForgets' annotation.
  | WillAllKnown
  -- ^ Will process a '_concALlKnown' annotation.
  | WillStop
  -- ^ Will cease execution and terminate.
  deriving (Eq, Show)

instance NFData Lookahead where
  rnf (WillSetNumCapabilities i) = rnf i
  rnf (WillPutVar c) = rnf c
  rnf (WillTryPutVar c) = rnf c
  rnf (WillReadVar c) = rnf c
  rnf (WillTakeVar c) = rnf c
  rnf (WillTryTakeVar c) = rnf c
  rnf (WillReadRef c) = rnf c
  rnf (WillReadRefCas c) = rnf c
  rnf (WillPeekTicket c) = rnf c
  rnf (WillModRef c) = rnf c
  rnf (WillModRefCas c) = rnf c
  rnf (WillWriteRef c) = rnf c
  rnf (WillCasRef c) = rnf c
  rnf (WillCommitRef t c) = rnf (t, c)
  rnf (WillThrowTo t) = rnf t
  rnf (WillSetMasking b m) = b `seq` m `seq` ()
  rnf (WillResetMasking b m) = b `seq` m `seq` ()
  rnf l = l `seq` ()

-- | Look as far ahead in the given continuation as possible.
lookahead :: Action n r s -> NonEmpty Lookahead
lookahead = unsafeToNonEmpty . lookahead' where
  lookahead' (AFork _ _ _)           = [WillFork]
  lookahead' (AMyTId _)              = [WillMyThreadId]
  lookahead' (AGetNumCapabilities _) = [WillGetNumCapabilities]
  lookahead' (ASetNumCapabilities i k) = WillSetNumCapabilities i : lookahead' k
  lookahead' (ANewVar _ _)           = [WillNewVar]
  lookahead' (APutVar (CVar c _) _ k)    = WillPutVar c : lookahead' k
  lookahead' (ATryPutVar (CVar c _) _ _) = [WillTryPutVar c]
  lookahead' (AReadVar (CVar c _) _)     = [WillReadVar c]
  lookahead' (ATakeVar (CVar c _) _)     = [WillTakeVar c]
  lookahead' (ATryTakeVar (CVar c _) _)  = [WillTryTakeVar c]
  lookahead' (ANewRef _ _ _)         = [WillNewRef]
  lookahead' (AReadRef (CRef r _) _)     = [WillReadRef r]
  lookahead' (AReadRefCas (CRef r _) _)  = [WillReadRefCas r]
  lookahead' (APeekTicket (Ticket r _ _) _) = [WillPeekTicket r]
  lookahead' (AModRef (CRef r _) _ _)    = [WillModRef r]
  lookahead' (AModRefCas (CRef r _) _ _) = [WillModRefCas r]
  lookahead' (AWriteRef (CRef r _) _ k) = WillWriteRef r : lookahead' k
  lookahead' (ACasRef (CRef r _) _ _ _) = [WillCasRef r]
  lookahead' (ACommit t c)           = [WillCommitRef t c]
  lookahead' (AAtom _ _)             = [WillSTM]
  lookahead' (AThrow _)              = [WillThrow]
  lookahead' (AThrowTo tid _ k)      = WillThrowTo tid : lookahead' k
  lookahead' (ACatching _ _ _)       = [WillCatching]
  lookahead' (APopCatching k)        = WillPopCatching : lookahead' k
  lookahead' (AMasking ms _ _)       = [WillSetMasking False ms]
  lookahead' (AResetMask b1 b2 ms k) = (if b1 then WillSetMasking else WillResetMasking) b2 ms : lookahead' k
  lookahead' (ALift _)               = [WillLift]
  lookahead' (AKnowsAbout _ k)       = WillKnowsAbout : lookahead' k
  lookahead' (AForgets _ k)          = WillForgets : lookahead' k
  lookahead' (AAllKnown k)           = WillAllKnown : lookahead' k
  lookahead' (AYield k)              = WillYield : lookahead' k
  lookahead' (AReturn k)             = WillReturn : lookahead' k
  lookahead' AStop                   = [WillStop]

-- | Check if an operation could enable another thread.
willRelease :: Lookahead -> Bool
willRelease WillFork = True
willRelease WillYield = True
willRelease (WillPutVar _) = True
willRelease (WillTryPutVar _) = True
willRelease (WillReadVar _) = True
willRelease (WillTakeVar _) = True
willRelease (WillTryTakeVar _) = True
willRelease WillSTM = True
willRelease WillThrow = True
willRelease (WillSetMasking _ _) = True
willRelease (WillResetMasking _ _) = True
willRelease WillStop = True
willRelease _ = False

-- | A simplified view of the possible actions a thread can perform.
data ActionType =
    UnsynchronisedRead  CRefId
  -- ^ A 'readCRef' or a 'readForCAS'.
  | UnsynchronisedWrite CRefId
  -- ^ A 'writeCRef'.
  | UnsynchronisedOther
  -- ^ Some other action which doesn't require cross-thread
  -- communication.
  | PartiallySynchronisedCommit CRefId
  -- ^ A commit.
  | PartiallySynchronisedWrite  CRefId
  -- ^ A 'casCRef'
  | PartiallySynchronisedModify CRefId
  -- ^ A 'modifyCRefCAS'
  | SynchronisedModify  CRefId
  -- ^ An 'atomicModifyCRef'.
  | SynchronisedRead    CVarId
  -- ^ A 'readCVar' or 'takeCVar' (or @try@/@blocked@ variants).
  | SynchronisedWrite   CVarId
  -- ^ A 'putCVar' (or @try@/@blocked@ variant).
  | SynchronisedOther
  -- ^ Some other action which does require cross-thread
  -- communication.
  deriving (Eq, Show)

instance NFData ActionType where
  rnf (UnsynchronisedRead  r) = rnf r
  rnf (UnsynchronisedWrite r) = rnf r
  rnf (PartiallySynchronisedCommit r) = rnf r
  rnf (PartiallySynchronisedWrite  r) = rnf r
  rnf (PartiallySynchronisedModify  r) = rnf r
  rnf (SynchronisedModify  r) = rnf r
  rnf (SynchronisedRead    c) = rnf c
  rnf (SynchronisedWrite   c) = rnf c
  rnf a = a `seq` ()

-- | Check if an action imposes a write barrier.
isBarrier :: ActionType -> Bool
isBarrier (SynchronisedModify _) = True
isBarrier (SynchronisedRead   _) = True
isBarrier (SynchronisedWrite  _) = True
isBarrier SynchronisedOther = True
isBarrier _ = False

-- | Check if an action is synchronises a given 'CRef'.
synchronises :: ActionType -> CRefId -> Bool
synchronises (PartiallySynchronisedCommit c) r = c == r
synchronises (PartiallySynchronisedWrite  c) r = c == r
synchronises (PartiallySynchronisedModify c) r = c == r
synchronises a _ = isBarrier a

-- | Get the 'CRef' affected.
crefOf :: ActionType -> Maybe CRefId
crefOf (UnsynchronisedRead  r) = Just r
crefOf (UnsynchronisedWrite r) = Just r
crefOf (SynchronisedModify  r) = Just r
crefOf (PartiallySynchronisedCommit r) = Just r
crefOf (PartiallySynchronisedWrite  r) = Just r
crefOf (PartiallySynchronisedModify r) = Just r
crefOf _ = Nothing

-- | Get the 'CVar' affected.
cvarOf :: ActionType -> Maybe CVarId
cvarOf (SynchronisedRead  c) = Just c
cvarOf (SynchronisedWrite c) = Just c
cvarOf _ = Nothing

-- | Throw away information from a 'ThreadAction' and give a
-- simplified view of what is happening.
--
-- This is used in the SCT code to help determine interesting
-- alternative scheduling decisions.
simplify :: ThreadAction -> ActionType
simplify (PutVar c _)       = SynchronisedWrite c
simplify (BlockedPutVar c)  = SynchronisedWrite c
simplify (TryPutVar c _ _)  = SynchronisedWrite c
simplify (ReadVar c)        = SynchronisedRead c
simplify (BlockedReadVar c) = SynchronisedRead c
simplify (TakeVar c _)      = SynchronisedRead c
simplify (BlockedTakeVar c) = SynchronisedRead c
simplify (TryTakeVar c _ _) = SynchronisedRead c
simplify (ReadRef r)     = UnsynchronisedRead r
simplify (ReadRefCas r)  = UnsynchronisedRead r
simplify (ModRef r)      = SynchronisedModify r
simplify (ModRefCas r)   = PartiallySynchronisedModify r
simplify (WriteRef r)    = UnsynchronisedWrite r
simplify (CasRef r _)    = PartiallySynchronisedWrite r
simplify (CommitRef _ r) = PartiallySynchronisedCommit r
simplify (STM _ _)          = SynchronisedOther
simplify (BlockedSTM _)     = SynchronisedOther
simplify (ThrowTo _)        = SynchronisedOther
simplify (BlockedThrowTo _) = SynchronisedOther
simplify _ = UnsynchronisedOther

-- | Variant of 'simplify' that takes a 'Lookahead'.
simplify' :: Lookahead -> ActionType
simplify' (WillPutVar c)     = SynchronisedWrite c
simplify' (WillTryPutVar c)  = SynchronisedWrite c
simplify' (WillReadVar c)    = SynchronisedRead c
simplify' (WillTakeVar c)    = SynchronisedRead c
simplify' (WillTryTakeVar c) = SynchronisedRead c
simplify' (WillReadRef r)     = UnsynchronisedRead r
simplify' (WillReadRefCas r)  = UnsynchronisedRead r
simplify' (WillModRef r)      = SynchronisedModify r
simplify' (WillModRefCas r)   = PartiallySynchronisedModify r
simplify' (WillWriteRef r)    = UnsynchronisedWrite r
simplify' (WillCasRef r)      = PartiallySynchronisedWrite r
simplify' (WillCommitRef _ r) = PartiallySynchronisedCommit r
simplify' WillSTM         = SynchronisedOther
simplify' (WillThrowTo _) = SynchronisedOther
simplify' _ = UnsynchronisedOther

--------------------------------------------------------------------------------
-- * Failures

-- | An indication of how a concurrent computation failed.
data Failure =
    InternalError
  -- ^ Will be raised if the scheduler does something bad. This should
  -- never arise unless you write your own, faulty, scheduler! If it
  -- does, please file a bug report.
  | Abort
  -- ^ The scheduler chose to abort execution. This will be produced
  -- if, for example, all possible decisions exceed the specified
  -- bounds (there have been too many pre-emptions, the computation
  -- has executed for too long, or there have been too many yields).
  | Deadlock
  -- ^ The computation became blocked indefinitely on @CVar@s.
  | STMDeadlock
  -- ^ The computation became blocked indefinitely on @CTVar@s.
  | UncaughtException
  -- ^ An uncaught exception bubbled to the top of the computation.
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

instance NFData Failure where
  rnf f = f `seq` () -- WHNF == NF

-- | Pretty-print a failure
showFail :: Failure -> String
showFail Abort = "[abort]"
showFail Deadlock = "[deadlock]"
showFail STMDeadlock = "[stm-deadlock]"
showFail InternalError = "[internal-error]"
showFail UncaughtException = "[exception]"

--------------------------------------------------------------------------------
-- * Memory Models

-- | The memory model to use for non-synchronised 'CRef' operations.
data MemType =
    SequentialConsistency
  -- ^ The most intuitive model: a program behaves as a simple
  -- interleaving of the actions in different threads. When a 'CRef'
  -- is written to, that write is immediately visible to all threads.
  | TotalStoreOrder
  -- ^ Each thread has a write buffer. A thread sees its writes
  -- immediately, but other threads will only see writes when they are
  -- committed, which may happen later. Writes are committed in the
  -- same order that they are created.
  | PartialStoreOrder
  -- ^ Each 'CRef' has a write buffer. A thread sees its writes
  -- immediately, but other threads will only see writes when they are
  -- committed, which may happen later. Writes to different 'CRef's
  -- are not necessarily committed in the same order that they are
  -- created.
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

instance NFData MemType where
  rnf m = m `seq` () -- WHNF == NF
