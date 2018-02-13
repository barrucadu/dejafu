{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

{-
The monad-par package:
https://hackage.haskell.org/package/monad-par

This is the code from Control.Monad.Par.Scheds.DirectInternal, with
CPP expanded in its default configuration, modified to use MonadConc.

- - - - -

Copyright Simon Marlow, Ryan Newton 2011

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the authors nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

-- | Type definiton and some helpers.  This is used mainly by
-- Direct.hs but can also be used by other modules that want access to
-- the internals of the scheduler (i.e. the private `Par` type constructor).

module Examples.ParMonad.DirectInternal where

import           "mtl" Control.Monad.Cont    as C
import qualified "mtl" Control.Monad.Reader  as RD

import qualified System.Random.MWC           as Random

import           Control.Concurrent.Classy
import           Data.Concurrent.Deque.Class (WSDeque)
import qualified Data.Set                    as S
import           Data.Word                   (Word64)


-- Our monad stack looks like this:
--      ---------
--        ContT
--       ReaderT
--         IO
--      ---------
-- The ReaderT monad is there for retrieving the scheduler given the
-- fact that the API calls do not get it as an argument.
--
-- Note that the result type for continuations is unit.  Forked
-- computations return nothing.
--
newtype Par m a = Par { unPar :: C.ContT () (ROnly m) a }
    deriving (Functor, Applicative, Monad, MonadCont, RD.MonadReader (Sched m))
type ROnly m = RD.ReaderT (Sched m) m

type SessionID = Word64

-- An ID along with a flag to signal completion:
data Session m = Session SessionID (HotVar m Bool)

data Sched m = Sched
    {
      ---- Per worker ----
      no       :: {-# UNPACK #-} !Int,
      workpool :: WSDeque (Par m ()),
      rng      :: HotVar m Random.GenIO, -- Random number gen for work stealing.
      isMain :: Bool, -- Are we the main/master thread?

      -- The stack of nested sessions that THIS worker is participating in.
      -- When a session finishes, the worker can return to its Haskell
      -- calling context (it's "real" continuation).
      sessions :: HotVar m [Session m],
      -- (1) This is always non-empty, containing at least the root
      --     session corresponding to the anonymous system workers.
      -- (2) The original invocation of runPar also counts as a session
      --     and pushes a second
      -- (3) Nested runPar invocations may push further sessions onto the stack.

      ---- Global data: ----
      idle     :: HotVar m [MVar m Bool], -- waiting idle workers
      scheds   :: [Sched m],            -- A global list of schedulers.

      -- Any thread that enters runPar (original or nested) registers
      -- itself in this global list.  When the list becomes null,
      -- worker threads may shut down or at least go idle.
      activeSessions :: HotVar m (S.Set SessionID),

      -- A counter to support unique session IDs:
      sessionCounter :: HotVar m SessionID
     }


--------------------------------------------------------------------------------
-- Helpers #1:  Atomic Variables
--------------------------------------------------------------------------------
-- TEMP: Experimental

newHotVar      :: MonadConc m => a -> m (HotVar m a)
modifyHotVar   :: MonadConc m => HotVar m a -> (a -> (a,b)) -> m b
modifyHotVar_  :: MonadConc m => HotVar m a -> (a -> a) -> m ()
writeHotVar    :: MonadConc m => HotVar m a -> a -> m ()
readHotVar     :: MonadConc m => HotVar m a -> m a
readHotVarRaw  :: MonadConc m => HotVar m a -> m a
writeHotVarRaw :: MonadConc m => HotVar m a -> a -> m ()

{-# INLINE newHotVar     #-}
{-# INLINE modifyHotVar  #-}
{-# INLINE modifyHotVar_ #-}
{-# INLINE readHotVar    #-}
{-# INLINE writeHotVar   #-}

type HotVar m a = CRef m a
newHotVar     = newCRef
modifyHotVar  = atomicModifyCRef
modifyHotVar_ v fn = atomicModifyCRef v (\a -> (fn a, ()))
readHotVar    = readCRef
writeHotVar   = writeCRef

readHotVarRaw  = readHotVar
writeHotVarRaw = writeHotVar
