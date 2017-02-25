dejafu [![Build Status][build-status]][build-log]
======

[build-status]: https://travis-ci.org/barrucadu/dejafu.svg?branch=master
[build-log]:    https://travis-ci.org/barrucadu/dejafu

> [Déjà Fu is] A martial art in which the user's limbs move in time as
> well as space, […] It is best described as "the feeling that you
> have been kicked in the head this way before"
>
> -- Terry Pratchett, Thief of Time

Have you ever written a concurrent Haskell program and then, Heaven
forbid, wanted to *test* it? Testing concurrency is normally a hard
problem, because of the nondeterminism of scheduling: you can run your
program ten times and get ten different results if you're unlucky.

There is a solution! Through these libraries, you can write concurrent
programs and test them *deterministically*. By abstracting out the
actual implementation of concurrency through a typeclass, an
alternative implementation can be used for testing, allowing the
*systematic* exploration of the possible results of your program.


Table of Contents
-----------------

- [Packages](#packages)
- [Concurrent Programming](#getting-started)
  - [Asynchronous IO](#asynchronous-io)
  - [Threads and MVars](#threads-and-mvars)
  - [Software Transactional Memory](#software-transactional-memory)
  - [Relaxed Memory and IORefs](#relaxed-memory-and-iorefs)
  - [Program Testing](#program-testign)
  - [Porting](#porting)
- [Contributing](#contributing)
- [Bibliography](#bibliography)


Packages
--------

|     | Version | Intended Users | Summary |
| --- | ------- | -------------- | ------- |
| async-dejafu [[docs][d:async]]  [[hackage][h:async]]  | 0.1.3.0 | Authors | Run MonadConc operations asynchronously and wait for their results. |
| concurrency  [[docs][d:conc]]   [[hackage][h:conc]]   | 1.1.0.0 | Authors | Typeclasses, functions, and data types for concurrency and STM. |
| dejafu       [[docs][d:dejafu]] [[hackage][h:dejafu]] | 0.5.1.1 | Testers | Systematic testing for Haskell concurrency. |
| hunit-dejafu [[docs][d:hunit]]  [[hackage][h:hunit]]  | 0.4.0.0 | Testers | Deja Fu support for the HUnit test framework. |
| tasty-dejafu [[docs][d:tasty]]  [[hackage][h:tasty]]  | 0.4.0.0 | Testers | Deja Fu support for the Tasty test framework. |

Each package has its own README in its subdirectory.

There is also dejafu-tests, the test suite for dejafu. This is in a
separate package due to Cabal being bad with test suite transitive
dependencies.

[d:conc]:   https://docs.barrucadu.co.uk/concurrency/
[d:dejafu]: https://docs.barrucadu.co.uk/dejafu/
[d:async]:  https://docs.barrucadu.co.uk/async-dejafu/
[d:hunit]:  https://docs.barrucadu.co.uk/hunit-dejafu/
[d:tasty]:  https://docs.barrucadu.co.uk/tasty-dejafu/

[h:conc]:   https://hackage.haskell.org/package/concurrency
[h:dejafu]: https://hackage.haskell.org/package/dejafu
[h:async]:  https://hackage.haskell.org/package/async-dejafu
[h:hunit]:  https://hackage.haskell.org/package/hunit-dejafu
[h:tasty]:  https://hackage.haskell.org/package/tasty-dejafu


Concurrent Programming
----------------------

You should read [Parallel and Concurrent Programming in Haskell][parconc],
by Simon Marlow. It's very good, and the API of the *async-dejafu* and
*concurrency* packages is intentionally kept very similar to the
standard packages, so all the knowledge transfers.

[parconc]: http://chimera.labs.oreilly.com/books/1230000000929/

### Asynchronous IO

The wonderful *[async][]* package by Simon Marlow greatly eases the
difficulty of writing programs which merely need to perform some
asynchronous IO. The *async-dejafu* package provides an almost-total
reimplementation of *async*, using the abstractions provided by
*concurrency*.

For example, assuming a suitable `getURL` function, to fetch the
contents of two web pages at the same time:

```haskell
withAsync (getURL url1) $ \a1 -> do
  withAsync (getURL url2) $ \a2 -> do
    page1 <- wait a1
    page2 <- wait a2
    -- ...
```

The `withAsync` function starts an operation in a separate thread, and
kills it if the inner action finishes before it completes.

Another example, this time waiting for any of a number of web pages to
download, and cancelling the others as soon as one completes:

```haskell
let download url = do
      res <- getURL url
      pure (url, res)

downloads <- mapM (async . download) urls
(url, res) <- waitAnyCancel downloads
printf "%s was first (%d bytes)\n" url (B.length res)
```

The `async` function starts an operation in another thread but, unlike
`withAsync` takes no inner action to execute: the programmer needs to
make sure the computation is waited for or cancelled as appropriate.

[async]: http://hackage.haskell.org/package/async

### Threads and MVars

The fundamental unit of concurrency is the thread, and the most basic
communication mechanism is the `MVar`:

```haskell
main = do
  var <- newEmptyMVar
  fork $ putMVar var 'x'
  fork $ putMVar var 'y'
  r <- takeMVar m
  print r
```

The `fork` function starts executing a `MonadConc` action in a
separate thread, and `takeMVar`/`putMVar` are used to communicate
values (`newEmptyMVar` just makes an `MVar` with nothing in it). This
will either print `'x'` or `'y'`, depending on which of the two
threads "wins".

On top of the simple `MVar`, we can build more complicated concurrent
data structures, like channels. A collection of these are provided in
the *concurrency* package.

If a thread attempts to read from an `MVar` which is never written to,
or write to an `MVar` which is never read from, it blocks.

### Software Transactional Memory

Software transactional memory (STM) simplifies stateful concurrent
programming by allowing complex atomic state operations. Whereas only
one `MVar` can be modified atomically at a time, any number of `TVar`s
can be. STM is normally provided by the *stm* package, but the
*concurrency* package exposes it directly.

For example, we can swap the values of two variables, and read them in
another thread:

```haskell
main = do
  var1 <- newTVar 'x'
  var2 <- newTVar 'y'
  fork . atomically $ do
    a <- readTVar var1
    b <- readTVar var2
    writeTVar var2 a
    writeTVar var1 b
  a <- atomically $ readTVar var1
  b <- atomically $ readTVar var2
  print (a, b)
```

Even though the reads and writes appear to be done in multiple steps
inside the forked thread, the entire transaction is executed in a
single step, by the `atomically` function. This means that the main
thread will observe the values `('x', 'y')` or `('y', 'x')`, it can
never get `('x', 'x')` as naive `MVar` implementation would.

### Relaxed Memory and CRefs

There is a third type of communication primitive, the `CRef` (known in
normal Haskell as the `IORef`). These do not impose synchronisation,
and so the behaviour of concurrent reads and writes depends on the
memory model of the underlying processor.

```haskell
crefs = do
  r1 <- newCRef False
  r2 <- newCRef False
  x <- spawn $ writeCRef r1 True >> readCRef r2
  y <- spawn $ writeCRef r2 True >> readCRef r1
  (,) <$> readMVar x <*> readMVar y
```

Here `spawn` forks a thread and gives an `MVar` which can be read from
to get the return value. Under a sequentially consistent memory model,
there are three possible results: `(True, True)`, `(True, False)`, and
`(False, True)`. Under the relaxed memory model of modern processors,
the result `(False, False)` is also possible. Relaxed memory models
allow for reads and writes to be re-ordered between threads.

For testing, three memory models are supported (with the default being
TSO):

- **Sequential Consistency:** A program behaves as a simple
    interleaving of the actions in different threads. When a `CRef` is
    written to, that write is immediately visible to all threads.

- **Total Store Order (TSO):** Each thread has a write buffer. A
    thread sees its writes immediately, but other threads will only
    see writes when they are committed, which may happen later. Writes
    are committed in the same order that they are created.

- **Partial Store Order (PSO):** Each `CRef` has a write buffer. A
    thread sees its writes immediately, but other threads will only
    see writes when they are committed, which may happen later. Writes
    to different `CRefs` are not necessarily committed in the same
    order that they are created.

### Program Testing

If you just write your concurrent program using the `MonadConc` and
`MonadSTM` typeclasses (maybe with `MonadIO` if you need `IO` as
well), then it is testable with dejafu!

Testing is similar to unit testing, the programmer produces a
self-contained monadic action to execute. It is run under many
schedules, and the results gathered into a list. This is a little
different from normal unit testing, where you just return "true" or
"false", here you have *many* results, and need to decide if the
collection is valid or not.

For the simple cases, where you just want to check something is
deterministic and doesn't deadlock, there is a handy `autocheck`
function. For example:

```haskell
example = do
  a <- newEmptyMVar
  b <- newEmptyMVar

  c <- newMVar 0

  let lock m = putMVar m ()
  let unlock = takeMVar

  j1 <- spawn $ lock a >> lock b >> modifyMVar_ c (return . succ) >> unlock b >> unlock a
  j2 <- spawn $ lock b >> lock a >> modifyMVar_ c (return . pred) >> unlock a >> unlock b

  takeMVar j1
  takeMVar j2

  takeMVar c
```

The correct result is 0, as it starts out as 0 and is incremented and
decremented by threads 1 and 2, respectively. However, note the order
of acquisition of the locks in the two threads. If thread 2 pre-empts
thread 1 between the acquisition of the locks (or if thread 1
pre-empts thread 2), a deadlock situation will arise, as thread 1 will
have lock a and be waiting on b, and thread 2 will have b and be
waiting on a.

Here is what `autocheck` has to say about it:

```
> autocheck example
[fail] Never Deadlocks (checked: 5)
        [deadlock] S0------------S1-P2--S1-
[pass] No Exceptions (checked: 12)
[fail] Consistent Result (checked: 11)
        0 S0------------S2-----------------S1-----------------S0----

        [deadlock] S0------------S1-P2--S1-
False
```

It identifies the deadlock, and also the possible results the
computation can produce, and displays a simplified trace leading to
each failing outcome. The traces contain thread numbers, which the
programmer can give a thread a name when forking. It also returns
false as there are test failures.

Note that if your test case does `IO`, the `IO` will be executed a lot
of times. It needs to be deterministic enough to not invalidate the
results of testing. That may seem a burden, but it's a requirement of
any form of testing.

### Porting

As a general rule of thumb, to convert some existing code to work with
dejafu:

- Depend on "concurrency".
- Import `Control.Concurrent.Classy.*` instead of `Control.Concurrent.*`
- Change `IO a` to `MonadConc m => m a`
- Change `STM a` to `MonadSTM stm => stm a`
- Parameterise all the types by the monad: `MVar` -> `MVar m`, `TVar`
  -> `TVar stm`, `IORef` -> `CRef m`, etc
- Fix the type errors.


Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).


Bibliography
------------

These libraries wouldn't be possible without prior research, which I
mention in the documentation. Haddock comments get the full citation,
whereas in-line comments just get the shortened name:

- [BPOR] *Bounded partial-order reduction*, K. Coons, M. Musuvathi,
  and K. McKinley (2013)
  http://research.microsoft.com/pubs/202164/bpor-oopsla-2013.pdf

- [RDPOR] *Dynamic Partial Order Reduction for Relaxed Memory Models*,
  N. Zhang, M. Kusano, and C. Wang (2015)
  http://www.faculty.ece.vt.edu/chaowang/pubDOC/ZhangKW15.pdf

- [Empirical] *Concurrency Testing Using Schedule Bounding: an
  Empirical Study*, P. Thompson, A. Donaldson, and A. Betts (2014)
  http://www.doc.ic.ac.uk/~afd/homepages/papers/pdfs/2014/PPoPP.pdf

- [RMMVerification] *On the Verification of Programs on Relaxed Memory
  Models*, A. Linden (2014)
  https://orbi.ulg.ac.be/bitstream/2268/158670/1/thesis.pdf

There are also a couple of papers on dejafu itself:

- *Déjà Fu: A Concurrency Testing Library for Haskell*, M. Walker and
  C. Runciman (2015)
  https://www.barrucadu.co.uk/publications/dejafu-hs15.pdf

  This details dejafu-0.1, and was presented at the 2015 Haskell
  Symposium.

- *Déjà Fu: A Concurrency Testing Library for Haskell*, M. Walker and
  C. Runciman (2016)
  https://www.barrucadu.co.uk/publications/YCS-2016-503.pdf

  This is a more in-depth technical report, written between the
  dejafu-0.2 and dejafu-0.3 releases.
