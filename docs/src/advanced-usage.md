Advanced Usage
==============

Déjà Fu tries to have a sensible set of defaults, but there are some
times when the defaults are not suitable.  There are a lot of knobs
provided to tweak how things work.


Execution settings
------------------

The `autocheckWithSettings`, `dejafuWithSettings`, and `dejafusWithSettings` let
you provide a `Settings` value, which controls some of Déjà Fu's behaviour:

```haskell
dejafuWithSettings mySettings "Assert the thing holds" myPredicate myAction
```

The available settings are:

- **"Way"**, how to explore the behaviours of the program under test.

- **Length bound**, a cut-off point to terminate an execution even if it's not
  done yet.

- **Memory model**, which affects how non-synchronised operations, such as
  `readIORef` and `writeIORef` behave.

- **Discarding**, which allows throwing away uninteresting results, rather than
  keeping them around in memory.

- **Early exit**, which allows exiting as soon as a result matching a predicate
  is found.

- **Representative traces**, keeping only one execution trace for each distinct
  result.

- **Trace simplification**, rewriting execution traces into a simpler form
  (particularly effective with the random testing).

- **Safe IO**, pruning needless schedules when your IO is only used to manage
  thread-local state.

See the `Test.DejaFu.Settings` module for more information.


Performance tuning
------------------

- Are you happy to trade space for time?

    Consider computing the results once and running multiple predicates over the
    output: this is what `dejafus` / `testDejafus` / etc does.

- Can you sacrifice completeness?

    Consider using the random testing functionality. See the `*WithSettings`
    functions.

- Would strictness help?

    Consider using the strict functions in `Test.DejaFu.SCT` (the ones ending
    with a `'`).

- Do you just want the set of results, and don't care about traces?

    Consider using `Test.DejaFu.SCT.resultsSet`.

- Do you know something about the sort of results you care about?

    Consider discarding results you *don't* care about. See the `*WithSettings`
    functions in `Test.DejaFu`, `Test.DejaFu.SCT`, and
    `Test.{HUnit,Tasty}.DejaFu`.

For example, let's say you want to know if your test case deadlocks, but you
don't care about the execution trace, and you are going to sacrifice
completeness because your possible state-space is huge.  You could do it like
this:

```haskell
dejafuWithSettings
  ( set ldiscard
    -- "efa" == "either failure a", discard everything but deadlocks
    (Just $ \efa -> Just (if either isDeadlock (const False) efa then DiscardTrace else DiscardResultAndTrace))
  . set lway
    -- try 10000 executions with random scheduling
    (randomly (mkStdGen 42) 10000)
  $ defaultSettings
  )
  -- the name of the test
  "Never Deadlocks"
  -- the predicate to check
  deadlocksNever
  -- your test case
  testCase
```
