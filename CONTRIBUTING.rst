Contributing
============

Thanks for caring about Déjà Fu!


Ways to contribute
------------------

Déjà Fu is a project under active development, there's always
something to do.  Here's a list of ideas to get you started:

* Submit bug reports.
* Submit feature requests.
* Got a particularly slow test case which you think should be faster?
  Open an issue for that too.
* Blog about how and why you use Déjà Fu.
* Check if any bugs which have been open for a while are still bugs.

If you want to contribute code, you could:

* Tackle one of the issues tagged `"good first issue"`__.
* Tackle a bigger issue!
* Run code coverage and try to fix a gap in the tests.
* Profile the test suite and try to improve a slow function.

.. __: https://github.com/barrucadu/dejafu/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22

If you have a support question, you can talk to me on IRC (#haskell on
freenode) or send an email (mike@barrucadu.co.uk) rather than opening
an issue.  But maybe your question is a bug report about poor
documentation in disguise!


Making the change
-----------------

1. Talk to me!

   I don't bite, and chances are I can quickly tell you where you
   should start.  It's better to ask what seems like a stupid question
   than to waste a lot of time on the wrong approach.

2. Make the change.

   Figure out what needs to be changed, how to change it, and do it.
   If you're fixing a bug, make sure to add a minimal reproduction to
   Cases.Regressions in dejafu-tests.

3. Document the change.

   All top-level definitions should have a `Haddock`__ comment
   explaining what it does.  If you've added or changed a top-level
   function, consider commenting its arguments too.

   If you've added a top-level definition, or changed one in a
   backwards-incompatible way, add an ``@since unreleased`` Haddock
   comment to it.  This is to help prevent me from missing things when
   I update the changelog ahead of a release.

4. Submit a PR.

   Travis will run some checks, which might prompt further action.
   You should expect a response from me in a day or two.

Don't worry about your PR being perfect the first time.  We'll work
through any issues together, to ensure that Déjà Fu gets the best code
it can.

.. __: https://github.com/aisamanra/haddock-cheatsheet


Coding style
------------

There isn't really a prescribed style.  It's not quite the wild west
though; keep these three rules in mind:

1. Be consistent.
2. Run `stylish-haskell`__ to format import lists.
3. Use `hlint`__ and `weeder`__ and fix lints unless you have a good
   reason not to.

Travis runs stylish-haskell, hlint, and weeder on all PRs.

.. __: https://github.com/jaspervdj/stylish-haskell
.. __: https://github.com/ndmitchell/hlint
.. __: https://github.com/ndmitchell/weeder


Coverage
--------

`hpc`__ can generate a coverage report from the execution of
dejafu-tests:

.. code-block:: none

  $ stack build --coverage
  $ stack exec dejafu-tests
  $ stack hpc report --all dejafu-tests.tix

This will print some stats and generate an HTML coverage report:

.. code-block:: none

  Generating combined report
   52% expressions used (4052/7693)
   48% boolean coverage (63/129)
        43% guards (46/106), 31 always True, 9 always False, 20 unevaluated
        68% 'if' conditions (11/16), 2 always True, 3 unevaluated
        85% qualifiers (6/7), 1 unevaluated
   61% alternatives used (392/635)
   80% local declarations used (210/261)
   26% top-level declarations used (280/1063)
  The combined report is available at /home/barrucadu/projects/dejafu/.stack-work/install/x86_64-linux/nightly-2016-06-20/8.0.1/hpc/combined/custom/hpc_index.html

The highlighted code in the HTML report emphasises branch coverage:

* Red means a branch was evaluated as always false.
* Green means a branch was evaluated as always true.
* Yellow means an expression was never evaluated.

See also the `stack coverage documentation`__.

.. __: https://wiki.haskell.org/Haskell_program_coverage
.. __: https://docs.haskellstack.org/en/latest/coverage/


Performance
-----------

GHC can generate performance statistics from the execution of
dejafu-tests:

.. code-block:: none

  $ stack build --profile
  $ stack exec  -- dejafu-tests +RTS -p
  $ less dejafu-tests.prof

This prints a detailed breakdown of where memory and time are being
spent:

.. code-block:: none

      Mon Mar 20 19:26 2017 Time and Allocation Profiling Report  (Final)

         dejafu-tests +RTS -p -RTS

      total time  =      105.94 secs   (105938 ticks @ 1000 us, 1 processor)
      total alloc = 46,641,766,952 bytes  (excludes profiling overheads)

  COST CENTRE                           MODULE                     %time %alloc

  findBacktrackSteps.doBacktrack.idxs'  Test.DejaFu.SCT.Internal    21.9   12.0
  ==                                    Test.DejaFu.Common          12.4    0.0
  yieldCount.go                         Test.DejaFu.SCT             12.1    0.0
  dependent'                            Test.DejaFu.SCT              5.1    0.0
  runThreads.go                         Test.DejaFu.Conc.Internal    2.7    4.1
  [...]

Be careful, however!  Compiling with profiling can significantly
affect the behaviour of a program!  Use profiling to get an idea of
where the hot spots are, but make sure to confirm with a non-profiled
build that things are actually getting faster.

If you compile with ``-rtsopts`` you can get some basic stats from a
non-profiled build:

.. code-block:: none

  $ stack exec -- dejafu-tests +RTS -s

  [...]
  86,659,658,504 bytes allocated in the heap
  13,057,037,448 bytes copied during GC
      13,346,952 bytes maximum residency (4743 sample(s))
         127,824 bytes maximum slop
              37 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     78860 colls,     0 par   32.659s  32.970s     0.0004s    0.0669s
  Gen  1      4743 colls,     0 par    3.043s   3.052s     0.0006s    0.0086s

  TASKS: 174069 (174065 bound, 4 peak workers (4 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time   98.685s  (101.611s elapsed)
  GC      time   35.702s  ( 36.022s elapsed)
  EXIT    time    0.001s  (  0.007s elapsed)
  Total   time  134.388s  (137.640s elapsed)

  Alloc rate    878,145,635 bytes per MUT second

  Productivity  73.4% of total user, 73.8% of total elapsed
